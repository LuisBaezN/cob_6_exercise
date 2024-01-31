      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date:
      * Purpose: Project learning 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmpNom.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MOVIM ASSIGN TO DISK.
           SELECT EMPINX ASSIGN TO DISK
                           ORGANISATION IS INDEXED
                           ACCESS MODE IS DYNAMIC
                           RECORD KEY IS EMI-NOMI.
           SELECT TMP-FILE ASSIGN TO DISK.
           SELECT EMPREP ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  MOVIM.
       01  MOV-REG.
           02  MOV-NOMI    PIC 9(06).
           02  MOV-CLAV    PIC X(14).
           02  MOV-IMP     PIC 9(05)V99.
           02  FILLER      PIC XX.
       FD  EMPINX.
       01  EMI-REG.
           02  EMI-NOMI    PIC 9(06).
           02  EMI-NOMB    PIC X(15).
           02  EMI-DEPT    PIC X(03).
           02  EMI-SUEL    PIC 9(05)V99.
           02  FILLER      PIC XX.

       SD  TMP-FILE.
       01  TMP-REG.
           02  TMP-DEPT    PIC X(03).
           02  TMP-NOMI    PIC 9(06).
           02  TMP-NOMB    PIC X(15).
           02  TMP-CLAV    PIC X(14).
           02  TMP-PERC    PIC 9(05)V99.
           02  TMP-DEDU    PIC 9(05)V99.

       FD  EMPREP.
       01  EMR-REG         PIC X(132).
       WORKING-STORAGE SECTION.
       77  MOV-EOF         PIC 9.
       77  TMP-EOF         PIC 9.
       77  LIN             PIC 99.
       01  LIMPAG          CONSTANT 15.
       77  PAG             PIC 999.
       77  ANT-DEPT        PIC X(03).
       77  SUEL-EMPL       PIC 9(06)V99.
       77  EMPL-ST         PIC 9(03).
       77  PERC-ST         PIC 9(06)V99.
       77  DEDU-ST         PIC 9(06)V99.
       77  SUEL-ST         PIC 9(06)V99.
       77  ORG-EMPL        PIC 9(05).
       77  ORG-PERC        PIC 9(09)V99.
       77  ORG-DEDU        PIC 9(09)V99.
       77  ORG-SUEL        PIC 9(09)V99.
       77  DEPTS           PIC X(03).
           88 DEPT-VAL  VALUE "ADM", "TEC", "SIS", "RH", "CON", "MER".
       77  CLAVES          PIC X(14).
           88 CLAV      VALUE  "Sueldo        ", "Bono          ",
                               "Puntualidad   ", "Productividad ",
                               "Prestamo      ", "Horas Extras  ",
                               "Impuestos     ", "IMSS          ",
                               "Faltas        ", "Pago prestamo ",
                               "Vales         ".
       77  CLAVES-P        PIC X(14).
           88 CLAV-P    VALUE  "Sueldo        ", "Bono          ",
                               "Puntualidad   ", "Productividad ",
                               "Prestamo      ", "Horas Extras  ".
       77  CLAVES-D        PIC X(14).
           88 CLAV-D    VALUE  "Impuestos     ", "IMSS          ",
                               "Faltas        ", "Pago prestamo ",
                               "Vales         ".
       01  FECHA.
           03 FECHA-AA     PIC 99.
           03 FECHA-MM     PIC 99.
           03 FECHA-DD     PIC 99.
       01 MESES.
           03 FILLER       PIC X(36) VALUE
              "ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC".
       01 MESES-R REDEFINES MESES.
          03 MESES-OC OCCURS 12 TIMES.
             05 MES        PIC X(03).

       01  REPORT-LAYOUT.
      *> 121 CARACTERES
           02 EMS-TIT-0.
               03 FILLER       PIC X(22) VALUE ">>--------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(11) VALUE "---------<<".
           02 EMS-TIT-1.
               03 FILLER       PIC X(05) VALUE "PAG: ".
               03 EMS-TI-PAG   PIC ZZ.
               03 FILLER       PIC X(43) VALUE SPACES.
               03 FILLER       PIC X(21) VALUE "\\ STEFANINI GROUP //".
               03 FILLER       PIC X(39) VALUE SPACES.
               03 EMS-T1-DD    PIC 99.
               03 FILLER       PIC X VALUE "/".
               03 EMS-T1-MM    PIC X(03).
               03 FILLER       PIC X(03) VALUE "/20".
               03 EMS-T1-AA    PIC 99.
           02 EMS-TIT-2.
               03 FILLER       PIC X(47) VALUE SPACES.
               03 FILLER       PIC X(27) VALUE
                                   "REPORTE MENSUAL DE SALARIOS".
       PROCEDURE DIVISION.
       MAIN.
           PERFORM 100-START.
           PERFORM 200-PROCESS.
           PERFORM 300-END.

       100-START.
           DISPLAY "> Running start...".
           PERFORM 101-LOAD-DATE.
           OPEN INPUT MOVIM.
           OPEN I-O EMPINX.
           READ MOVIM.


       200-PROCESS.
           DISPLAY "> Running process...".
           SORT TMP-FILE ON ASCENDING KEY TMP-DEPT
                                          TMP-NOMI
                       INPUT  PROCEDURE 202-LOAD-TMP-FILE
                       OUTPUT PROCEDURE 204-GENERATE-REP.

       300-END.
           DISPLAY "> Running end...".
           CLOSE MOVIM.
           CLOSE EMPINX.
           CLOSE EMPREP.
           STOP RUN.

       101-LOAD-DATE.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-AA         TO EMS-T1-AA.
           MOVE MES(FECHA-MM)    TO EMS-T1-MM.
           MOVE FECHA-DD         TO EMS-T1-DD.

       201-READ-MOVIM.
           READ MOVIM AT END MOVE 1 TO MOV-EOF.

       202-LOAD-TMP-FILE SECTION.
           PERFORM 201-READ-MOVIM
           PERFORM 203-SCAN-MOVIM UNTIL MOV-EOF = 1.
           GO TO END-PROG.

       203-SCAN-MOVIM.
           MOVE MOV-NOMI TO EMI-NOMI.
           READ EMPINX INVALID KEY
                       MOVE ZEROS TO EMI-NOMI
                       MOVE SPACES TO EMI-NOMB.
           MOVE EMI-DEPT TO DEPTS.
           IF (EMI-NOMI=ZEROES) OR (EMI-DEPT=SPACES) OR (NOT DEPT-VAL)
               MOVE "ZER" TO EMI-DEPT.

           MOVE MOV-CLAV TO CLAVES.
           IF NOT CLAV
               MOVE "Invalida      " TO MOV-CLAV.


           MOVE EMI-DEPT TO TMP-DEPT.
           MOVE EMI-NOMI TO TMP-NOMI.
           MOVE EMI-NOMB TO TMP-NOMB.
           MOVE MOV-CLAV TO TMP-CLAV.
           MOVE MOV-CLAV TO CLAVES-P.
           IF CLAV-P
               MOVE MOV-IMP TO TMP-PERC
               MOVE ZEROES  TO TMP-DEDU
           ELSE
               MOVE MOV-IMP TO TMP-DEDU
               MOVE ZEROES  TO TMP-PERC.

           RELEASE TMP-REG.
           PERFORM 201-READ-MOVIM.

      *> VERIFY THIS!
       END-PROG.

       204-GENERATE-REP SECTION.
           OPEN OUTPUT EMPREP.
           PERFORM 205-READ-TMP-FILE UNTIL TMP-EOF = 1.


       205-READ-TMP-FILE.
           RETURN TMP-FILE AT END MOVE 1 TO TMP-EOF.
      *     DISPLAY TMP-REG.



       END PROGRAM EmpNom.
