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
           02  EMI-SUEL    PIC S9(05)V99.
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
       77  I               PIC 9.
       77  NAME-FLAG       PIC 9 VALUE 1.
       77  DEPT-FLAG       PIC 9 VALUE 1.
       77  MOV-EOF         PIC 9.
       77  TMP-EOF         PIC 9.
       77  LIN             PIC 99.
       01  LINLIM          CONSTANT 15.
       77  PAG             PIC 999.
       77  ANT-DEPT        PIC X(03).
       77  ANT-NOMI        PIC 9(06).
       77  SUEL-EMPL       PIC S9(05)V99.
       77  EMPL-ST         PIC 9(04).
       77  PERC-ST         PIC 9(08)V99.
       77  DEDU-ST         PIC 9(08)V99.
       77  SUEL-ST         PIC S9(05)V99.
       77  ORG-EMPL        PIC 9(05).
       77  ORG-PERC        PIC 9(09)V99.
       77  ORG-DEDU        PIC 9(09)V99.
       77  ORG-SUEL        PIC S9(09)V99.
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
       01  MESES.
           03 FILLER       PIC X(36) VALUE
               "ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC".
       01  MESES-R REDEFINES MESES.
           03 MESES-OC OCCURS 12 TIMES.
               05 MES      PIC X(03).
       01  DEPT-NAMES.
           02 DEPT-ROW     OCCURS 7 TIMES.
               03 DEPT-FULL PIC X(16).
       01  DEPT-NAMES-F.
           02 STR-LENGHT-F PIC 99.
           02 STR-F.
               03 CHAR-F   PIC X OCCURS 0 TO 20 TIMES
                           DEPENDING ON STR-LENGHT-F.
       01  DEPT-NAMES-S.
           02 STR-LENGHT-S PIC 9.
           02 STR-S.
               03 CHAR-S   PIC X OCCURS 0 TO 7 TIMES
                           DEPENDING ON STR-LENGHT-S.
       01  REPORT-LAYOUT.
      *> 121 CARACTERES
           02 EMS-TIT-0.
               03 FILLER       PIC X(5) VALUE SPACES.
               03 FILLER       PIC X(22) VALUE ">>--------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(11) VALUE "---------<<".
           02 EMS-TIT-1.
               03 FILLER       PIC X(5) VALUE SPACES.
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
               03 FILLER       PIC X(52) VALUE SPACES.
               03 FILLER       PIC X(27) VALUE
                                   "REPORTE MENSUAL DE SALARIOS".
           02 EMS-TAB-TIT.
      * 115 CARACTERES
               03 FILLER       PIC X(08) VALUE SPACES.
               03 FILLER       PIC XX VALUE "| ".
               03 FILLER       PIC X(16) VALUE "  DEPARTAMENTO  ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(16) VALUE "     NOMINA     ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(16) VALUE "     NOMBRE     ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(16) VALUE "     CLAVE      ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(16) VALUE "  PERCEPCIONES  ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(16) VALUE "   DEDUCCIONES  ".
               03 FILLER       PIC XX VALUE " |".
           02 EMS-TAB-SEP.
               03 FILLER       PIC X(08) VALUE SPACES.
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(05) VALUE "=====".
           02 EMS-TAB-INFO.
               03 FILLER       PIC X(08) VALUE SPACES.
               03 FILLER       PIC X(02) VALUE "| ".
               03 EMS-TAB-DEPT PIC X(16).
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(05) VALUE SPACES.
               03 EMS-TAB-NOMI PIC Z(06).
               03 FILLER       PIC X(05) VALUE SPACES.
               03 FILLER       PIC X(03) VALUE " | ".
               03 EMS-TAB-NOMB PIC X(15).
               03 FILLER       PIC X(05) VALUE "  |  ".
               03 EMS-TAB-CLAV PIC X(14).
               03 FILLER       PIC X(05) VALUE "  |  ".
               03 EMS-TAB-PERC PIC $$$$,$$$,$$9.99.
               03 FILLER       PIC X(03) VALUE " | ".
               03 EMS-TAB-DEDU PIC $$$$,$$$,$$9.99.
               03 FILLER       PIC X(04) VALUE "  | ".
               03 EMS-TAB-SUEL PIC $$$$,$$$,$$9.99-.
               03 FILLER       PIC X(04) VALUE "  | ".
           02 EMS-CORTE-EMPL.
               03 FILLER       PIC X(77) VALUE SPACES.
               03 FILLER       PIC X(22) VALUE "SALARIO DEL EMPLEADO: ".
               03 EMS-CE-SALA  PIC $$$$,$$$,$$9.99-.
           02 EMS-CORTE-DEPT.
               03 FILLER       PIC X(08) VALUE SPACES.
               03 EMS-CD-EMPL  PIC Z(04).
               03 FILLER       PIC X(12) VALUE " EMPLEADOS".
               03 FILLER       PIC X(28) VALUE SPACES.
               03 FILLER       PIC X(09) VALUE " SUBTOTAL".
               03 FILLER       PIC X(06) VALUE " PERC:".
               03 EMS-CD-PERC  PIC $$$,$$$,$$9.99.
               03 FILLER       PIC X(06) VALUE " DEDU:".
               03 EMS-CD-DEDU  PIC $$$,$$$,$$9.99.
               03 FILLER       PIC X(06) VALUE " SUEL:".
               03 EMS-CD-SALA  PIC $$$$,$$$,$$9.99-.
           02 EMS-CORTE-ORG.
               03 FILLER       PIC X(5) VALUE SPACES.
               03 EMS-CO-EMPL  PIC Z(05).
               03 FILLER       PIC X(17) VALUE " EMPLEADOS EN LA ".
               03 FILLER       PIC X(12) VALUE "ORGANIZACION".
               03 FILLER       PIC X(14) VALUE SPACES.
               03 FILLER       PIC X(05) VALUE "TOTAL".
               03 FILLER       PIC X(06) VALUE " PERC:".
               03 EMS-CO-PERC  PIC $$$$,$$$,$$9.99.
               03 FILLER       PIC X(06) VALUE " DEDU:".
               03 EMS-CO-DEDU  PIC $$$$,$$$,$$9.99.
               03 FILLER       PIC X(06) VALUE " SUEL:".
               03 EMS-CO-SALA  PIC $$$$,$$$,$$9.99-.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM 100-START.
           PERFORM 200-PROCESS.
           PERFORM 300-END.

       100-START.
           DISPLAY "> Running start...".
           PERFORM 101-LOAD-DATE.
           PERFORM 102-LOAD-DEPT-NAMES.
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

       102-LOAD-DEPT-NAMES.
           MOVE "ADMINISTRACION"   TO DEPT-FULL(1).
           MOVE "CONTADURIA"       TO DEPT-FULL(2).
           MOVE "SISTEMAS"         TO DEPT-FULL(3).
           MOVE "TECNOLOGIA"       TO DEPT-FULL(4).
           MOVE "RECURSOS HUMANOS" TO DEPT-FULL(5).
           MOVE "MERCADOTECNIA"    TO DEPT-FULL(6).
           MOVE "ZERO"             TO DEPT-FULL(7).

       201-READ-MOVIM.
           READ MOVIM AT END MOVE 1 TO MOV-EOF.

       202-LOAD-TMP-FILE SECTION.
           PERFORM 201-READ-MOVIM
           PERFORM 203-SCAN-MOVIM UNTIL MOV-EOF = 1.
           GO TO END-SEC1.

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
       END-SEC1.

       204-GENERATE-REP SECTION.
           OPEN OUTPUT EMPREP.
           PERFORM 205-READ-TMP-FILE.
           COMPUTE LIN = LINLIM + 1.
           MOVE TMP-DEPT TO ANT-DEPT.
           MOVE TMP-NOMI TO ANT-NOMI.
           PERFORM 206-BUILD-DOC UNTIL TMP-EOF = 1.
           PERFORM 210-DEPT-CUT.
           PERFORM 212-ORG-CUT.
           GO TO END-SEC2.

       205-READ-TMP-FILE.
           RETURN TMP-FILE AT END MOVE 1 TO TMP-EOF.
      *     DISPLAY TMP-REG.

       206-BUILD-DOC.
           IF LIN >= LINLIM
               PERFORM 207-NEW-PAGE.
           IF ANT-NOMI NOT = TMP-NOMI
               PERFORM 209-EMPL-CUT.
           IF ANT-DEPT NOT = TMP-DEPT
               PERFORM 210-DEPT-CUT.

           IF DEPT-FLAG = 1
               PERFORM 211-DEPT-NAME-CHOICE
               MOVE 0 TO DEPT-FLAG
           ELSE
               MOVE SPACES TO EMS-TAB-DEPT.
           IF NAME-FLAG = 1
               MOVE TMP-NOMI TO EMS-TAB-NOMI
               MOVE TMP-NOMB TO EMS-TAB-NOMB
               MOVE 0 TO NAME-FLAG
               ADD 1 TO EMPL-ST
           ELSE
               MOVE " " TO EMS-TAB-NOMI
               MOVE SPACES TO EMS-TAB-NOMB.
           MOVE TMP-CLAV TO EMS-TAB-CLAV.
           MOVE TMP-PERC TO EMS-TAB-PERC.
           MOVE TMP-DEDU TO EMS-TAB-DEDU.
           WRITE EMR-REG FROM EMS-TAB-INFO AFTER 1 LINE.
           ADD 1 TO LIN.
           ADD TMP-PERC TO PERC-ST.
           ADD TMP-DEDU TO DEDU-ST.
           PERFORM 205-READ-TMP-FILE.


       207-NEW-PAGE.
           ADD 1 TO PAG.
           MOVE PAG TO EMS-TI-PAG.
           PERFORM 208-WRITE-TITLES.
           MOVE 6 TO LIN.

       208-WRITE-TITLES.
           WRITE EMR-REG FROM EMS-TIT-0 AFTER PAGE.
           WRITE EMR-REG FROM EMS-TIT-1 AFTER 1 LINES.
           WRITE EMR-REG FROM EMS-TIT-2 AFTER 1 LINE.
           WRITE EMR-REG FROM EMS-TAB-TIT AFTER 2 LINES.
           WRITE EMR-REG FROM EMS-TAB-SEP AFTER 1 LINE.

       209-EMPL-CUT.
           COMPUTE SUEL-EMPL = PERC-ST - DEDU-ST.
           MOVE TMP-NOMI TO ANT-NOMI.
      *> IF WE WANT TO PRINT THE SALARY UNCOMENT THE NEXT 4 LINES
      *     MOVE SUEL-EMPL TO EMS-CE-SALA.
      *     WRITE EMR-REG FROM EMS-CORTE-EMPL AFTER 2 LINES.
      *     WRITE EMR-REG FROM SPACES AFTER 1 LINE.
      *     ADD 2 TO LIN.
           MOVE ANT-NOMI TO EMI-NOMI.
           MOVE TMP-NOMB TO EMI-NOMB.
           MOVE TMP-DEPT TO EMI-DEPT.
           MOVE SUEL-EMPL TO EMI-SUEL.
           REWRITE EMI-REG.
           READ EMPINX.
      *> TO VERIFY THE SALARIES IN EMPINX
      *     DISPLAY EMI-NOMI, ",", EMI-NOMB, ",", EMI-DEPT, ",", EMI-SUEL.
           MOVE 0 TO SUEL-EMPL.
           MOVE 1 TO NAME-FLAG.

       210-DEPT-CUT.
           COMPUTE SUEL-ST = PERC-ST - DEDU-ST.
           ADD EMPL-ST TO ORG-EMPL.
           ADD PERC-ST TO ORG-PERC.
           ADD DEDU-ST TO ORG-DEDU.
           ADD SUEL-ST TO ORG-SUEL.
           MOVE EMPL-ST TO EMS-CD-EMPL.
           MOVE PERC-ST TO EMS-CD-PERC.
           MOVE DEDU-ST TO EMS-CD-DEDU.
           MOVE SUEL-ST TO EMS-CD-SALA.
           WRITE EMR-REG FROM EMS-CORTE-DEPT AFTER 2 LINES.
           INITIALISE EMPL-ST, PERC-ST, DEDU-ST, SUEL-ST
           MOVE TMP-DEPT TO ANT-DEPT.
           MOVE 1 TO DEPT-FLAG.
           PERFORM 207-NEW-PAGE.

       212-ORG-CUT.
           MOVE ORG-EMPL TO EMS-CO-EMPL.
           MOVE ORG-PERC TO EMS-CO-PERC.
           MOVE ORG-DEDU TO EMS-CO-DEDU.
           MOVE ORG-SUEL TO EMS-CO-SALA.
           WRITE EMR-REG FROM EMS-CORTE-ORG AFTER 2 LINES.

       END-SEC2.

       211-DEPT-NAME-CHOICE.
           MOVE LENGTH OF TMP-DEPT TO STR-LENGHT-S.
           MOVE TMP-DEPT TO STR-S.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
               MOVE LENGTH OF DEPT-FULL(I) TO STR-LENGHT-F
               MOVE DEPT-FULL(I) TO STR-F
               IF CHAR-S(1) = CHAR-F(1)
                   MOVE DEPT-FULL(I) TO EMS-TAB-DEPT
           END-PERFORM.


       END PROGRAM EmpNom.
