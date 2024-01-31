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
       DATA DIVISION.
       FILE SECTION.
       FD  MOVIM.
       01  MOV-REG.
           02  MOV-NOMI    PIC 9(06).
           02  MOV-CLAV    PIC 9(14).
           02  MOV-IMP     PIC 9(05)V99.
           02  FILLER      PIC XX.
       FD  EMPINX.
       01  EMI-REG.
           02 EMI-NOMI PIC 9(06).
           02 EMI-NOMB PIC X(15).
           02 EMI-DEPT PIC X(03).
           02 EMI-SUEL PIC 9(05)V99.
           02 FILLER   PIC XX.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-START.
           PERFORM 200-PROCESS.
           PERFORM 300-END.

       100-START.
           DISPLAY "> Running start...".
           OPEN INPUT MOVIM.
           OPEN I-O EMPINX.
           READ MOVIM.


       200-PROCESS.
           DISPLAY "> Running process...".
           READ MOVIM.
      *     DISPLAY MOV-NOMI, ", ", MOV-CLAV, ", ", MOV-IMP.
           MOVE MOV-NOMI TO EMI-NOMI.
           READ EMPINX
           DISPLAY MOV-REG.
           DISPLAY EMI-REG.

       300-END.
           DISPLAY "> Running end...".
           CLOSE MOVIM.
           CLOSE EMPINX.
           STOP RUN.
       END PROGRAM EmpNom.
