      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date:
      * Purpose: Project learning 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmpNom.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-START.
           PERFORM 200-PROCESS.
           PERFORM 300-END.

       100-START.
           DISPLAY "> Running start...".

       200-PROCESS.
           DISPLAY "> Running process...".

       300-END.
           DISPLAY "> Running end...".
           STOP RUN.
       END PROGRAM EmpNom.
