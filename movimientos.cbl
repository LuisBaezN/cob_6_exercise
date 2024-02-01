      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date:
      * Purpose: Genera diversos movimientos de diferentes empleados
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVIMIENTOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MOVIM ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  MOVIM.
       01  MOV-REG.
           02  MOV-NOMI    PIC 9(6).
           02  MOV-CLAV    PIC X(14).
           02  MOV-IMP     PIC 9(05)V99.
           02  FILLER      PIC XX.
       WORKING-STORAGE SECTION.
       77  I               PIC 999.
       77  N-RAND1         PIC 9(19).
       77  N-RAND2         PIC 9(19).
       77  CANT            PIC 9(19)V99.
       01  CLAVE-TABLA.
           02 CLAVE-ROW OCCURS 11 TIMES.
               03 CLAVE  PIC X(13).
       01  NOMINA-TABLA.
           02 NOMI-ROW OCCURS 11 TIMES.
               03  NOMI-D  PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIALIZACION.
           OPEN OUTPUT MOVIM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 102
               PERFORM GENERATE-RAND
               MOVE NOMI-D(N-RAND1) TO MOV-NOMI
               MOVE CLAVE(N-RAND2) TO MOV-CLAV
               MOVE CANT TO MOV-IMP
               WRITE MOV-REG
           END-PERFORM
           CLOSE MOVIM
           STOP RUN.

       INICIALIZACION.
           MOVE "Sueldo       "    TO CLAVE(1).
           MOVE "Bono         "    TO CLAVE(2).
           MOVE "Puntualidad  "    TO CLAVE(3).
           MOVE "Productividad"    TO CLAVE(4).
           MOVE "Prestamo     "    TO CLAVE(5).
           MOVE "Horas Extras "    TO CLAVE(6).
           MOVE "Impuestos    "    TO CLAVE(7).
           MOVE "IMSS         "    TO CLAVE(8).
           MOVE "Faltas       "    TO CLAVE(9).
           MOVE "Pago prestamo"    TO CLAVE(10).
           MOVE "Vales        "    TO CLAVE(11).

           MOVE 32421              TO NOMI-D(1).
           MOVE 87343              TO NOMI-D(2).
           MOVE 23491              TO NOMI-D(3).
           MOVE 45861              TO NOMI-D(4).
           MOVE 78562              TO NOMI-D(5).
           MOVE 98752              TO NOMI-D(6).
           MOVE 12348              TO NOMI-D(7).
           MOVE 22215              TO NOMI-D(8).
           MOVE 12345              TO NOMI-D(9).
           MOVE 13135              TO NOMI-D(10).
           MOVE 0                  TO NOMI-D(11).

       GENERATE-RAND.
           COMPUTE N-RAND1 = FUNCTION RANDOM(I) * 10000000000000000.
           COMPUTE N-RAND1 = FUNCTION MOD(N-RAND1, 11) + 1.

           COMPUTE N-RAND2 = FUNCTION RANDOM(I*I)*10000000000000000.
           COMPUTE N-RAND2 = FUNCTION MOD(N-RAND2, 11) + 1.

           COMPUTE CANT = FUNCTION RANDOM(I) * 10000000000000000.
           COMPUTE CANT = FUNCTION MOD(CANT, 99999).
           COMPUTE CANT = CANT + (N-RAND1 + N-RAND2)/100.

       END PROGRAM MOVIMIENTOS.
