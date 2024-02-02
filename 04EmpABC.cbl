      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date: 25/01/24
      * Purpose: Project learning number 4
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPABC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPINX ASSIGN TO DISK
               ORGANISATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMI-NOMI.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPINX.
       01  EMI-REG.
           02 EMI-NOMI PIC 9(06).
           02 EMI-NOMB PIC X(15).
           02 EMI-DEPT PIC X(03).
           02 EMI-SUEL PIC S9(05)V99.
           02 FILLER   PIC XX.
       WORKING-STORAGE SECTION.
       77  OPC PIC X.
           88 ALTA VALUE "A".
           88 BAJA VALUE "B".
           88 CONS VALUE "C".
           88 MODI VALUE "M".
           88 SALI VALUE "S".
           88 OPC-VALIDA VALUE "A" THROUGH "C", "M".
       77  EMI-EXI PIC 9.
       77  WS-AUT  PIC 999.
       77  RESP    PIC XX.
       PROCEDURE DIVISION.
      *-------------------- MAIN --------------------
       MAIN-PROCEDURE.
      *     OPEN OUTPUT EMPINX.
      *     CLOSE EMPINX.
           OPEN I-O EMPINX.
           PERFORM PROC-MENU UNTIL SALI.
           CLOSE EMPINX.
           STOP RUN.

      *-------------------- MENU --------------------
       PROC-MENU.
           DISPLAY "MENU ABC EMPLEADOS".
           DISPLAY "A = ALTA".
           DISPLAY "B = BAJA".
           DISPLAY "C = CONSULTA".
           DISPLAY "M = MODIFICAR".
           DISPLAY "S = SALIR".
           DISPLAY " : "
           ACCEPT OPC.
           IF NOT OPC-VALIDA
               IF OPC = "S"
                   DISPLAY "SALIENDO..."
               ELSE
                   DISPLAY "OPCION NO VALIDA"
               END-IF
           ELSE
               PERFORM PROC-OPC-SELECT.

      *-------------------- OPTION SELECTOR --------------------
       PROC-OPC-SELECT.
           MOVE 1 TO EMI-EXI.
           DISPLAY "INGRESE LA NOMINA: ".
           ACCEPT EMI-NOMI.
           READ EMPINX INVALID KEY MOVE 0 TO EMI-EXI.
           IF ALTA
               PERFORM PROC-ALTA.
           IF BAJA
               PERFORM PROC-BAJA.
           IF CONS
               PERFORM PROC-CONS.
           IF MODI
               PERFORM PROC-MODI.

      *-------------------- DISPLAY EMPLOYEE --------------------
       PROC-DISP.
           DISPLAY "NOMINA: ", EMI-NOMI.
           DISPLAY "NOMBRE: ", EMI-NOMB.
           DISPLAY "DEPARTAMENTO: ", EMI-DEPT.
           DISPLAY "SALARIO: ", EMI-SUEL.

      *-------------------- CAPTURE EMPLOYEE --------------------
       PROC-CAPT.
           DISPLAY "INGRESE EL NOMBRE: ".
           ACCEPT EMI-NOMB.
           DISPLAY "INGRESE EL DEPARTAMENTO: ".
           ACCEPT EMI-DEPT.

      *-------------------- CONSULTE --------------------
       PROC-CONS.
           IF EMI-EXI = 0
               DISPLAY "EMPLEADO INEXISTENTE"
           ELSE
               PERFORM PROC-DISP.

      *-------------------- NEW EMPLOYEE --------------------
       PROC-ALTA.
           IF EMI-EXI = 0
               PERFORM PROC-CAPT
               WRITE EMI-REG
               ADD 1 TO WS-AUT
               DISPLAY "ALTA SATISFACTORIA", WS-AUT
           ELSE
               DISPLAY "EL EMPLEADO YA EXISTE".

      *-------------------- DELETE EMPLOYEE --------------------
       PROC-BAJA.
           IF EMI-EXI = 0
               DISPLAY "EMPLEADO INEXISTENTE"
           ELSE
               DISPLAY "ESTA SEGURO DE DAR DE BAJA AL EMPLEADO"
               PERFORM PROC-DISP
               DISPLAY " : "
               ACCEPT RESP
               IF RESP = "S"
                   DELETE EMPINX
                   ADD 1 TO WS-AUT
                   DISPLAY "BAJA SATISFACTORIA", WS-AUT
               ELSE
                   DISPLAY "BAJA CANCELADA".

      *-------------------- UPDATE EMPLOYEE --------------------
       PROC-MODI.
           IF EMI-EXI = 0
               DISPLAY "EMPLEADO INEXISTENTE"
           ELSE
               DISPLAY "ESTA SEGURO QUE QUIERE MODIFICAR AL EMPLEADO"
               PERFORM PROC-DISP
               DISPLAY " : "
               ACCEPT RESP
               IF RESP = "S"
                   PERFORM PROC-CAPT
                   REWRITE EMI-REG
                   ADD 1 TO WS-AUT
                   DISPLAY "MODIFICACION SATISFACTORIA", WS-AUT
               ELSE
                   DISPLAY "MODIFICACION CANCELADA".

       END PROGRAM EMPABC.
