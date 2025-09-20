       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTAS-CLIENTES.
       AUTHOR. Rodas, Matteoda, Lopez,Condotta.
       DATE-WRITTEN. 2025-09-05.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "cuentas.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CSV-FILE.
       01 CSV-REGISTRO.
           05 CBF  PIC 9(6).
           05 APELLIDO PIC X(30).
           05 NOMBRE PIC X(30).
           05 EMAIL  PIC X(50).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG      PIC 9 VALUE 0.
       01 ID-CUENTA-STR PIC X(6).

       01 MENU-CHOICE-STR PIC X(2).
       01 MENU-CHOICE     PIC 9.
           88 CREATE-CBF   VALUE 1.
           88 EXIT-PROGRAM VALUE 2.

       COPY "id-cuentas.cpy".
       01 CLAVE-BANCARIA PIC X(6).
       
       COPY "cliente.cpy".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               PERFORM DISPLAY-MENU UNTIL EXIT-PROGRAM.
            STOP RUN.

        DISPLAY-MENU.
           DISPLAY "-------------------------------------------------".
           DISPLAY "---PROGRAMA CALCULO DE CLAVE BANCARIA FICTICIA---".
           DISPLAY "-------------------------------------------------".
           DISPLAY "1. Crear claves bancarias desde CSV".
           DISPLAY "2. Salir".
           DISPLAY "SU OPCION: " WITH NO ADVANCING.
           ACCEPT MENU-CHOICE-STR.
           MOVE FUNCTION NUMVAL(MENU-CHOICE-STR) TO MENU-CHOICE.

           OPEN INPUT CSV-FILE.
      
           EVALUATE TRUE
            WHEN CREATE-CBF
             PERFORM UNTIL EOF-FLAG = 1
             READ CSV-FILE
               AT END MOVE 1 TO EOF-FLAG
               NOT AT END
                
                MOVE CSV-REGISTRO(1:6) TO ID-CUENTA-STR
                INSPECT ID-CUENTA-STR REPLACING ALL ',' BY ' '
                  CALL "CREAR-CBF" USING ID-CUENTA-STR, ID-CUENTA
                  PERFORM PROCESAR-CSV
               END-PERFORM
            WHEN EXIT-PROGRAM
                  CONTINUE
            WHEN OTHER
                  DISPLAY "OPCION INVALIDA"
           END-EVALUATE.
           CLOSE CSV-FILE.

       PROCESAR-CSV SECTION.
           
            CALL "CALCULATE-CBF" USING ID-CUENTA, CLAVE-BANCARIA
            DISPLAY "Cuenta: " ID-CUENTA-STR
             " | Clave Bancaria: " CLAVE-BANCARIA

            MOVE CLAVE-BANCARIA TO P-CBF

            CALL "BUSCAR-CLIENTE" USING CLIENTE

            IF P-CBF = "000000"
             CALL "GUARDAR-CLIENTE" USING CLAVE-BANCARIA, CSV-REGISTRO
            ELSE
             DISPLAY "Cliente ya existente"
            END-IF.
               
       END PROGRAM ALTAS-CLIENTES.
       
       

       
