       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTAS-CLIENTES.
       AUTHOR. Rodas, Matteoda, Lopez,Condotta.
       DATE-WRITTEN. 2025-09-05.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MENU-CHOICE-STR PIC X(2).
       01 MENU-CHOICE     PIC 9.
          88 CREATE-CBF   VALUE 1.
          88 EXIT-PROGRAM VALUE 2.

       COPY "id-cuentas.cbl".
       COPY "clave-bancaria.cbl".

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

           EVALUATE TRUE
              WHEN CREATE-CBF
                  CALL "CREAR-CBF" USING ID-CUENTA, CLAVE-BANCARIA
           WHEN EXIT-PROGRAM
                  CONTINUE
           WHEN OTHER
                  DISPLAY "OPCION INVALIDA"
           END-EVALUATE.

