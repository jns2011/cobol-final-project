       IDENTIFICATION DIVISION.
       PROGRAM-ID. GUARDAR-CLIENTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUENTAS-FILE ASSIGN TO "cuentas.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CBF
       FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUENTAS-FILE.
       01 C-REGISTRO.
             05 CLI-CBF  PIC 9(6).
             05 CLI-APELLIDO PIC X(30).
             05 CLI-NOMBRE PIC X(30).
             05 CLI-EMAIL  PIC X(50).

       WORKING-STORAGE SECTION.
         01  FILE-STATUS            PIC XX.
           88  FILE-SUCCESS       VALUE "00".
           88  FILE-NOT-FOUND     VALUE "23".
           88  FILE-NOT-READY     VALUE "35".
           88  FILE-DUPLICATE     VALUE "22".

       LINKAGE SECTION.
        01 CLAVE-BANCARIA PIC 9(6).
        01 CSV-REGISTRO.
             05 CBF  PIC 9(6).
             05 APELLIDO PIC X(30).
             05 NOMBRE PIC X(30).
             05 EMAIL  PIC X(50).

        PROCEDURE DIVISION USING CLAVE-BANCARIA, CSV-REGISTRO.
        MAIN-GUARDAR-CLIENTE.
           COPY "includes/open-file-client.cbl".

           MOVE CLAVE-BANCARIA TO CLI-CBF
           MOVE APELLIDO TO CLI-APELLIDO
           MOVE NOMBRE TO CLI-NOMBRE
           MOVE EMAIL TO CLI-EMAIL

           WRITE C-REGISTRO
               INVALID KEY
                 IF FILE-DUPLICATE
                  DISPLAY "Error: Clave Bancaria Existente!"
                  CLAVE-BANCARIA
                ELSE
                    DISPLAY "Error al guardar; " FILE-STATUS
                END-IF
             NOT INVALID KEY
                 DISPLAY "CLIENTE GUARDADO CON EXITO."

           COPY "includes/close-file-client.cbl".

           GOBACK.
