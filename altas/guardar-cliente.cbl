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
       COPY "file-status.cbl".

       LINKAGE SECTION.
       COPY "clave-bancaria.cbl".
        01 CSV-REGISTRO.
             05 CBF  PIC 9(6).
             05 APELLIDO PIC X(30).
             05 NOMBRE PIC X(30).
             05 EMAIL  PIC X(50).

        PROCEDURE DIVISION USING CLAVE-BANCARIA, CSV-REGISTRO.
        MAIN-GUARDAR-CLIENTE.
           COPY "open-file-client.cbl".

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

           COPY "close-file-client.cbl".

           GOBACK.
