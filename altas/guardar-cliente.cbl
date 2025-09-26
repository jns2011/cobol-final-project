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
       
       COPY "registro-cliente.cpy".

       WORKING-STORAGE SECTION.
       COPY "file-status.cpy".

       LINKAGE SECTION.
       COPY "clave-bancaria.cpy".
       
       01 CSV-REGISTRO.
             05 CBF  PIC 9(6).
             05 APELLIDO PIC X(30).
             05 NOMBRE PIC X(30).
             05 EMAIL  PIC X(50).

        PROCEDURE DIVISION USING CLAVE-BANCARIA, CSV-REGISTRO.
        MAIN-GUARDAR-CLIENTE.
           COPY "open-file-client.cpy".

           MOVE CLAVE-BANCARIA TO CLI-CBF
           MOVE APELLIDO TO CLI-APELLIDO
           MOVE NOMBRE TO CLI-NOMBRE
           MOVE EMAIL TO CLI-EMAIL

           WRITE REGISTRO-CLIENTE
               INVALID KEY
                 IF FILE-DUPLICATE
                  DISPLAY "Error: Clave Bancaria Existente!"
                  CLAVE-BANCARIA
                ELSE
                    DISPLAY "Error al guardar; " FILE-STATUS
                END-IF
             NOT INVALID KEY
                 DISPLAY "CLIENTE GUARDADO CON EXITO."

           COPY "close-file-client.cpy".

           GOBACK.
