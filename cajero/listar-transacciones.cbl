       IDENTIFICATION DIVISION.
       PROGRAM-ID. listar-transacciones.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "transacciones.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS R-KEY
               ALTERNATE RECORD KEY IS R-CBF
                   WITH DUPLICATES
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
           COPY "registro-transaccion.cpy".

       WORKING-STORAGE SECTION.
       01  FILE-STATUS        PIC XX.

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT TRANS-FILE
           IF FILE-STATUS NOT = "00"
              DISPLAY "ERROR AL ABRIR ARCHIVO. STATUS = " FILE-STATUS
              STOP RUN
           END-IF

           PERFORM UNTIL FILE-STATUS = "10"
              READ TRANS-FILE NEXT RECORD
                 AT END MOVE "10" TO FILE-STATUS
                 NOT AT END
                    DISPLAY "KEY: " R-KEY
                    DISPLAY "CBF: " R-CBF
                    DISPLAY "DESC: " R-DESCRIPCION
                    MOVE R-IMPORTE TO R-IMPORTE-FORM
                    DISPLAY "IMPORTE: $ " R-IMPORTE-FORM
                    *>DISPLAY "FECHA: " R-TIMESTAMP
                    DISPLAY "FECHA: " R-DIA "/" R-MES "/" R-ANIO
                    DISPLAY "HORA:  " R-HORA ":" R-MIN ":" R-SEG
                    DISPLAY "-----------------------------"
              END-READ
           END-PERFORM

           CLOSE TRANS-FILE
           STOP RUN.
