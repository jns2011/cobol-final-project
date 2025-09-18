              IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-TRANSACCION.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "transacciones.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS R-KEY
               ALTERNATE RECORD KEY IS R-CBF
                   WITH DUPLICATES
               FILE STATUS IS FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
       
       COPY "registro-transaccion.cpy".

       WORKING-STORAGE SECTION.
       01  FILE-STATUS            PIC XX.
       
       LINKAGE SECTION.
           COPY "transaccion.cpy".
       
       PROCEDURE DIVISION USING TRANSACCION,RESULTADO.
       
       MOVE 0 TO RESULTADO.
       OPEN I-O TRANS-FILE.
       IF FILE-STATUS NOT = "00" AND NOT = "05"
           OPEN OUTPUT TRANS-FILE
       END-IF.
       
       MOVE P-CBF TO R-CBF.
       MOVE P-DESCRIPCION TO R-DESCRIPCION.
       MOVE P-IMPORTE TO R-IMPORTE.

       MOVE FUNCTION CURRENT-DATE(1:14) TO R-TIMESTAMP.

       WRITE R-RECORD
              INVALID KEY 
                 MOVE 1 TO RESULTADO
              NOT INVALID KEY
                 MOVE 0 TO RESULTADO
       END-WRITE.
       CLOSE TRANS-FILE.
       GOBACK.
