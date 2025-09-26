       IDENTIFICATION DIVISION.
       PROGRAM-ID. obtener-sald.
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
       01  FILE-STATUS                       PIC XX.
       01  WS-SALDO-POSITIVO  PIC S9(10)V99 VALUE 0.
       01  WS-SALDO-NEGATIVO  PIC S9(10)V99 VALUE 0.
       01  WS-TIPO-OPERACION              PIC X(20).

       LINKAGE SECTION.
       01  LK-CBF                  PIC X(6).
       01  LK-SALDO           PIC S9(10)V99.

       PROCEDURE DIVISION USING LK-CBF LK-SALDO.
       MAIN-PARA.
           MOVE 0 TO WS-SALDO-POSITIVO
           MOVE 0 TO WS-SALDO-NEGATIVO
           MOVE 0 TO LK-SALDO

           OPEN INPUT TRANS-FILE
             IF FILE-STATUS NOT = "00"
             DISPLAY "ERROR: No se pudo abrir archivo de transacciones"
             GOBACK
             END-IF

           PERFORM UNTIL FILE-STATUS = "10"
               READ TRANS-FILE NEXT RECORD
                    AT END 
                        MOVE "10" TO FILE-STATUS
                    NOT AT END
                    IF R-CBF = LK-CBF
                             
                    EVALUATE R-DESCRIPCION
                           WHEN "D" 
                               ADD R-IMPORTE TO WS-SALDO-POSITIVO
                           WHEN "E" 
                               SUBTRACT R-IMPORTE FROM WS-SALDO-POSITIVO
                           WHEN "T" 
                               SUBTRACT R-IMPORTE FROM WS-SALDO-POSITIVO
                           WHEN OTHER
                               CONTINUE
                    END-EVALUATE      
                   
                END-IF
               END-READ
           END-PERFORM

           CLOSE TRANS-FILE
           
           COMPUTE LK-SALDO = WS-SALDO-POSITIVO
         
           GOBACK.
