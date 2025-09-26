       IDENTIFICATION DIVISION.
       PROGRAM-ID. listar-transacciones.

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
       01  FILE-STATUS                    PIC XX.
       01  WS-CONTADOR           PIC 9(2) VALUE 0.
       01  WS-OPCION-VOLVER                 PIC X.
       01  WS-START-KEY                  PIC 9(6). 
       01  WS-IMPORTE-SIGNED         PIC S9(9)V99. 
       01  WS-IMPORTE-DISPLAY PIC -ZZZ,ZZZ,ZZ9.99. 

       LINKAGE SECTION.
       01  P-CBF              PIC 9(6).

       PROCEDURE DIVISION USING P-CBF.
       MAIN-PARA.
           MOVE 0 TO WS-CONTADOR 
           OPEN INPUT TRANS-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "ERROR AL ABRIR ARCHIVO. STATUS = " FILE-STATUS
           GOBACK
           END-IF

           MOVE LOW-VALUES TO R-CBF  
           MOVE LOW-VALUES TO R-KEY    
           START TRANS-FILE KEY IS >= R-CBF  
               INVALID KEY
                     CONTINUE
           END-START 

           DISPLAY "Transacciones para CBF: " P-CBF
           DISPLAY "=============================="
           
           PERFORM UNTIL FILE-STATUS = "10" OR WS-CONTADOR >= 25
               READ TRANS-FILE NEXT RECORD
                   AT END 
                MOVE "10" TO FILE-STATUS
                   NOT AT END
                   
                IF R-CBF = P-CBF
                
                ADD 1 TO WS-CONTADOR
                
                IF R-DESCRIPCION = "T" OR R-DESCRIPCION = "E"
                        COMPUTE WS-IMPORTE-SIGNED = R-IMPORTE * -1
                    ELSE
                         MOVE R-IMPORTE TO WS-IMPORTE-SIGNED
                END-IF
                MOVE WS-IMPORTE-SIGNED TO WS-IMPORTE-DISPLAY 

                DISPLAY "KEY: " R-KEY           
                DISPLAY "CBF: " R-CBF
                DISPLAY "DESC: " R-DESCRIPCION
                DISPLAY "IMPORTE: $ " WS-IMPORTE-DISPLAY
                DISPLAY "FECHA: " R-DIA "/" R-MES "/" R-ANIO
                DISPLAY "HORA:  " R-HORA ":" R-MIN ":" R-SEG
                DISPLAY "-----------------------------"
                END-IF
               END-READ
           END-PERFORM
           
           IF WS-CONTADOR = 0
               DISPLAY "No se encontraron transacciones"
           END-IF


           CLOSE TRANS-FILE
           DISPLAY " "
           DISPLAY "Presione ENTER para MENU OPERACIONES"
           ACCEPT WS-OPCION-VOLVER
           GOBACK.
       