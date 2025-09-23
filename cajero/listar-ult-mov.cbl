       IDENTIFICATION DIVISION.
       PROGRAM-ID. listar-ult-mov.

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
       01  FILE-STATUS                     PIC XX.
       01  WS-CONTADOR           PIC 9(2) VALUE 0.
       01  WS-IMPORTE-DISPLAY PIC -ZZZ,ZZZ,ZZ9.99.
       01  WS-TIPO-DESCR                PIC X(15).
       01  WS-FECHA-FORMATEADA          PIC X(10).
       01  WS-SIGNAL                        PIC X.
       01  WS-CANTIDAD-MAX               PIC 9(2).
       01  WS-CBF-BUSCAR                 PIC 9(6). 

       LINKAGE SECTION.
       01  LK-CBF             PIC 9(6).
       01  LK-CANTIDAD        PIC 9(2).

       PROCEDURE DIVISION USING LK-CBF, LK-CANTIDAD.
       MAIN-PARA.
           IF LK-CANTIDAD = 0 OR LK-CANTIDAD > 50
               MOVE 40 TO WS-CANTIDAD-MAX 
           ELSE
               MOVE LK-CANTIDAD TO WS-CANTIDAD-MAX
           END-IF

           MOVE 0 TO WS-CONTADOR
           MOVE LK-CBF TO WS-CBF-BUSCAR
    
           DISPLAY " "
           DISPLAY "--- Historial de Movimientos ---"
           DISPLAY " "   

           OPEN INPUT TRANS-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error al abrir archivo de transacciones"
               GOBACK
           END-IF

           DISPLAY "Buscando transacciones para CBF: " WS-CBF-BUSCAR
           DISPLAY "Mostrando ultimas "WS-CANTIDAD-MAX " transacciones:"
           DISPLAY "----------------------------------------"  
    
           MOVE LOW-VALUES TO R-CBF
          
           START TRANS-FILE KEY IS >= R-CBF
               INVALID KEY
                   DISPLAY "No se puede posicionar en archivo"
                CLOSE TRANS-FILE
                GOBACK
           END-START
            
           PERFORM UNTIL FILE-STATUS = "10" 
               OR WS-CONTADOR >= WS-CANTIDAD-MAX
           READ TRANS-FILE NEXT RECORD
                AT END 
                    MOVE "10" TO FILE-STATUS
                NOT AT END
                    IF R-CBF = WS-CBF-BUSCAR 
                        ADD 1 TO WS-CONTADOR
                    
                    
                    EVALUATE R-DESCRIPCION
                        WHEN "D" 
                            MOVE "DEPOSITO" TO WS-TIPO-DESCR
                            MOVE "+" TO WS-SIGNAL
                        WHEN "E" 
                            MOVE "EXTRACCION" TO WS-TIPO-DESCR
                            MOVE "-" TO WS-SIGNAL
                        WHEN "T" 
                            MOVE "TRANSFERENCIA" TO WS-TIPO-DESCR
                            MOVE "-" TO WS-SIGNAL
                        WHEN OTHER
                            MOVE "OTRA" TO WS-TIPO-DESCR
                            MOVE " " TO WS-SIGNAL
                    END-EVALUATE
                    
                    MOVE R-IMPORTE TO WS-IMPORTE-DISPLAY
                    
                   
                    STRING 
                        R-DIA DELIMITED BY SIZE "/" 
                        R-MES DELIMITED BY SIZE "/" 
                        R-ANIO DELIMITED BY SIZE
                    INTO WS-FECHA-FORMATEADA
                    
                    DISPLAY 
                        WS-TIPO-DESCR " " WS-SIGNAL "$" 
                        WS-IMPORTE-DISPLAY
                        " - Fecha: " WS-FECHA-FORMATEADA
                        " Hora: " R-HORA ":" R-MIN ":" R-SEG
                    END-IF
           END-READ
           END-PERFORM
           CLOSE TRANS-FILE

           IF WS-CONTADOR = 0
           DISPLAY "No se encontraron transacciones en el historial"
           ELSE
           DISPLAY "----------------------------------------"
           DISPLAY "Total mostradas: " WS-CONTADOR " transacciones"
           END-IF

           *>CLOSE TRANS-FILE
           GOBACK.
