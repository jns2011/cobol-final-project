       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-CBF.
       AUTHOR. Rodas, Matteoda, Lopez,Condotta.

       DATA DIVISION.
     
       
       WORKING-STORAGE SECTION.
       01  VALID-ID      PIC X VALUE 'N'.
       01  MSG-ERROR     PIC X(100). 

       LINKAGE SECTION.
       01 ID-CUENTA-STR PIC X(6).
      
       COPY "id-cuentas.cpy".

       PROCEDURE DIVISION USING ID-CUENTA-STR, ID-CUENTA.
           PERFORM VALIDAR-ID-CUENTA.
            IF VALID-ID='S'
             MOVE ID-CUENTA-STR(1:1) TO ID-D1
             MOVE ID-CUENTA-STR(2:1) TO ID-D2
             MOVE ID-CUENTA-STR(3:1) TO ID-D3
             MOVE ID-CUENTA-STR(4:1) TO ID-D4
             MOVE ID-CUENTA-STR(5:1) TO ID-D5
            ELSE
                DISPLAY MSG-ERROR
            END-IF.

           GOBACK.
       
       VALIDAR-ID-CUENTA.
           IF FUNCTION NUMVAL(ID-CUENTA-STR) > 0 AND LENGTH OF 
              ID-CUENTA-STR = 6
               MOVE 'S' TO VALID-ID
           ELSE
               MOVE 'N' TO VALID-ID
               MOVE "Error: ID de cuenta inválido en CSV: " TO 
               MSG-ERROR
               STRING ID-CUENTA-STR DELIMITED BY SIZE
                      " (debe ser numérico y de 6 dígitos)" 
                      DELIMITED BY SIZE
                      INTO MSG-ERROR
           END-IF.


