       IDENTIFICATION DIVISION.
       PROGRAM-ID. cajero.
       AUTHOR. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUENTAS-FILE ASSIGN TO "altas\includes\cuentas.dat" 
              ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
              RECORD KEY IS CLI-CBF.

       DATA DIVISION.
       FILE SECTION.
       FD  CUENTAS-FILE.
           COPY "registro-cliente.cpy".

       WORKING-STORAGE SECTION.

           COPY "transaccion.cpy".

           COPY "cliente.cpy".


       01  MENU-CHOICE                    PIC 9.
           88 CONSULTA-SALDO            VALUE 1.
           88 REALIZAR-TRANSFERENCIA    VALUE 2.
           88 REALIZAR-DEPOSITO         VALUE 3.
           88 REALIZAR-EXTRACCION       VALUE 4.
           88 ULT-MOVIMIENTOS           VALUE 5.
           88 LISTAR-TRANS              VALUE 6.
           88 EXIT-PROGRAM              VALUE 7.
    
       01  WS-CBF                      PIC X(6).
       01  WS-VALIDO            PIC X VALUE 'N'. 

       01  WS-MONTO-FORMAT       PIC ZZZ.ZZZ.ZZ9.
       01  WS-MONTO                    PIC 9(11).
       01  WS-OPCION-DEP        PIC 9(1) VALUE 0.

       01  WS-FIN                       PIC x(1).
       01  WS-MENU-FIN                  PIC X(1).

       01  WS-SALDO           PIC S9(11) VALUE 0.
       01  WS-SALDO-FORMAT      PIC -ZZZ.ZZZ.ZZ9.
       01  WS-MONTO-EXT                PIC S9(9).    
       01  WS-MONTO-EXT-FOR       PIC -Z.ZZZ.ZZ9.      
       01  WS-OPCION-EXT                PIC 9(1).
       01  TOPE-EXT        PIC 9(6) VALUE 100000.

       01  WS-OPCION-SALDO      PIC 9(9) VALUE 0.

       01  WS-SALDO-CALCULADO      PIC S9(10)V99. 
       01  WS-SALDO-CALCULADO-FORM PIC Z.ZZZ.ZZ9.

       01  WS-CBF-DESTINO                  PIC 9(6).
       01  WS-MONTO-TRANSF   PIC S9(9) value 100000. 
       01  WS-MONTO-TRANSF-FOR       PIC -Z.ZZZ.ZZ9. 

       01  WS-DESCRIPCION-TRANS           PIC X(1). 
       01  WS-MONTO-TRANS             PIC S9(9)V99.
       
       01  WS-OPCION-MOV PIC 9(1).
       01  WS-MOVIMIENTOS.
           05 WS-MOV-ITEM OCCURS 5 TIMES.
              10 WS-MOV-TIPO        PIC X(20).
              10 WS-MOV-MONTO        PIC 9(9). 
              10 WS-MOV-ANIO         PIC 9(4).
              10 WS-MOV-MES          PIC 9(2).
              10 WS-MOV-DIA          PIC 9(2).
              10 WS-MOV-HORA         PIC 9(8). 
              
       01  WS-INDICE-MOV        PIC 9 VALUE 0.
       01  WS-INDICE-LISTAR     PIC 9 VALUE 0. 

       01  WS-FECHA-HORA.
           05 WS-ANIO    PIC 9(4).
           05 WS-MES     PIC 9(2).
           05 WS-DIA     PIC 9(2).
           
       01  WS-HORA-COMPLETA PIC 9(8). 
       01  WS-HH               PIC 9(2).
       01  WS-MM               PIC 9(2).
       01  WS-SS               PIC 9(2).

       01  WS-CLIENTE-ACTUAL.  
           05  WS-CBF-ACT      PIC X(6).
           05  WS-APELLIDO-ACT PIC X(30).
           05  WS-NOMBRE-ACT   PIC X(30).
           05  WS-EMAIL-ACT    PIC X(50).

    
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE 'N' TO WS-FIN
           PERFORM UNTIL WS-FIN = 'S'
              PERFORM DISPLAY-MENU
              PERFORM VALIDAR-PIN
           END-PERFORM
           STOP RUN.
           
       DISPLAY-MENU.
           DISPLAY SPACES.
           DISPLAY "*********************************".
           DISPLAY "*                               *".
           DISPLAY "*     BIENVENIDO AL CAJERO      *".
           DISPLAY "*                               *".
           DISPLAY "*********************************".
           DISPLAY "---------------------------------".
           DISPLAY SPACES.
           DISPLAY "    -Gracias por elegirnos-".
           DISPLAY SPACES.
            
       VALIDAR-PIN.
           MOVE 'N' TO WS-VALIDO 
           PERFORM UNTIL WS-VALIDO = 'S'
               DISPLAY "================================="
               DISPLAY "#  Ingrese su CBF de 6 digitos  #"
               DISPLAY "================================="
               ACCEPT WS-CBF
        
           IF WS-CBF IS NUMERIC
            AND FUNCTION LENGTH(WS-CBF) = 6
           
           MOVE WS-CBF TO P-CBF OF CLIENTE
           CALL 'buscar-cliente' USING CLIENTE
           
           IF P-CBF OF CLIENTE NOT = '000000' AND
              P-CBF OF CLIENTE NOT = SPACES
               MOVE 'S' TO WS-VALIDO
               CALL "SYSTEM" USING "CLS"
               DISPLAY "Cliente encontrado: "  P-NOMBRE " " P-APELLIDO
               

                CALL 'obtener-saldo' USING P-CBF OF CLIENTE 

                                           WS-SALDO-CALCULADO
                MOVE WS-SALDO-CALCULADO TO WS-SALDO
                MOVE WS-SALDO-CALCULADO TO WS-SALDO-CALCULADO-FORM
                DISPLAY SPACES 
                DISPLAY "*************************************"
                DISPLAY "Saldo actual: $" WS-SALDO-CALCULADO-FORM
                DISPLAY SPACES
                DISPLAY "*************************************"
                DISPLAY "Presione ENTER para MENU OPERACIONES"
                     
               ACCEPT OMITTED
             
           ELSE
               DISPLAY "*************************************"
               DISPLAY "CBF NO VALIDO - Cliente no encontrado"
               DISPLAY "*************************************"
           END-IF
           ELSE
           DISPLAY "CBF debe contener solo números"
           END-IF
           END-PERFORM
           MOVE 'N' TO WS-MENU-FIN
           PERFORM MENU-OPERACIONES UNTIL WS-MENU-FIN = 'S'.

       MENU-OPERACIONES.
           CALL "SYSTEM" USING "CLS".
           
           DISPLAY " "
           DISPLAY "****************************************".
           DISPLAY "**          MENU PRINCIPAL           **".
           DISPLAY "****************************************".
           DISPLAY "|*||||||||||||||||***|||||||||||||||||*|".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 1. Saldo                             |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 2. Transferencia                     |" .
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 3. Deposito                          |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 4. Extraccion                        |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 5. Ultimos Movimientos               |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 6. Ver transacciones                 |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "| 7. Salir                             |".
           DISPLAY "|--------------------------------------|".
           DISPLAY "|                                      |".
           DISPLAY "|  .Por favor, elija una opcion:       |".
           DISPLAY "|______________________________________|".
           DISPLAY "Su opcion: " WITH NO ADVANCING.

          
           ACCEPT MENU-CHOICE.
           CALL "SYSTEM" USING "CLS".
           
           EVALUATE TRUE 
                WHEN CONSULTA-SALDO PERFORM MENU-SALDO
                WHEN REALIZAR-TRANSFERENCIA PERFORM MENU-TRANSFERENCIA
                WHEN REALIZAR-DEPOSITO PERFORM MENU-DEPOSITO
                WHEN REALIZAR-EXTRACCION PERFORM MENU-EXTRACCION
                WHEN ULT-MOVIMIENTOS PERFORM MENU-ULT-MOVIMIENTOS
                WHEN EXIT-PROGRAM MOVE 'S' TO WS-MENU-FIN 
                WHEN LISTAR-TRANS CALL 'listar-transacciones'
                    USING P-CBF OF CLIENTE  
                WHEN OTHER DISPLAY "Opcion invalida"
           END-EVALUATE.
           
       REGISTRAR-TRANSACCION.
           MOVE P-CBF OF CLIENTE TO P-CBF OF TRANSACCION 
           MOVE WS-DESCRIPCION-TRANS TO P-DESCRIPCION
           MOVE WS-MONTO-TRANS TO P-IMPORTE
    
           CALL "crear-transaccion" USING TRANSACCION, RESULTADO

           IF RESULTADO NOT = 0
               DISPLAY "Error al registrar transaccion en archivo"
           END-IF.

       MENU-SALDO.
           MOVE 0 TO WS-OPCION-SALDO
           MOVE WS-SALDO TO WS-SALDO-FORMAT
           DISPLAY "Su saldo actual es de: " "$" WS-SALDO-FORMAT
           PERFORM UNTIL WS-OPCION-SALDO = 1 OR WS-OPCION-SALDO = 2
           DISPLAY "****************************************"
           DISPLAY "1.Enviar por e-mail"
           DISPLAY "----------------------------------------"
           DISPLAY "2.Volver a MENU OPERACIONES"
           DISPLAY "----------------------------------------"
           ACCEPT WS-OPCION-SALDO

           EVALUATE WS-OPCION-SALDO
               WHEN 1 
                   DISPLAY "------------------------------------"
                   DISPLAY "Enviando por e-mail..."
                   DISPLAY "------------------------------------"
                   DISPLAY "Presione ENTER para MENU OPERACIONES"
                   ACCEPT OMITTED 
                   MOVE 2 TO WS-OPCION-SALDO
               WHEN 2 
                   CONTINUE 
               WHEN OTHER 
                   DISPLAY "Opcion invalida"
                   ACCEPT OMITTED
           END-EVALUATE
           END-PERFORM

           IF WS-OPCION-SALDO = 2
               PERFORM MENU-OPERACIONES
           END-IF.

       MENU-TRANSFERENCIA.
           
           MOVE CLIENTE TO WS-CLIENTE-ACTUAL 
           DISPLAY "*******************************"
           DISPLAY "    Ingrese el CBF destino:    "
           DISPLAY SPACES
           ACCEPT WS-CBF-DESTINO

           MOVE WS-CBF-DESTINO TO P-CBF OF CLIENTE
           CALL 'buscar-cliente' USING CLIENTE
   
           IF P-CBF OF CLIENTE = '000000' OR P-CBF OF CLIENTE = SPACES
               DISPLAY "-------------------------------------------"
               DISPLAY "CBF destino no valido. Operacion cancelada."
               DISPLAY "-------------------------------------------"
               DISPLAY "Presione ENTER para MENU OPERACIONES"
               ACCEPT OMITTED
               MOVE WS-CLIENTE-ACTUAL TO CLIENTE 
               PERFORM MENU-OPERACIONES
           ELSE
           DISPLAY "***************************************"
           DISPLAY "Nombre del destinatario: " P-NOMBRE " " P-APELLIDO
           DISPLAY "***************************************"
           DISPLAY "Ingrese monto a transferir: $ " WITH NO ADVANCING
           *>DISPLAY SPACES
           ACCEPT WS-MONTO-TRANSF
           DISPLAY SPACES

           MOVE WS-CLIENTE-ACTUAL TO CLIENTE 
           
           IF WS-MONTO-TRANSF > WS-SALDO 
               DISPLAY "Fondos insuficientes. Operacion cancelada."
               ACCEPT OMITTED
                       
               PERFORM MENU-OPERACIONES
           ELSE
              
               MOVE "T" TO WS-DESCRIPCION-TRANS 
               MOVE WS-MONTO-TRANSF TO WS-MONTO-TRANS
               PERFORM REGISTRAR-TRANSACCION

               MOVE WS-CBF-DESTINO TO P-CBF OF CLIENTE
               
               MOVE "D" TO WS-DESCRIPCION-TRANS 
               MOVE WS-MONTO-TRANSF TO WS-MONTO-TRANS
               PERFORM REGISTRAR-TRANSACCION   
              
               MOVE WS-CLIENTE-ACTUAL TO CLIENTE
                   CALL 'obtener-sald' USING P-CBF OF CLIENTE
                                            WS-SALDO-CALCULADO
                   MOVE WS-SALDO-CALCULADO TO WS-SALDO
                   MOVE WS-SALDO TO WS-SALDO-FORMAT
                   MOVE WS-MONTO-TRANSF TO WS-MONTO-TRANSF-FOR 
  
           IF WS-INDICE-MOV >= 5 
                MOVE 0 TO WS-INDICE-MOV
           END-IF
                ADD 1 TO WS-INDICE-MOV
                MOVE 'Transferencia' TO WS-MOV-TIPO(WS-INDICE-MOV)
                MOVE WS-MONTO-TRANSF TO WS-MOV-MONTO(WS-INDICE-MOV) 
                   
           DISPLAY "---------------------------------------"
           DISPLAY "Transferencia exitosa de $ -" WS-MONTO-TRANSF-FOR 
           DISPLAY "---------------------------------------"
           DISPLAY "Su nuevo saldo es de: $" WS-SALDO-FORMAT
           DISPLAY "---------------------------------------"
           DISPLAY SPACES
           DISPLAY "Presiones ENTER para MENU OPERACIONES"
               
                 ACCEPT WS-FECHA-HORA FROM DATE YYYYMMDD    
                 ACCEPT WS-HORA-COMPLETA      FROM TIME
                 MOVE WS-ANIO    TO WS-MOV-ANIO(WS-INDICE-MOV)
                 MOVE WS-MES     TO WS-MOV-MES(WS-INDICE-MOV)
                 MOVE WS-DIA     TO WS-MOV-DIA(WS-INDICE-MOV)
                 MOVE WS-HORA-COMPLETA TO WS-MOV-HORA(WS-INDICE-MOV)
                 MOVE WS-HORA-COMPLETA(1:2) TO WS-HH
                 MOVE WS-HORA-COMPLETA(3:2) TO WS-MM
                 MOVE WS-HORA-COMPLETA(5:2) TO WS-SS
               ACCEPT OMITTED
               PERFORM MENU-OPERACIONES
 	       END-IF.


       MENU-DEPOSITO.
           MOVE 0 TO WS-OPCION-DEP
           DISPLAY "*****************************"
           DISPLAY "Ingrese monto a depositar: $ "
           DISPLAY "*****************************"
           DISPLAY SPACES
           ACCEPT WS-MONTO
           PERFORM UNTIL WS-OPCION-DEP = 1 OR WS-OPCION-DEP = 2 
           DISPLAY "-------------------------------------------"
           DISPLAY "1. Confirmar Deposito"
           DISPLAY "-------------------------------------------"
           DISPLAY "2. Volver a MENU OPERACIONES"
           DISPLAY "-------------------------------------------"
           ACCEPT WS-OPCION-DEP

           EVALUATE WS-OPCION-DEP
              WHEN 1
                 MOVE WS-MONTO TO WS-MONTO-FORMAT
                 DISPLAY "-------------------------------------------"
                 DISPLAY "Deposito confirmado por $" WS-MONTO-FORMAT
                 DISPLAY "-------------------------------------------"
                 DISPLAY "Presione ENTER para MENU OPERACIONES"
                 COMPUTE WS-SALDO = WS-MONTO + WS-SALDO
                 MOVE WS-SALDO TO WS-SALDO-FORMAT

                 MOVE "D" TO WS-DESCRIPCION-TRANS 
                 MOVE WS-MONTO TO WS-MONTO-TRANS
                 PERFORM REGISTRAR-TRANSACCION

                 CALL 'obtener-sald' USING P-CBF OF CLIENTE 
                                          WS-SALDO-CALCULADO 
                 MOVE WS-SALDO-CALCULADO TO WS-SALDO 

                 IF WS-INDICE-MOV >= 5 
                 MOVE 0 TO WS-INDICE-MOV
                 END-IF
                 ADD 1 TO WS-INDICE-MOV
                 MOVE 'Deposito' TO WS-MOV-TIPO(WS-INDICE-MOV)
                 MOVE WS-MONTO TO WS-MOV-MONTO(WS-INDICE-MOV)
                 ACCEPT WS-FECHA-HORA FROM DATE YYYYMMDD
                 ACCEPT WS-HORA-COMPLETA      FROM TIME
                 MOVE WS-ANIO    TO WS-MOV-ANIO(WS-INDICE-MOV)
                 MOVE WS-MES     TO WS-MOV-MES(WS-INDICE-MOV)
                 MOVE WS-DIA     TO WS-MOV-DIA(WS-INDICE-MOV)
                 MOVE WS-HORA-COMPLETA TO WS-MOV-HORA(WS-INDICE-MOV)
                 MOVE WS-HORA-COMPLETA(1:2) TO WS-HH
                 MOVE WS-HORA-COMPLETA(3:2) TO WS-MM
                 MOVE WS-HORA-COMPLETA(5:2) TO WS-SS
                 
                 ACCEPT OMITTED
                 MOVE 2 TO WS-OPCION-DEP
              WHEN 2
                  CONTINUE              
              WHEN OTHER
                 DISPLAY "---------------"
                 DISPLAY "Opcion invalida"
                 DISPLAY "---------------"
                 ACCEPT OMITTED
           
           END-EVALUATE
           END-PERFORM

           IF WS-OPCION-DEP = 2
              PERFORM MENU-OPERACIONES
           END-IF.

       MENU-EXTRACCION.
           MOVE 0 TO WS-OPCION-EXT
           MOVE 0 TO WS-MONTO-EXT

           PERFORM UNTIL WS-MONTO-EXT > 0 
                  AND WS-MONTO-EXT <= WS-SALDO
                  AND WS-MONTO-EXT <= TOPE-EXT
           
           MOVE WS-SALDO TO WS-SALDO-FORMAT
           DISPLAY "***************************************"
           DISPLAY "Su saldo actual es de: $" WS-SALDO-FORMAT
           DISPLAY "***************************************"
           DISPLAY "Ingrese monto a extraer: $"
           DISPLAY SPACES
           ACCEPT WS-MONTO-EXT

           IF WS-MONTO-EXT > WS-SALDO
                   DISPLAY "-----------------------------------------"
                   DISPLAY "Fondos insuficientes. Ingrese otro monto."
                   DISPLAY "-----------------------------------------"
                   DISPLAY "Presiones ENTER"
                   MOVE 0 TO WS-MONTO-EXT
                   ACCEPT OMITTED
              ELSE
                 IF WS-MONTO-EXT > TOPE-EXT
                    DISPLAY "---------------------------------------"
                    DISPLAY "Supera el tope permitido ($" TOPE-EXT ")."
                    DISPLAY "---------------------------------------"
                    DISPLAY "Ingrese un monto menor, apriete ENTER."
                    DISPLAY SPACES
                    MOVE 0 TO WS-MONTO-EXT
                    ACCEPT OMITTED
                 END-IF
              END-IF
           END-PERFORM

           PERFORM UNTIL WS-OPCION-EXT = 1 OR WS-OPCION-EXT = 2 
              DISPLAY "----------------------------------------"
              DISPLAY "1. Confirmar Extraccion"
              DISPLAY "----------------------------------------"
              DISPLAY "2. Volver a pantalla de operaciones"
              DISPLAY "----------------------------------------"
              ACCEPT WS-OPCION-EXT

           EVALUATE WS-OPCION-EXT
                 WHEN 1
                    COMPUTE WS-SALDO = WS-SALDO - WS-MONTO-EXT
                    MOVE WS-SALDO TO WS-SALDO-FORMAT
                    MOVE WS-MONTO-EXT TO WS-MONTO-EXT-FOR
                    DISPLAY "***************************************"
                    DISPLAY "Extraccion exitosa de $ -" WS-MONTO-EXT-FOR
                    DISPLAY "***************************************"

                    MOVE "E" TO WS-DESCRIPCION-TRANS 
                    MOVE WS-MONTO-EXT TO WS-MONTO-TRANS
                    PERFORM REGISTRAR-TRANSACCION

                    CALL 'obtener-sald' USING P-CBF OF CLIENTE 
                                              WS-SALDO-CALCULADO 
                    MOVE WS-SALDO-CALCULADO TO WS-SALDO 

                    *>DISPLAY WS-INDICE-MOV
                    IF WS-INDICE-MOV >= 5 
                    MOVE 0 TO WS-INDICE-MOV
                    END-IF
                    ADD 1 TO WS-INDICE-MOV
                    MOVE 'Extraccion' TO WS-MOV-TIPO(WS-INDICE-MOV)
                    MOVE WS-MONTO-EXT TO WS-MOV-MONTO(WS-INDICE-MOV)
                    DISPLAY "Saldo restante: $" WS-SALDO-FORMAT
                    DISPLAY SPACES
                    DISPLAY "Presione ENTER para MENU OPERACIONES"
                    ACCEPT WS-FECHA-HORA FROM DATE YYYYMMDD  
                    ACCEPT WS-HORA-COMPLETA      FROM TIME
                    MOVE WS-ANIO    TO WS-MOV-ANIO(WS-INDICE-MOV)
                    MOVE WS-MES     TO WS-MOV-MES(WS-INDICE-MOV)
                    MOVE WS-DIA     TO WS-MOV-DIA(WS-INDICE-MOV)
                    MOVE WS-HORA-COMPLETA TO WS-MOV-HORA(WS-INDICE-MOV)
                    MOVE WS-HORA-COMPLETA(1:2) TO WS-HH
                    MOVE WS-HORA-COMPLETA(3:2) TO WS-MM
                    MOVE WS-HORA-COMPLETA(5:2) TO WS-SS
                    ACCEPT OMITTED
                    MOVE 2 TO WS-OPCION-EXT
                 WHEN 2
                    CONTINUE 
                 WHEN OTHER
                    DISPLAY "----------------"
                    DISPLAY "Opcion invalida."
                    DISPLAY "----------------"
                    ACCEPT OMITTED
              END-EVALUATE
           END-PERFORM
           IF WS-OPCION-EXT = 2
               PERFORM MENU-OPERACIONES
           END-IF.

       MENU-ULT-MOVIMIENTOS.
           DISPLAY " "
           DISPLAY "********************************************"
           DISPLAY " "
           
           CALL 'listar-ult-mov' 
                USING P-CBF OF CLIENTE, 10 
           
           DISPLAY " "
           MOVE 0 TO WS-OPCION-MOV
           PERFORM UNTIL WS-OPCION-MOV = 1 OR WS-OPCION-MOV = 2
              DISPLAY "1. Enviar historial por e-mail"
              DISPLAY "2. Volver a pantalla de operaciones"
              DISPLAY "Su opcion: " WITH NO ADVANCING
              ACCEPT WS-OPCION-MOV
              DISPLAY " "

           EVALUATE WS-OPCION-MOV
                  WHEN 1
                      DISPLAY "Enviando historial por email..."
                      DISPLAY "Email enviado correctamente."
                      DISPLAY " "
                      DISPLAY "Presiones ENTER para MENU OPERACIONES"
                      ACCEPT OMITTED
                      MOVE 2 TO WS-OPCION-MOV
                  WHEN 2
                      CONTINUE
                  WHEN OTHER
                      DISPLAY "Opcion invalida. Intente nuevamente."
                      DISPLAY " "
                      ACCEPT OMITTED
              END-EVALUATE
           END-PERFORM

           IF WS-OPCION-MOV = 2
               PERFORM MENU-OPERACIONES
           END-IF.
           
           