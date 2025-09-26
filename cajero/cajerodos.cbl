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
           88 EXIT-PROGRAM              VALUE 7.
           88 LISTAR-TRANS VALUE 6.   *>

       01  WS-CBF               PIC X(6).
       *>01  WS-PIN-NUM           PIC 9(6).
       01  WS-VALIDO     PIC X VALUE 'N'. 

    
       01  WS-MONTO-FORMAT       PIC ZZZ.ZZZ.ZZ9.
       01  WS-MONTO PIC 9(11).
       01  WS-OPCION-DEP PIC 9(1) VALUE 0.

       
       01  WS-FIN PIC x(1).

       
       01  WS-SALDO            PIC S9(11) VALUE 0.
       01  WS-SALDO-FORMAT PIC -ZZZ.ZZZ.ZZ9.
       01  WS-MONTO-EXT                PIC S9(9).    
       01  WS-MONTO-EXT-FOR PIC -Z.ZZZ.ZZ9.      
       01  WS-OPCION-EXT               PIC 9(1).
       01  TOPE-EXT       PIC 9(6) VALUE 100000.

       
       01  WS-OPCION-SALDO   PIC 9(9) VALUE 0.

       
       01  WS-CBF-DESTINO                 PIC 9(6).
       01  WS-MONTO-TRANSF   PIC S9(9) value 100000. 
       01  WS-MONTO-TRANSF-FOR PIC -Z.ZZZ.ZZ9. 

       01  WS-DESCRIPCION-TRANS          PIC X(1). *>nuevas
       01  WS-MONTO-TRANS                PIC S9(9)V99.
       
       01  WS-OPCION-MOV PIC 9(1).
       01  WS-MOVIMIENTOS.
           05 WS-MOV-ITEM OCCURS 5 TIMES.
              10 WS-MOV-TIPO        PIC X(20).
              10 WS-MOV-MONTO        PIC 9(9). *>S
              *>10 WS-MOV-MONTO-FORM  PIC Z.ZZZ.ZZ9. *> AGREGADO
              10 WS-MOV-ANIO         PIC 9(4).
              10 WS-MOV-MES          PIC 9(2).
              10 WS-MOV-DIA          PIC 9(2).
              10 WS-MOV-HORA        PIC 9(8). 
              
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

       
       
       PROCEDURE DIVISION.

       MAIN-PROGRAM.
           MOVE 'N' TO WS-FIN
           PERFORM UNTIL WS-FIN = 'S'
              PERFORM DISPLAY-MENU
              PERFORM VALIDAR-PIN
           END-PERFORM
           STOP RUN.
           
       DISPLAY-MENU.
           DISPLAY "==============================".
           DISPLAY "=====Bienvenido al Cajero=====".
           DISPLAY "==============================".
           DISPLAY "    -Gracias por elegirnos-".
            
       VALIDAR-PIN.
           MOVE 'N' TO WS-VALIDO 
           PERFORM UNTIL WS-VALIDO = 'S'
               DISPLAY "# Ingrese su CBF de 6 digitos #"
               ACCEPT WS-CBF
        
        
           IF WS-CBF IS NUMERIC
           
           MOVE WS-CBF TO P-CBF OF CLIENTE
           
           CALL 'buscar-cliente' USING CLIENTE
           
          
           IF P-CBF OF CLIENTE NOT = '000000' AND
              P-CBF OF CLIENTE NOT = SPACES
               MOVE 'S' TO WS-VALIDO
               DISPLAY "Cliente encontrado: " P-NOMBRE " " P-APELLIDO
               ACCEPT OMITTED
               PERFORM MENU-OPERACIONES
           ELSE
               DISPLAY "CBF NO VALIDO - Cliente no encontrado"
           END-IF
           ELSE
           DISPLAY "CBF debe contener solo números"
           END-IF
           END-PERFORM.

           






           *>MOVE 'N' TO WS-VALIDO 
           *>PERFORM UNTIL WS-VALIDO = 'S'
           *>DISPLAY "# Ingrese su CBF de 6 digitos #"
           *>ACCEPT WS-CBF
               
               *>IF WS-CBF IS NUMERIC AND LENGTH OF WS-CBF = 6
                   *>MOVE WS-CBF TO WS-PIN-NUM
                   *>CALL 'buscar-cliente' USING CLIENTE
                   *>IF (P-CBF OF CLIENTE NOT= '000000')
                   *>MOVE 'S' TO WS-VALIDO
                   
                   *>PERFORM MENU-OPERACIONES
               *>ELSE
                   *>DISPLAY "CBF NO VALIDO"
               *>END-IF
           *>END-PERFORM.

           

       MENU-OPERACIONES.
           CALL "SYSTEM" USING "CLS".
           DISPLAY "================================".
           DISPLAY "// Por favor, elija una opcion \\".
           DISPLAY "1.Saldo".
           DISPLAY "2.Transferencia".
           DISPLAY "3.Deposito".
           DISPLAY "4.Extraccion".
           DISPLAY "5.Ultimos Movimientos".
           DISPLAY "6.Ver transacciones".
           DISPLAY "7.Salir"
           DISPLAY "Su opcion: " WITH NO ADVANCING.
           ACCEPT MENU-CHOICE.
           
           EVALUATE TRUE 
                WHEN CONSULTA-SALDO PERFORM MENU-SALDO
                WHEN REALIZAR-TRANSFERENCIA PERFORM MENU-TRANSFERENCIA
                WHEN REALIZAR-DEPOSITO PERFORM MENU-DEPOSITO
                WHEN REALIZAR-EXTRACCION PERFORM MENU-EXTRACCION
                WHEN ULT-MOVIMIENTOS PERFORM MENU-ULT-MOVIMIENTOS
                WHEN EXIT-PROGRAM MOVE 'N' TO WS-FIN 
                WHEN LISTAR-TRANS CALL 'listar-transacciones'
                    USING P-CBF OF CLIENTE  *>USING P-CBF CANT-TRANS
                WHEN OTHER DISPLAY "Opcion invalida"
           END-EVALUATE.
           
       REGISTRAR-TRANSACCION.
           MOVE WS-CBF TO P-CBF OF TRANSACCION 
           MOVE WS-DESCRIPCION-TRANS TO P-DESCRIPCION
           MOVE WS-MONTO-TRANS TO P-IMPORTE
    
           CALL "crear-transaccion" USING TRANSACCION, RESULTADO

           *>PERFORM UNTIL WS-OPCION-SALDO = 1 OR WS-OPCION-SALDO = 2
           *>DISPLAY "1.Enviar por e-mail"
           *>DISPLAY "2.Volver a pantalla de operaciones"
           *>ACCEPT WS-OPCION-SALDO 

           *>EVALUATE WS-OPCION-SALDO
               *>WHEN 1 
                   *>DISPLAY "Enviando por e-mail..."
                   *>ACCEPT OMITTED 
                   *>MOVE 2 TO WS-OPCION-SALDO
               *>WHEN 2 
                   *>CONTINUE 
               *>WHEN OTHER 
                   *>DISPLAY "Opcion invalida"
                   *>ACCEPT OMITTED
           *>END-EVALUATE
           *>END-PERFORM

           *>IF WS-OPCION-SALDO = 2
               *>PERFORM MENU-OPERACIONES
           *>END-IF.

    
           IF RESULTADO NOT = 0
               DISPLAY "Error al registrar transaccion en archivo"
           END-IF.



       MENU-SALDO.
           MOVE 0 TO WS-OPCION-SALDO
           MOVE WS-SALDO TO WS-SALDO-FORMAT
           DISPLAY "Su saldo actual es de: " "$" WS-SALDO-FORMAT
           PERFORM UNTIL WS-OPCION-SALDO = 1 OR WS-OPCION-SALDO = 2
           DISPLAY "1.Enviar por e-mail"
           DISPLAY "2.Volver a pantalla de operaciones"
           ACCEPT WS-OPCION-SALDO

           EVALUATE WS-OPCION-SALDO
               WHEN 1 
                   DISPLAY "Enviando por e-mail..."
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
           DISPLAY "Ingrese el CBF destino:"
           ACCEPT WS-CBF-DESTINO
           *>DISPLAY "Nombre: " P-NOMBRE " " P-APELLIDO
           *>DISPLAY "Ingrese monto a transferir:"
           *>ACCEPT WS-MONTO-TRANSF
           MOVE P-CBF OF CLIENTE TO WS-CBF
           MOVE WS-CBF-DESTINO TO P-CBF OF CLIENTE
           *>UTILIZAR REGISTRO INTERMEDIO
           CALL 'buscar-cliente' USING CLIENTE
    
   
           IF P-CBF OF CLIENTE = '000000' OR P-CBF OF CLIENTE = SPACES
               DISPLAY "CBF destino no válido. Operación cancelada."
               ACCEPT OMITTED
               PERFORM MENU-OPERACIONES
           ELSE
       
           DISPLAY "Nombre del destinatario: " P-NOMBRE " " P-APELLIDO
           DISPLAY "Ingrese monto a transferir:"
           ACCEPT WS-MONTO-TRANSF
           
           
           
           
           IF WS-MONTO-TRANSF > WS-SALDO 
               DISPLAY "Fondos insuficientes. Operacion cancelada."
               ACCEPT OMITTED
               PERFORM MENU-OPERACIONES
           ELSE
               SUBTRACT WS-MONTO-TRANSF FROM WS-SALDO
               MOVE WS-SALDO TO WS-SALDO-FORMAT
               MOVE WS-MONTO-TRANSF TO WS-MONTO-TRANSF-FOR
               MOVE WS-MONTO-TRANSF TO WS-MONTO-TRANS


               MOVE "D" TO WS-DESCRIPCION-TRANS
               PERFORM REGISTRAR-TRANSACCION

               IF (RESULTADO NOT = 0)
                   ADD WS-MONTO-TRANSF TO WS-SALDO
                   DISPLAY("TRANSFERENCIA NO POSIBLE.")
                   ACCEPT OMITTED
                   PERFORM MENU-OPERACIONES
               END-IF

               MOVE "T" TO WS-DESCRIPCION-TRANS
               MOVE WS-CBF TO P-CBF OF CLIENTE
               PERFORM REGISTRAR-TRANSACCION

               IF WS-INDICE-MOV >= 5 
               MOVE 0 TO WS-INDICE-MOV
               END-IF
               ADD 1 TO WS-INDICE-MOV
                 MOVE 'Transferencia' TO WS-MOV-TIPO(WS-INDICE-MOV)
                 MOVE WS-MONTO-TRANSF TO WS-MOV-MONTO(WS-INDICE-MOV) 
               DISPLAY "Trasferencia exitosa de $ -" WS-MONTO-TRANSF-FOR 
               DISPLAY "Su nuevo saldo es de: $" WS-SALDO-FORMAT
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
           DISPLAY "Ingrese monto a depositar:"
           ACCEPT WS-MONTO
           PERFORM UNTIL WS-OPCION-DEP = 1 OR WS-OPCION-DEP = 2 
           DISPLAY "1. Confirmar Deposito"
           DISPLAY "2. Volver a pantalla de operaciones"
           ACCEPT WS-OPCION-DEP

           EVALUATE WS-OPCION-DEP
              WHEN 1
                 MOVE WS-MONTO TO WS-MONTO-FORMAT
                 DISPLAY "Deposito confirmado por $" WS-MONTO-FORMAT
                 COMPUTE WS-SALDO = WS-MONTO + WS-SALDO
                 MOVE WS-SALDO TO WS-SALDO-FORMAT
                 MOVE P-CBF OF CLIENTE TO WS-CBF
                 MOVE "D" TO WS-DESCRIPCION-TRANS 
                 MOVE WS-MONTO TO WS-MONTO-TRANS
                 PERFORM REGISTRAR-TRANSACCION

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
                 DISPLAY "Opcion invalida"
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
           DISPLAY "Su saldo actual es de: $" WS-SALDO-FORMAT
           DISPLAY "Ingrese monto a extraer:"
           ACCEPT WS-MONTO-EXT

           IF WS-MONTO-EXT > WS-SALDO
                   DISPLAY "Fondos insuficientes. Ingrese otro monto."
                   MOVE 0 TO WS-MONTO-EXT
                   ACCEPT OMITTED
              ELSE
                 IF WS-MONTO-EXT > TOPE-EXT
                    DISPLAY "Supera el tope permitido ($" TOPE-EXT ")."
                    DISPLAY "Ingrese un monto menor, apriete ENTER."
                    MOVE 0 TO WS-MONTO-EXT
                    ACCEPT OMITTED
                 END-IF
              END-IF
           END-PERFORM

           PERFORM UNTIL WS-OPCION-EXT = 1 OR WS-OPCION-EXT = 2  
              DISPLAY "1. Confirmar Extraccion"
              DISPLAY "2. Volver a pantalla de operaciones"
              ACCEPT WS-OPCION-EXT

           EVALUATE WS-OPCION-EXT
                 WHEN 1
                    COMPUTE WS-SALDO = WS-SALDO - WS-MONTO-EXT
                    MOVE WS-SALDO TO WS-SALDO-FORMAT
                    MOVE WS-MONTO-EXT TO WS-MONTO-EXT-FOR
                    DISPLAY "Extraccion exitosa de $ -" WS-MONTO-EXT-FOR
                    MOVE P-CBF OF CLIENTE TO WS-CBF
                    MOVE "E" TO WS-DESCRIPCION-TRANS 
                    MOVE WS-MONTO-EXT TO WS-MONTO-TRANS
                    PERFORM REGISTRAR-TRANSACCION

                    DISPLAY WS-INDICE-MOV
                    IF WS-INDICE-MOV >= 5 
                    MOVE 0 TO WS-INDICE-MOV
                    END-IF
                    ADD 1 TO WS-INDICE-MOV
                    MOVE 'Extraccion' TO WS-MOV-TIPO(WS-INDICE-MOV)
                    MOVE WS-MONTO-EXT TO WS-MOV-MONTO(WS-INDICE-MOV)
                    DISPLAY "Saldo restante: $" WS-SALDO-FORMAT
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
                    DISPLAY "Opcion invalida."
                    ACCEPT OMITTED
              END-EVALUATE
           END-PERFORM
           IF WS-OPCION-EXT = 2
               PERFORM MENU-OPERACIONES
           END-IF.

       MENU-ULT-MOVIMIENTOS.
           DISPLAY "====== Ultimos Movimientos ======"
           PERFORM VARYING WS-INDICE-LISTAR FROM 1 BY 1 
               UNTIL WS-INDICE-LISTAR > 5
               IF WS-MOV-TIPO(WS-INDICE-LISTAR) NOT = SPACES
                MOVE WS-MOV-HORA(WS-INDICE-LISTAR)(1:2) TO WS-HH
                MOVE WS-MOV-HORA(WS-INDICE-LISTAR)(3:2) TO WS-MM
                MOVE WS-MOV-HORA(WS-INDICE-LISTAR)(5:2) TO WS-SS
                 *>COMPUTE WS-MOV-MONTO(WS-INDICE-LISTAR) = 
                 *>WS-MOV-MONTO(WS-INDICE-LISTAR) * -1
                *>MOVE WS-MOV-MONTO(WS-INDICE-LISTAR) 
                *>TO WS-MOV-MONTO-FORM
                       *>IF WS-DESCRIPCION-TRANS = "T" OR 
                       *>WS-DESCRIPCION-TRANS = "E"
                
                 *>END-IF
                DISPLAY WS-MOV-TIPO(WS-INDICE-LISTAR) " por $" 
                    WS-MOV-MONTO(WS-INDICE-LISTAR)
                    " - Fecha: " WS-MOV-DIA(WS-INDICE-LISTAR) "/"
                                 WS-MOV-MES(WS-INDICE-LISTAR) "/"
                                 WS-MOV-ANIO(WS-INDICE-LISTAR)
                    " " WS-HH ":" WS-MM ":" WS-SS
               END-IF
           END-PERFORM

           MOVE 0 TO WS-OPCION-MOV
           PERFORM UNTIL WS-OPCION-MOV = 1 OR WS-OPCION-MOV = 2
              DISPLAY "1. Enviar por e-mail"
              DISPLAY "2. Volver a pantalla de operaciones"
              ACCEPT WS-OPCION-MOV

           EVALUATE WS-OPCION-MOV
                  WHEN 1
                      DISPLAY "Enviando ultimos movimientos por email"
                      ACCEPT OMITTED
                      MOVE 2 TO WS-OPCION-MOV
                  WHEN 2
                      CONTINUE
                  WHEN OTHER
                      DISPLAY "Opcion invalida"
                      ACCEPT OMITTED
              END-EVALUATE
           END-PERFORM

           IF WS-OPCION-MOV = 2
               PERFORM MENU-OPERACIONES
           END-IF.

           