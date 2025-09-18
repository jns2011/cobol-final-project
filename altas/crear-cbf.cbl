       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-CBF.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "cuentas.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CSV-FILE.
        01 CSV-REGISTRO.
             05 CBF  PIC 9(6).
             05 APELLIDO PIC X(30).
             05 NOMBRE PIC X(30).
             05 EMAIL  PIC X(50).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG      PIC 9 VALUE 0.
       01 ID-CUENTA-STR PIC X(6).
       
       COPY "file-status.cpy".
       COPY "cliente.cpy".

       LINKAGE SECTION.
       COPY "id-cuentas.cpy".
       COPY "clave-bancaria.cpy".

       PROCEDURE DIVISION USING ID-CUENTA, CLAVE-BANCARIA.

           OPEN INPUT CSV-FILE.
           PERFORM UNTIL EOF-FLAG = 1
             READ CSV-FILE
               AT END MOVE 1 TO EOF-FLAG
               NOT AT END

           MOVE CSV-REGISTRO(1:6) TO ID-CUENTA-STR
             INSPECT ID-CUENTA-STR REPLACING ALL ',' BY ' '
           MOVE ID-CUENTA-STR(1:1) TO ID-D1
           MOVE ID-CUENTA-STR(2:1) TO ID-D2
           MOVE ID-CUENTA-STR(3:1) TO ID-D3
           MOVE ID-CUENTA-STR(4:1) TO ID-D4
           MOVE ID-CUENTA-STR(5:1) TO ID-D5


           CALL "CALCULATE-CBF" USING ID-CUENTA, CLAVE-BANCARIA
           DISPLAY "Cuenta: " ID-CUENTA-STR
             " | Clave Bancaria: " CLAVE-BANCARIA

           MOVE CLAVE-BANCARIA TO P-CBF

           CALL "BUSCAR-CLIENTE" USING CLIENTE

           IF P-CBF = "000000"
             CALL "GUARDAR-CLIENTE" USING CLAVE-BANCARIA,CSV-REGISTRO
           ELSE
             DISPLAY "Cliente ya existente"
           END-IF
             END-READ
           END-PERFORM.

           CLOSE CSV-FILE.
           GOBACK.
           
