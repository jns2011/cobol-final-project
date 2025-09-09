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
       COPY "includes/file-status.cbl".

       LINKAGE SECTION.
         01 ID-CUENTA.
           05 ID-D1   PIC 9.
           05 ID-D2   PIC 9.
           05 ID-D3   PIC 9.
           05 ID-D4   PIC 9.
           05 ID-D5   PIC 9.
       01 CLAVE-BANCARIA PIC X(6).

       PROCEDURE DIVISION USING ID-CUENTA, CLAVE-BANCARIA.

           COPY "includes/open.cbl".

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

             CALL "BUSCAR-CLIENTE" USING CLAVE-BANCARIA, ID-CUENTA-STR

             IF ID-CUENTA-STR = "000000"
                 CALL "GUARDAR-CLIENTE" USING CLAVE-BANCARIA,
                  CSV-REGISTRO
             ELSE
               DISPLAY "Cliente ya existente"
                END-IF
             END-READ
           END-PERFORM.

           COPY "includes/close.cbl".

           GOBACK.
