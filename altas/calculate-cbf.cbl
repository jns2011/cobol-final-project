          IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-CBF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SUMA-DIGITOS   PIC 9(4) VALUE 0.
       01 TMP            PIC 9(4).
       01 RESTO          PIC 9(2).
       01 DV             PIC 9.
       01 I              PIC 9 VALUE 1.
       01 PESOS.
           05 PESO OCCURS 5 TIMES PIC 9 VALUE ZEROS.
       01 DIGITOS.
           05 DIGITO OCCURS 5 TIMES PIC 9 VALUE ZEROS.

       LINKAGE SECTION.
       COPY "id-cuentas.cpy".
       COPY "clave-bancaria.cpy".

       PROCEDURE DIVISION USING ID-CUENTA, CLAVE-BANCARIA.
           MOVE 6 TO PESO(1)
           MOVE 5 TO PESO(2)
           MOVE 4 TO PESO(3)
           MOVE 3 TO PESO(4)
           MOVE 2 TO PESO(5)

           MOVE ID-D1 TO DIGITO(1)
           MOVE ID-D2 TO DIGITO(2)
           MOVE ID-D3 TO DIGITO(3)
           MOVE ID-D4 TO DIGITO(4)
           MOVE ID-D5 TO DIGITO(5)

           MOVE 0 TO SUMA-DIGITOS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MULTIPLY PESO(I) BY DIGITO(I) GIVING TMP
               ADD TMP TO SUMA-DIGITOS
           END-PERFORM

           DIVIDE SUMA-DIGITOS BY 11 GIVING TMP REMAINDER RESTO
           COMPUTE DV = 11 - RESTO
           IF DV = 10 OR DV = 11
              MOVE 0 TO DV
           END-IF

           MOVE ID-D1 TO CLAVE-BANCARIA(1:1)
           MOVE ID-D2 TO CLAVE-BANCARIA(2:1)
           MOVE ID-D3 TO CLAVE-BANCARIA(3:1)
           MOVE ID-D4 TO CLAVE-BANCARIA(4:1)
           MOVE ID-D5 TO CLAVE-BANCARIA(5:1)
           MOVE DV    TO CLAVE-BANCARIA(6:1)

           GOBACK.
           