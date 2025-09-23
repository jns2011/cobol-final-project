       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-CBF.
       AUTHOR. Rodas, Matteoda, Lopez,Condotta.

       DATA DIVISION.
     
       
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01 ID-CUENTA-STR PIC X(6).
      
       COPY "id-cuentas.cpy".

       PROCEDURE DIVISION USING ID-CUENTA-STR, ID-CUENTA.

           MOVE ID-CUENTA-STR(1:1) TO ID-D1
           MOVE ID-CUENTA-STR(2:1) TO ID-D2
           MOVE ID-CUENTA-STR(3:1) TO ID-D3
           MOVE ID-CUENTA-STR(4:1) TO ID-D4
           MOVE ID-CUENTA-STR(5:1) TO ID-D5



           GOBACK.
           
