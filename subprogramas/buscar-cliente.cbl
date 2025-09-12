       IDENTIFICATION DIVISION.
       PROGRAM-ID. buscar-cliente.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUENTAS-FILE ASSIGN TO "cuentas.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CBF.
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTE-FILE.
       COPY "registro-cliente.cbl".

       LINKAGE SECTION.
       COPY "cliente.cbl". 

       PROCEDURE DIVISION USING CLIENTE.
       
       OPEN I-O CLIENTE-FILE.
      
        MOVE P-CBF TO CLI-CBF.
        READ CUENTAS-FILE
               KEY IS CLI-CBF
               INVALID KEY
                   MOVE "000000" TO P-CBF
               NOT INVALID KEY 
                   MOVE REGISTRO-CLIENTE TO CLIENTE                   
           END-READ.              
       
       CLOSE CUENTAS-FILE.
       
       GOBACK.
       
       END PROGRAM buscar-cliente.
       