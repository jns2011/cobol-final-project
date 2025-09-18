       IDENTIFICATION DIVISION.
       PROGRAM-ID. buscar-cl.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUENTAS-FILE ASSIGN TO "cuentas.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CBF.
       DATA DIVISION.
       FILE SECTION.
       FD  CUENTAS-FILE.
       COPY "registro-cliente.cpy".

       WORKING-STORAGE SECTION.
       COPY "file-status.cpy".
       
       LINKAGE SECTION.
       COPY "cliente.cpy". 

       PROCEDURE DIVISION USING CLIENTE.
       
       COPY "open-file-client.cpy".
      
        MOVE P-CBF TO CLI-CBF.
        READ CUENTAS-FILE
               KEY IS CLI-CBF
               INVALID KEY
                   MOVE "000000" TO P-CBF
               NOT INVALID KEY 
                   MOVE REGISTRO-CLIENTE TO CLIENTE                   
           END-READ.              
       
       COPY "close-file-client.cpy".
       
       GOBACK.
       
       END PROGRAM buscar-cliente.
       