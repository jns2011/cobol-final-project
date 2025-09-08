        OPEN I-O CUENTAS-FILE
       IF FILE-NOT-READY
           DISPLAY "Archivo no existe, creando uno nuevo..."
           OPEN OUTPUT CUENTAS-FILE
           CLOSE CUENTAS-FILE
           OPEN I-O CUENTAS-FILE
           IF NOT FILE-SUCCESS
               DISPLAY "No se pudo abrir el archivo"
               STOP RUN
           END-IF
       END-IF.
