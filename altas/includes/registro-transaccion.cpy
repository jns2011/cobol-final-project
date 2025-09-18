       01  R-RECORD.
           05 R-KEY.
              10 R-CBF        PIC 9(6).
              10 R-TIMESTAMP  PIC 9(14).
              10  R-TIMESTAMP-REDEF REDEFINES R-TIMESTAMP.                          
                 15 R-ANIO              PIC 9(4).
                 15 R-MES               PIC 9(2).
                 15 R-DIA               PIC 9(2).
                 15 R-HORA              PIC 9(2).
                 15 R-MIN               PIC 9(2).
                 15 R-SEG               PIC 9(2).
           05 R-DESCRIPCION   PIC X(1).
           05 R-IMPORTE       PIC S9(9)V99.
           05 R-IMPORTE-FORM  PIC -Z(9).99.

      
       
       

