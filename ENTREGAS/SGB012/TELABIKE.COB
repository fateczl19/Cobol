
       01  TELABIKE.
           05  LINE 01  COLUMN 01 
               VALUE  "---CONSULTA DE BICICLETAS---------------".
           05  LINE 01  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 05  COLUMN 01 
               VALUE  "   NUMERO:        MARCA".
           05  LINE 05  COLUMN 41 
               VALUE  "        MODELO".
           05  LINE 15  COLUMN 01 
               VALUE  "---AREA DE MENSAGENS--------------------".
           05  LINE 15  COLUMN 41 
               VALUE  "----------------------------------------".
           05  TNUMERO
               LINE 05  COLUMN 12  PIC 9(04)
               USING  NUMERO.
           05  TMARCA
               LINE 05  COLUMN 25  PIC X(20)
               USING  MARCA.
           05  TMODELO
               LINE 05  COLUMN 56  PIC X(20)
               USING  MODELO.
