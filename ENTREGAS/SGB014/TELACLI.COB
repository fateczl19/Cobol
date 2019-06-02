
       01  TELACLI.
           05  LINE 01  COLUMN 01 
               VALUE  "---CONSULTA DE CLIENTE------------------".
           05  LINE 01  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  "   CPF:".
           05  LINE 06  COLUMN 01 
               VALUE  "   NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  "   TELEFONE:".
           05  LINE 10  COLUMN 01 
               VALUE  "   BAIRRO:".
           05  LINE 16  COLUMN 01 
               VALUE  "---AREA DE MENSAGENS--------------------".
           05  LINE 16  COLUMN 41 
               VALUE  "----------------------------------------".
           05  TCPF
               LINE 04  COLUMN 10  PIC 999.999.999.99
               USING  CPF.
           05  TNOME
               LINE 06  COLUMN 10  PIC X(35)
               USING  NOME.
           05  TTEL
               LINE 08  COLUMN 14  PIC 9(11)
               USING  TEL.
           05  TBAIRRO
               LINE 10  COLUMN 12  PIC X(20)
               USING  BAIRRO.
