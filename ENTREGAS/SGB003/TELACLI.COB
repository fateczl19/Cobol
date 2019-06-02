
       01  TELACLI.
           05  LINE 01  COLUMN 01 
               VALUE  "                    * CADASTRO DE CLIENT".
           05  LINE 01  COLUMN 41 
               VALUE  "E *".
           05  LINE 04  COLUMN 01 
               VALUE  "   CPF:".
           05  LINE 05  COLUMN 01 
               VALUE  "   RG:".
           05  LINE 06  COLUMN 01 
               VALUE  "   NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "   DAT. NASCIMENTO:".
           05  LINE 08  COLUMN 01 
               VALUE  "   E-MAIL:".
           05  LINE 09  COLUMN 01 
               VALUE  "   CEP:             LOGRADOURO:".
           05  LINE 10  COLUMN 01 
               VALUE  "   NUMERO:       COMPLEMENTO:".
           05  LINE 11  COLUMN 01 
               VALUE  "   BAIRRO:                       CIDADE:".
           05  LINE 11  COLUMN 41 
               VALUE  "              -".
           05  LINE 12  COLUMN 01 
               VALUE  "   TELEFONE:".
           05  LINE 13  COLUMN 01 
               VALUE  "   SITUACAO:   -".
           05  LINE 16  COLUMN 01 
               VALUE  "---AREA DE MENSAGENS--------------------".
           05  LINE 16  COLUMN 41 
               VALUE  "----------------------------------------".
           05  TCPF
               LINE 04  COLUMN 10  PIC 999.999.999.99
               USING  CPF.
           05  TRG
               LINE 05  COLUMN 08  PIC X(09)
               USING  RG.
           05  TNOME
               LINE 06  COLUMN 10  PIC X(35)
               USING  NOME.
           05  TDATNASC
               LINE 07  COLUMN 22  PIC 99/99/9999
               USING  DATNASC.
           05  TEMAIL
               LINE 08  COLUMN 12  PIC X(20)
               USING  EMAIL.
           05  TCEP
               LINE 09  COLUMN 09  PIC 9(08)
               USING  CEP.
           05  TLOGRAD
               LINE 09  COLUMN 33  PIC X(35)
               USING  LOGRAD.
           05  TNUM
               LINE 10  COLUMN 12  PIC Z(04)
               USING  NUM.
           05  TCOMPL
               LINE 10  COLUMN 31  PIC X(47)
               USING  COMPL.
           05  TBAIRRO
               LINE 11  COLUMN 12  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 11  COLUMN 42  PIC X(12)
               USING  CIDADE.
           05  TUF
               LINE 11  COLUMN 57  PIC X(02)
               USING  UF.
           05  TTEL
               LINE 12  COLUMN 14  PIC 9(11)
               USING  TEL.
           05  TSITUAC
               LINE 13  COLUMN 14  PIC X(01)
               USING  SITUAC.
           05  TDSCSITUAC
               LINE 13  COLUMN 18  PIC X(08)
               USING  DSCSITUAC.
