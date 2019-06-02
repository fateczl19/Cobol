
       01  CADLOC.
           05  LINE 01  COLUMN 01 
               VALUE  "                    * MOVIMENTO DE LOCAC".
           05  LINE 01  COLUMN 41 
               VALUE  "AO *".
           05  LINE 03  COLUMN 01 
               VALUE  "   CPF:                      NOME:".
           05  LINE 04  COLUMN 01 
               VALUE  "                         TELEFONE:".
           05  LINE 05  COLUMN 01 
               VALUE  "   BIKE:        MARCA:".
           05  LINE 05  COLUMN 41 
               VALUE  "    MODELO:".
           05  LINE 07  COLUMN 01 
               VALUE  "   LOCACAO:                 HORARIO:   :".
           05  LINE 08  COLUMN 01 
               VALUE  "   PREVISAO:                HORARIO:   :".
           05  LINE 09  COLUMN 01 
               VALUE  "   ENTREGA:                 HORARIO:   :".
           05  LINE 10  COLUMN 01 
               VALUE  "                       TEMPO DE USO:   :".
           05  LINE 12  COLUMN 01 
               VALUE  "   VALOR LOCACAO:".
           05  LINE 13  COLUMN 01 
               VALUE  "   VALOR COBRADO:".
           05  LINE 15  COLUMN 01 
               VALUE  "   STATUS:   -".
           05  TCPF-CLI
               LINE 03  COLUMN 09  PIC 999.999.999.99
               USING  CPF-CLI.
           05  TNOME-CLI
               LINE 03  COLUMN 36  PIC X(30)
               USING  NOME-CLI.
           05  TTEL-CLI
               LINE 04  COLUMN 36  PIC 9(11)
               USING  TEL-CLI.
           05  TNUM-BIKE
               LINE 05  COLUMN 10  PIC 9(04)
               USING  NUM-BIKE.
           05  TMARCA-BIKE
               LINE 05  COLUMN 24  PIC X(20)
               USING  MARCA-BIKE.
           05  TMODELO-BIKE
               LINE 05  COLUMN 53  PIC X(20)
               USING  MODELO-BIKE.
           05  TDTLOCACAO
               LINE 07  COLUMN 14  PIC 99/99/9999
               USING  DTLOCACAO.
           05  THRLOCACAO
               LINE 07  COLUMN 38  PIC 9(02)
               USING  HRLOCACAO.
           05  TMINLOCACAO
               LINE 07  COLUMN 41  PIC 9(02)
               USING  MINLOCACAO.
           05  TDTPREVISAO
               LINE 08  COLUMN 14  PIC 99/99/9999
               USING  DTPREVISAO.
           05  THRPREVISAO
               LINE 08  COLUMN 38  PIC 9(02)
               USING  HRPREVISAO.
           05  TMINPREVISAO
               LINE 08  COLUMN 41  PIC 9(02)
               USING  MINPREVISAO.
           05  TDTENTREGA
               LINE 09  COLUMN 14  PIC 99/99/9999
               USING  DTENTREGA.
           05  THRENTREGA
               LINE 09  COLUMN 38  PIC 9(02)
               USING  HRENTREGA.
           05  TMINENTREGA
               LINE 09  COLUMN 41  PIC 9(02)
               USING  MINENTREGA.
           05  THRUSO
               LINE 10  COLUMN 38  PIC 9(02)
               USING  HRUSO.
           05  TMINUSO
               LINE 10  COLUMN 41  PIC 9(02)
               USING  MINUSO.
           05  TVALLOCACAO
               LINE 12  COLUMN 19  PIC ZZZZZ9,99
               USING  VALLOCACAO.
           05  TVALCOBRADO
               LINE 13  COLUMN 19  PIC ZZZZZ9,99
               USING  VALCOBRADO.
           05  TSTATUS-BIKE
               LINE 15  COLUMN 12  PIC X(01)
               USING  STATUS-BIKE.
           05  TDSCSTATUS-BIKE
               LINE 15  COLUMN 16  PIC X(08)
               USING  DSCSTATUS-BIKE.
