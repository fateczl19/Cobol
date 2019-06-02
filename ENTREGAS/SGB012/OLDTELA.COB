 
01  TELABIKE.
           05  LINE 01  COLUMN 01 
               VALUE  "                          * CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  "BICICLETAS *".
           05  LINE 03  COLUMN 01 
               VALUE  "   NUMERO:".
           05  LINE 04  COLUMN 01 
               VALUE  "   MARCA:".
           05  LINE 05  COLUMN 01 
               VALUE  "   MODELO:".
      *     05  LINE 06  COLUMN 01 
      *         VALUE  "   CATEGORIA:".
      *     05  LINE 07  COLUMN 01 
      *         VALUE  "   ARO:".
      *     05  LINE 07  COLUMN 41 
      *         VALUE  "   DATA COMPRA:".
      *     05  LINE 08  COLUMN 01 
      *         VALUE  "   COR:    -                           D".
      *     05  LINE 08  COLUMN 41 
      *         VALUE  "ATA ULTIMO USO:".
      *     05  LINE 10  COLUMN 01 
      *         VALUE  "   VALOR COMPRA:".
      *     05  LINE 11  COLUMN 01 
      *         VALUE  "   VALOR HORA:".
      *     05  LINE 12  COLUMN 01 
      *         VALUE  "   VALOR LOCACAO HORA:".
      *     05  LINE 14  COLUMN 01 
      *         VALUE  "                                       S".
      *     05  LINE 14  COLUMN 41 
      *         VALUE  "ITUACAO:   -".
           05  LINE 15  COLUMN 04 
               VALUE  "AREA DE MENSAGEMS".
           05  TNUMERO
               LINE 03  COLUMN 15  PIC ZZZ9
               USING  WNUMERO
               HIGHLIGHT.
           05  TMARCA
               LINE 04  COLUMN 15  PIC X(20)
               USING  MARCA-BIKE.
           05  TMODELO
               LINE 05  COLUMN 15  PIC X(20)
               USING  MODELO-BIKE.
      *     05  TCATEG
      *         LINE 06  COLUMN 15  PIC X(01)
      *         USING  CATEG.
      *     05  TARO
      *         LINE 07  COLUMN 10  PIC 9(02)
      *         USING  ARO.
      *     05  TDATCOMPRA
      *         LINE 07  COLUMN 57  PIC 99/99/9999
      *         USING  DATCOMPRA-BIKE
      *         HIGHLIGHT.
      *     05  TCOR
      *         LINE 08  COLUMN 10  PIC 9(01)
      *         USING  COR.
      *     05  TCORDESC
      *         LINE 08  COLUMN 14  PIC X(10)
      *         USING  CORDESC.
      *     05  TDATULTUSO
      *         LINE 08  COLUMN 57  PIC 99/99/9999
      *         USING  DATULTUSO-BIKE
      *         HIGHLIGHT.
      *     05  TVALCOMPRA
      *         LINE 10  COLUMN 24  PIC ZZZZZ9,99
      *         USING  VALCOMPRA.
      *     05  TVALHORA
      *         LINE 11  COLUMN 24  PIC ZZZZZ9,99
      *         USING  VALHORA.
      *     05  TVALLOC
      *         LINE 12  COLUMN 24  PIC ZZZZZ9,99
      *         USING  VALLOC.
      *     05  TSITUAC
      *         LINE 14  COLUMN 50  PIC X(01)
      *         USING  SITUAC.
      *     05  TSITUACDESC
      *         LINE 14  COLUMN 54  PIC X(11)
      *         USING  SITUACDESC.
