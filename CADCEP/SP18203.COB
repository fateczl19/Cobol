       IDENTIFICATION DIVISION.
       PROGRAM-ID. SP18203.
       AUTHOR. WILLIAM RODRIGUES BRITO.
      **************************************
      * MANUTENCAO DO CADASTRO DE CEP   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOG
                                            WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CEP.DAT".
       01 REGCEP.
                03 CEP-COD          PIC 99999999.
                03 CEP-LOG          PIC X(35).
                03 CEP-BAIR         PIC X(20).
                03 CEP-CID          PIC X(12).
                03 CEP-UF           PIC X(02).
                03 CEP-UFC          PIC X(20).
                03 CEP-LONG         PIC X(12).
                03 CEP-LATI         PIC X(12).
                03 CEP-CODRC        PIC 9(01).
                03 CEP-RC           PIC X(12).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
       01 TABCEP.
          03 TN1 PIC X(48) VALUE
          "NORTE       NORDESTE    SUL         SUDOESTE  ".
          03 TN2 PIC X(48) VALUE
          "LESTE       OESTE       CENTRO      RURAL     ".
          03 TN3 PIC X(12) VALUE "NAO INFORMDO".

       01 TABAUX1 REDEFINES TABCEP.
           03 TDCEP       PIC X(12) OCCURS 10 TIMES.
      *---------------------------------------------------------
       SCREEN SECTION.
       01  SP18203.
           05  LINE 02  COLUMN 01
               VALUE  "                         CADASTRO DE CEP".
           05  LINE 04  COLUMN 01
               VALUE  "           CODIGO".
           05  LINE 06  COLUMN 01
               VALUE  "           LOGRADOURO".
           05  LINE 08  COLUMN 01
               VALUE  "           BAIRRO".
           05  LINE 10  COLUMN 01
               VALUE  "           CIDADE".
           05  LINE 12  COLUMN 01
               VALUE  "           UF                    -".
           05  LINE 14  COLUMN 01
               VALUE  "           LONGITUDE".
           05  LINE 16  COLUMN 01
               VALUE  "           LATITUDE".
           05  LINE 18  COLUMN 01
               VALUE  "           REGIAO DA CIDADE     -".
           05  TCOD
               LINE 04  COLUMN 31  PIC 99999999
               USING  CEP-COD
               HIGHLIGHT.
           05  TLOG
               LINE 06  COLUMN 31  PIC X(35)
               USING  CEP-LOG
               HIGHLIGHT.
           05  TBAIR
               LINE 08  COLUMN 31  PIC X(20)
               USING  CEP-BAIR
               HIGHLIGHT.
           05  TCID
               LINE 10  COLUMN 31  PIC X(20)
               USING  CEP-CID
               HIGHLIGHT.
           05  TUF
               LINE 12  COLUMN 31  PIC X(02)
               USING  CEP-UF
               HIGHLIGHT.
           05  TUFC
               LINE 12  COLUMN 36  PIC X(20)
               USING  CEP-UFC
               HIGHLIGHT.
           05  TLONG
               LINE 14  COLUMN 31  PIC X(12)
               USING  CEP-LONG
               HIGHLIGHT.
           05  TLATI
               LINE 16  COLUMN 31  PIC X(12)
               USING  CEP-LATI
               HIGHLIGHT.
           05  TCODRC
               LINE 18  COLUMN 31  PIC 9(01)
               USING  CEP-CODRC
               HIGHLIGHT.
           05  TRC
               LINE 18  COLUMN 35  PIC X(12)
               USING  CEP-RC
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CEP
                      MOVE "*** ARQUIVOCEP SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      CLOSE CEP
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CEP-COD CEP-CODRC

                MOVE SPACES TO CEP-LOG CEP-BAIR CEP-CID CEP-UF CEP-UFC
                               CEP-LONG CEP-LATI CEP-RC

                DISPLAY SP18203.

       INC-002.
                ACCEPT TCOD
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CEP
                   GO TO ROT-FIM.
                IF CEP-COD = ZEROS
                   MOVE "*** COD INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP.
                MOVE 0 TO W-SEL
                READ CEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY SP18203
                      MOVE "***CEP JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TLOG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-LOG = SPACES
                     MOVE "****PREENCHA O DENOM DOCEP****" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-003
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TBAIR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-BAIR = SPACES
                    MOVE "****PREENCHA O TS DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TCID
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-CID = ZEROS
                    MOVE "****PREENCHA O NIVEL DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT  TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-UF = SPACES
                    MOVE "****PREENCHA O SALB DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006
                ELSE
                IF CEP-UF = 'AC'
                    MOVE "ACRE" TO CEP-UFC
                ELSE IF CEP-UF = 'AL'
                        MOVE "ALAGOAS" TO CEP-UFC
                ELSE IF CEP-UF = 'AM'
                        MOVE "AMAZONAS" TO CEP-UFC
                ELSE IF CEP-UF = 'AP'
                        MOVE "AMAPA" TO CEP-UFC
                ELSE IF CEP-UF = 'BA'
                        MOVE "BAHIA" TO CEP-UFC
                ELSE IF CEP-UF = 'CE'
                        MOVE "CEARA" TO CEP-UFC
                ELSE IF CEP-UF = 'DF'
                        MOVE "DISTRITO F" TO CEP-UFC
                ELSE IF CEP-UF = 'ES'
                        MOVE "ESP SANTO" TO CEP-UFC
                ELSE IF CEP-UF = 'GO'
                        MOVE "GOIAS" TO CEP-UFC
                ELSE IF CEP-UF = 'MA'
                        MOVE "MARANHAO" TO CEP-UFC
                ELSE IF CEP-UF = 'MT'
                        MOVE "MATO GROS" TO CEP-UFC
                ELSE IF CEP-UF = 'MS'
                        MOVE "MAT GROS SUL" TO CEP-UFC
                ELSE IF CEP-UF = 'MG'
                        MOVE "MINAS GEIS" TO CEP-UFC
                ELSE IF CEP-UF = 'PA'
                        MOVE "PARA" TO CEP-UFC
                ELSE IF CEP-UF = 'MG'
                        MOVE "PARAIBA" TO CEP-UFC
                ELSE IF CEP-UF = 'PB'
                        MOVE "PARA" TO CEP-UFC
                ELSE IF CEP-UF = 'PB'
                        MOVE "PARAIBA" TO CEP-UFC
                ELSE IF CEP-UF = 'PR'
                        MOVE "PARANA" TO CEP-UFC
                ELSE IF CEP-UF = 'PE'
                        MOVE "PERNAMBCO" TO CEP-UFC
                ELSE IF CEP-UF = 'PI'
                        MOVE "PIAUI" TO CEP-UFC
                ELSE IF CEP-UF = 'RJ'
                        MOVE "RIO DE JEI" TO CEP-UFC
                ELSE IF CEP-UF = 'RN'
                        MOVE "R GRAND NT" TO CEP-UFC
                ELSE IF CEP-UF = 'RS'
                        MOVE "R GRAND SL" TO CEP-UFC
                ELSE IF CEP-UF = 'RO'
                        MOVE "RONDONIA" TO CEP-UFC
                ELSE IF CEP-UF = 'RR'
                        MOVE "RORAIMA" TO CEP-UFC
                ELSE IF CEP-UF = 'SC'
                        MOVE "SANTA CAT" TO CEP-UFC
                ELSE IF CEP-UF = 'SP'
                        MOVE "SAO PAULO" TO CEP-UFC
                ELSE IF CEP-UF = 'SE'
                        MOVE "SERGIPE" TO CEP-UFC
                ELSE IF CEP-UF = 'TO'
                        MOVE "TOCANTINS" TO CEP-UFC
                        ELSE
                            MOVE "***PREENCHA O TS INVALIDO***" TO MENS
                                PERFORM ROT-MENS THRU ROT-MENS-FIM
                                GO TO INC-004
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TLONG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-LONG = SPACES
                    MOVE "****PREENCHA O TRISC DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-007
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TLATI
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-LATI = SPACES
                    MOVE "****PREENCHA O TSITU DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-008
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TCODRC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CEP-CODRC = ZEROS
                    MOVE "****PREENCHA O TSITU DOCEP****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-009
                IF W-ACT = 02 GO TO INC-008.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
                MOVE TDCEP(CEP-CODRC)  TO CEP-RC
                DISPLAY SP18203.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (24, 40) "DADOS OK (S/N) : ".
                ACCEPT (24, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO USUARIO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "***CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CEP"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (24, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (24, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (24, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (24, 40) "ALTERAR  (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (24, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 1000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (24, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       ROT-ALFA-FIM.
