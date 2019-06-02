       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB001.
       AUTHOR. VINICIUS ESCAME DOS SANTOS.
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
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS LOGRAD
                                            WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP             PIC 9(08).
                03 LOGRAD          PIC X(35).
                03 BAIRRO          PIC X(20).
                03 CIDADE          PIC X(12).
                03 UF              PIC X(02).
                03 DSCUF           PIC X(20).
                03 LONGITU         PIC X(12).
                03 LATITU          PIC X(12).
                03 REGIAO-CDD      PIC 9(01).
                03 DSC-REGIAOCDD   PIC X(12).
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
       01 TTABCEP.
          03 TN1 PIC X(13) VALUE "NORTE        ".
          03 TN2 PIC X(13) VALUE "NORDESTE     ".
          03 TN3 PIC X(13) VALUE "SUL          ".
          03 TN4 PIC X(13) VALUE "SUDOESTE     ".
          03 TN5 PIC X(13) VALUE "LESTE        ".
          03 TN6 PIC X(13) VALUE "OESTE        ".
          03 TN7 PIC X(13) VALUE "CENTRO       ".
          03 TN8 PIC X(13) VALUE "RURAL        ".
          03 TN9 PIC X(13) VALUE "NAO INFORMADO".

       01 TTAB REDEFINES TTABCEP.
           03 TABCEP PIC X(13) OCCURS 9 TIMES.
      *---------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
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
               LINE 04  COLUMN 31  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TLOG
               LINE 06  COLUMN 31  PIC X(35)
               USING  LOGRAD
               HIGHLIGHT.
           05  TBAIR
               LINE 08  COLUMN 31  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCID
               LINE 10  COLUMN 31  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 12  COLUMN 31  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TUFC
               LINE 12  COLUMN 36  PIC X(20)
               USING  DSCUF
               HIGHLIGHT.
           05  TLONG
               LINE 14  COLUMN 31  PIC X(12)
               USING  LONGITU
               HIGHLIGHT.
           05  TLATI
               LINE 16  COLUMN 31  PIC X(12)
               USING  LATITU
               HIGHLIGHT.
           05  TCODRC
               LINE 18  COLUMN 31  PIC 9(01)
               USING  REGIAO-CDD
               HIGHLIGHT.
           05  TRC
               LINE 18  COLUMN 35  PIC X(12)
               USING  DSC-REGIAOCDD
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCEP
                      MOVE "*** ARQUIVOCEP SENDO CRIADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      CLOSE CADCEP
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CEP REGIAO-CDD

                MOVE SPACES TO LOGRAD BAIRRO CIDADE UF DSCUF
                               LONGITU LATITU DSC-REGIAOCDD

                DISPLAY TELACEP.

       INC-002.
                ACCEPT TCOD
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CEP = ZEROS
                   MOVE "*** COD INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      DISPLAY TELACEP
                      MOVE "***CEP JA CADASTRADO***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO PRODUTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
                
       INC-003.
                ACCEPT TLOG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-001.
                IF LOGRAD = SPACES
                     MOVE "** PREENCHA O LOGRADOURO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-003.
       INC-004.
                ACCEPT TBAIR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-003.
                IF BAIRRO = SPACES
                    MOVE "** PREENCHA O BAIRRO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004.
       INC-005.
                ACCEPT TCID
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-004.
                IF CIDADE = ZEROS
                    MOVE "** PREENCHA A CIDADE **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005.

       INC-006.
                ACCEPT  TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-005.
                IF UF = SPACES
                    MOVE "** PREENCHA O UF **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006
                ELSE
                    IF UF = 'AC'
                        MOVE "ACRE" TO DSCUF
                    ELSE IF UF = 'AL'
                            MOVE "ALAGOAS" TO DSCUF
                    ELSE IF UF = 'AM'
                            MOVE "AMAZONAS" TO DSCUF
                    ELSE IF UF = 'AP'
                            MOVE "AMAPA" TO DSCUF
                    ELSE IF UF = 'BA'
                            MOVE "BAHIA" TO DSCUF
                    ELSE IF UF = 'CE'
                            MOVE "CEARA" TO DSCUF
                    ELSE IF UF = 'DF'
                            MOVE "DISTRITO FEDERAL" TO DSCUF
                    ELSE IF UF = 'ES'
                            MOVE "ESPIRITO SANTO" TO DSCUF
                    ELSE IF UF = 'GO'
                            MOVE "GOIAS" TO DSCUF
                    ELSE IF UF = 'MA'
                            MOVE "MARANHAO" TO DSCUF
                    ELSE IF UF = 'MT'
                            MOVE "MATO GROSSO" TO DSCUF
                    ELSE IF UF = 'MS'
                            MOVE "MATO GROSSO DO SUL" TO DSCUF
                    ELSE IF UF = 'MG'
                            MOVE "MINAS GERAIS" TO DSCUF
                    ELSE IF UF = 'PA'
                            MOVE "PARA" TO DSCUF
                    ELSE IF UF = 'PB'
                            MOVE "PARAIBA" TO DSCUF
                    ELSE IF UF = 'PR'
                            MOVE "PARANA" TO DSCUF
                    ELSE IF UF = 'PE'
                            MOVE "PERNAMBUCO" TO DSCUF
                    ELSE IF UF = 'PI'
                            MOVE "PIAUI" TO DSCUF
                    ELSE IF UF = 'RJ'
                            MOVE "RIO DE JANEIRO" TO DSCUF
                    ELSE IF UF = 'RN'
                            MOVE "RIO GRANDE DO NORTE" TO DSCUF
                    ELSE IF UF = 'RS'
                            MOVE "RIO GRANDE DO SUL" TO DSCUF
                    ELSE IF UF = 'RO'
                            MOVE "RONDONIA" TO DSCUF
                    ELSE IF UF = 'RR'
                            MOVE "RORAIMA" TO DSCUF
                    ELSE IF UF = 'SC'
                            MOVE "SANTA CATARINA" TO DSCUF
                    ELSE IF UF = 'SP'
                            MOVE "SAO PAULO" TO DSCUF
                    ELSE IF UF = 'SE'
                            MOVE "SERGIPE" TO DSCUF
                    ELSE IF UF = 'TO'
                            MOVE "TOCANTINS" TO DSCUF
                    ELSE
                      MOVE "** UF INVALIDO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-006.
                DISPLAY TELACEP.
       INC-007.
                ACCEPT TLONG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-006.
                IF LONGITU = SPACES
                    MOVE "*PREENCHA A LONG DO CEP*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-007.
       INC-008.
                ACCEPT TLATI
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-007.
                IF LATITU = SPACES
                    MOVE "*PREENCHA A LATI DO CEP*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-008.
       INC-009.
                ACCEPT TCODRC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-008.
                IF REGIAO-CDD = ZEROS
                    MOVE "** PREENCHA O CODIGO DA REGIAO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-009.
                MOVE TABCEP(REGIAO-CDD) TO DSC-REGIAOCDD
                DISPLAY TELACEP.
                IF W-SEL = 1
                    GO TO ALT-OPC.
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
                DELETE CADCEP RECORD
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
                IF W-ACT = 02
                    GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
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
