       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB002.
       AUTHOR. VINICIUS ESCAME DOS SANTOS.
      ************************************************
      * MANUTENCAO DO CADASTRO DE BIKES PARA ALUGUEL *
      ************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADBIKE ASSIGN TO DISK
            ORGANIZATION IS INDEXED
            ACCESS MODE  IS DYNAMIC
            RECORD KEY   IS NUMERO
            FILE STATUS  IS ST-ERRO
            ALTERNATE RECORD KEY IS MARCA WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADBIKE
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADBIKE.DAT".
      * CAMPOS PARA REGISTRO NO ARQUIVO CADBIKE.DAT
       01 REGBIKE.
            03 NUMERO        PIC 9(04).
            03 MARCA         PIC X(20).
            03 MODELO        PIC X(20).
            03 CATEG         PIC X(O1).
            03 ARO           PIC 9(02).
            03 COR           PIC 9(01).
            03 VALCOMPRA     PIC 9(06)V99.
            03 VALHORA       PIC 9(06)V99.
            03 VALLOC        PIC 9(06)V99.
            03 SITUAC        PIC X(01).
            03 SITUACDESC    PIC X(10).
       01 DATCOMPRA.
            03 ANO-COMPRA    PIC 99.
            03 MES-COMPRA    PIC 99.
            03 DIA-COMPRA    PIC 99.
       01 DATULTUSO.
            03 ANO-ULTUSO    PIC 99.
            03 MES-ULTUSO    PIC 99.
            03 DIA-ULTUSO    PIC 99.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL           PIC 9(01) VALUE ZEROS.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 ST-ERRO         PIC X(02) VALUE "00".
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
      * CAMPOS LOCAIS DE REGISTRO
       01 W-NUMERO        PIC 9(04) VALUE ZEROS.
       01 W-MARCA         PIC X(20) VALUE SPACES.
       01 W-MODELO        PIC X(20) VALUE SPACES.
       01 W-CATEG         PIC X(O1) VALUE SPACES.
       01 W-ARO           PIC 9(02) VALUE ZEROS.
       01 W-COR           PIC 9(01) VALUE ZEROS.
       01 W-CORDSC        PIC X(10) VALUE SPACES.
       01 W-VALCOMPRA     PIC 9(06)V99 VALUE ZEROS.
       01 W-VALHORA       PIC 9(06)V99 VALUE ZEROS.
       01 W-VALLOC        PIC 9(06)V99 VALUE ZEROS.
       01 W-SITUAC        PIC X(01) VALUE SPACES.
       01 W-SITUACDESC    PIC X(10) VALUE SPACES.
      *
      *******************
      * TABELA DE CORES *
      *******************
      *
       01 TCOR.
            03 TCOR1 PIC X(09) VALUE "BRANCA".
            03 TCOR2 PIC X(16) VALUE "PRETA   AMARELA".
            03 TCOR3 PIC X(16) VALUE "VERDE   VERMELHA".
            03 TCOR4 PIC X(16) VALUE "AZUL   LARANJA".
            03 TCOR5 PIC X(16) VALUE "ROSA    ROXA".
        
        01 TTABCOR REDEFINES TCOR.
            03 TABCOR PIC X(10) OCCURS 9 TIMES.
      *---------------------------------------------------------
       SCREEN SECTION.
       01  TELA-CADBIKE.
           05  LINE 01  COLUMN 01 
               VALUE  "                          * CADATROS DE".
           05  LINE 01  COLUMN 41 
               VALUE  "BICICLETAS *".
           05  LINE 03  COLUMN 01 
               VALUE  "  NUMERO:".
           05  LINE 05  COLUMN 01 
               VALUE  "  MARCA:".
           05  LINE 06  COLUMN 01 
               VALUE  "  MODELO:".
           05  LINE 07  COLUMN 01 
               VALUE  "  CATEG:".
           05  LINE 07  COLUMN 41 
               VALUE  "  DAT COMPRA:".
           05  LINE 08  COLUMN 01 
               VALUE  "  ARO:".
           05  LINE 08  COLUMN 41 
               VALUE  "  DAT ULT USO:".
           05  LINE 09  COLUMN 01 
               VALUE  "  COR:   -".
           05  LINE 11  COLUMN 01 
               VALUE  "  VALOR COMPRA:".
           05  LINE 12  COLUMN 01 
               VALUE  "  VALOR HORA:".
           05  LINE 13  COLUMN 01 
               VALUE  "  VALOR LOCACAO (HORA):".
           05  LINE 13  COLUMN 41 
               VALUE  "  SITUAC:   -".
           05  LINE 15  COLUMN 01 
               VALUE  "ÚÄAREA DE MENSAGENSÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 15  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 16  COLUMN 01 
               VALUE  "³".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 17  COLUMN 01 
               VALUE  "³".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 18  COLUMN 01 
               VALUE  "³".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 19  COLUMN 01 
               VALUE  "³".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 20  COLUMN 01 
               VALUE  "³".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 21  COLUMN 01 
               VALUE  "³".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 22  COLUMN 01 
               VALUE  "³".
           05  LINE 22  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 23  COLUMN 01 
               VALUE  "³".
           05  LINE 24  COLUMN 01 
               VALUE  "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 24  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  TNUMERO
               LINE 03  COLUMN 11  PIC 9(04)
               USING  W-NUMERO.
           05  TMARCA
               LINE 05  COLUMN 11  PIC X(20)
               USING  W-MARCA.
           05  TMODELO
               LINE 06  COLUMN 11  PIC X(20)
               USING  W-MODELO.
           05  TCATEGORIA
               LINE 07  COLUMN 14  PIC X(01)
               USING  W-CATEG.
           05  TDATCOMPRA
               LINE 07  COLUMN 56  PIC XX/XX/XX
               USING  W-DATCOMPRA.
           05  TARO
               LINE 08  COLUMN 08  PIC 9(02)
               USING  W-ARO.
           05  TDATULTUSO
               LINE 08  COLUMN 56  PIC XX/XX/XX
               USING  W-TDATULTUSO.
           05  TCOR
               LINE 09  COLUMN 08  PIC X(01)
               USING  W-COR.
           05  TCORDSC
               LINE 09  COLUMN 12  PIC X(09)
               USING  W-CORDSC.
           05  TVALCOMPRA
               LINE 11  COLUMN 17  PIC Z9,99
               USING  W-VALCOMPRA.
           05  TVALHORA
               LINE 12  COLUMN 17  PIC Z9,99
               USING  W-VALHORA.
           05  TVALLOC
               LINE 13  COLUMN 26  PIC Z9,99
               USING  W-VALLOC.
           05  TSITUAC
               LINE 13  COLUMN 53  PIC X(01)
               USING  W-SITUAC.
           05  TSITUACDSC
               LINE 13  COLUMN 57  PIC X(10)
               USING  W-SITUACDESC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADBIKE
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADBIKE
                      MOVE "* CRIANDO ARQUIVO CADBIKE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      CLOSE CADBIKE
                      GO TO INC-OP0
                   ELSE
                      MOVE "* ERRO AO ABRIR ARQUIVO CADBIKE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
           MOVE ZEROS TO NUMERO ARO COR VALCOMPRA VALHORA VALLOC.
           MOVE SPACES TO MARCA CATEG MODELO SITUAC SITUACDESC.
           DISPLAY TELA-CADBIKE.
       INC-002.
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                CLOSE CADBIKE
                GO TO ROT-FIM.
           IF W-NUMERO = ZEROS
                MOVE "* NUMERO INVALIDO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-002.
       LER-CADBIKE.
           MOVE 0 TO W-SEL
           READ CADBIKE
           IF ST-ERRO NOT = "23"
                IF ST-ERRO = "00"
                    DISPLAY TELA-CADBIKE
                    MOVE "* BIKE JA CADASTRADA *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    MOVE 1 TO W-SEL
                    GO TO ACE-001.
                ELSE
                    MOVE "* ERRO AO LER ARQUIVO CADBIKE *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM.
           ELSE
                NEXT SENTENCE.
       LER-MARCA.
           ACCEPT TMARCA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-MARCA = SPACES
                MOVE "* PREENCHA A MARCA *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO LER-MARCA.
           IF W-ACT = 02
               GO TO INC-001.
       INC-OPC.
           MOVE "S" TO W-OPCAO
           DISPLAY (24, 40) "DADOS OK (S/N) : ".
           ACCEPT (24, 57) W-OPCAO WITH UPDATE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
      *          GO TO INC-008.
           IF W-OPCAO = "N" OR "n"
                MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-001.
           IF W-OPCAO NOT = "S" AND "s"
                MOVE "* DIGITE S=SIM OU N=NAO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-OPC.
       INC-WR1.
           WRITE REGBIKE
           IF ST-ERRO = "00" OR "02"
                MOVE "* DADOS SALVOS * " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-001.
           IF ST-ERRO = "22"
                MOVE "* COD BIKE JA EXISTE *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-001.
           ELSE
                MOVE "* ERRO AO GRAVAR ARQ CADBIKE *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
            DISPLAY (24, 12) "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
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
                   GO TO LER-MARCA.
      *
       EXC-OPC.
           DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
           ACCEPT (24, 57) W-OPCAO
           IF W-OPCAO = "N" OR "n"
                MOVE "* REGISTRO NAO EXCLUIDO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-001.
           IF W-OPCAO NOT = "S" AND "s"
                MOVE "* DIGITE APENAS S=SIM OU N=NAO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO EXC-OPC.
       EXC-DL1.
           DELETE CADBIKE RECORD
           IF ST-ERRO = "00"
                MOVE "* REGISTRO EXCLUIDO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-001.
           MOVE "* ERRO AO EXCLUIR REGISTRO *" TO MENS
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
                MOVE "* AS INFORMACOES NAO FORAM ALTERADAS * " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
      *          GO TO INC-009.
           IF W-OPCAO NOT = "S" AND "s"
                MOVE "* DIGITE APENAS S=SIM OU N=NAO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGBIKE
           IF ST-ERRO = "00" OR "02"
                MOVE "* REGISTRO ALTERADO *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INICIO.
           MOVE "* ERRO AO EXCLUIR REGISTRO *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
      *
      *****************
      * ROTINA DE FIM *
      *****************
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
