       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB012.
       AUTHOR. VINICIUS ESCAME DOS SANTOS.
      *--------------------------------------------------------------
      * LOCACAO DE BICICLETAS
      *--------------------------------------------------------------
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      * 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADBIKE
           ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS NUM-BIKE
           FILE STATUS  IS ST-ERRO
           ALTERNATE RECORD KEY IS MARCA-BIKE WITH DUPLICATES.
      *
      *--------------------------------------------------------------
      *
       DATA DIVISION.
       FILE SECTION.
       FD CADBIKE LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADBIKE.DAT".
       01 REGBIKE.
           03 NUM-BIKE      PIC 9(04).
           03 MARCA-BIKE    PIC X(20).
           03 MODELO-BIKE   PIC X(20).
           03 CATEG         PIC X(01).
           03 ARO           PIC 9(02).
           03 COR           PIC 9(01).
           03 CORDESC       PIC X(10).
           03 VALCOMPRA     PIC 9(06)V99.
           03 VALHORA       PIC 9(06)V99.
           03 VALLOC        PIC 9(06)V99.
           03 SITUAC        PIC X(01).
           03 SITUACDESC    PIC X(11).
           03 DATCOMPRA-BIKE.
               05 DIACOMPRA-BIKE    PIC 9(02).
               05 MESCOMPRA-BIKE    PIC 9(02).
               05 ANOCOMPRA-BIKE    PIC 9(04).
           03 DATULTUSO-BIKE.
               05 DIAULTUSO-BIKE    PIC 9(02).
               05 MESULTUSO-BIKE    PIC 9(02).
               05 ANOULTUSO-BIKE    PIC 9(04).
      *
      *--------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------
      *
      * CAMPOS PARA TRATAR DATA 
       77 WDIA         PIC 9(02) VALUE ZEROS.
       77 WMES         PIC 9(02) VALUE ZEROS.
       77 WANO         PIC 9(04) VALUE ZEROS.
       77 WFLAGDAT     PIC 9(01) VALUE ZEROS.
       77 WRESTO       PIC 9(04) VALUE ZEROS.
      *
      *--------------------------------------------------------------
      *
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       
      * TABELA DE COR
       01 TTCOR.
           03 TCOR1 PIC X(08) VALUE "BRANCA  ".
           03 TCOR2 PIC X(08) VALUE "PRETA   ".
           03 TCOR3 PIC X(08) VALUE "AMARELA ".
           03 TCOR4 PIC X(08) VALUE "VERDE   ".
           03 TCOR5 PIC X(08) VALUE "VERMELHA".
           03 TCOR6 PIC X(08) VALUE "AZUL    ".
           03 TCOR7 PIC X(08) VALUE "LARANJA ".
           03 TCOR8 PIC X(08) VALUE "ROSA    ".
           03 TCOR9 PIC X(08) VALUE "ROXA    ".
       
       01 TTABCOR REDEFINES TTCOR.
           03 TABCOR PIC X(08) OCCURS 9 TIMES.

      * TABELA DE SITUACAO
       01 TTSITUAC.
           03 TSITUA1 PIC X(11) VALUE "ATIVA      ".
           03 TSITUA2 PIC X(11) VALUE "DESATIVADA ".
           03 TSITUA3 PIC X(11) VALUE "MANUTENCAO ".
           03 TSITUA4 PIC X(11) VALUE "LOCADA     ".
           03 TSITUA5 PIC X(11) VALUE "ROUBADA    ".
           03 TSITUA6 PIC X(11) VALUE "BAIXADA    ".
       
       01 TTABSITUAC REDEFINES TTSITUAC.
           03 TABSITUAC PIC X(11) OCCURS 6 TIMES.
      *
      *--------------------------------------------------------------
      *
       SCREEN SECTION.
       01  TELABIKE.
           05  LINE 01  COLUMN 01 
               VALUE  "   CONSULTA DE BICICLETAS               ".
           05  LINE 01  COLUMN 41 
               VALUE  "                                        ".
           05  LINE 05  COLUMN 01 
               VALUE  "   NUMERO:        MARCA".
           05  LINE 05  COLUMN 41 
               VALUE  "        MODELO".
           05  LINE 15  COLUMN 01 
               VALUE  "   AREA DE MENSAGENS                    ".
           05  LINE 20  COLUMN 41 
               VALUE  "                                        ".
           05  TNUMERO
               LINE 05  COLUMN 12  PIC 9(04)
               USING  NUM-BIKE.
           05  TMARCA
               LINE 05  COLUMN 25  PIC X(20)
               USING  MARCA-BIKE.
           05  TMODELO
               LINE 05  COLUMN 56  PIC X(20)
               USING  MODELO-BIKE.
      *
      *--------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           DISPLAY (01, 01) ERASE
           OPEN I-O CADBIKE
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN INPUT CADBIKE
                   MOVE "* CRIANDO O ARQUIVO CADBIKE.DAT *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   CLOSE CADBIKE
                   GO TO INC-OP0
                ELSE
                    MOVE "* ERRO AO ABRIR O ARQUIVO CADBIKE *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
           MOVE ZEROS TO NUM-BIKE ARO VALCOMPRA VALLOC VALHORA COR
           MOVE ZEROS TO DATCOMPRA-BIKE DATULTUSO-BIKE ANOCOMPRA-BIKE
           MOVE ZEROS TO MESCOMPRA-BIKE DIACOMPRA-BIKE
           MOVE ZEROS TO ANOULTUSO-BIKE MESULTUSO-BIKE DIAULTUSO-BIKE
           MOVE SPACES TO MARCA-BIKE MODELO-BIKE CATEG
           MOVE SPACES TO CORDESC SITUAC SITUACDESC
           DISPLAY TELABIKE.
      *
       INC-002.
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR 01
               GO TO ROT-CONFIRMA.
           IF NUM-BIKE = ZEROS
               MOVE "* CODIGO INVALIDO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-002.
      * VALIDA A EXISTENCIA DO REGISTRO
       LER-BIKE.
           MOVE 0 TO W-SEL
           READ CADBIKE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   DISPLAY TELABIKE
                   MOVE "* BIKE JA CADASTRADA *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE 1 TO W-SEL
                   GO TO ACE-001
               ELSE
                   MOVE "* ERRO AO LER REGISTRO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
           ELSE
               MOVE "* REGISTRO NAO EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-002.
           IF W-ACT = 01
               PERFORM ROT-MENS THRU ROT-MENS-FIM.
      *
      *--------------------------------------------------------------
      *
       ACE-001.
           MOVE "S" TO W-OPCAO
           DISPLAY (24, 30) "DESEJA LER OUTRO REGISTRO? (S): ".
           ACCEPT (24, 63) W-OPCAO WITH UPDATE
           IF W-OPCAO = "N" OR "n"
               GO TO ROT-CONFIRMA.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S PARA 'SIM' *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACE-001
           ELSE
               CLOSE CADBIKE
               GO TO INC-OP0.
      *
      *--------------------------------------------------------------
      * CONFIRMAR FIM
      *--------------------------------------------------------------
      *
       ROT-CONFIRMA.
           DISPLAY (24, 01) ERASE
           DISPLAY (24, 30) "CONFIRMAR FIM? (S/N): ".
           ACCEPT (24, 53) W-OPCAO WITH UPDATE
           IF W-OPCAO = "N" OR "n"
               GO TO ACE-001.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S PARA 'SIM' *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACE-001
           ELSE
               CLOSE CADBIKE
               GO TO ROT-FIM.
      *
      *--------------------------------------------------------------
      * ROTINA DE FIM
      *--------------------------------------------------------------
      *
       ROT-FIM.
           DISPLAY (01, 01) ERASE
           EXIT PROGRAM.
       ROT-FIMP.
           EXIT PROGRAM.
       ROT-FIMS.
           STOP RUN.
      *
      *--------------------------------------------------------------
      * ROTINA DE MENSAGEM
      *--------------------------------------------------------------
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
      *
      *--------------------------------------------------------------
      * FIM DE APLICACAO
      *--------------------------------------------------------------
      *
