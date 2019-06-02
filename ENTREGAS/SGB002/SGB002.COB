       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB002.
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
      *
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------
      * CODIGO CRIADO NA WORKING STORAGE SECTION
      * POIS O CODIGO ESTAVA ALTERANDO SOZINHO
      * DURANTE O DISPLAY DA TELA APOS O VALOR
      * JA ESTAR PREENCHIDO
      *
      *--------------------------------------------------------------
      *
       77 WNUMERO      PIC 9(04) VALUE ZEROS.
      *
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
               VALUE  "                          * CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  "BICICLETAS *".
           05  LINE 03  COLUMN 01 
               VALUE  "   NUMERO:".
           05  LINE 04  COLUMN 01 
               VALUE  "   MARCA:".
           05  LINE 05  COLUMN 01 
               VALUE  "   MODELO:".
           05  LINE 06  COLUMN 01 
               VALUE  "   CATEGORIA:".
           05  LINE 07  COLUMN 01 
               VALUE  "   ARO:".
           05  LINE 07  COLUMN 41 
               VALUE  "   DATA COMPRA:".
           05  LINE 08  COLUMN 01 
               VALUE  "   COR:    -                           D".
           05  LINE 08  COLUMN 41 
               VALUE  "ATA ULTIMO USO:".
           05  LINE 10  COLUMN 01 
               VALUE  "   VALOR COMPRA:".
           05  LINE 11  COLUMN 01 
               VALUE  "   VALOR HORA:".
           05  LINE 12  COLUMN 01 
               VALUE  "   VALOR LOCACAO HORA:".
           05  LINE 14  COLUMN 01 
               VALUE  "                                       S".
           05  LINE 14  COLUMN 41 
               VALUE  "ITUACAO:   -".
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
           05  TCATEG
               LINE 06  COLUMN 15  PIC X(01)
               USING  CATEG.
           05  TARO
               LINE 07  COLUMN 10  PIC 9(02)
               USING  ARO.
           05  TDATCOMPRA
               LINE 07  COLUMN 57  PIC 99/99/9999
               USING  DATCOMPRA-BIKE
               HIGHLIGHT.
           05  TCOR
               LINE 08  COLUMN 10  PIC 9(01)
               USING  COR.
           05  TCORDESC
               LINE 08  COLUMN 14  PIC X(10)
               USING  CORDESC.
           05  TDATULTUSO
               LINE 08  COLUMN 57  PIC 99/99/9999
               USING  DATULTUSO-BIKE
               HIGHLIGHT.
           05  TVALCOMPRA
               LINE 10  COLUMN 24  PIC ZZZZZ9,99
               USING  VALCOMPRA.
           05  TVALHORA
               LINE 11  COLUMN 24  PIC ZZZZZ9,99
               USING  VALHORA.
           05  TVALLOC
               LINE 12  COLUMN 24  PIC ZZZZZ9,99
               USING  VALLOC.
           05  TSITUAC
               LINE 14  COLUMN 50  PIC X(01)
               USING  SITUAC.
           05  TSITUACDESC
               LINE 14  COLUMN 54  PIC X(11)
               USING  SITUACDESC.
      *
      *--------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADBIKE
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN OUTPUT CADBIKE
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
           MOVE ZEROS TO MESCOMPRA-BIKE DIACOMPRA-BIKE WNUMERO
           MOVE ZEROS TO ANOULTUSO-BIKE MESULTUSO-BIKE DIAULTUSO-BIKE
      *
           MOVE SPACES TO MARCA-BIKE MODELO-BIKE CATEG
           MOVE SPACES TO CORDESC SITUAC SITUACDESC
      *
           DISPLAY TELABIKE.
       INC-002.
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR 01
               CLOSE CADBIKE
               GO TO ROT-FIM.
           IF WNUMERO = ZEROS
               MOVE "* CODIGO INVALIDO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-002.
           MOVE WNUMERO TO NUM-BIKE.
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
               NEXT SENTENCE.
           IF W-ACT = 01
               PERFORM ROT-MENS THRU ROT-MENS-FIM.
      *
      *--------------------------------------------------------------
      * NESTA LABEL O PROGRAMA COMEÇA A LEITURA DOS CAMPOS
      * A PARTIR DA MARCA
      *--------------------------------------------------------------
      *
       INC-003.
           ACCEPT TMARCA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-001.
           IF MARCA-BIKE = SPACES
               MOVE "* MARCA VAZIA! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-003.
      *    NEXT SENTENCE
      
       INC-004.
           ACCEPT TMODELO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-003.
           IF MODELO-BIKE = SPACES
               MOVE "* MODELO VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-004.
      *    NEXT SENTENCE
      
       INC-005.
           ACCEPT TCATEG
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-004.
           IF CATEG = SPACES
               MOVE "* CATEGORIA VAZIA! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-005.
      *    NEXT SENTENCE
      
       INC-006.
           ACCEPT  TARO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-005.
           IF ARO = ZEROS
               MOVE "* ARO VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-006.
      *    NEXT SENTENCE
      
       INC-007.
           ACCEPT TCOR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-006.
           IF COR = ZEROS
               MOVE "* PREENCHA A COR DE COM 1~9 *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-007.
           MOVE TABCOR(COR) TO CORDESC
           DISPLAY TCORDESC.
      *    NEXT SENTENCE
      
       INC-008.
           ACCEPT TVALCOMPRA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-007.
           IF VALCOMPRA = SPACES
               MOVE "* VALOR DA COMPRA NAO PODE SER ZERO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-008.
      *    NEXT SENTENCE
      
       INC-009.
           ACCEPT TVALHORA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-008.
           IF VALHORA = ZEROS
               MOVE "* VALOR DA HORA NAO PODE SER ZERO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-009.
      *    NEXT SENTENCE
      
       INC-010.
           ACCEPT TVALLOC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-009.
           IF VALLOC = ZEROS
               MOVE "* VALOR DA LOCACAO NAO PODE SER ZERO *"
                                                   TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-010.
      *    NEXT SENTENCE
      
       INC-011.
           ACCEPT TDATCOMPRA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-010.
           IF DATCOMPRA-BIKE = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-011.
           GO TO VALDATCOMPRA.
      *
      *--------------------------------------------------------------
      * LABEL QUE SERA ACESSADA CASO A DATA SEJA VALIDA
      * PARA A DATA DE COMPRA
      *--------------------------------------------------------------
      *
       INC-011FIM.
      *    NEXT SENTENCE
      
       INC-012.
           ACCEPT TDATULTUSO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-011.
           IF DATULTUSO-BIKE = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-012.
           GO TO VALDATULTUSO.
      *
      *--------------------------------------------------------------
      * LABEL QUE SERA ACESSADA CASO A DATA SEJA VALIDA
      * PARA A DATA DE ULTIMO USO
      *--------------------------------------------------------------
      *
       INC-012FIM.
      *    NEXT SENTENCE
      
       INC-013.
           ACCEPT TSITUAC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-012.
           IF SITUAC = SPACES
               MOVE "* TODA BICICLETA TEM SITUACAO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-013.
      * SITUACAO
           IF SITUAC = 'A' OR 'a'
               MOVE TABSITUAC(1) TO SITUACDESC
           ELSE IF SITUAC = 'D' OR 'd'
               MOVE TABSITUAC(2) TO SITUACDESC
           ELSE IF SITUAC = 'M' OR 'm'
               MOVE TABSITUAC(3) TO SITUACDESC
           ELSE IF SITUAC = 'L' OR 'l'
               MOVE TABSITUAC(4) TO SITUACDESC
           ELSE IF SITUAC = 'R' OR 'r'
               MOVE TABSITUAC(5) TO SITUACDESC
           ELSE IF SITUAC = 'B' OR 'b'
               MOVE TABSITUAC(6) TO SITUACDESC
           ELSE
               MOVE "* STATUS INVALIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-013.
           DISPLAY TSITUACDESC.
           IF W-SEL = 1
               GO TO ALT-OPC.
      *
      *--------------------------------------------------------------
      * O FIM DO INPUT DE DADOS DO USUARIO
      *--------------------------------------------------------------
      *
      ***************************************************************
      *
      *--------------------------------------------------------------
      * O INICIO DA GRAVACAO DE DADOS
      *--------------------------------------------------------------
      *
       INC-OPC.
           MOVE "S" TO W-OPCAO
           DISPLAY (24, 40) "DADOS OK (S/N) : ".
           ACCEPT (24, 57) W-OPCAO WITH UPDATE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-013.
           IF W-OPCAO = "N" OR "n"
              MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-001.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM e N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-OPC.
       INC-WR1.
           WRITE REGBIKE
           IF ST-ERRO = "00" OR "02"
               MOVE "* DADOS GRAVADOS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-001.
           IF ST-ERRO = "22"
               MOVE "* BIKE JA EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-002
           ELSE
               MOVE "* ERRO AO GRAVAR REGISTRO *" TO MENS 
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
      *
      *--------------------------------------------------------------
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO
      *--------------------------------------------------------------
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
           IF W-ACT = 04
               GO TO EXC-OPC.
       EXC-OPC.
           DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
           ACCEPT (24, 57) W-OPCAO
           IF W-OPCAO = "N" OR "n"
               MOVE "* REGISTRO NAO EXCLUIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-001.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO EXC-OPC.
       EXC-DL1.
           DELETE CADBIKE RECORD
           IF ST-ERRO = "00"
               MOVE "* REGISTRO EXCLUIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-001.
           MOVE "* ERRO NA EXCLUSAO DO REGISTRO *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
       ALT-OPC.
           DISPLAY (24, 40) "ALTERAR  (S/N) : ".
           ACCEPT (24, 57) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO INC-012.
           IF W-OPCAO = "N" OR "n"
               MOVE "* INFORMACOES NAO ALTERADAS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-013.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGBIKE
           IF ST-ERRO = "00" OR "02"
               MOVE "* REGISTRO ALTERADO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-001.
           MOVE "* ERRO AO ALTERAR REGISTRO *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
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
      *     VALIDACAO DAS DUAS DATAS (COMPRA E ULTIMO USO)
      *--------------------------------------------------------------
      *
      * VALIDACAO DAS DUAS DATAS
      *
       VALDATCOMPRA.
           MOVE 1 TO WFLAGDAT
           MOVE DIACOMPRA-BIKE TO WDIA
           MOVE MESCOMPRA-BIKE TO WMES
           MOVE ANOCOMPRA-BIKE TO WANO
           GO TO VALIDARMES.
      *
       VALDATULTUSO.
           MOVE 2 TO WFLAGDAT
           MOVE DIAULTUSO-BIKE TO WDIA
           MOVE MESULTUSO-BIKE TO WMES
           MOVE ANOULTUSO-BIKE TO WANO
           GO TO VALIDARMES.
      *
       VALIDARMES.
           IF WMES > 12 OR WMES < 1
               GO TO VALDATCOMPRA-ERRO
           ELSE
               GO TO MESVAL.
       MESVAL.
            GO TO D31 FEV D31 D30 D31 D30 D31 D31 D30 D31 D30 D31
               DEPENDING ON WMES.
       D31.
           IF WDIA > 31 OR WDIA < 1
               MOVE ZEROS TO DATCOMPRA-BIKE
               GO TO VALDATCOMPRA-ERRO
           ELSE
               GO TO VALDATCOMPRA-FIM.
       D30.
           IF WDIA > 30 OR WDIA < 1
               GO TO VALDATCOMPRA-ERRO
           ELSE
               GO TO VALDATCOMPRA-FIM.
      * MES DE FEVEREIRO SEGUNDO CALENDARIO GREGORIANO
      * ANOS BISSEXTOS SAO RELEVANTES
       FEV.
           GO TO DIV4.
      * DIVIDIR O ANO POR 4 O RESTO PODE SER >= 0 PARA SER BISSEXTO
       DIV4.
           DIVIDE WANO BY 4 GIVING WRESTO REMAINDER WRESTO
               IF WRESTO = ZEROS
                   GO TO DIV100
               ELSE
                   GO TO DIV400.
      * DIVIDIR O ANO POR 100 O RESTO NAO PODE SER 0 PARA SER BISSEXTO
       DIV100.
           DIVIDE WANO BY 100 GIVING WRESTO REMAINDER WRESTO
               IF WRESTO > ZEROS
                   GO TO BISSEXTO
               ELSE
                   GO TO NOTBISSEXTO.
      * DIVIDIR O ANO POR 400 O RESTO DEVE SER 0 PARA SER BISSEXTO
       DIV400.
           DIVIDE WANO BY 400 GIVING WRESTO REMAINDER WRESTO
               IF WRESTO = ZEROS
                   GO TO BISSEXTO
               ELSE
                   GO TO NOTBISSEXTO.
      * SE O ANO E BISSEXTO
       BISSEXTO.
           IF WDIA > 29 OR WDIA < 1
               GO TO VALDATCOMPRA-ERRO
           ELSE
               GO TO VALDATCOMPRA-FIM.
      * SE O ANO NAO E BISSEXTO
       NOTBISSEXTO.
           IF WDIA > 28 OR WDIA < 1
               GO TO VALDATCOMPRA-ERRO
           ELSE
               GO TO VALDATCOMPRA-FIM.
      *
      *--------------------------------------------------------------
      * SE WFLAGDAT = 1 ESTAMOS VALIDANDO A DATA DE COMPRA
      * SE WFLAGDAT = 2 ESTAMOS VALIDANDO A DATA DE ULTIMO USO
      *--------------------------------------------------------------
      *
       VALDATCOMPRA-ERRO.
           IF WFLAGDAT = 1
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-011
           ELSE
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-012.
       VALDATCOMPRA-FIM.
           IF WFLAGDAT = 1
               GO TO INC-011FIM
           ELSE
               GO TO INC-012FIM.
      *
      *--------------------------------------------------------------
      * FIM DE APLICACAO
      *--------------------------------------------------------------
      *
