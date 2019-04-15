       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB003.
       AUTHOR. VINICIUS ESCAME DOS SANTOS.
      *--------------------------------------------------------------
      * CADASTRO DE CLIENTES
      *--------------------------------------------------------------
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT CADCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS SKCLI = NOME
                                                      WITH DUPLICATES.
            SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS SKCEP = LOGRAD
                                                      WITH DUPLICATES.

      *
      *--------------------------------------------------------------
      *
       DATA DIVISION.
       FILE SECTION.
       FD CADCLI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
      *
       01 REGCLI.
           03 CPF           PIC 9(11).
           03 RG            PIC X(09).
           03 NOME          PIC X(30).
           03 EMAIL         PIC X(20).
           03 NUM           PIC 9(04).
           03 TEL           PIC 9(11).
           03 SITUAC        PIC X(01).
           03 DSCSITUAC     PIC X(08).
           03 COMPL         PIC X(13).
      * ESSE CAMPO SERVIRA PRA QUANDO CARREGAR O CLIENTE
      * CARREGAR O CEP E SER POSSIVEL CARREGAR AS INFORMACOES
      * DO CADASTRO DO CEP
      * BASICAMENTE UMA FOREIGN KEY 
           03 FKCEP         PIC 9(08).
           03 DATNASC.
               05 DIA-COMPRA    PIC 9(02).
               05 MES-COMPRA    PIC 9(02).
               05 ANO-COMPRA    PIC 9(04).
      *
       FD CADCEP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
      * NAO LE OUTRO ARQUIVO SE O 'LAYOUT'
      * NAO FOR O MESMO
      *     03 CEP           PIC 9(08).
      *     03 LOGRAD        PIC X(35).
      *     03 BAIRRO        PIC X(20).
      *     03 CIDADE        PIC X(12).
      *     03 UF            PIC X(02).

      * OS CAMPOS SAO OS MESMOS DO PROJETO ANTERIOR
      * POREM, AGORA USO APENAS OS NECESSARIOS
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
      *--------------------------------------------------------------
      *
       WORKING-STORAGE SECTION.
      *
      *--------------------------------------------------------------
      *
      * CAMPOS PARA TRATAR DATA 
       77 WDIA         PIC 9(02) VALUE ZEROS.
       77 WMES         PIC 9(02) VALUE ZEROS.
       77 WANO         PIC 9(04) VALUE ZEROS.
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
       
      * TABELA DE SITUACAO DO CLIENTE
       01 TTSITUAC.
           03 TSITUAC1 PIC X(08) VALUE "ATIVO   ".
           03 TSITUAC1 PIC X(08) VALUE "SUSPENSO".
           03 TSITUAC3 PIC X(08) VALUE "DEVEDOR ".
           03 TSITUAC4 PIC X(08) VALUE "EXCLUIDO".
       
       01 TTABSITUAC REDEFINES TTSITUAC.
           03 TABSITUAC PIC X(08) OCCURS 4 TIMES.
      *
      *--------------------------------------------------------------
      *
       SCREEN SECTION.
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
      *
      *--------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.
       INC-OP0.
           OPEN I-O CADCLI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN OUTPUT CADCLI
                   MOVE "* CRIANDO O ARQUIVO CADCLI.DAT *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   CLOSE CADCLI
                   GO TO INC-OP0
                ELSE
                    MOVE "* ERRO AO ABRIR O ARQUIVO CADCLI *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
            ELSE
                NEXT SENTENCE.
            OPEN INPUT CADCEP
            IF ST-ERRO NOT = "00"
                MOVE "* CRIANDO O ARQUIVO CADCEP.DAT *" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
            ELSE
                NEXT SENTENCE.
      *     NEXT SENTENCE
            
       CLEAR-VARS.
           MOVE ZEROS TO CPF DATNASC CEP NUM TEL
      *
           MOVE SPACES TO NOME RG EMAIL LOGRAD COMPL UF
           MOVE SPACES TO BAIRRO CIDADE UF SITUAC DSCSITUAC
      *
           DISPLAY TELACLI.
       ACC-PK.
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR 01
               CLOSE CADCLI CADCEP
               GO TO ROT-FIM.
           IF CPF = ZEROS
               MOVE "* CODIGO INVALIDO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-PK
           ELSE
               NEXT SENTENCE.
      * VALIDA A EXISTENCIA DO REGISTRO
       LER-CLIENTE.
           MOVE 0 TO W-SEL
           READ CADCLI
           ACCEPT W-ACT FROM ESCAPE KEY
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   MOVE FKCEP TO CEP
                   READ CADCEP
                   DISPLAY TELACLI
                   DISPLAY (01, 01) "CEP: " CEP
                   MOVE "* CLIENTE JA CADASTRADO *" TO MENS
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
      * NESTA LABEL O PROGRAMA COMECA A LEITURA DOS CAMPOS
      * A PARTIR DO RG
      *--------------------------------------------------------------
      *
       ACC-RG.
           ACCEPT TRG
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO CLEAR-VARS.
           IF RG = SPACES
               MOVE "* RG VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-NOME.
      *    NEXT SENTENCE

       ACC-NOME.
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-RG.
           IF NOME = SPACES
               MOVE "* NOME VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-NOME.
      *    NEXT SENTENCE

       ACC-DATNASC.
           ACCEPT TDATNASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-DATNASC.
           IF DATNASC = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-DATNASC.
           GO TO VALDATNASC.
      *
      *--------------------------------------------------------------
      * LABEL QUE SERA ACESSADA CASO A DATA SEJA VALIDA
      * PARA A DATA DE NASCIMENTO
      *--------------------------------------------------------------
      *
       ACC-DATNASCFIM.
       ACC-EMAIL.
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-NOME.
           IF EMAIL = SPACES
               MOVE "* EMAIL VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-EMAIL.
      *    NEXT SENTENCE
       ACC-CEP.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-NOME.
           IF EMAIL = SPACES
               MOVE "* EMAIL VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-CEP.
      * LEITURA DO CEP
       READ-CEP.
           READ CADCEP
           IF ST-ERRO = "00"
               DISPLAY TELACLI
           ELSE
               IF ST-ERRO = "23"
                   MOVE "* CEP INVALIDO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
               ELSE
                   MOVE "* ERRO AO LER CADDEP *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
           MOVE CEP TO FKCEP.
       READ-CEPFIM.
       ACC-NUM.
           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-CEP.
           IF NUM = ZEROS
               MOVE "* NUMERO DA CASA VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-NUM.
      *    NEXT SENTENCE
       ACC-COMPL.
           ACCEPT TCOMPL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-NUM.
      *    NEXT SENTENCE
       ACC-TEL.
           ACCEPT TTEL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-COMPL.
           IF TEL = ZEROS
               MOVE "* TELEFONE VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-TEL.
      *    NEXT SENTENCE
       ACC-SITUAC.
           ACCEPT TSITUAC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-TEL.
           IF SITUAC = SPACES
               MOVE "* TELEFONE VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-SITUAC.
           IF SITUAC = 'A' OR 'a'
               MOVE TABSITUAC(1) TO DSCSITUAC
           ELSE IF SITUAC = 'S' OR 's'
               MOVE TABSITUAC(2) TO DSCSITUAC
           ELSE IF SITUAC = 'D' OR 'd'
               MOVE TABSITUAC(3) TO DSCSITUAC
           ELSE IF SITUAC = 'E' OR 'e'
               MOVE TABSITUAC(4) TO DSCSITUAC
           ELSE
               MOVE "* SITUACAO INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-SITUAC.
           DISPLAY TELACLI.
           IF W-SEL = 1
               GO TO ALT-OPC.
      *    NEXT SENTENCE
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
      *---------------------         GO TO INC-013.
           IF W-OPCAO = "N" OR "n"
              MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO CLEAR-VARS.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM e N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-OPC.
       INC-WR1.
           WRITE REGCLI
           IF ST-ERRO = "00" OR "02"
               MOVE "* DADOS GRAVADOS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
           IF ST-ERRO = "22"
               MOVE "* BIKE JA EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-PK
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
               GO TO CLEAR-VARS.
           IF W-ACT = 03
               GO TO ACC-NOME.
           IF W-ACT = 04
               GO TO EXC-OPC.
       EXC-OPC.
           DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
           ACCEPT (24, 57) W-OPCAO
           IF W-OPCAO = "N" OR "n"
               MOVE "* REGISTRO NAO EXCLUIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO EXC-OPC.
       EXC-DL1.
           DELETE CADCLI RECORD
           IF ST-ERRO = "00"
               MOVE "* REGISTRO EXCLUIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
           MOVE "* ERRO NA EXCLUSAO DO REGISTRO *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
       ALT-OPC.
           DISPLAY (24, 40) "ALTERAR  (S/N) : ".
           ACCEPT (24, 57) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
      *---------------------         GO TO INC-012.
           IF W-OPCAO = "N" OR "n"
               MOVE "* INFORMACOES NAO ALTERADAS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
      *---------------------         GO TO INC-013.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGCLI
           IF ST-ERRO = "00" OR "02"
               MOVE "* REGISTRO ALTERADO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
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
       VALDATNASC.
           MOVE DIA-COMPRA TO WDIA
           MOVE MES-COMPRA TO WMES
           MOVE ANO-COMPRA TO WANO
           GO TO VALIDARMES.
      *
       VALIDARMES.
           IF WMES > 12 OR WMES < 1
               GO TO VALDATNASC-ERRO
           ELSE
               GO TO MESVAL.
       MESVAL.
            GO TO D31 FEV D31 D30 D31 D30 D31 D31 D30 D31 D30 D31
               DEPENDING ON WMES.
       D31.
           IF WDIA > 31 OR WDIA < 1
               MOVE ZEROS TO DATNASC
               GO TO VALDATNASC-ERRO
           ELSE
               GO TO VALDATNASC-FIM.
       D30.
           IF WDIA > 30 OR WDIA < 1
               GO TO VALDATNASC-ERRO
           ELSE
               GO TO VALDATNASC-FIM.
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
               GO TO VALDATNASC-ERRO
           ELSE
               GO TO VALDATNASC-FIM.
      * SE O ANO NAO E BISSEXTO
       NOTBISSEXTO.
           IF WDIA > 28 OR WDIA < 1
               GO TO VALDATNASC-ERRO
           ELSE
               GO TO VALDATNASC-FIM.
      *
      *--------------------------------------------------------------
      *
       VALDATNASC-ERRO.
            MOVE "* DATA INVALIDA *" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO CLEAR-VARS.
       VALDATNASC-FIM.
            GO TO ACC-DATNASCFIM.
      *
      *--------------------------------------------------------------
      * FIM DE APLICACAO
      *--------------------------------------------------------------
      *
