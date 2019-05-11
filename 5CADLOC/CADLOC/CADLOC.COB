       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB004.
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
            SELECT CADLOC
                    ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS KEYLOCACAO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS FKCPF-CLI
                                            WITH DUPLICATES.
            SELECT CADCLI
                    ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF-CLI
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS RG-CLI
                                            WITH DUPLICATES.

            SELECT CADBIKE
                    ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS NUM-BIKE
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS MARCA-BIKE
                                            WITH DUPLICATES.

      *
      *--------------------------------------------------------------
      *
       DATA DIVISION.
       FILE SECTION.
       FD CADLOC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADLOC.DAT".
       01 REGLOC.
           03 KEYLOCACAO.
             05 FKCPF-CLI      PIC 9(11).
             05 FKNUM-BIKE     PIC 9(04).
             05 DTLOCACAO.
               07 DIA-LOCA     PIC 9(02).
               07 MES-LOCA     PIC 9(02).
               07 ANO-LOCA     PIC 9(04).
             05 HRLOCACAO      PIC 9(02).
             05 MINLOCACAO     PIC 9(02).
           03 DTPREVISAO.
               05 DIA-PREV     PIC 9(02).
               05 MES-PREV     PIC 9(02).
               05 ANO-PREV     PIC 9(04).
           03 HRPREVISAO       PIC 9(02).
           03 MINPREVISAO      PIC 9(02).
           03 DTENTREGA.
               05 DIA-ENTR     PIC 9(02).
               05 MES-ENTR     PIC 9(02).
               05 ANO-ENTR     PIC 9(04).
           03 HRENTREGA        PIC 9(02).
           03 MINENTREGA       PIC 9(02).
           03 HRUSO            PIC 9(02).
           03 MINUSO           PIC 9(02).
           03 VALLOCACAO       PIC 9(06)V99.
           03 VALCOBRADO       PIC 9(06)V99.
           03 STATUS-BIKE      PIC X(01).
           03 DSCSTATUS-BIKE   PIC X(08).

       FD CADCLI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
       01 REGCLI.
           03 CPF-CLI           PIC 9(11).
           03 RG-CLI            PIC X(09).
           03 NOME-CLI          PIC X(30).
           03 EMAIL-CLI         PIC X(20).
           03 NUM-CLI           PIC 9(04).
           03 TEL-CLI           PIC 9(11).
           03 SITUAC-CLI        PIC X(01).
           03 DSCSITUAC-CLI     PIC X(08).
           03 COMPLEM-CLI       PIC X(13).
           03 FKCEP             PIC 9(08).
           03 DATNASC.
               05 DIANASC-CLI    PIC 9(02).
               05 MESNASC-CLI    PIC 9(02).
               05 ANONASC-CLI    PIC 9(04).

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
      *
      *--------------------------------------------------------------
      *
      * CAMPOS PARA TRATAR DATA 
       77 WDIA         PIC 9(02) VALUE ZEROS.
       77 WMES         PIC 9(02) VALUE ZEROS.
       77 WANO         PIC 9(04) VALUE ZEROS.
       77 WRESTO       PIC 9(04) VALUE ZEROS.
       77 WFLAGDAT     PIC 9(01) VALUE ZEROS.
       77 WFLAGHORAS   PIC 9(01) VALUE ZEROS.
       77 WFLAGMINUTOS PIC 9(01) VALUE ZEROS.
       77 WHORAS       PIC 9(02) VALUE ZEROS.
       77 WMINUTOS     PIC 9(02) VALUE ZEROS.
      *
       77 WFLAGDATMEN  PIC 9(01) VALUE ZEROS.
       77 DIALOCTMP    PIC 9(02) VALUE ZEROS.
       77 MESLOCTMP    PIC 9(02) VALUE ZEROS.
       77 ANOLOCTMP    PIC 9(04) VALUE ZEROS.
      *
       77 DIATMP       PIC 9(02) VALUE ZEROS.
       77 MESTMP       PIC 9(02) VALUE ZEROS.
       77 ANOTMP       PIC 9(04) VALUE ZEROS.
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
       01 TABLE-DSCSTATUS.
           03 TSTATUS1 PIC X(08) VALUE "LOCADA  ".
           03 TSTATUS1 PIC X(08) VALUE "ENTREGUE".
           03 TSTATUS3 PIC X(08) VALUE "PAGA    ".
       
       01 TTABLE-STATUS REDEFINES TABLE-DSCSTATUS.
           03 VALTABLE-DSCSTATUS PIC X(08) OCCURS 3 TIMES.
      *
      *--------------------------------------------------------------
      *
       SCREEN SECTION.
       01  TELALOC.
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
      *
      *--------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.
       INC-OP0.
           OPEN I-O CADLOC.
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN OUTPUT CADLOC
                   MOVE "* CRIANDO O ARQUIVO CADLOC.DAT *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   CLOSE CADLOC
                   GO TO INC-OP0
               ELSE
                    MOVE "* ERRO AO CRIAR O ARQUIVO CADLOC.DAT *"
                                                          TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM.
            OPEN INPUT CADCLI CADBIKE.

       CLEAR-VARS.
      * LOCACAO
           MOVE ZEROS TO KEYLOCACAO
           MOVE ZEROS TO FKCPF-CLI FKNUM-BIKE DTLOCACAO HRLOCACAO
           MOVE ZEROS TO MINLOCACAO DTPREVISAO HRPREVISAO
           MOVE ZEROS TO MINPREVISAO DTENTREGA HRENTREGA MINENTREGA
           MOVE ZEROS TO HRUSO MINUSO VALLOCACAO VALCOBRADO
           MOVE SPACES TO STATUS-BIKE DSCSTATUS-BIKE

      * CLIENTE
           MOVE ZEROS TO CPF-CLI NUM-CLI TEL-CLI FKCEP DATNASC
           MOVE SPACES TO RG-CLI NOME-CLI EMAIL-CLI SITUAC-CLI
           MOVE SPACES TO COMPLEM-CLI DSCSITUAC-CLI

      * CEP
      *     MOVE ZEROS TO CEP REGIAO-CDD
      *     MOVE SPACES TO LOGRAD BAIRRO CIDADE UF DSCUF LONGITU
      *     MOVE SPACES TO LATITU DSC-REGIAOCDD

      * BIKE
           MOVE ZEROS TO NUM-BIKE ARO COR VALCOMPRA VALHORA
           MOVE ZEROS TO VALLOC DATCOMPRA-BIKE DATULTUSO-BIKE
           MOVE SPACES TO MARCA-BIKE MODELO-BIKE CATEG CORDESC
           MOVE SPACES TO SITUAC SITUACDESC
      
      * DISPLAY INICIAL DA TELA
           DISPLAY TELALOC.

       ACCFKCPF-CLI.
           ACCEPT TCPF-CLI
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR 01
               CLOSE CADLOC CADCLI CADBIKE
               GO TO ROT-FIM.
           IF CPF-CLI = ZEROS
               MOVE "* CODIGO INVALIDO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCFKCPF-CLI.
           MOVE CPF-CLI TO FKCPF-CLI.
                    
       READ-CADCLI.
           READ CADCLI
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                  MOVE CPF-CLI TO FKCPF-CLI
                  DISPLAY TELALOC
              ELSE
                  MOVE "* ERRO AO LER CLIENTE *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
           ELSE
               MOVE "* CLIENTE NAO EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCFKCPF-CLI.
        
       ACCFKNUM-BIKE.
           ACCEPT TNUM-BIKE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACCFKCPF-CLI.
           IF NUM-BIKE = ZEROS
               MOVE "* NUMERO DA BIKE VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCFKNUM-BIKE.
           MOVE NUM-BIKE TO FKNUM-BIKE.
        
       READ-CADBIKE.               
           READ CADBIKE
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   MOVE NUM-BIKE TO FKNUM-BIKE
                   DISPLAY TELALOC
               ELSE
                   MOVE "* ERRO AO LER BIKE *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
           ELSE
               MOVE "* BIKE NAO EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCFKNUM-BIKE.
            

      *
      *--------------------------------------------------------------
      * NESTA LABEL O PROGRAMA COMECA A LEITURA DOS CAMPOS
      * A PARTIR DA DATA DE LOCACAO
      *--------------------------------------------------------------
      *
       ACC-DATALOCACAO.
           ACCEPT TDTLOCACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 
               GO TO ACCFKNUM-BIKE
           IF DTLOCACAO = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-DATALOCACAO.
           MOVE 1 TO WFLAGDAT
           GO TO VALIDARDATAS.
       ACC-DATALOCACAOFIM.
       
       ACC-HORALOCACAO.
           ACCEPT THRLOCACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-DATALOCACAO.
      *     IF HRLOCACAO = ZEROS
      *         MOVE "* HORA INVALIDA *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-HORALOCACAO.
           MOVE 1 TO WFLAGHORAS
           GO TO VALIDARHORAS.
       ACC-HORALOCACAOFIM.
       
       ACC-MINLOCACAO.
           ACCEPT TMINLOCACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-HORALOCACAO.
      *     IF MINLOCACAO = ZEROS
      *         MOVE "* MINUTOS INVALIDOS *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-MINLOCACAO.
           MOVE 1 TO WFLAGMINUTOS
           GO TO VALIDARMINUTOS.
       ACC-MINLOCACAOFIM.
       
      *
      * LEITURA DA CHAVE PRIMARIA
      *
       READ-CADLOC.
           MOVE CPF-CLI TO FKCPF-CLI
           MOVE NUM-BIKE TO FKNUM-BIKE
           READ CADLOC
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   MOVE 1 TO W-SEL
                   DISPLAY TELALOC
                   GO TO ACE-001
               ELSE
                   MOVE "* ERRO AO LER LOCACAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
           ELSE
               NEXT SENTENCE.
       
       ACC-DATAPREVISAO.
           DISPLAY (01, 02) W-SEL
           ACCEPT TDTPREVISAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               IF W-SEL NOT = 1
                   GO TO ACC-MINLOCACAO
               ELSE
                   MOVE "* NAO PODE ALTERAR CHAVES *"
                       TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACC-DATAPREVISAO.
           IF DTPREVISAO = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-DATAPREVISAO.
           MOVE 2 TO WFLAGDAT
      ACC-DATAPREVISAOFIM.
          IF WFLAGDAT NOT = 0
              GO TO VALIDARDATAS
          ELSE
              PERFORM VALDATPREV.
       
       ACC-HORAPREVISAO.
           ACCEPT THRPREVISAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-DATAPREVISAO.
      *     IF HRLOCACAO = ZEROS
      *         MOVE "* HORA INVALIDA *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-HORAPREVISAO.
           MOVE 2 TO WFLAGHORAS
           GO TO VALIDARHORAS.
       ACC-HORAPREVISAOFIM.
       
       ACC-MINPREVISAO.
           ACCEPT TMINPREVISAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-HORAPREVISAO.
      *     IF MINPREVISAO = ZEROS
      *         MOVE "* MINUTOS INVALIDOS *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-MINPREVISAO.
           MOVE 2 TO WFLAGMINUTOS
           GO TO VALIDARMINUTOS.
       ACC-MINPREVISAOFIM.
       
       ACC-DATAENTREGA.
           ACCEPT TDTENTREGA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-MINPREVISAO.
           IF DTENTREGA = ZEROS
               MOVE "* DATA INVALIDA *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-DATAENTREGA.
           MOVE 3 TO WFLAGDAT
           GO TO VALIDARDATAS.
       ACC-DATAENTREGAFIM.
       
       ACC-HORAENTREGA.
           ACCEPT THRENTREGA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-DATAENTREGA.
      *     IF HRLOCACAO = ZEROS
      *         MOVE "* HORA INVALIDA *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-HORAENTREGA.
           MOVE 3 TO WFLAGHORAS
           GO TO VALIDARHORAS.
       ACC-HORAENTREGAFIM.
       
       ACC-MINENTREGA.
           ACCEPT TMINENTREGA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-HORAENTREGA.
      *     IF MINENTREGA = ZEROS
      *         MOVE "* MINUTOS INVALIDOS *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-MINENTREGA.
           MOVE 3 TO WFLAGMINUTOS
           GO TO VALIDARMINUTOS.
       ACC-MINENTREGAFIM.
       
       ACC-HORAUSO.
           ACCEPT THRUSO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-MINENTREGA.
      *     IF HRLOCACAO = ZEROS
      *         MOVE "* HORA INVALIDA *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-HORAUSO.
           MOVE 4 TO WFLAGHORAS
           GO TO VALIDARHORAS.
       ACC-HORAUSOFIM.
       
       ACC-MINUSO.
           ACCEPT TMINUSO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-HORAUSO.
      *     IF MINUSO = ZEROS
      *         MOVE "* MINUTOS INVALIDOS *" TO MENS
      *         PERFORM ROT-MENS THRU ROT-MENS-FIM
      *         GO TO ACC-MINUSO.
           MOVE 4 TO WFLAGMINUTOS
           GO TO VALIDARMINUTOS.
       ACC-MINUSOFIM.
       
       ACC-VALLOCACAO.
           ACCEPT TVALLOCACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-MINUSO.
           IF VALLOCACAO = ZEROS
               MOVE "* VALOR NAO PODE SER ZERO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-VALLOCACAO.
               
       ACC-VALCOBRADO.
           ACCEPT TVALCOBRADO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-VALLOCACAO.
           IF VALCOBRADO = ZEROS
               MOVE "* VALOR NAO PODE SER ZERO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACC-VALCOBRADO.
       
       ACCSTATUS-BIKE.
           ACCEPT TSTATUS-BIKE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO ACC-MINUSO.
           IF STATUS-BIKE = SPACES
               MOVE "* STATUS VAZIO! *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCSTATUS-BIKE.
           IF STATUS-BIKE = 'L' OR 'l'
               MOVE VALTABLE-DSCSTATUS(1) TO DSCSTATUS-BIKE
           ELSE IF STATUS-BIKE = 'E' OR 'e'
               MOVE VALTABLE-DSCSTATUS(2) TO DSCSTATUS-BIKE
           ELSE IF STATUS-BIKE = 'P' OR 'p'
               MOVE VALTABLE-DSCSTATUS(3) TO DSCSTATUS-BIKE
           ELSE
               MOVE "* STATUS INVALIDO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCSTATUS-BIKE.
           DISPLAY TELALOC
           IF W-SEL = 1
               GO TO ALT-OPC
           GO TO INC-OPC.

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
              GO TO ACCSTATUS-BIKE.
           IF W-OPCAO = "N" OR "n"
              MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO CLEAR-VARS.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM e N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-OPC.
       INC-WR1.
           WRITE REGLOC
           IF ST-ERRO = "00" OR "02"
               MOVE "* DADOS GRAVADOS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
           IF ST-ERRO = "22"
               MOVE "* BIKE JA EXISTE *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACCFKCPF-CLI
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
               GO TO ACC-DATAPREVISAO.
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
           DELETE CADLOC RECORD
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
           IF W-OPCAO = "N" OR "n"
               MOVE "* INFORMACOES NAO ALTERADAS *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO CLEAR-VARS.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGLOC
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
      * DIA-PREV
      * MES-PREV
      * ANO-PREV
      *
      VALDATPREV.
          IF ANO-PREV > ANO-LOCA OR ANO-PREV = ANO-LOCA
              IF MES-PREV > MES-LOCA OR MES-PREV = MES-LOCA
                  IF DIA-PREV > DIA-LOCA
                      EXIT.
                  ELSE
                      MOVE "* DATA DEVE SER MAIOR *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ACC-DATAPREVISAO.
              ELSE
                  MOVE "* DATA DEVE SER MAIOR *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ACC-DATAPREVISAO.
          ELSE
              MOVE "* DATA DEVE SER MAIOR *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ACC-DATAPREVISAO.
      *
      * DIA-ENTR
      * MES-ENTR
      * ANO-ENTR
      *
       VALDATENTR.
           IF ANO-ENTR > ANO-LOCA OR ANO-ENTR = ANO-LOCA
              IF MES-ENTR > MES-LOCA OR MES-ENTR = MES-LOCA
                  IF DIA-ENTR > DIA-LOCA
                      EXIT.
                  ELSE
                      MOVE "* DATA DEVE SER MAIOR *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ACC-DATAENTREGA.
              ELSE
                  MOVE "* DATA DEVE SER MAIOR *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ACC-DATAENTREGA.
          ELSE
              MOVE "* DATA DEVE SER MAIOR *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ACC-DATAENTREGA.
      *
      *--------------------------------------------------------------
      *     VALIDACAO DE HORAS
      *--------------------------------------------------------------
      *
       VALIDARHORAS.
           IF WFLAGHORAS = 1
               MOVE HRLOCACAO TO WHORAS
           ELSE IF WFLAGHORAS = 2
               MOVE HRPREVISAO TO WHORAS
           ELSE IF WFLAGHORAS = 3
               MOVE HRENTREGA TO WHORAS
           ELSE IF WFLAGHORAS = 4
               MOVE HRUSO TO WHORAS.
       HORAS.
           IF WHORAS > 23 OR WHORAS < 0
               GO TO HORAS-ERRO
           ELSE
               GO TO HORAS-FIM.
       HORAS-ERRO.
           MOVE "* HORA INVALIDA *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           IF WFLAGHORAS = 1
               GO TO ACC-HORALOCACAO
           ELSE IF WFLAGHORAS = 2
               GO TO ACC-HORAPREVISAO
           ELSE IF WFLAGHORAS = 3
               GO TO ACC-HORAENTREGA
           ELSE IF WFLAGHORAS = 4
               MOVE 0 TO WFLAGHORAS
               GO TO ACC-HORAUSO.
       HORAS-FIM.
           IF WFLAGHORAS = 1
               GO TO ACC-HORALOCACAOFIM
           ELSE IF WFLAGHORAS = 2
               GO TO ACC-HORAPREVISAOFIM
           ELSE IF WFLAGHORAS = 3
               GO TO ACC-HORAENTREGAFIM
           ELSE IF WFLAGHORAS = 4
               MOVE 0 TO WFLAGHORAS
               GO TO ACC-HORAUSOFIM.
      *
      *--------------------------------------------------------------
      *     VALIDACAO DE MINUTOS
      *--------------------------------------------------------------
      *
       VALIDARMINUTOS.
           IF WFLAGMINUTOS = 1
               MOVE MINLOCACAO TO WMINUTOS
           ELSE IF WFLAGMINUTOS = 2
               MOVE MINPREVISAO TO WMINUTOS
           ELSE IF WFLAGMINUTOS = 3
               MOVE MINENTREGA TO WMINUTOS
           ELSE IF WFLAGMINUTOS = 4
               MOVE MINUSO TO WMINUTOS.
       MINUTOS.
           IF WMINUTOS > 59 OR WMINUTOS < 0
               GO TO MINUTOS-ERRO
           ELSE
               GO TO MINUTOS-FIM.
       MINUTOS-ERRO.
           MOVE "* MINUTOS INVALIDOS *" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           IF WFLAGMINUTOS = 1
                GO TO ACC-MINLOCACAO
            ELSE IF WFLAGMINUTOS = 2
                GO TO ACC-MINPREVISAO
            ELSE IF WFLAGMINUTOS = 3
                GO TO ACC-MINENTREGA
            ELSE IF WFLAGMINUTOS = 4
                MOVE 0 TO WFLAGMINUTOS
                GO TO ACC-MINUSO.
           
       MINUTOS-FIM.
            IF WFLAGMINUTOS = 1
                GO TO ACC-MINLOCACAOFIM
            ELSE IF WFLAGMINUTOS = 2
                GO TO ACC-MINPREVISAOFIM
            ELSE IF WFLAGMINUTOS = 3
                GO TO ACC-MINENTREGAFIM
            ELSE IF WFLAGMINUTOS = 4
                MOVE 0 TO WFLAGMINUTOS
                GO TO ACC-MINUSOFIM.
      *
      *--------------------------------------------------------------
      *     VALIDACAO DAS DATAS
      *--------------------------------------------------------------
      *
      * VALIDACAO DAS DUAS DATAS
      *
       VALIDARDATAS.
           IF WFLAGDAT = 1
               MOVE DIA-LOCA TO WDIA
               MOVE MES-LOCA TO WMES
               MOVE ANO-LOCA TO WANO
           ELSE IF WFLAGDAT = 2
               MOVE DIA-PREV TO WDIA
               MOVE MES-PREV TO WMES
               MOVE ANO-PREV TO WANO
           ELSE IF WFLAGDAT = 3
               MOVE DIA-ENTR TO WDIA
               MOVE MES-ENTR TO WMES
               MOVE ANO-ENTR TO WANO
           GO TO VALIDARMES.
       
       VALIDARMES.
           IF WMES > 12 OR WMES < 1
               GO TO VALIDARDATAS-ERRO
           ELSE
               GO TO MESVAL.
       MESVAL.
            GO TO D31 FEV D31 D30 D31 D30 D31 D31 D30 D31 D30 D31
               DEPENDING ON WMES.
       D31.
           IF WDIA > 31 OR WDIA < 1
               MOVE ZEROS TO DATNASC
               GO TO VALIDARDATAS-ERRO
           ELSE
               GO TO VALIDARDATAS-FIM.
       D30.
           IF WDIA > 30 OR WDIA < 1
               GO TO VALIDARDATAS-ERRO
           ELSE
               GO TO VALIDARDATAS-FIM.
      *  MES DE FEVEREIRO SEGUNDO CALENDARIO GREGORIANO
      *  ANOS BISSEXTOS SAO RELEVANTES
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
               GO TO VALIDARDATAS-ERRO
           ELSE
               GO TO VALIDARDATAS-FIM.
      *  SE O ANO NAO E BISSEXTO
       NOTBISSEXTO.
           IF WDIA > 28 OR WDIA < 1
               GO TO VALIDARDATAS-ERRO
           ELSE
               GO TO VALIDARDATAS-FIM.
       
      *--------------------------------------------------------------
       
       VALIDARDATAS-ERRO.
            MOVE "* DATA INVALIDA *" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            IF WFLAGDAT = 1
               GO TO ACC-DATALOCACAO
            ELSE IF WFLAGDAT = 2
               GO TO ACC-DATAPREVISAO
            ELSE IF WFLAGDAT = 3
               MOVE 0 TO WFLAGDAT
               GO TO ACC-DATAENTREGA.
       VALIDARDATAS-FIM.
            IF WFLAGDAT = 1
               GO TO ACC-DATALOCACAOFIM
            ELSE IF WFLAGDAT = 2
               GO TO ACC-DATAPREVISAOFIM
            ELSE IF WFLAGDAT = 3
               MOVE 0 TO WFLAGDAT
               GO TO ACC-DATAENTREGAFIM.
      *
      *--------------------------------------------------------------
      * FIM DE APLICACAO
      * AUTHOR: VINICIUS ESCAME
      *--------------------------------------------------------------
      *
