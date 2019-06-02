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
            SELECT CADCLI
                    ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF-CLI
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS FKCEP
                                            WITH DUPLICATES.

            SELECT CADCEP
                    ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS LOGRAD
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
           03 CPF-CLI           PIC 9(11).
           03 RG-CLI            PIC X(09).
           03 NOME-CLI          PIC X(30).
           03 EMAIL-CLI         PIC X(20).
           03 NUM-CLI           PIC 9(04).
           03 TEL-CLI           PIC 9(11).
           03 SITUAC-CLI        PIC X(01).
           03 DSCSITUAC-CLI     PIC X(08).
           03 COMPLEM-CLI       PIC X(13).
      * ESSE CAMPO SERVIRA PRA QUANDO CARREGAR O CLIENTE
      * CARREGAR O CEP E SER POSSIVEL CARREGAR AS INFORMACOES
      * DO CADASTRO DO CEP
      * BASICAMENTE UMA FOREIGN KEY 
           03 FKCEP         PIC 9(08).
           03 DATNASC.
               05 DIANASC-CLI    PIC 9(02).
               05 MESNASC-CLI    PIC 9(02).
               05 ANONASC-CLI    PIC 9(04).
      *
       FD CADCEP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP.DAT".

      * OS CAMPOS SAO OS MESMOS DO PROJETO ANTERIOR
      * POREM, AGORA USO APENAS OS NECESSARIOS
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
      *--------------------------------------------------------------
      *
       WORKING-STORAGE SECTION.
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
      *
      *--------------------------------------------------------------
      *
       SCREEN SECTION.
       01  TELACLI.
           05  LINE 01  COLUMN 01 
               VALUE  "---CONSULTA DE CLIENTE------------------".
           05  LINE 01  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  "   CPF:".
           05  LINE 06  COLUMN 01 
               VALUE  "   NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  "   TELEFONE:".
           05  LINE 10  COLUMN 01 
               VALUE  "   BAIRRO:".
           05  LINE 16  COLUMN 01 
               VALUE  "---AREA DE MENSAGENS--------------------".
           05  LINE 16  COLUMN 41 
               VALUE  "----------------------------------------".
           05  TCPF
               LINE 04  COLUMN 10  PIC 999.999.999.99
               USING  CPF-CLI.
           05  TNOME
               LINE 06  COLUMN 10  PIC X(35)
               USING  NOME-CLI.
           05  TTEL
               LINE 08  COLUMN 14  PIC 9(11)
               USING  TEL-CLI.
           05  TBAIRRO
               LINE 10  COLUMN 12  PIC X(20)
               USING  BAIRRO.
      *
      *--------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.
       INC-OP0.
           OPEN INPUT CADCLI.
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   MOVE "* O ARQUIVO CADCLI.DAT NAO EXISTE*" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   CLOSE CADCLI
                   GO TO ROT-FIM.
            OPEN INPUT CADCEP.
       CLEAR-VARS.
           MOVE ZEROS TO CPF-CLI DATNASC CEP NUM-CLI TEL-CLI
           MOVE ZEROS TO FKCEP
           MOVE SPACES TO NOME-CLI RG-CLI EMAIL-CLI LOGRAD 
           MOVE SPACES TO COMPLEM-CLI UF BAIRRO CIDADE
           MOVE SPACES TO SITUAC-CLI DSCSITUAC-CLI
           DISPLAY TELACLI.
       ACC-PK.
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR 01
               CLOSE CADCLI CADCEP
               GO TO ROT-CONFIRMA.
           IF CPF-CLI = ZEROS
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
                   MOVE 1 TO W-SEL
                   MOVE FKCEP TO CEP
                   PERFORM READ-CADCEP
                   MOVE "* CLIENTE JA CADASTRADO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "* ERRO AO LER REGISTRO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
           IF W-ACT = 01
               PERFORM ROT-MENS THRU ROT-MENS-FIM.
       READ-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   DISPLAY TELACLI
               ELSE
                   MOVE "ERRO AO LER O ARQUIVO CADCEP" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
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
               CLOSE CADCEP CADCLI
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
               CLOSE CADCEP CADCLI
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
