        IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG004.
       AUTHOR. THALES ANDRÉ PEREIRA SOUZA SERRA.
      **************************************
      * MANUTENCAO DO CADASTRO DE NOTAS    *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT SISNOTAS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS MATRICULA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD SISNOTAS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "SISNOTAS.DAT".
       01 REGNOTAS.
                03 MATRICULA         PIC X(15).
		03 NOME        PIC X(30).
       		03 NOTA1       PIC 9(02).
       		03 NOTA2       PIC 9(02).
       		03 FALTA1      PIC 9(02).
       		03 FALTA2      PIC 9(02).      
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 SITUACAO     PIC X(50) VALUE SPACES.
       01 MEDIA        PIC 9(02) VALUE ZEROS.
       01 FALTAS       PIC 9(02) VALUE ZEROS. 
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O SISNOTAS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT SISNOTAS
                      CLOSE SISNOTAS
                      MOVE "* ARQUIVO SISNOTAS SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO PRODUTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE SPACES TO MATRICULA NOME.
		MOVE ZEROS TO NOTA1 NOTA2 FALTA1 FALTA2
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE NOTAS"
                DISPLAY (04, 01) "MATRICULA          : ".
                DISPLAY (05, 01) "NOME               : ".
		DISPLAY (06, 01) "NOTA 1 BIM         : ".
                DISPLAY (07, 01) "NOTA 2 BIM         : ".
                DISPLAY (08, 01) "       ".
                DISPLAY (09, 01) "FALTAS 1 BIM       : ".
                DISPLAY (10, 01) "FALTAS 2 BIM       : ".
                DISPLAY (11, 01) "       ".
       INC-002.
                DISPLAY (04, 22) "                    ".
                ACCEPT  (04, 22) MATRICULA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE SISNOTAS
                   GO TO ROT-FIM.
                IF MATRICULA = SPACES 
                   MOVE "*** MATRICULA INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ SISNOTAS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      GO TO LTR-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                DISPLAY (05, 22) "                    ".
                ACCEPT (05, 22) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES 
                   MOVE "NOME DEVE SER DIFERENTE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       INC-004.
                DISPLAY (06, 22) "       ".
                ACCEPT (06, 22) NOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
		IF NOTA1 < 0 OR NOTA1 > 10
                   MOVE "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                DISPLAY (07, 22) "       ".
                ACCEPT (07, 22) NOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
		IF NOTA2 < 0 OR NOTA2 > 10
                   MOVE "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
                GO TO CALC-001.
       INC-006.
                DISPLAY (09, 22) "       ".
                ACCEPT (09, 22) FALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                DISPLAY (10, 22) "       ".
                ACCEPT (10, 22) FALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
		GO TO CALC-002.
      *

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGNOTAS
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** MATRICULA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE SISNOTAS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       CALC-001.
                MOVE ZEROS TO MEDIA FALTAS.
                COMPUTE MEDIA = (NOTA1 + NOTA2)/2
                DISPLAY (08, 01) "MEDIA              : ".
                DISPLAY (08, 22) MEDIA
                GO TO INC-006.

       CALC-002.
                MOVE ZEROS TO FALTAS.
                COMPUTE FALTAS = (FALTA1 + FALTA2)
                DISPLAY (11, 01) "TOTAL FALTAS       : ".
                DISPLAY (11, 22) FALTAS
                GO TO TST-001.

       TST-001.
                IF FALTAS > 20 AND MEDIA < 05
                      MOVE "REPROVADO" TO SITUACAO.
                IF FALTAS < 21 AND MEDIA < 05
                      MOVE "RECUPERACAO POR NOTA" TO SITUACAO.
                IF FALTAS > 20 AND (MEDIA > 05 OR MEDIA = 5)
                      MOVE "RECUPERACAO POR FALTA" TO SITUACAO.
                IF FALTAS < 21 AND (MEDIA > 05 OR MEDIA = 5)
                      MOVE "APROVADO" TO SITUACAO.
		DISPLAY (13, 01) SITUACAO
                IF W-SEL = 1
                              GO TO ALT-OPC.
            
		GO TO INC-OPC.

       LTR-001.
                MOVE ZEROS TO MEDIA FALTAS.
                DISPLAY (05, 22) NOME
                DISPLAY (06, 22) NOTA1
                DISPLAY (07, 22) NOTA2
                DISPLAY (08, 01) "MEDIA              : ".
                COMPUTE MEDIA = (NOTA1 + NOTA2)/2
                DISPLAY (08, 22) MEDIA
		DISPLAY (09, 22) FALTA1
		DISPLAY (10, 22) FALTA2
                DISPLAY (11, 01) "TOTAL FALTAS       : ".
                COMPUTE FALTAS = (FALTA1 + FALTA2)
                DISPLAY (11, 22) FALTAS
                IF FALTAS > 20 AND MEDIA < 05
                      MOVE "REPROVADO" TO SITUACAO.
                IF FALTAS < 21 AND MEDIA < 05
                      MOVE "RECUPERACAO POR NOTA" TO SITUACAO.
                IF FALTAS > 20 AND MEDIA > 04,99
                      MOVE "RECUPERACAO POR FALTA" TO SITUACAO.
                IF FALTAS < 21 AND MEDIA > 04,99
                      MOVE "APROVADO" TO SITUACAO.
		DISPLAY (13, 01) SITUACAO
                MOVE "*** MATRICULA JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                MOVE 1 TO W-SEL
                GO TO ACE-001.

       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE SISNOTAS RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGNOTAS
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA ALTERACAO DO REGISTRO SISNOTAS"   TO MENS
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
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.