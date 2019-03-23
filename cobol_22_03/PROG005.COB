        IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG005.
       AUTHOR. THALES ANDRE PEREIRA SOUZA SERRA.
      
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
      
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD SISNOTAS
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "SISNOTAS.DAT".
       01 REGNOTAS.
            03 MATRICULA   PIC X(13).
		    03 NOME        PIC X(30).
       		03 NOTA1       PIC 9(02)V99.
       		03 NOTA2       PIC 9(02)V99.
       		03 FALTA1      PIC 9(02).
       		03 FALTA2      PIC 9(02).
      
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 SITUACAO     PIC X(25) VALUE SPACES.
       01 MEDIA        PIC 9(02)V99 VALUE ZEROS.
       01 FALTAS       PIC 9(02) VALUE ZEROS. 
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELANOTA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "   CADASTRO DE NOTAS DO ALUNO".
           05  LINE 04  COLUMN 01 
               VALUE  "   Matricula".
           05  LINE 05  COLUMN 01 
               VALUE  "   Nome".
           05  LINE 06  COLUMN 01 
               VALUE  "   1a Nota".
           05  LINE 07  COLUMN 01 
               VALUE  "   2a Nota".
           05  LINE 08  COLUMN 01 
               VALUE  "   Media".
           05  LINE 09  COLUMN 01 
               VALUE  "   Falta 1".
           05  LINE 10  COLUMN 01 
               VALUE  "   Falta 2".
           05  LINE 11  COLUMN 01 
               VALUE  "   Situacao".
           05  TMATRICULA
               LINE 04  COLUMN 14  PIC X(13)
               USING  MATRICULA
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 14  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TNOTA1
               LINE 06  COLUMN 14  PIC 9(02)
               USING  NOTA1
               HIGHLIGHT.
           05  TNOTA2
               LINE 07  COLUMN 14  PIC 9(02)
               USING  NOTA2
               HIGHLIGHT.
           05  TMEDIA
               LINE 08  COLUMN 14  PIC 9(02)
               USING  MEDIA.
           05  TFALTA1
               LINE 09  COLUMN 14  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TFALTA2
               LINE 10  COLUMN 14  PIC 9(02)
               USING  FALTA2
               HIGHLIGHT.
           05  TSITUACAO
               LINE 11  COLUMN 14  PIC X(25)
               USING  SITUACAO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       
       INC-OP0.
           OPEN I-O SISNOTAS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                    OPEN OUTPUT SISNOTAS
                    CLOSE SISNOTAS
                    MOVE "* ARQUIVO DE NOTAS SENDO CRIADO *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-OP0
               ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       INC-001.
            MOVE SPACES TO MATRICULA NOME SITUACAO.
		    MOVE ZEROS TO NOTA1 NOTA2 FALTA1 FALTA2 MEDIA FALTAS.
            DISPLAY TELANOTA.
       INC-002.
		    MOVE SPACES TO MATRICULA
            ACCEPT TMATRICULA
            ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE SISNOTAS
                   GO TO ROT-FIM.
                IF MATRICULA = SPACES 
                   MOVE "MATRICULA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ SISNOTAS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      GO TO LTR-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ."   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
		MOVE SPACES TO NOME
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES 
                   MOVE "NOME INVALIDO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       INC-004.
		MOVE ZEROS TO NOTA1
                ACCEPT TNOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
		IF NOTA1 < 0 OR NOTA1 > 10
                   MOVE "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
		MOVE ZEROS TO NOTA2
                ACCEPT TNOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
		IF NOTA2 < 0 OR NOTA2 > 10
                   MOVE "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
                PERFORM CALC-001
		DISPLAY TELANOTA.

       INC-006.
		MOVE ZEROS TO FALTA1
                ACCEPT TFALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
		   MOVE ZEROS TO MEDIA
		   GO TO INC-005.
       INC-007.
		MOVE ZEROS TO FALTA2
                ACCEPT TFALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
		PERFORM CALC-002.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
		   MOVE ZEROS TO FALTAS
		   DISPLAY TELANOTA

		   GO TO INC-007.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS *" TO MENS
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
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE NOTAS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
       CALC-001.
                MOVE ZEROS TO MEDIA.
                COMPUTE MEDIA = (NOTA1 + NOTA2)/2.

       CALC-002.
                MOVE ZEROS TO FALTAS.
                COMPUTE FALTAS = (FALTA1 + FALTA2)
                PERFORM VRF-001.

       VRF-001.
                IF FALTAS > 20 AND MEDIA < 05
                      MOVE "REPROVADO" TO SITUACAO.
                IF FALTAS < 21 AND MEDIA < 05
                      MOVE "RECUPERACAO POR NOTA" TO SITUACAO.
                IF FALTAS > 20 AND MEDIA > 04,99
                      MOVE "RECUPERACAO POR FALTA" TO SITUACAO.
                IF FALTAS < 21 AND MEDIA > 04,99
                      MOVE "APROVADO" TO SITUACAO.
		DISPLAY TELANOTA.

       LTR-001.
		PERFORM CALC-001
		PERFORM CALC-002
                MOVE "*** MATRICULA JA CADASTRADA ***" TO MENS
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
                MOVE "ERRO NA ALTERACAO DO REGISTRO DE NOTAS"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      
      * ROTINA DE FIM
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
            
      * ROTINA DE MENSAGEM
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
