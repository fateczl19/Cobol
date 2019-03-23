       IDENTIFICATION DIVISION.
       PROGRAM-ID. SP18202.
       AUTHOR. WILLIAM RODRIGUES BRITO.
      **************************************
      * MANUTENCAO DO CADASTRO DE CARGOS   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CARGOS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CAR-COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CAR-DENOM
                                            WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CARGOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CARGO.DAT".
       01 REGCARGO.
                03 CAR-COD          PIC 9(04).
                03 CAR-DENOM        PIC X(20).
                03 CAR-TS           PIC X(01).
                03 CAR-TSC          PIC X(12).
                03 CAR-NIVEL        PIC 9(01).
                03 CAR-NIVELC       PIC X(15).
                03 CAR-SALB         PIC 9(06).
                03 CAR-TRISC        PIC X(01).
                03 CAR-TRISCC       PIC X(12).
                03 CAR-TSITU        PIC X(01).
                03 CAR-TSITUC       PIC X(12).

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
       01 TABNIVEL.
          03 TN1 PIC X(48) VALUE
          "PRESIDENCIA DIRETORIA   GERENCIA    SUPERVISAO".
          03 TN2 PIC X(48) VALUE
          "COORDENAC   OPERACIONAL ADMINISTRTV TECNICO   ".
          03 TN3 PIC X(12) VALUE "CONSULTORIA ".
          03 TN4 PIC X(12) VALUE "ASSESSORIA  ".

       01 TABAUX3 REDEFINES TABNIVEL.
           03 TDNIVL       PIC X(12) OCCURS 10 TIMES.
      *---------------------------------------------------------
       SCREEN SECTION.
       01  SP18202.
           05  LINE 02  COLUMN 01
               VALUE  "                         CADASTRO DE CAR".
           05  LINE 02  COLUMN 41
               VALUE  "GOS".
           05  LINE 04  COLUMN 01
               VALUE  "           CODIGO".
           05  LINE 06  COLUMN 01
               VALUE  "           DENOMINACAO".
           05  LINE 08  COLUMN 01
               VALUE  "           TIPO SALARIO     -".
           05  LINE 10  COLUMN 01
               VALUE  "           NIVEL            -".
           05  LINE 12  COLUMN 01
               VALUE  "           SALARIO BASE".
           05  LINE 14  COLUMN 01
               VALUE  "           TIPO RISCO       -".
           05  LINE 16  COLUMN 01
               VALUE  "           SITUACAO         -".
           05  LINE 23  COLUMN 01
               VALUE  "           MENSAGEM".
           05  TCOD
               LINE 04  COLUMN 27  PIC 9(04)
               USING  CAR-COD
               HIGHLIGHT.
           05  TDENOM
               LINE 06  COLUMN 27  PIC X(20)
               USING  CAR-DENOM
               HIGHLIGHT.
           05  TTS
               LINE 08  COLUMN 27  PIC X(01)
               USING  CAR-TS
               HIGHLIGHT.
           05  TTSC
               LINE 08  COLUMN 31  PIC X(12)
               USING  CAR-TSC
               HIGHLIGHT.
           05  TNIVEL
               LINE 10  COLUMN 27  PIC 9(01)
               USING  CAR-NIVEL
               HIGHLIGHT.
           05  TNIVELC
               LINE 10  COLUMN 31  PIC X(15)
               USING  CAR-NIVELC
               HIGHLIGHT.
           05  TSALB
               LINE 12  COLUMN 27  PIC 9(06)
               USING  CAR-SALB
               HIGHLIGHT.
           05  TTRISC
               LINE 14  COLUMN 27  PIC X(01)
               USING  CAR-TRISC
               HIGHLIGHT.
           05  TTRISCC
               LINE 14  COLUMN 31  PIC X(12)
               USING  CAR-TRISCC
               HIGHLIGHT.
           05  TSITU
               LINE 16  COLUMN 27  PIC X(01)
               USING  CAR-TSITU
               HIGHLIGHT.
           05  TSITUC
               LINE 16  COLUMN 31  PIC X(12)
               USING  CAR-TSITUC
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CARGOS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CARGOS
                      MOVE "*** ARQUIVO CARGO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      CLOSE CARGOS
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CARGOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CAR-SALB CAR-NIVEL CAR-COD

                MOVE SPACES TO CAR-NIVELC CAR-TSC CAR-TS
                               CAR-DENOM CAR-TSITUC CAR-TRISCC
                               CAR-TSITU CAR-TRISC

                DISPLAY SP18202.

       INC-002.
                ACCEPT TCOD
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CARGOS
                   GO TO ROT-FIM.
                IF CAR-COD = ZEROS
                   MOVE "*** COD INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CARGOS.
                MOVE 0 TO W-SEL
                READ CARGOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY SP18202
                      MOVE "*** CARGO JA CADASTRAD0 ***" TO MENS
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
                ACCEPT TDENOM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-DENOM = SPACES
                     MOVE "****PREENCHA O DENOM DO CARGO****" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-003
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TTS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-TS = SPACES
                    MOVE "****PREENCHA O TS DO CARGO****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004
                ELSE
                IF CAR-TS = 'H'
                    MOVE "HORISTA" TO CAR-TSC
                ELSE IF CAR-TS = 'M'
                        MOVE "MENSALISTA" TO CAR-TSC
                    ELSE IF CAR-TS = 'D'
                        MOVE "DIARISTA" TO CAR-TSC
                            ELSE IF CAR-TS = 'C'
                            MOVE "COMISSIONAD" TO CAR-TSC
                            ELSE
                            MOVE "***PREENCHA O TS INVALIDO***" TO MENS
                                PERFORM ROT-MENS THRU ROT-MENS-FIM
                                GO TO INC-004
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TNIVEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-NIVEL = ZEROS
                    MOVE "****PREENCHA O NIVEL DO CARGO****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT  TSALB
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-SALB = ZEROS
                    MOVE "****PREENCHA O SALB DO CARGO****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TTRISC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-TRISC = SPACES
                    MOVE "****PREENCHA O TRISC DO CARGO****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-007
                ELSE
                IF CAR-TRISC = 'A'
                    MOVE "ALTO" TO CAR-TRISCC
                ELSE IF CAR-TRISC = 'B'
                        MOVE "BAIXO" TO CAR-TRISCC
                    ELSE IF CAR-TRISC = 'M'
                        MOVE "MEDIO" TO CAR-TRISCC
                            ELSE IF CAR-TRISC = 'G'
                            MOVE "GRAVE" TO CAR-TRISCC
                            ELSE
                            MOVE "***PREENCHA O TS INVALIDO***" TO MENS
                                PERFORM ROT-MENS THRU ROT-MENS-FIM
                                GO TO INC-007
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TSITU
                ACCEPT W-ACT FROM ESCAPE KEY
                IF CAR-TSITU = SPACES
                    MOVE "****PREENCHA O TSITU DO CARGO****" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-008
                ELSE
                IF CAR-TSITU = 'A'
                    MOVE "ATIVO" TO CAR-TSITUC
                ELSE IF CAR-TSITU = 'I'
                        MOVE "INATIVO" TO CAR-TSITUC
                    ELSE IF CAR-TSITU = 'C'
                        MOVE "CANCELADO" TO CAR-TSITUC
                            ELSE
                            MOVE "**PREENCHA A SITU INVALIDO**" TO MENS
                                PERFORM ROT-MENS THRU ROT-MENS-FIM
                                GO TO INC-008
                IF W-ACT = 02 GO TO INC-007.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
                MOVE TDSALR(CAR-NIVEL)  TO CAR-NIVELC
                DISPLAY SP18202.
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
                WRITE REGCARGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CARGO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CARGOS"
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
                DELETE CARGOS RECORD
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
                REWRITE REGCARGO
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
