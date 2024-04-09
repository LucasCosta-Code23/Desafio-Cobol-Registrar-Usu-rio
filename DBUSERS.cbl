      ******************************************************************
      * Author: LUCAS PEREIRA COSTA
      * Date: 06/04/2024
      * Purpose: LISTAR USUARIOS CADASTRADOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBUSERS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT CONTATOS ASSIGN TO
                'C:\Users\Lucas\CONTATOS.DAT'
                ORGANIZATION IS INDEXED
                ACCESS MODE  IS SEQUENTIAL
                RECORD  KEY  IS ID-USUARIO
                FILE STATUS  IS WK-FS.

       DATA DIVISION.
       FILE SECTION.
       FD CONTATOS.
            COPY REGDADOS.

       WORKING-STORAGE SECTION.
       01 WK-REGISTRO                      PIC X(88) VALUE SPACES.
       01 FILLER REDEFINES WK-REGISTRO.
          03 WK-ID-USUARIO                 PIC 9(2).
          03 WK-NOME-USUARIO               PIC X(35).
          03 WK-SENHA-USUARIO              PIC X(10).
          03 WK-NUMERO-USUARIO             PIC X(11).
          03 WK-EMAIL-USUARIO              PIC X(30).
       77 WK-FS                            PIC 99.
          88 FS-OK                                   VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE'S'  FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE'F'  FALSE 'N'.
       77 WK-ID                            PIC 99    VALUE 0.
       77 WK-CONT                          PIC 9(3)  VALUES ZEROS.

       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0001-PRINCIPAL.
            SET EXIT-OK TO FALSE
            DISPLAY ' REG  ID  NOME                    '
                    '             SENHA      TELEFONE    EMAIL'

            PERFORM 0101-DBLISTA THRU 0101-FIM
            PERFORM 0901-FINALIZAR
            .
       0100-DBLISTA                SECTION.
       0101-DBLISTA.
            SET EOF-OK              TO FALSE
            SET FS-OK               TO TRUE
            SET WK-CONT             TO 0.
      *
            OPEN INPUT CONTATOS

      *
            IF FS-OK THEN
               PERFORM UNTIL EOF-OK
                   READ CONTATOS INTO WK-REGISTRO
                   AT END
                       SET EOF-OK TO TRUE
                   NOT AT END
                       ADD 1 TO WK-CONT
                       DISPLAY ' '
                       WK-CONT
                       '  '
                       WK-ID-USUARIO
                       '  '
                       WK-NOME-USUARIO
                       '  '
                       WK-SENHA-USUARIO
                       ' '
                       WK-NUMERO-USUARIO
                       ' '
                       WK-EMAIL-USUARIO
                       ' '

                   END-READ
               END-PERFORM
            ELSE
                DISPLAY 'Erro ao abrir o arquivo de Contatos.' WK-FS
            END-IF

            CLOSE CONTATOS
            .
       0101-FIM.

       0900-FINALIZAR SECTION.
       0901-FINALIZAR.
            STOP RUN.
       END PROGRAM DBUSERS.
