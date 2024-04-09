      ******************************************************************
      * Author: LUCAS PEREIRA COSTA
      * Date: 06/04/2024
      * Purpose: CADASTRAR CONTATOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT CONTATOS ASSIGN TO
                'C:\Users\Lucas\CONTATOS.DAT'
                ORGANIZATION IS INDEXED
                ACCESS MODE  IS RANDOM
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
          88 FS-OK                         VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE'F' FALSE 'N'.
       77 WK-ID                            PIC 99    VALUE 0.
       77 WK-OPCAO                         PIC X.
      *                                                              *
       77 WK-COMPLETO                      PIC X(60) VALUE SPACES.
       01 WK-NOME.
          03 WK-PRIMEIRO                   PIC X(20) VALUE SPACES.
          03 WK-SOBRENOME                  PIC X(20) VALUE SPACES.
          03 WK-ULTIMO                     PIC X(20) VALUE SPACES.
       77 WK-ESPACOS                       PIC 9(02) VALUE ZEROS.
       77 WK-I                             PIC 9(02) VALUE ZEROS.
       77 WK-P                             PIC 9(02) VALUE ZEROS.
       77 WK-S                             PIC 9(02) VALUE ZEROS.
       77 WK-U                             PIC 9(02) VALUE ZEROS.
      *                                                              *
       01 WK-SENHA                         PIC X(10).
       01 WK-SENHA-VALIDA                  PIC X VALUE 'N'.
       01 WK-TEM-NUMERO                    PIC X VALUE 'N'.
       01 WK-TEM-MAIUSCULA                 PIC X VALUE 'N'.
       01 WK-TEM-MINUSCULA                 PIC X VALUE 'N'.
       01 WK-TEM-ESPECIAL                  PIC X VALUE 'N'.
       01 WK-I-S                           PIC 9(3) VALUE 1.
       01 WK-CARACTERES                    PIC 9(3) VALUE 0.
       01 WK-CARACTERES-WITH-SPACE         PIC X(20).
       01 WK-LENGTH-SENHA                  PIC 9(3) VALUE 0.
       01 WK-NUMERIC                       PIC 9(3) VALUE 0.
       01 WK-MAIUSCULAS                    PIC 9(3) VALUE 0.
       01 WK-MINUSCULAS                    PIC 9(3) VALUE 0.
       01 WK-ESPECIAIS                     PIC 9(3) VALUE 0.
      *                                                              *
       77 WK-OPCAO-T                       PIC 9.
       77 WK-TAMANHO-TEL                   PIC 99    VALUE ZERO.
       01 WK-TELEFONE.
          03 WK-DDD                        PIC 99.
          03 WK-PREFIXO                    PIC 9(04).
          03 WK-SUFIXO                     PIC 9(04).
       77 WK-TAMANHO-CEL                   PIC 99    VALUE ZERO.
       01 WK-CEL.
          03 WK-DDD-CEL                    PIC 99.
          03 WK-PREFIXO-CEL                PIC 9(05).
          03 WK-SUFIXO-CEL                 PIC 9(04).
       77 WK-TM-1                          PIC 99.
       77 WK-TM-2                          PIC 99.
       77 WK-NUMERO                        PIC X(11).
      *                                                              *
       77 WK-EMAIL                         PIC X(30) VALUE SPACES.
       77 WK-ARRPOS                        PIC 9(02) VALUE ZEROS.
       77 WK-ARRTEM                        PIC 9(01) VALUE ZEROS.
       77 WK-QTD-ANTES-ARROBA              PIC 99 VALUE ZEROS.
       77 WK-MIN-CARACTERES                PIC 99 VALUE 1.
       77 WK-POSICAO-ARROBA                PIC 99 VALUE ZEROS.
       77 WK-QTD-FIM-EMAIL                 PIC 99 VALUE ZEROS.
       77 WK-DOMINIO1               PIC X(15) VALUE 'capgemini.com'.
       77 WK-DOMINIO2               PIC X(15) VALUE 'bradesco.com'.
       01 WK-EMAIL-VALID            PIC X VALUE 'N'.





       PROCEDURE DIVISION.
            SET EXIT-OK TO FALSE
            PERFORM 0001-PRINCIPAL THRU 0901-CADASTRAR UNTIL EXIT-OK
            PERFORM 1101-FINALIZAR
            .

       0000-PRINCIPAL SECTION.
       0001-PRINCIPAL.
            PERFORM 0100-PREENCHENOME
            PERFORM 0200-PREENCHESENHA
            PERFORM 0300-PREENCHETEL
            PERFORM 0600-PREENCHEEMAIL
            PERFORM 0800-DOMINIO
            PERFORM 0900-CADASTRAR
            PERFORM 1000-CONSULTAR
            PERFORM 1100-FINALIZAR
            .
      *                    VALIDAR NOME                               *
       0100-PREENCHENOME                          SECTION.
       0101-PREENCHENOME.
               INITIALIZE WK-COMPLETO
               INITIALIZE WK-ESPACOS
               INITIALIZE WK-NOME
               DISPLAY 'Digite seu nome completo:'
               ACCEPT WK-NOME
               MOVE WK-NOME TO WK-COMPLETO

               PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I >
               LENGTH OF WK-COMPLETO
                    UNSTRING
                        WK-COMPLETO
                        DELIMITED BY ALL SPACES
                        INTO WK-PRIMEIRO  COUNT  IN WK-P
                             WK-SOBRENOME COUNT  IN  WK-S
                             WK-ULTIMO    COUNT  IN  WK-U
                        WITH POINTER WK-I
                        TALLYING IN WK-ESPACOS
                    END-UNSTRING

                    IF WK-ESPACOS >= 2
                       DISPLAY 'Nome valido'
                       DISPLAY ' '
                       EXIT PERFORM
                    ELSE
                       DISPLAY 'Nome deve ter pelo menos 2 palavras'
                       PERFORM 0101-PREENCHENOME
                    END-IF
               END-PERFORM


               .
      *
       0200-PREENCHESENHA SECTION.
       0201-PREENCHESENHA.
            INITIALIZE WK-SENHA
            DISPLAY 'Digite sua senha: '
            ACCEPT WK-SENHA

            PERFORM 0202-VALIDA-SENHA.

            IF WK-SENHA-VALIDA = 'N'
               DISPLAY 'Senha invalida.'
               PERFORM 0201-PREENCHESENHA
            ELSE
               DISPLAY 'Senha valida.'
               DISPLAY ' '
            END-IF
            .

       0202-VALIDA-SENHA.
            MOVE 'N' TO WK-SENHA-VALIDA
            MOVE 'N' TO WK-TEM-NUMERO
            MOVE 'N' TO WK-TEM-MAIUSCULA
            MOVE 'N' TO WK-TEM-MINUSCULA
            MOVE 'N' TO WK-TEM-ESPECIAL
            MOVE 0   TO WK-CARACTERES


            MOVE FUNCTION LENGTH(WK-SENHA) TO WK-LENGTH-SENHA

            PERFORM VARYING WK-I-S FROM 1 BY 1 UNTIL WK-I-S >
            LENGTH OF WK-SENHA
               IF WK-SENHA(WK-I-S:1) NUMERIC
                   MOVE 'Y' TO WK-TEM-NUMERO
               ELSE IF WK-SENHA(WK-I-S:1) >= "A" AND WK-SENHA(WK-I-S:1)
                   <= "Z"
                   MOVE 'Y' TO WK-TEM-MAIUSCULA
               ELSE IF WK-SENHA(WK-I-S:1) >= "a" AND WK-SENHA(WK-I-S:1)
                   <= "z"
                 MOVE 'Y' TO WK-TEM-MINUSCULA
               ELSE IF WK-SENHA(WK-I-S:1) NOT NUMERIC AND WK-SENHA
                   (WK-I-S:1) NOT ALPHABETIC
                   MOVE 'Y' TO WK-TEM-ESPECIAL
               END-IF
            END-PERFORM

             COMPUTE WK-CARACTERES = FUNCTION LENGTH(WK-SENHA)

            IF WK-CARACTERES < 8 OR WK-TEM-NUMERO = 'N'
            OR WK-TEM-MAIUSCULA = 'N' OR WK-TEM-MINUSCULA = 'N'
            OR WK-TEM-ESPECIAL = 'N'
               DISPLAY 'Senha deve ter pelo menos oito caracteres,'
               ' um numero, uma letra maiuscula, uma letra minuscula '
               'e um caractere especial.'
               MOVE 'N' TO WK-SENHA-VALIDA
            ELSE
               MOVE 'Y' TO WK-SENHA-VALIDA
            END-IF.
      *                                                               *
       0300-PREENCHETEL SECTION.
       0301-PREENCHETEL.
                INITIALIZE WK-OPCAO-T
                DISPLAY 'Escolha o tipo de telefone:'
                DISPLAY '1 - Fixo'
                DISPLAY '2 - Celular'
                ACCEPT WK-OPCAO-T.
                EVALUATE WK-OPCAO-T
                    WHEN 1
                        PERFORM 0401-PREENCHE-FIXO
                    WHEN 2
                        PERFORM 0501-PREENCHE-CELULAR
                    WHEN OTHER
                        DISPLAY 'Opção inválida. Tente novamente.'
                        PERFORM 0301-PREENCHETEL
                END-EVALUATE
            .
       0400-PREENCHE-FIXO SECTION.
       0401-PREENCHE-FIXO.
                INITIALIZE WK-TELEFONE
                DISPLAY 'Informe o numero de telefone fixo '
                '(apenas numeros):'
                ACCEPT WK-TELEFONE

                COMPUTE WK-TAMANHO-TEL = FUNCTION LENGTH
                (FUNCTION TRIM(WK-TELEFONE))
                IF WK-TAMANHO-TEL < 10 OR WK-TAMANHO-TEL > 11
                    DISPLAY 'Numero de telefone invalido!'
                    'Insira um numero com 10 digitos '
                    PERFORM 0401-PREENCHE-FIXO
                ELSE
                    DISPLAY 'Telefone valido'
                    DISPLAY ' '
                    MOVE WK-TELEFONE TO WK-NUMERO
                END-IF

             .

       0500-PREENCHE-CELULAR SECTION.
       0501-PREENCHE-CELULAR.
                INITIALIZE WK-CEL
                DISPLAY 'Informe o numero de celular (apenas numeros):'
                ACCEPT WK-CEL

                COMPUTE WK-TAMANHO-CEL = FUNCTION LENGTH
                (FUNCTION TRIM(WK-CEL))
                IF WK-TAMANHO-CEL < 11 OR WK-TAMANHO-CEL > 11
                    DISPLAY 'Numero de celular invalido!'
                    'Insira um numero com 11 digitos '
                    PERFORM 0501-PREENCHE-CELULAR
                ELSE
                    DISPLAY 'Telefone valido'
                    DISPLAY ' '
                    MOVE WK-CEL TO WK-NUMERO
                END-IF
            .
       0600-PREENCHEEMAIL SECTION.
       0601-PREENCHEEMAIL.
            DISPLAY 'Digite seu endereco de e-mail: '
            ACCEPT WK-EMAIL.
            PERFORM 0701-VALIDAR-EMAIL.

       0700-VALIDAR-EMAIL                         SECTION.
       0701-VALIDAR-EMAIL.
            PERFORM VARYING WK-ARRPOS FROM 1 BY 1 UNTIL WK-ARRPOS >
            FUNCTION LENGTH(WK-EMAIL)
               IF WK-EMAIL(WK-ARRPOS:1) = "@"
                   MOVE 1 TO WK-ARRTEM
                   COMPUTE WK-QTD-ANTES-ARROBA = WK-ARRPOS
                   COMPUTE WK-POSICAO-ARROBA = WK-ARRPOS
                   EXIT PERFORM
               END-IF
            END-PERFORM.

            IF WK-ARRTEM = 0
                DISPLAY 'E-mail invalido, nao tem @'
                PERFORM 0601-PREENCHEEMAIL
                EXIT
            END-IF
            IF WK-QTD-ANTES-ARROBA < WK-MIN-CARACTERES
               DISPLAY 'O e-mail deve ter pelo menos 01 caracteres '
               'antes do "@".'
               PERFORM 0601-PREENCHEEMAIL
               EXIT
            END-IF
            .

       0800-DOMINIO                             SECTION.
       0801-DOMINIO.
            COMPUTE WK-QTD-FIM-EMAIL = FUNCTION LENGTH(WK-EMAIL) -
            WK-POSICAO-ARROBA
            IF WK-ARRTEM = 0 THEN
               DISPLAY 'E-mail inválido, nao tem "@"'
               PERFORM 0601-PREENCHEEMAIL
               EXIT
            END-IF

                IF WK-EMAIL(WK-POSICAO-ARROBA + 1: WK-QTD-FIM-EMAIL) =
            WK-DOMINIO1 OR
            WK-EMAIL(WK-POSICAO-ARROBA + 1: WK-QTD-FIM-EMAIL) =
            WK-DOMINIO2
               DISPLAY 'Dominio valido.'
            ELSE
               DISPLAY 'Dominio invalido. O dominio deve ser '
               'capgemini.com ou bradesco.com apos o @'
               PERFORM 0601-PREENCHEEMAIL
            END-IF
            DISPLAY 'Email valido.'
            .


       0900-CADASTRAR SECTION.
       0901-CADASTRAR.
            SET EOF-OK          TO FALSE
            SET FS-OK           TO TRUE
            ADD 1               TO WK-ID
            MOVE WK-ID          TO WK-ID-USUARIO
            MOVE WK-COMPLETO    TO WK-NOME-USUARIO
            MOVE WK-SENHA       TO WK-SENHA-USUARIO
            MOVE WK-NUMERO      TO WK-NUMERO-USUARIO
            MOVE WK-EMAIL       TO WK-EMAIL-USUARIO

            OPEN I-O CONTATOS

            IF WK-FS EQUAL 35 THEN
              OPEN OUTPUT CONTATOS
            END-IF

            IF FS-OK THEN
               MOVE WK-ID-USUARIO       TO ID-USUARIO
               MOVE WK-NOME-USUARIO     TO NOME-USUARIO
               MOVE WK-SENHA-USUARIO    TO SENHA-USUARIO
               MOVE WK-NUMERO-USUARIO   TO NUMERO-USUARIO
               MOVE WK-EMAIL-USUARIO    TO EMAIL-USUARIO


               WRITE REG-USUARIOS
                   INVALID KEY
                       DISPLAY 'Usuario ja cadastrado!'
                   NOT INVALID KEY
                       DISPLAY 'Usuario cadastrado com sucesso!'
                   END-WRITE
               CLOSE CONTATOS
            END-IF
            .
      *                                                               *
       1000-CONSULTAR                           SECTION.
       1001-CONSULTAR.
               DISPLAY 'TECLE <C> PARA CONTINUAR OU <F> PARA FINALIZAR'
               ACCEPT WK-OPCAO.
               EVALUATE WK-OPCAO
                   WHEN "C"
                   WHEN "c"
                     DISPLAY '----------------------------------------'
                     PERFORM 0001-PRINCIPAL THRU 0901-CADASTRAR
                  WHEN 'f'
                  WHEN 'F'
                     DISPLAY 'OBRIGADO'
                     SET EXIT-OK TO TRUE
                  WHEN OTHER
                     DISPLAY 'DADO INVALIDO, DIGITE A LETRA CORRETA'
                     PERFORM 1001-CONSULTAR
              END-EVALUATE
              .
      *                                                               *
       1100-FINALIZAR SECTION.
       1101-FINALIZAR.

            STOP RUN.
       END PROGRAM CADCONTT.
