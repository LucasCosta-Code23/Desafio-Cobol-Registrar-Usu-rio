# Desafio-Cobol-Registrar-Usuário

Este programa tem como objetivo registrar um usuário. Ele deve receber e
validar os dados do usuário, e registrar em arquivo indexado. Este é um
programa online, que recebe as seguintes informações do usuário:

- Email
- Name
- Password
- Phone

### Fluxo:
O usuário acessa a interface, preenche os dados citados.
O programa realiza as seguintes validações:

1. Email é válido (ter pelo menos 10 caracteres, ter pelo menos um
caractere antes do @ e pertencer aos domínios “[capgemini.com](http://capgemini.com/)” ou
"[bradesco.com](http://bradesco.com/)”.
2. Nome com pelo menos duas palavras.
3. Senha com pelo menos 8 caracteres, 1 número, 1 letra maiúscula, 1 letra minúscula e um caractere especial.
4. Telefone com no mínimo 11 caracteres e máximo 12 caracteres. Ex:
(999999999999).

Caso algum dos campos esteja inválido, o programa deve apresentar um erro e uma sugestão de correção ao usuário. Em caso de sucesso, o programa chama o módulo de geração de ID e registra o usuário, indexado pelo id gerado. Ex:

- Name → Carlos Santos
- ID (password) → 0001Carlos@34
- Email → [carlos@capgemini.com](mailto:carlos@capgemini.com)
- Phone → 756666666666
