const systemFSyntax = {
  keywords: ['fix', 'if0', 'then', 'else', 'Λ', 'let', 'in'],

  typeKeywords: ['int', '∀'],

  operators: ['+', '-', '*'],
  symbols: /[=><!~?:&|+\-*\/\^%]+/,
  tokenizer: {
    root: [
      [
        /[a-z_][\w$]*/,
        {
          cases: {
            '@typeKeywords': 'keyword',
            '@keywords': 'keyword',
            '@default': 'identifier',
          },
        },
      ],
      {include: '@whitespace'},
      [/[{}()\[\]]/, '@brackets'],

      [/[%$][a-zA-Z_]*/, 'annotation'],
      [/[<>](?!@symbols)/, '@brackets'],
      // numbers
      [/\d+/, 'number'],
    ],
    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/(^#.*$)/, 'comment'],
    ],
  },
};
