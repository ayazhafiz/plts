const systemFSyntax = {
  defaultToken: 'invalid',
  keywords: ['fix', 'if0', 'then', 'else', 'Λ', 'let', 'in'],

  typeKeywords: ['int', '∀'],

  symbols: /[=><!~?:&|+\-*\/\^%.,]+/,
  tokenizer: {
    root: [
      [
        /[a-z_A-Z][\w0-9$]*|[∀Λ]/,
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

      [/[%$][a-z_A-Z0-9]*/, 'annotation'],
      [/@symbols/, 'operator'],
      // numbers
      [/\d+/, 'number'],
    ],
    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/(^#.*$)/, 'comment'],
    ],
  },
};
