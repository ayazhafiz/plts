// Adapted from
// https://github.com/compiler-explorer/compiler-explorer/blob/main/static/modes/asm-mode.js

const talSyntax = {
  // Set defaultToken to invalid to see what you do not tokenize yet
  defaultToken: 'invalid',

  registers: /%?\b(r[0-9]+)\b/,

  keywords: ['pack', 'code', 'halt', 'as'],

  tokenizer: {
    root: [
      // Error document
      [/^<.*>$/, {token: 'annotation'}],
      // Label definition
      [/^[.a-zA-Z0-9_$?@].*:/, {token: 'type.identifier'}],
      // Constant definition
      [/^[.a-zA-Z0-9_$?@][^=]*=/, {token: 'type.identifier'}],
      // opcode
      [/[.a-zA-Z_][.a-zA-Z_0-9]*/, {token: 'keyword', next: '@rest'}],
      [/[(){}\[\]\->]/, {token: 'operator', next: '@rest'}],
      [/\d+/, 'number'],

      // whitespace
      {include: '@whitespace'},
    ],

    rest: [
      // pop at the beginning of the next line and rematch
      [/^.*$/, {token: '@rematch', next: '@pop'}],

      [/@registers/, 'variable.predefined'],

      [/\d+/, 'number'],

      [/[,\(\):,\[\]\-\>∃∀]/, 'operator'],
      // Assume anything else is a label reference
      [
        /%?[.?_$a-zA-Z@][.?_$a-zA-Z0-9@]*/,
        {cases: {'@keywords': 'keyword', '@default': 'type.identifier'}}
      ],

      // whitespace
      {include: '@whitespace'},
    ],

    comment: [
      [/[^/*]+/, 'comment'],
      [/\/\*/, 'comment', '@push'],  // nested comment
      ['\\*/', 'comment', '@pop'],
      [/[/*]/, 'comment'],
    ],

    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/\/\*/, 'comment', '@comment'],
      [/\/\/.*$/, 'comment'],
      [/[#;\\@].*$/, 'comment'],
    ],
  },
};

