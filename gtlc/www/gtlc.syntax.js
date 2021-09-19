const builtin_docs = docs();
const builtin_fns = builtin_docs.map(d => d.name);

const gtlcSyntax = {
  defaultToken: 'invalid',

  builtin_types: ['nat', 'bool', '?'],
  keywords:
      ['let', 'in', 'if', 'then', 'else', '\u03BB', '\\'].concat(builtin_fns),
  symbols: /[<>\\?\->.:=\u03BB]+/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, 'error'],
      [/(Parse error.*)/, 'error'],
      [/#[tf]/, 'keyword'],
      [
        /[a-z][a-zA-Z0-9_'\w$]*/, {
          cases: {
            '@builtin_types': 'keyword.type',
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }
      ],
      [/\d+/, 'number'],
      {include: '@whitespace'},
      [/[()]/, '@brackets'],
      [
        /@symbols/, {
          cases: {
            '@builtin_types': 'keyword.type',
            '@keywords': 'keyword',
            '@default': 'operator',
          }
        }
      ],
    ],
    whitespace: [
      [/(\uFF5C.*$)/, 'annotation'],
      [/[ \t\r\n]+/, 'white'],
      [/(--.*$)/, 'comment'],
    ],
  },
};

const liftIrSyntax = {
  defaultToken: 'invalid',

  builtin_types: ['nat', 'bool', '?', 'Clos'],
  keywords: [
    'fn', 'decl', 'apply', 'pack', 'let', 'in', 'if', 'then', 'else', '\u03BB',
    '\\'
  ].concat(builtin_fns),
  symbols: /[,;<>\\?\->.:=\u03BB]+/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, 'error'],
      [/(Parse error.*)/, 'error'],
      [
        /[a-zA-Z_'][a-zA-Z_'\w$]*/, {
          cases: {
            '@builtin_types': 'keyword.type',
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }
      ],
      [/\d+/, 'number'],
      {include: '@whitespace'},
      [/[{()}]/, '@brackets'],
      [
        /@symbols/, {
          cases: {
            '@builtin_types': 'keyword.type',
            '@keywords': 'keyword',
            '@default': 'operator',
          }
        }
      ],
    ],
    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/(--.*$)/, 'comment'],
    ],
  },
}

const gtlcTheme = {
  base: 'vs',
  inherit: true,
  rules: [
    {token: 'error', foreground: 'ff0000'},
  ],
};

const grammarInfo = `
**Expressions**

- \`x\`: variables
- \`100\`: numbers
- \`#t|#f\`: the true and false literals
- \`(\\|λ)x: t. e\`: a lambda expression with \`x\` bound to type \`t\`
- \`let x: t = e1 in e2\`: binds \`x\` to \`e1\` and then evaluates \`e2\`
- \`f a\`: application to a lambda
- \`if e1 then e2 else e3\`: an if-then-else expression

All type annotations of form \`: t\` are optional. If not specified, the
type defaults to the unknown type \`?\`.

**Types**

- \`?\`: the unknown type, admitting any value
- \`nat\`: the type of natural numbers
- \`bool\`: the type of booleans
- \`t1 -> t2\`: function type

**Builtin Functions**

${
                        builtin_docs
                            .map(({name, ty, doc}) => {
                              return `- \`${name}: ${ty}\`\n\t- ${doc}`;
                            })
                            .join('\n')}

[Full Parser Specification](https://github.com/ayazhafiz/plts/blob/base/gtlc/parser.mly)
`.trim();

function getLanguageGrammarInfo() {
  return md.render(grammarInfo);
}

function getHoverContent(word) {
  if (word === '?') {
    return [{value: 'The unknown type'}]
  }
  for (const {name, ty, doc} of builtin_docs) {
    if (word === name) {
      return [
        {value: `\`\`\`gtlc\n${name}: ${ty}\n\`\`\``},
        {value: doc},
      ];
    }
  }
  return null;
}

function gtlcHover(model, position) {
  const {lineNumber: line, column} = position;
  const maxColumn = model.getLineMaxColumn(line);
  const value = model.getValueInRange(
      new monaco.Range(line, model.getLineMinColumn(line), line, maxColumn));
  const tokens = monaco.editor.tokenize(model.getValue(), 'gtlc')[line - 1];

  for (let i = 0; i < tokens.length; ++i) {
    const start = tokens[i].offset + 1;
    const end = tokens[i + 1] ? tokens[i + 1].offset + 1 : maxColumn;
    if (column >= start && column < end) {
      const word = value.substring(start - 1, end - 1);
      const range = new monaco.Range(line, start, line, end);
      const contents = getHoverContent(word);
      return contents && {range, contents};
    }
  }
  return null;
}
