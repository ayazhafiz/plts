const ho21Syntax = {
  defaultToken: 'invalid',

  constant_types: ['!', '*', '\u22A4', '\u22A5'],
  queries: ['??'],
  judgements: ['<:', ':>', '~=', '#', '\u227A', '\u227B', '\u2245'],
  operators: ['&', '|', '->', '\u2227', '\u2228', '\u2192'],
  symbols:
      /[=><!~?:&|+\-*\/\^%#\u22A4\u22A5\u2227\u2228\u2192\u227A\u227B\u2245]+/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, 'error'],
      [/(Parse error.*)/, 'error'],
      [/[A-Z][a-zA-Z0-9_'\w$]*/, 'type.identifier'],
      {include: '@whitespace'},
      [/[()]/, '@brackets'],
      [
        /@symbols/, {
          cases: {
            '@constant_types': 'keyword',
            '@queries': 'annotation',
            '@judgements': 'keyword',
            '@operators': 'operator',
          }
        }
      ],
    ],
    whitespace: [
      [/(\uFF5C.*$)/, 'annotation'],
      [/[ \t\r\n]+/, 'white'],
      [/(--.*$)/, 'comment'],
      [/(\\.*$)/, 'annotation'],
    ],
  },
};

const ho21Theme = {
  base: 'vs',
  inherit: true,
  rules: [
    {token: 'error', foreground: 'ff0000'},
  ],
};

const languageContent = [
  {aliases: ['!', '⊥'], kind: '⊥', text: 'The bottom type', binary: false},
  {aliases: ['*', '⊤'], kind: '⊤', text: 'The top type', binary: false},
  {
    aliases: ['&', '∧'],
    kind: 'Operator',
    text: 'An intersection type',
    binary: true
  },
  {aliases: ['|', '∨'], kind: 'Operator', text: 'A union type', binary: true},
  {
    aliases: ['->', '→'],
    kind: 'Operator',
    text: 'An arrow (function) type',
    binary: true
  },
  {
    aliases: ['??'],
    kind: 'Query',
    text:
        'A judgement query for the relationship between A and B, which will be answered in the output editor.',
    binary: true,
  },
  {
    aliases: ['<:', '≺'],
    kind: 'Judgement',
    text:
        '<code>S <: T</code> means <code>S</code> is a subtype of <code>T</code>',
    outputOnly: true,
  },
  {
    aliases: [':>', '≻'],
    kind: 'Judgement',
    text:
        '<code>T :> S</code> means <code>T</code> is a supertype of <code>S</code>',
    outputOnly: true,
  },
  {
    aliases: ['~=', '≅'],
    kind: 'Judgement',
    text:
        '<code>T ~= S</code> means <code>T</code> is isomorphic to <code>S</code>',
    outputOnly: true,
  },
  {
    aliases: ['#'],
    kind: 'Judgement',
    text:
        '<code>T # U</code> means <code>T</code> and <code>U</code> are incomparable',
    outputOnly: true,
  },
];

function getLanguageGrammarInfo() {
  const inputItems = languageContent.filter(lc => !lc.outputOnly);
  const itemDocs = inputItems.map(({aliases, text, binary}) => {
    const examples = aliases
                         .map(alias => {
                           const example = binary ? `A${alias}B` : alias;
                           return `<code>${example}</code>`;
                         })
                         .join('/');
    return `<li>${examples}: ${text}</li>`;
  });
  itemDocs.push(
      `<li>Other types: Identifiers beginning with an uppercase letter are treated as primitive types.</li>`);
  const listDocs = `<ul>${itemDocs.join('\n')}</ul>`;
  return `${
      listDocs}\n<a class="ml-3" href="https://github.com/ayazhafiz/plts/blob/base/huang_oliveira_21/parser.mly">Full Parser Specification</a>`;
}

function getHoverContent(word) {
  for (const {aliases, text} of languageContent) {
    for (const alias of aliases) {
      if (word === alias) {
        const content = [
          // {value: `**${kind}**`},
          {value: text},
        ];
        if (aliases.length > 1) {
          content.push({value: `*Aliases*: ${aliases.join(', ')}`});
        }
        return content;
      }
    }
  }
  if (word[0] === word[0].toUpperCase() && word[0] !== word[0].toLowerCase()) {
    return [{value: '**Primitive**'}, {value: `Type primitive \`${word}\``}];
  }
  return null;
}

function ho21Hover(model, position) {
  const {lineNumber: line, column} = position;
  const maxColumn = model.getLineMaxColumn(line);
  const value = model.getValueInRange(
      new monaco.Range(line, model.getLineMinColumn(line), line, maxColumn));
  const tokens = monaco.editor.tokenize(model.getValue(), 'ho21')[line - 1];

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

function ho21FormatQueries(model, _options) {
  const text = formatQueries(model.getValue(), prettifySymbols);
  return [{
    text,
    range: model.getFullModelRange(),
  }];
}

function getHo21AutoFormatCharacters() {
  const all = [];
  all.push(...'abcdefghijklmnopqrstuvwxyz'.split(''));
  all.push(...'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split(''));
  all.push(...'0123456789'.split(''));
  all.push(...'() \n\t_\''.split(''));
  all.push(...ho21Syntax.constant_types.join('').split(''));
  all.push(...ho21Syntax.queries.join('').split(''));
  all.push(...ho21Syntax.judgements.join('').split(''));
  all.push(...ho21Syntax.operators.join('').split(''));
  return all;
}
function ho21OnTypeFormatQueries(model, _position, _ch, _options) {
  const text = formatQueries(model.getValue(), prettifySymbols);
  return [{
    text,
    range: model.getFullModelRange(),
  }];
}
