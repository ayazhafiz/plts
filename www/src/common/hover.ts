import type * as monaco from 'monaco-editor';

function doHover(
    m: typeof monaco,
    lang: string,
    contentProvider: (item: string) => (monaco.IMarkdownString[] | null),
    model: monaco.editor.IModel,
    position: monaco.Position,
    ): monaco.languages.ProviderResult<monaco.languages.Hover> {
    const {lineNumber: line, column} = position;
    const maxColumn = model.getLineMaxColumn(line);
    const value = model.getValueInRange(
        new m.Range(line, model.getLineMinColumn(line), line, maxColumn));
    const tokens = m.editor.tokenize(model.getValue(), lang)[line - 1];

    for (let i = 0; i < tokens.length; ++i) {
        const start = tokens[i].offset + 1;
        const end = tokens[i + 1] ? tokens[i + 1].offset + 1 : maxColumn;
        if (column >= start && column < end) {
            const word = value.substring(start - 1, end - 1);
            const range = new m.Range(line, start, line, end);
            const contents = contentProvider(word);
            return contents && {range, contents};
        }
    }
    return null;
}

export const createHoverProvider =
    (lang: string,
     contentProvider: (item: string) => (monaco.IMarkdownString[] | null)) =>
        (m: typeof monaco) =>
            (model: monaco.editor.IModel, position: monaco.Position) =>
                doHover(m, lang, contentProvider, model, position);
