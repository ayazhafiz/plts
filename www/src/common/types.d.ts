import type * as monaco from 'monaco-editor';
import type * as React from 'react';

export type Result = {
    readonly result: string|null,
    readonly error: string|null,
};

export interface LanguageRegistration {
    syntax: monaco.languages.IMonarchLanguage;
    hover?:
        (m: typeof monaco) => monaco.languages.HoverProvider['provideHover'];
    format?: monaco.languages
        .DocumentFormattingEditProvider['provideDocumentFormattingEdits'];
    autoFormat?: {
        format: monaco.languages
                  .OnTypeFormattingEditProvider['provideOnTypeFormattingEdits'],
        triggerCharacters: monaco.languages.OnTypeFormattingEditProvider['autoFormatTriggerCharacters'],
    }
}

export type BackendExecutor = (input: string, ...options: boolean[]) =>
    Promise<Result>;

export interface Backend {
    title: string;
    do: BackendExecutor;
    options: [string, boolean][];
    info?: [string, React.ReactNode][];
    editorLanguage: string;
}

/**
 * A kind of backend associated with a name. Provides either one backend
 * (e.g. typechecking) or two (e.g. codegen and evaluation).
 */
export type BackendKind = [Backend]|[Backend, Backend];
