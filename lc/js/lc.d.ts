/** Compiler target phases */
export declare const phases: string[]

/** Compiler target emits */
export declare const emits: string[]

/** Gets raw user program */
export declare const userProgram: (_: string) => string

/** Compiles a program under a given language to a given phase, and returns the give emit */
export declare const compile:
  (prog: string, phase: string, emit: string) =>
    { readonly result: string|null, readonly error: string|null }

/** Get hover information */
export declare const hover:
  (prog: string, line: number, column: number) =>
    { readonly range:
        { readonly start: { readonly line: number, readonly col: number },
          readonly fin: { readonly line: number, readonly col: number } },
      readonly info: string[] }
