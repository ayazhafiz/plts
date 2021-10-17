/** Compile a tiger program to the compiler-internal IR */
export declare const compileIR:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }

/** Compile a tiger program to x86 assembly */
export declare const compileX86:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }
