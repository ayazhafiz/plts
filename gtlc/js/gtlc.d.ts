/** Infers marked type variables */
export declare const infer:
  (program: string, width: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Compiles to compiler-internal IR */
export declare const irCompile:
  (program: string, optimize: boolean, width: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Compiles to TypeScript */
export declare const tsCompile:
  (program: string, optimize: boolean, width: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Compiles to C */
export declare const cCompile:
  (program: string, optimize: boolean, width: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Evaluate a GTLC program */
export declare const doEval:
  (program: string, width: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Documentation for builtin primitives */
export declare const docs:
  Array<{ readonly name: string, readonly ty: string, readonly doc: string }>
