export declare const talCompile:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }

export declare const talEval:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }

export declare const x86Compile:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }

export declare const x86Emulate:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }
