export declare const ftCheck:
  (program: string) =>
    (do_infer: boolean) =>
      { readonly result: string|null, readonly error: string|null }

export declare const ftInfer:
  (program: string) =>
    { readonly result: string|null, readonly error: string|null }

export declare const subtypeCheck:
  (query: string) =>
    { readonly result: string|null, readonly error: string|null }
