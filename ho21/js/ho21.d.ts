export declare const judge:
  (queries: string) =>
    (print_debug: boolean) =>
      (prettify_symbols: boolean) =>
        { readonly result: string|null, readonly error: string|null }

export declare const formatQueries:
  (queries: string) => (prettify_symbols: boolean) => string
