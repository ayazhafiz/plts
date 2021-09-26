export function promisify<A extends unknown[], R>(f: (...args: A) => R):
    (...args: A) => Promise<R> {
    return async (...args: A) => f(...args);
}

export function chain<A extends unknown[], R1, R2>(
    f1: (...args: A) => R1, f2: (arg: R1) => R2): (...args: A) => R2 {
    return (...args: A) => f2(f1(...args));
}
