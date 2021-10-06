import {Backend, Result} from './types'

// clang-format off
type Uncurried<T extends (arg: any) => any>
  = ReturnType<T> extends Result ? (...args: Parameters<T>) => ReturnType<T>
  : ReturnType<T> extends infer T_
    ? T_ extends (...args: any[]) => any
      ? Uncurried<T_> extends infer T_
        ? T_ extends (...args: any[]) => any
          ? (...args: [...Parameters<T>, ...Parameters<T_>]) => ReturnType<T_>
          : never
        : never
      : never
    : never;
// clang-format on

export function uncurry<F extends(arg: any) => any>(f: F): Uncurried<F> {
  return ((...args: Parameters<Uncurried<F>>) =>
              args.reduce((f: Function, x) => f(x), f)) as Uncurried<F>;
}

export function promisify<A extends unknown[], R>(f: (...args: A) => R):
    (...args: A) => Promise<R> {
  return async (...args: A) => f(...args);
}

export function chain<A extends unknown[], R1, R2>(
    f1: (...args: A) => R1, f2: (arg: R1) => R2): (...args: A) => R2 {
  return (...args: A) => f2(f1(...args));
}

type Duo<T extends unknown[]> =
    T extends [infer E, ...infer R] ? [[string, E], ...Duo<R>] : [];

export function shapeBackend<O extends Array<boolean|number>>(
    doit: (program: string, ...options: O) => Result,
    options: Duo<O>): Pick<Backend, 'do'|'options'> {
  return {do: promisify(doit), options};
}
export function shapeBackendP<O extends Array<boolean|number>>(
    doit: (program: string, ...options: O) => Promise<Result>,
    options: Duo<O>): Pick<Backend, 'do'|'options'> {
  return {do: doit, options};
}
