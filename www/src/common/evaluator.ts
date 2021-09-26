import {transpile} from 'typescript';
import type {Result} from './types';

export const C = {
    title: 'Execution (Clang 12.0.1, -O3)',
    async eval(input: Result): Promise<Result> {
        if (input.result === null) return input;
        const data =
            await fetch('https://godbolt.org/api/compiler/cclang1201/compile', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    Accept: 'application/json',
                },
                body: JSON.stringify({
                    source: input.result,
                    options: {
                        userArguments: '-O3',
                        compilerOptions: {
                            executorRequest: true,
                        },
                        filters: {
                            execute: true,
                        },
                        tools: [],
                    },
                    lang: 'c',
                    allowStoreCodeDebug: true,
                }),
            }).then((response) => response.json());
        if (data.code === -1) {
            return {
                result: null,
                error: `Unexpected compilation error:\n${
                    JSON.stringify(data.buildResult.stderr)}`,
            };
        }
        return {
            result: data.stdout[0].text,
            error: null,
        };
    }
}

export const TS = {
    title: 'JS Execution',
    async eval(input: Result): Promise<Result> {
        if (input.error) return input;
        const jsCode = transpile(input.result!);
        try {
            const result = await eval(jsCode);
            return {result: `${result}`, error: null};
        } catch (e: unknown) {
            return {result: null, error: (e as EvalError).message};
        }
    }
}
