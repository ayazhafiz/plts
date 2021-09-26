import {Result} from 'www/src/common/types';

export function talCompile(program: string): Result;
export function talEval(program: string): Result;

export function x86Compile(program: string): Result;
export function x86Emulate(program: string): Result;
