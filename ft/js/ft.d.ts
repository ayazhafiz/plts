import {Result} from 'www/src/common/types';

export function ftCheck(program: string, doInfer: boolean): Result;
export function ftInfer(program: string): Result;
export function subtypeCheck(program: string): Result;
