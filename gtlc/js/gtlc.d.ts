import {Result} from 'www/src/common/types';

export function irCompile(program: string, optimize: boolean): Result;
export function tsCompile(program: string, optimize: boolean): Result;
export function cCompile(program: string, optimize: boolean): Result;

export function doEval(program: string): Result;

export interface BuiltinDoc {
  name: string;
  ty: string;
  doc: string;
}

export function docs(): BuiltinDoc[];
