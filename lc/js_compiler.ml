(** Compiler from lambda calculus to JavaScript *)

open Ast

(** Compile a lambda calculus expression to JavaScript *)
let rec compile_expr symbols env (_, _, expr) =
  match expr with
  | Var x ->
      (* Find the variable in the environment *)
      let rec find_var env_list x idx =
        match env_list with
        | [] ->
            Symbol.string_of symbols x (* Use the symbol name if not in env *)
        | y :: rest ->
            if y = x then "env[" ^ string_of_int idx ^ "]"
            else find_var rest x (idx + 1)
      in
      find_var env x 0
  | Abs (x, body) ->
      (* Compile the body with the parameter added to the environment *)
      let param_name = Symbol.string_of symbols x in
      let body_js = compile_expr symbols (x :: env) body in
      "(function(" ^ param_name ^ ") { return " ^ body_js ^ "; })"
  | App (f, arg) ->
      (* Compile function and argument, then apply *)
      let f_js = compile_expr symbols env f in
      let arg_js = compile_expr symbols env arg in
      "(" ^ f_js ^ ")(" ^ arg_js ^ ")"

(** Compile a program to JavaScript *)
let compile_program symbols (program : program) : string =
  let js_expr = compile_expr symbols [] program in
  let prelude = "// Generated JavaScript from Lambda Calculus\n\n" in
  let main = "function main() {\n  return " ^ js_expr ^ ";\n}\n\n" in
  let exports =
    "if (typeof module !== 'undefined' && module.exports) {\n"
    ^ "  module.exports = { main };\n" ^ "}\n"
  in
  prelude ^ main ^ exports

(** Compile a program to JavaScript with runtime support *)
let compile_with_runtime symbols (program : program) : string =
  let js_expr = compile_expr symbols [] program in
  let prelude = "// Generated JavaScript from Lambda Calculus\n\n" in

  (* Runtime support for Church numerals and other common patterns *)
  let runtime =
    "// Runtime support\n" ^ "const church = {\n"
    ^ "  // Convert Church numeral to JavaScript number\n"
    ^ "  toNumber: n => n(x => x + 1)(0),\n"
    ^ "  // Convert JavaScript number to Church numeral\n"
    ^ "  fromNumber: n => {\n" ^ "    if (n === 0) return f => x => x;\n"
    ^ "    return f => x => {\n" ^ "      let result = x;\n"
    ^ "      for (let i = 0; i < n; i++) {\n" ^ "        result = f(result);\n"
    ^ "      }\n" ^ "      return result;\n" ^ "    };\n" ^ "  },\n"
    ^ "  // Church booleans\n" ^ "  true: t => f => t,\n"
    ^ "  false: t => f => f,\n"
    ^ "  // Convert Church boolean to JavaScript boolean\n"
    ^ "  toBool: b => b(true)(false),\n" ^ "  // Church pair\n"
    ^ "  pair: x => y => f => f(x)(y),\n" ^ "  first: p => p(x => y => x),\n"
    ^ "  second: p => p(x => y => y)\n" ^ "};\n\n"
  in

  let main =
    "function main() {\n  const result = " ^ js_expr ^ ";\n"
    ^ "  // Try to convert result if it's a Church numeral\n" ^ "  try {\n"
    ^ "    const num = church.toNumber(result);\n"
    ^ "    console.log('Result as number:', num);\n" ^ "    return num;\n"
    ^ "  } catch (e) {\n" ^ "    // Not a Church numeral\n"
    ^ "    return result;\n" ^ "  }\n" ^ "}\n\n"
  in

  let exports =
    "if (typeof module !== 'undefined' && module.exports) {\n"
    ^ "  module.exports = { main, church };\n" ^ "}\n"
  in

  prelude ^ runtime ^ main ^ exports
