open Ast
open Sast
open Semant

let _ = let lexbuf = Lexing.from_channel stdin in
let (funcs,strucs) = Parser.program Scanner.token lexbuf in let expr = check(funcs, strucs) in 
let result = string_of_sprogram expr in print_endline result