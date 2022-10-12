structure Compiler = struct
local open Syntax
      open Instruction
      structure StringMap = RedBlackMapFn (open String; type ord_key = string)
      datatype location = LOCAL of int
                        | FREE of int
      type env = location StringMap.map
in
fun getVar (env, name) = (case StringMap.find (env, name) of
                              SOME (LOCAL i) => [OP_PUSH_LOCAL i]
                            | SOME (FREE i) => [OP_PUSH_FREE i]
                            | NONE => raise Fail ("unbound variable: " ^ name)
                         )
(* compileExp : env * Syntax.exp -> Instruction.instruction list *)
fun compileExp (env, NIL) = [OP_PUSH_NIL]
  | compileExp (env, INT n) = [OP_PUSH_INT n]
  | compileExp (env, VAR name) = getVar (env, name)
  | compileExp (env, f as LAMBDA (name, body)) = let val (innerEnv, prepare, n) = StringSet.foldr (fn (name, (innerEnv, prepare, i)) =>
                                                                                                      (StringMap.insert (innerEnv, name, FREE i), prepare @ getVar (env, name), i + 1)
                                                                                                  ) (StringMap.empty, [], 0) (Syntax.freeVars (Syntax.StringSet.empty, f))
                                                 in prepare @ [OP_CLOSURE { body = compileExp (StringMap.insert (innerEnv, name, LOCAL 1), body) @ [OP_RETURN], nFreeVars = n }]
                                                 end
  | compileExp (env, IF (a, b, c)) = let val b' = compileExp (env, b)
                                         val c' = compileExp (env, c)
                                     in compileExp (env, a) @ [OP_JUMP_IF_FALSE (length b' + 1)] @ b' @ [OP_JUMP (length c')] @ c'
                                     end
  | compileExp (env, APP (a, b)) = compileExp (env, a) @ compileExp (env, b) @ [OP_CALL]
  | compileExp (env, PLUS (a, b)) = compileExp (env, a) @ compileExp (env, b) @ [OP_PLUS]
  | compileExp (env, MINUS (a, b)) = compileExp (env, a) @ compileExp (env, b) @ [OP_MINUS]
  | compileExp (env, LT (a, b)) = compileExp (env, a) @ compileExp (env, b) @ [OP_LT]
  | compileExp (env, LE (a, b)) = compileExp (env, a) @ compileExp (env, b) @ [OP_LE]
  | compileExp (env, PRINT a) = compileExp (env, a) @ [OP_PRINT]
fun compileProgram [] = []
  | compileProgram (x :: xs) = compileExp (StringMap.empty, x) @ OP_POP :: compileProgram xs
end
end;
