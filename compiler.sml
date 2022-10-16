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
(* compileExp : env * int * bool * Syntax.exp -> Instruction.instruction list *)
fun compileExp (env, top, isTail, NIL) = [OP_PUSH_NIL]
  | compileExp (env, top, isTail, INT n) = [OP_PUSH_INT n]
  | compileExp (env, top, isTail, VAR name) = getVar (env, name)
  | compileExp (env, top, isTail, f as LAMBDA (name, body))
    = let val (innerEnv, prepare, n) = StringSet.foldr (fn (name, (innerEnv, prepare, i)) =>
                                                           (StringMap.insert (innerEnv, name, FREE i), prepare @ getVar (env, name), i + 1)
                                                       ) (StringMap.empty, [], 0) (Syntax.freeVars (Syntax.StringSet.empty, f))
      in prepare @ [OP_CLOSURE { body = compileExp (StringMap.insert (innerEnv, name, LOCAL 1), 2, true, body) @ [OP_RETURN], nFreeVars = n }]
      end
  | compileExp (env, top, isTail, LET (bindings, body))
    = let val (top, innerEnv, insns) = List.foldl (fn ((name, exp), (top, innerEnv, insns)) =>
                                                      (top + 1, StringMap.insert (innerEnv, name, LOCAL top), insns @ compileExp (env, top, false, exp))
                                                  ) (top, env, []) bindings
      in insns @ compileExp (innerEnv, top, isTail, body) @ [OP_POP_EXCEPT_TOP (List.length bindings)]
      end
  | compileExp (env, top, isTail, IF (a, b, c))
    = let val b' = compileExp (env, top, isTail, b)
          val c' = compileExp (env, top, isTail, c)
      in compileExp (env, top, false, a) @ [OP_JUMP_IF_FALSE (length b' + 1)] @ b' @ [OP_JUMP (length c')] @ c'
      end
  | compileExp (env, top, isTail, APP (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [if isTail then OP_TAILCALL else OP_CALL]
  | compileExp (env, top, isTail, PLUS (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_PLUS]
  | compileExp (env, top, isTail, MINUS (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_MINUS]
  | compileExp (env, top, isTail, LT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_LT]
  | compileExp (env, top, isTail, LE (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_LE]
  | compileExp (env, top, isTail, PRINT a) = compileExp (env, top, false, a) @ [OP_PRINT]
fun compileProgram [] = []
  | compileProgram (x :: xs) = compileExp (StringMap.empty, 0, false, x) @ OP_POP :: compileProgram xs
end
end;
