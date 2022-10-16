structure Compiler = struct
local open Syntax
      open Instruction
      structure StringMap = RedBlackMapFn (open String; type ord_key = string)
      datatype location = LOCAL of int
                        | FREE of int
                        | UNINITIALIZED
      type env = location StringMap.map
in
fun getVar (env, name) = (case StringMap.find (env, name) of
                              SOME (LOCAL i) => [OP_PUSH_LOCAL i]
                            | SOME (FREE i) => [OP_PUSH_FREE i]
                            | SOME UNINITIALIZED => [OP_PUSH_NIL] (* dummy *)
                            | NONE => raise Fail ("unbound variable: " ^ name)
                         )
(* compileExp : env * int * bool * Syntax.exp -> Instruction.instruction list *)
fun compileExp (env, top, isTail, NIL) = [OP_PUSH_NIL]
  | compileExp (env, top, isTail, INT n) = [OP_PUSH_INT n]
  | compileExp (env, top, isTail, VAR name) = getVar (env, name)
  | compileExp (env, top, isTail, f as LAMBDA (name, body)) = #2 (compileLambda (env, f))
  | compileExp (env, top, isTail, LET (bindings, body))
    = let val (top, innerEnv, insns) = List.foldl (fn ((name, exp), (top, innerEnv, insns)) =>
                                                      (top + 1, StringMap.insert (innerEnv, name, LOCAL top), insns @ compileExp (env, top, false, exp))
                                                  ) (top, env, []) bindings
      in insns @ compileExp (innerEnv, top, isTail, body) @ [OP_POP_EXCEPT_TOP (List.length bindings)]
      end
  | compileExp (env, top, isTail, LETREC (bindings, body)) (* TODO: Check the values are functions *)
    = let val uninitEnv = List.foldl (fn ((name, exp), e) => StringMap.insert (e, name, UNINITIALIZED)) env bindings
          val bound = StringMap.foldli (fn (name, _, bound) => StringSet.add (bound, name)) StringSet.empty env
          val (top, innerEnv, _, insns, fixups) = List.foldl (fn ((name, exp), (top, innerEnv, bound, insns, fixups)) =>
                                                                 let val (innerEnv', insns') = compileLambda (innerEnv, exp)
                                                                 in ( top + 1
                                                                    , StringMap.insert (innerEnv, name, LOCAL top)
                                                                    , StringSet.add (bound, name)
                                                                    , insns @ compileExp (innerEnv, top, false, exp)
                                                                    , let val tofixup = freeVars (bound, exp)
                                                                      in StringSet.foldl (fn (freeName, acc) =>
                                                                                             case StringMap.find (innerEnv', freeName) of
                                                                                                 SOME (FREE i) => { target = top, freeIndex = i, real = freeName } :: acc
                                                                                               | _ => acc
                                                                                         ) fixups tofixup
                                                                      end
                                                                    )
                                                                 end
                                                             ) (top, uninitEnv, bound, [], []) bindings
          val fixups = List.foldl (fn ({ target, freeIndex, real }, fixups) =>
                                      case StringMap.find (innerEnv, real) of
                                          SOME (LOCAL i) => OP_FIX_CLOSURE { target = target, freeIndex = freeIndex, real = i } :: fixups
                                        | _ => raise Fail "compile: letrec"
                                  ) [] fixups
      in insns @ fixups @ compileExp (innerEnv, top, isTail, body) @ [OP_POP_EXCEPT_TOP (List.length bindings)]
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
and compileLambda (env, f as LAMBDA (name, body))
    = let val (innerEnv, prepare, n) = StringSet.foldr (fn (name, (innerEnv, prepare, i)) =>
                                                           (StringMap.insert (innerEnv, name, FREE i), prepare @ getVar (env, name), i + 1)
                                                       ) (StringMap.empty, [], 0) (Syntax.freeVars (Syntax.StringSet.empty, f))
      in (innerEnv, prepare @ [OP_CLOSURE { body = compileExp (StringMap.insert (innerEnv, name, LOCAL 1), 2, true, body) @ [OP_RETURN], nFreeVars = n }])
      end
  | compileLambda (env, exp) = raise Fail "invalid expression in letrec"
fun compileProgram [] = []
  | compileProgram (x :: xs) = compileExp (StringMap.empty, 0, false, x) @ OP_POP :: compileProgram xs
end
end;
