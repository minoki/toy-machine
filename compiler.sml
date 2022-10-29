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
  | compileExp (env, top, isTail, BOOL b) = [if b then OP_PUSH_TRUE else OP_PUSH_FALSE]
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
  | compileExp (env, top, isTail, TIMES (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_TIMES]
  | compileExp (env, top, isTail, EQ (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_EQ]
  | compileExp (env, top, isTail, LT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_LT]
  | compileExp (env, top, isTail, LE (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_LE]
  | compileExp (env, top, isTail, PRINT a) = compileExp (env, top, false, a) @ [OP_PRINT]
  | compileExp (env, top, isTail, RAISE a) = compileExp (env, top, false, a) @ [OP_RAISE]
  | compileExp (env, top, isTail, HANDLE (a, name, b)) = let val handler = compileExp (StringMap.insert (env, name, LOCAL top), top + 1, isTail, b) @ [OP_POP_EXCEPT_TOP 1]
                                                             val body = compileExp (env, top, false, a) @ [OP_POP_HANDLER (List.length handler)]
                                                         in OP_PUSH_HANDLER (List.length body) :: body @ handler
                                                         end
  | compileExp (env, top, isTail, NEW_PROMPT) = [OP_NEW_PROMPT]
  | compileExp (env, top, isTail, PUSH_PROMPT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_PUSH_PROMPT]
  | compileExp (env, top, isTail, WITH_SUBCONT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_WITH_SUBCONT]
  | compileExp (env, top, isTail, PUSH_SUBCONT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_PUSH_SUBCONT]
  | compileExp (env, top, isTail, ABORT (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_ABORT]
  | compileExp (env, top, isTail, SEQUENCE xs) = let fun go [] = [OP_PUSH_NIL]
                                                       | go [x] = compileExp (env, top, isTail, x)
                                                       | go (x :: xs) = compileExp (env, top, false, x) @ OP_POP :: go xs
                                                 in go xs
                                                 end
  | compileExp (env, top, isTail, CONS (a, b)) = compileExp (env, top, false, a) @ compileExp (env, top + 1, false, b) @ [OP_CONS]
  | compileExp (env, top, isTail, CAR a) = compileExp (env, top, false, a) @ [OP_CAR]
  | compileExp (env, top, isTail, CDR a) = compileExp (env, top, false, a) @ [OP_CDR]
  | compileExp (env, top, isTail, IS_PAIR a) = compileExp (env, top, false, a) @ [OP_IS_PAIR]
and compileLambda (env, f as LAMBDA (name, body))
    = let val (innerEnv, prepare, n) = StringSet.foldr (fn (name, (innerEnv, prepare, i)) =>
                                                           (StringMap.insert (innerEnv, name, FREE i), prepare @ getVar (env, name), i + 1)
                                                       ) (StringMap.empty, [], 0) (Syntax.freeVars (Syntax.StringSet.empty, f))
      in (innerEnv, prepare @ [OP_CLOSURE { body = compileExp (StringMap.insert (innerEnv, name, LOCAL 1), 2, true, body) @ [OP_RETURN], nFreeVars = n }])
      end
  | compileLambda (env, exp) = raise Fail "invalid expression in letrec"
fun compileStmt (env, top, DEFINE_LAMBDA (f, param, body))
    = let val uninitEnv = StringMap.insert (env, f, UNINITIALIZED)
          val (innerEnv, insns) = compileLambda (uninitEnv, LAMBDA (param, body))
          val insns = case StringMap.find (innerEnv, f) of
                          SOME (FREE i) => insns @ [OP_FIX_CLOSURE { target = top, freeIndex = i, real = top }]
                        | _ => insns
      in (StringMap.insert (env, f, LOCAL top), top + 1, insns)
      end
  | compileStmt (env, top, DEFINE (v, x)) = (StringMap.insert (env, v, LOCAL top), top + 1, compileExp (env, top, false, x))
  | compileStmt (env, top, EXP x) = (env, top, compileExp (env, top, false, x) @ [OP_POP])
fun compileProgram stmts = #3 (List.foldl (fn (stmt, (env, top, insns)) =>
                                              let val (env, top, insns') = compileStmt (env, top, stmt)
                                              in (env, top, insns @ insns')
                                              end) (StringMap.empty, 0, []) stmts)
end
end;
