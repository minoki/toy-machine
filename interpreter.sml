structure Interpreter = struct
datatype value = NIL
               | BOOL of bool
               | INT of int
type stack = value array
local open Instruction
      fun push (stack, stackTop, value) = (Array.update (stack, stackTop, value); stackTop + 1)
      fun pop (stack, stackTop) = let val stackTop = stackTop - 1
                                  in (stackTop, Array.sub (stack, stackTop))
                                  end
in
fun newStack () = Array.array (100, NIL)
(* run : Instruction.instruction list * stack * int -> unit *)
fun run ([], stack, stackTop) = ()
  | run (OP_POP :: insns, stack, stackTop) = if stackTop = 0
                                             then raise Fail "stack underflow"
                                             else run (insns, stack, stackTop - 1)
  | run (OP_PUSH_NIL :: insns, stack, stackTop) = run (insns, stack, push (stack, stackTop, NIL))
  | run (OP_PUSH_INT n :: insns, stack, stackTop) = run (insns, stack, push (stack, stackTop, INT n))
  | run (OP_JUMP_IF_FALSE offset :: insns, stack, stackTop) = let val (stackTop, value) = pop (stack, stackTop)
                                                              in case value of
                                                                     BOOL true => run (insns, stack, stackTop)
                                                                   | BOOL false => run (List.drop (insns, offset), stack, stackTop)
                                                                   | _ => raise Fail "type error: expected bool"
                                                              end
  | run (OP_JUMP offset :: insns, stack, stackTop) = run (List.drop (insns, offset), stack, stackTop)
  | run (OP_PLUS :: insns, stack, stackTop) = let val (stackTop, b) = pop (stack, stackTop)
                                                  val (stackTop, a) = pop (stack, stackTop)
                                              in case (a, b) of
                                                     (INT a, INT b) => run (insns, stack, push (stack, stackTop, INT (a + b)))
                                                   | _ => raise Fail "type error: +"
                                              end
  | run (OP_MINUS :: insns, stack, stackTop) = let val (stackTop, b) = pop (stack, stackTop)
                                                   val (stackTop, a) = pop (stack, stackTop)
                                               in case (a, b) of
                                                      (INT a, INT b) => run (insns, stack, push (stack, stackTop, INT (a - b)))
                                                    | _ => raise Fail "type error: -"
                                               end
  | run (OP_LT :: insns, stack, stackTop) = let val (stackTop, b) = pop (stack, stackTop)
                                                val (stackTop, a) = pop (stack, stackTop)
                                            in case (a, b) of
                                                   (INT a, INT b) => run (insns, stack, push (stack, stackTop, BOOL (a < b)))
                                                 | _ => raise Fail "type error: <"
                                            end
  | run (OP_LE :: insns, stack, stackTop) = let val (stackTop, b) = pop (stack, stackTop)
                                                val (stackTop, a) = pop (stack, stackTop)
                                            in case (a, b) of
                                                   (INT a, INT b) => run (insns, stack, push (stack, stackTop, BOOL (a <= b)))
                                                 | _ => raise Fail "type error: <="
                                            end
  | run (OP_PRINT :: insns, stack, stackTop) = let val (stackTop, a) = pop (stack, stackTop)
                                               in case a of
                                                      NIL => print "nil\n"
                                                    | BOOL b => print (Bool.toString b ^ "\n")
                                                    | INT n => print (Int.toString n ^ "\n")
                                                ; run (insns, stack, push (stack, stackTop, NIL))
                                               end
end
end;
