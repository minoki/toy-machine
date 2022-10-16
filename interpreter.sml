structure Interpreter = struct
datatype value = NIL
               | BOOL of bool
               | INT of int
               | CLOSURE of Instruction.instruction list * value array
type frame = { base : int
             , return : Instruction.instruction list
             }
type frames = frame array
type stack = value array
local open Instruction
      fun push (stack, stackTop, value) = if stackTop >= Array.length stack then
                                              raise Fail "stack overflow"
                                          else
                                              (Array.update (stack, stackTop, value); stackTop + 1)
      fun pop (stack, stackTop) = let val stackTop = stackTop - 1
                                  in (stackTop, Array.sub (stack, stackTop))
                                  end
in
fun newFrames () = Array.array (100, { base = ~100, return = [] })
fun newStack () = Array.array (100, NIL)
(* run : Instruction.instruction list * stack * int * frames * int * int -> unit *)
fun run ([], stack, stackTop, frames, framesTop, base) = ()
  | run (insn :: insns, stack, stackTop, frames, framesTop, base)
    = case insn of
          OP_POP => if stackTop = 0
                    then raise Fail "stack underflow"
                    else run (insns, stack, stackTop - 1, frames, framesTop, base)
        | OP_POP_EXCEPT_TOP n => let val (stackTop, top) = pop (stack, stackTop)
                                 in Array.update (stack, stackTop - n, top)
                                  ; run (insns, stack, stackTop - n + 1, frames, framesTop, base)
                                 end
        | OP_PUSH_NIL => run (insns, stack, push (stack, stackTop, NIL), frames, framesTop, base)
        | OP_PUSH_INT n => run (insns, stack, push (stack, stackTop, INT n), frames, framesTop, base)
        | OP_PUSH_LOCAL i => run (insns, stack, push (stack, stackTop, Array.sub (stack, base + i)), frames, framesTop, base)
        | OP_PUSH_FREE i => let val currentClosure = Array.sub (stack, base)
                            in case currentClosure of
                                   CLOSURE (_, free) => run (insns, stack, push (stack, stackTop, Array.sub (free, i)), frames, framesTop, base)
                                 | _ => raise Fail "type error"
                            end
        | OP_CALL => let val newFrame = { base = base, return = insns }
                     in Array.update (frames, framesTop, newFrame)
                      ; case Array.sub (stack, stackTop - 2) of
                            CLOSURE (insns', _) => run (insns', stack, stackTop, frames, framesTop + 1, stackTop - 2)
                          | _ => raise Fail "type error: expected function"
                     end
        | OP_TAILCALL => let val arg = Array.sub (stack, stackTop - 1)
                             val func = Array.sub (stack, stackTop - 2)
                         in Array.update (stack, base + 1, arg)
                          ; Array.update (stack, base, func)
                          ; case func of
                                CLOSURE (insns', _) => run (insns', stack, base + 2, frames, framesTop, base)
                              | _ => raise Fail "type error: expected function"
                         end
        | OP_RETURN => let val frame = Array.sub (frames, framesTop - 1)
                           val (_, result) = pop (stack, stackTop)
                       in Array.update (stack, base, result)
                        ; run (#return frame, stack, base + 1, frames, framesTop - 1, #base frame)
                       end
        | OP_CLOSURE { body, nFreeVars } => let val freeVars = Array.tabulate (nFreeVars, fn i => Array.sub (stack, stackTop - nFreeVars + i))
                                            in run (insns, stack, push (stack, stackTop - nFreeVars, CLOSURE (body, freeVars)), frames, framesTop, base)
                                            end
        | OP_FIX_CLOSURE { target, freeIndex, real } => let val target = Array.sub (stack, base + target)
                                                            val real = Array.sub (stack, base + real)
                                                        in case target of
                                                               CLOSURE (_, free) => Array.update (free, freeIndex, real)
                                                             | _ => raise Fail "type error: expected function"
                                                         ; run (insns, stack, stackTop, frames, framesTop, base)
                                                        end
        | OP_JUMP_IF_FALSE offset => let val (stackTop, value) = pop (stack, stackTop)
                                     in case value of
                                            BOOL true => run (insns, stack, stackTop, frames, framesTop, base)
                                          | BOOL false => run (List.drop (insns, offset), stack, stackTop, frames, framesTop, base)
                                          | _ => raise Fail "type error: expected bool"
                                     end
        | OP_JUMP offset => run (List.drop (insns, offset), stack, stackTop, frames, framesTop, base)
        | OP_PLUS => let val (stackTop, b) = pop (stack, stackTop)
                         val (stackTop, a) = pop (stack, stackTop)
                     in case (a, b) of
                            (INT a, INT b) => run (insns, stack, push (stack, stackTop, INT (a + b)), frames, framesTop, base)
                          | _ => raise Fail "type error: +"
                     end
        | OP_MINUS => let val (stackTop, b) = pop (stack, stackTop)
                          val (stackTop, a) = pop (stack, stackTop)
                      in case (a, b) of
                             (INT a, INT b) => run (insns, stack, push (stack, stackTop, INT (a - b)), frames, framesTop, base)
                           | _ => raise Fail "type error: -"
                      end
        | OP_LT => let val (stackTop, b) = pop (stack, stackTop)
                       val (stackTop, a) = pop (stack, stackTop)
                   in case (a, b) of
                          (INT a, INT b) => run (insns, stack, push (stack, stackTop, BOOL (a < b)), frames, framesTop, base)
                        | _ => raise Fail "type error: <"
                   end
        | OP_LE => let val (stackTop, b) = pop (stack, stackTop)
                       val (stackTop, a) = pop (stack, stackTop)
                   in case (a, b) of
                          (INT a, INT b) => run (insns, stack, push (stack, stackTop, BOOL (a <= b)), frames, framesTop, base)
                        | _ => raise Fail "type error: <="
                   end
        | OP_PRINT => let val (stackTop, a) = pop (stack, stackTop)
                      in case a of
                             NIL => print "nil\n"
                           | BOOL b => print (Bool.toString b ^ "\n")
                           | INT n => print (Int.toString n ^ "\n")
                           | CLOSURE _ => print "<function>\n"
                       ; run (insns, stack, push (stack, stackTop, NIL), frames, framesTop, base)
                      end
fun runProgram insns = let val stack = newStack ()
                       in run (insns, stack, 0, newFrames (), 0, 0)
                       end
end
end;
