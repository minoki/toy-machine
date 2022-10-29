structure Interpreter = struct
type prompt = unit ref
datatype frame = CALL_FRAME of { base : int
                               , return : Instruction.instruction list
                               }
               | EXN_FRAME of { handler : Instruction.instruction list
                              , stackPos : int
                              , base : int
                              }
               | CONT_MARKER of { prompt : prompt, stackPos : int }
               | DUMMY_FRAME
datatype value = NIL
               | BOOL of bool
               | INT of int
               | CLOSURE of Instruction.instruction list * value array
               | PROMPT of prompt
               | SUBCONT of value vector * frame vector
               | CONS of value * value
               | BOGUS
fun valueToString NIL = "()"
  | valueToString (BOOL false) = "#f"
  | valueToString (BOOL true) = "#t"
  | valueToString (INT n) = if n < 0 then
                                "-" ^ String.extract (Int.toString n, 1, NONE)
                            else
                                Int.toString n
  | valueToString (CLOSURE _) = "<closure>"
  | valueToString (PROMPT _) = "<prompt>"
  | valueToString (SUBCONT _) = "<subcont>"
  | valueToString (x as CONS _) = (case tryToList (x, []) of
                                       (xs, NONE) => "(" ^ String.concatWith " " (List.map valueToString xs) ^ ")"
                                     | (xs, SOME y) => "(" ^ String.concatWith " " (List.map valueToString xs) ^ " . " ^ valueToString y ^ ")"
                                  )
  | valueToString BOGUS = "<bogus>"
and tryToList (NIL, acc) = (List.rev acc, NONE)
  | tryToList (CONS (a, b), acc) = tryToList (b, a :: acc)
  | tryToList (x, acc) = (List.rev acc, SOME x)
type frames = frame array
type stack = value array
local open Instruction
      fun push (stack, stackTop, value) = if stackTop >= Array.length stack then
                                              raise Fail "stack overflow"
                                          else
                                              case value of
                                                  BOGUS => raise Fail "attempt to push <bogus>"
                                                | _ => (Array.update (stack, stackTop, value); stackTop + 1)
      fun pop (stack, stackTop) = if stackTop = 0
                                  then raise Fail "stack underflow"
                                  else let val stackTop = stackTop - 1
                                       in (stackTop, Array.sub (stack, stackTop)) before Array.update (stack, stackTop, BOGUS)
                                       end
      fun dumpStack (stack, 0) = print "stack: empty\n"
        | dumpStack (stack, stackTop) = ArraySlice.appi (fn (i, x) => print ("stack #" ^ Int.toString i ^ ": " ^ valueToString x ^ "\n")) (ArraySlice.slice (stack, 0, SOME stackTop))
      fun dumpFrames (frames, 0) = print "frame: empty\n"
        | dumpFrames (frames, framesTop) = ArraySlice.appi (fn (i, x) => print ("frame #" ^ Int.toString i ^ ": " ^ (case x of CALL_FRAME { base, return } => "CALL_FRAME " ^ Int.toString base
                                                                                                                             | EXN_FRAME { handler, stackPos, base } => "EXN_FRAME " ^ Int.toString stackPos ^ "," ^ Int.toString base
                                                                                                                             | CONT_MARKER { prompt, stackPos } => "CONT_MARKER " ^ Int.toString stackPos
                                                                                                                             | DUMMY_FRAME => "DUMMY_FRAME") ^ "\n")) (ArraySlice.slice (frames, 0, SOME framesTop))
in
fun newFrames () = Array.array (100, DUMMY_FRAME)
fun newStack () = Array.array (100, BOGUS)
(* run : Instruction.instruction list * stack * int * frames * int * int -> unit *)
fun run ([], stack, stackTop, frames, framesTop, base) = ()
  | run (insn :: insns, stack, stackTop, frames, framesTop, base)
    = case insn of
          OP_POP => let val (stackTop, _) = pop (stack, stackTop)
                    in run (insns, stack, stackTop, frames, framesTop, base)
                    end
        | OP_POP_EXCEPT_TOP n => let val (stackTop, top) = pop (stack, stackTop)
                                 in ArraySlice.modify (fn _ => BOGUS) (ArraySlice.slice (stack, stackTop - n, SOME n))
                                  ; Array.update (stack, stackTop - n, top)
                                  ; run (insns, stack, stackTop - n + 1, frames, framesTop, base)
                                 end
        | OP_PUSH_NIL => run (insns, stack, push (stack, stackTop, NIL), frames, framesTop, base)
        | OP_PUSH_FALSE => run (insns, stack, push (stack, stackTop, BOOL false), frames, framesTop, base)
        | OP_PUSH_TRUE => run (insns, stack, push (stack, stackTop, BOOL true), frames, framesTop, base)
        | OP_PUSH_INT n => run (insns, stack, push (stack, stackTop, INT n), frames, framesTop, base)
        | OP_PUSH_LOCAL i => run (insns, stack, push (stack, stackTop, Array.sub (stack, base + i)), frames, framesTop, base)
        | OP_PUSH_FREE i => let val currentClosure = Array.sub (stack, base)
                            in case currentClosure of
                                   CLOSURE (_, free) => run (insns, stack, push (stack, stackTop, Array.sub (free, i)), frames, framesTop, base)
                                 | _ => raise Fail "type error"
                            end
        | OP_CALL => let val newFrame = CALL_FRAME { base = base, return = insns }
                     in Array.update (frames, framesTop, newFrame)
                      ; case Array.sub (stack, stackTop - 2) of
                            CLOSURE (insns', _) => run (insns', stack, stackTop, frames, framesTop + 1, stackTop - 2)
                          | x => raise Fail ("type error: expected function, but got " ^ valueToString x)
                     end
        | OP_TAILCALL => let val arg = Array.sub (stack, stackTop - 1)
                             val func = Array.sub (stack, stackTop - 2)
                         in Array.update (stack, base + 1, arg)
                          ; Array.update (stack, base, func)
                          ; ArraySlice.modify (fn _ => BOGUS) (ArraySlice.slice (stack, base + 2, SOME (stackTop - (base + 2))))
                          ; case func of
                                CLOSURE (insns', _) => run (insns', stack, base + 2, frames, framesTop, base)
                              | _ => raise Fail "type error: expected function"
                         end
        | OP_RETURN => let val (stackTop, result) = pop (stack, stackTop)
                           val () = ArraySlice.modify (fn _ => BOGUS) (ArraySlice.slice (stack, base, SOME (stackTop - base)))
                           fun loop framesTop = case Array.sub (frames, framesTop - 1) before Array.update (frames, framesTop - 1, DUMMY_FRAME) of
                                                    CALL_FRAME { base = base', return } => run (return, stack, base + 1, frames, framesTop - 1, base')
                                                  | CONT_MARKER _ => loop (framesTop - 1)
                                                  | _ => raise Fail "invalid frame"
                       in Array.update (stack, base, result)
                        ; loop framesTop
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
        | OP_TIMES => let val (stackTop, b) = pop (stack, stackTop)
                          val (stackTop, a) = pop (stack, stackTop)
                      in case (a, b) of
                             (INT a, INT b) => run (insns, stack, push (stack, stackTop, INT (a * b)), frames, framesTop, base)
                           | _ => raise Fail "type error: *"
                      end
        | OP_EQ => let val (stackTop, b) = pop (stack, stackTop)
                       val (stackTop, a) = pop (stack, stackTop)
                   in case (a, b) of
                          (INT a, INT b) => run (insns, stack, push (stack, stackTop, BOOL (a = b)), frames, framesTop, base)
                        | (BOOL a, BOOL b) => run (insns, stack, push (stack, stackTop, BOOL (a = b)), frames, framesTop, base)
                        | (NIL, NIL) => run (insns, stack, push (stack, stackTop, BOOL true), frames, framesTop, base)
                        | _ => raise Fail "type error: ="
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
                      in print (valueToString a ^ "\n")
                       ; run (insns, stack, push (stack, stackTop, NIL), frames, framesTop, base)
                      end
        | OP_RAISE => let val (stackTop, a) = pop (stack, stackTop)
                          fun lookup i = if i < 0 then
                                             NONE
                                         else
                                             (* TODO: This is slow *)
                                             case Array.sub (frames, i) of
                                                 EXN_FRAME f => SOME (i, f)
                                               | _ => lookup (i - 1)
                      in case lookup (framesTop - 1) of
                             SOME (i, { handler, stackPos, base }) =>
                             let val () = ArraySlice.modify (fn _ => DUMMY_FRAME) (ArraySlice.slice (frames, i, SOME (framesTop - i)))
                                 val () = ArraySlice.modify (fn _ => BOGUS) (ArraySlice.slice (stack, stackPos, SOME (stackTop - stackPos)))
                                 val stackTop = push (stack, stackPos, a)
                             in run (handler, stack, stackTop, frames, i, base)
                             end
                           | NONE => ( print ("Uncaught exception: " ^ valueToString a ^ "\n")
                                     ; ()
                                     )
                      end
        | OP_PUSH_HANDLER offset => let val () = Array.update (frames, framesTop, EXN_FRAME { handler = List.drop (insns, offset), stackPos = stackTop, base = base })
                                    in run (insns, stack, stackTop, frames, framesTop + 1, base)
                                    end
        | OP_POP_HANDLER offset => let val () = Array.update (frames, framesTop - 1, DUMMY_FRAME)
                                   in run (List.drop (insns, offset), stack, stackTop, frames, framesTop - 1, base)
                                   end
        | OP_NEW_PROMPT => run (insns, stack, push (stack, stackTop, PROMPT (ref ())), frames, framesTop, base)
        | OP_PUSH_PROMPT => let val (stackTop, b) = pop (stack, stackTop)
                                val (stackTop, a) = pop (stack, stackTop)
                            in case (a, b) of
                                   (PROMPT p, CLOSURE (insns', _)) =>
                                   ( Array.update (frames, framesTop, CALL_FRAME { base = base, return = insns })
                                   ; Array.update (frames, framesTop + 1, CONT_MARKER { prompt = p, stackPos = stackTop })
                                   ; let val stackTop = push (stack, stackTop, b)
                                         val stackTop = push (stack, stackTop, NIL)
                                     in run (insns', stack, stackTop, frames, framesTop + 2, stackTop - 2)
                                     end
                                   )
                                 | _ => raise Fail "type error: push-prompt"
                            end
        | OP_WITH_SUBCONT => let val (stackTop, b) = pop (stack, stackTop)
                                 val (stackTop, a) = pop (stack, stackTop)
                             in case (a, b) of
                                    (PROMPT p, CLOSURE (insns', _)) =>
                                    let fun lookup i = if i < 0 then
                                                           NONE
                                                       else
                                                           (* TODO: This is slow *)
                                                           case Array.sub (frames, i) of
                                                               CONT_MARKER { prompt = q, stackPos = s } =>
                                                               if p = q then
                                                                   SOME (i, s)
                                                               else
                                                                   lookup (i - 1)
                                                             | _ => lookup (i - 1)
                                    in case lookup (framesTop - 1) of
                                           SOME (i, s) => let val () = Array.update (frames, framesTop, CALL_FRAME { base = base, return = insns })
                                                              val frameSlice = let val slice = ArraySlice.slice (frames, i, SOME (framesTop + 1 - i))
                                                                               in ArraySlice.vector slice before ArraySlice.modify (fn _ => DUMMY_FRAME) slice
                                                                               end
                                                              val frameSlice = Vector.map (fn CALL_FRAME { base, return } => CALL_FRAME { base = base - s, return = return }
                                                                                          | EXN_FRAME { handler, stackPos, base } => EXN_FRAME { handler = handler, stackPos = stackPos - s, base = base - s }
                                                                                          | CONT_MARKER { prompt, stackPos } => CONT_MARKER { prompt = prompt, stackPos = stackPos - s }
                                                                                          | DUMMY_FRAME => DUMMY_FRAME
                                                                                          ) frameSlice
                                                              val stackSlice = let val slice = ArraySlice.slice (stack, s, SOME (stackTop - s))
                                                                               in ArraySlice.vector slice before ArraySlice.modify (fn _ => BOGUS) slice
                                                                               end
                                                              val stackTop = push (stack, s, b)
                                                              val stackTop = push (stack, stackTop, SUBCONT (stackSlice, frameSlice))
                                                          in run (insns', stack, stackTop, frames, i, stackTop - 2)
                                                          end
                                         | NONE => raise Fail "with-subcont: prompt not found"
                                    end
                                  | _ => raise Fail "type error: with-subcont"
                             end
        | OP_PUSH_SUBCONT => let val (stackTop, b) = pop (stack, stackTop)
                                 val (stackTop, a) = pop (stack, stackTop)
                             in case (a, b) of
                                    (SUBCONT (stackSlice, frameSlice), CLOSURE (insns', _)) =>
                                    let val () = Array.update (frames, framesTop, CALL_FRAME { base = base, return = insns })
                                        val framesTop = framesTop + 1
                                        val frameSlice = Vector.map (fn CALL_FRAME { base, return } => CALL_FRAME { base = base + stackTop, return = return }
                                                                    | EXN_FRAME { handler, stackPos, base } => EXN_FRAME { handler = handler, stackPos = stackPos + stackTop, base = base + stackTop }
                                                                    | CONT_MARKER { prompt, stackPos } => CONT_MARKER { prompt = prompt, stackPos = stackPos + stackTop }
                                                                    | DUMMY_FRAME => DUMMY_FRAME
                                                                    ) frameSlice
                                        val () = Array.copyVec { src = frameSlice, dst = frames, di = framesTop }
                                        val framesTop = framesTop + Vector.length frameSlice
                                        val () = Array.copyVec { src = stackSlice, dst = stack, di = stackTop }
                                        val stackTop = stackTop + Vector.length stackSlice
                                        val stackTop = push (stack, stackTop, b)
                                        val stackTop = push (stack, stackTop, NIL)
                                    in run (insns', stack, stackTop, frames, framesTop, stackTop - 2)
                                    end
                                  | _ => raise Fail "type error: push-subcont"
                             end
        | OP_ABORT => let val (stackTop, b) = pop (stack, stackTop)
                          val (stackTop, a) = pop (stack, stackTop)
                      in case a of
                             PROMPT p =>
                             let fun lookup i = if i < 0 then
                                                    NONE
                                                else
                                                    case Array.sub (frames, i) of
                                                        CONT_MARKER { prompt = q, stackPos = s } =>
                                                        if p = q then
                                                            SOME (i, s)
                                                        else
                                                            lookup (i - 1)
                                                      | _ => lookup (i - 1)
                             in case lookup (framesTop - 1) of
                                    SOME (i, s) => let val stackTop = push (stack, s, b)
                                                   in run ([OP_RETURN], stack, stackTop, frames, i, stackTop)
                                                   end
                                  | NONE => raise Fail "with-subcont: prompt not found"
                             end
                           | _ => raise Fail "type error: abort"
                      end
        | OP_CONS => let val (stackTop, b) = pop (stack, stackTop)
                         val (stackTop, a) = pop (stack, stackTop)
                     in run (insns, stack, push (stack, stackTop, CONS (a, b)), frames, framesTop, base)
                     end
        | OP_CAR => let val (stackTop, a) = pop (stack, stackTop)
                    in case a of
                           CONS (car, cdr) => run (insns, stack, push (stack, stackTop, car), frames, framesTop, base)
                         | _ => raise Fail "type error: car"
                    end
        | OP_CDR => let val (stackTop, a) = pop (stack, stackTop)
                    in case a of
                           CONS (car, cdr) => run (insns, stack, push (stack, stackTop, cdr), frames, framesTop, base)
                         | _ => raise Fail "type error: cdr"
                    end
        | OP_IS_PAIR => let val (stackTop, a) = pop (stack, stackTop)
                        in run (insns, stack, push (stack, stackTop, BOOL (case a of CONS _ => true | _ => false)), frames, framesTop, base)
                        end
fun runProgram insns = let val stack = newStack ()
                       in run (insns, stack, 0, newFrames (), 0, 0)
                       end
end
end;
