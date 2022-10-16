structure Instruction = struct
datatype instruction = OP_POP
                     | OP_POP_EXCEPT_TOP of int
                     | OP_PUSH_NIL
                     | OP_PUSH_INT of int
                     | OP_PUSH_LOCAL of int
                     | OP_PUSH_FREE of int
                     | OP_CALL
                     | OP_TAILCALL
                     | OP_RETURN
                     | OP_CLOSURE of { body : instruction list, nFreeVars : int }
                     | OP_FIX_CLOSURE of { target : int, freeIndex : int, real : int } (* Make a recursive closure *)
                     | OP_JUMP_IF_FALSE of int
                     | OP_JUMP of int
                     | OP_PLUS
                     | OP_MINUS
                     | OP_LT
                     | OP_LE
                     | OP_PRINT
fun toString OP_POP = "OP_POP"
  | toString (OP_POP_EXCEPT_TOP n) = "OP_POP_EXCEPT_TOP(" ^ Int.toString n ^ ")"
  | toString OP_PUSH_NIL = "OP_PUSH_NIL"
  | toString (OP_PUSH_INT n) = "OP_PUSH_INT(" ^ Int.toString n ^ ")"
  | toString (OP_PUSH_LOCAL n) = "OP_PUSH_LOCAL(" ^ Int.toString n ^ ")"
  | toString (OP_PUSH_FREE n) = "OP_PUSH_FREE(" ^ Int.toString n ^ ")"
  | toString OP_CALL = "OP_CALL"
  | toString OP_TAILCALL = "OP_TAILCALL"
  | toString OP_RETURN = "OP_RETURN"
  | toString (OP_CLOSURE { body, nFreeVars }) = "OP_CLOSURE[" ^ String.concatWith ";" (map toString body) ^ "](" ^ Int.toString nFreeVars ^ ")"
  | toString (OP_FIX_CLOSURE { target, freeIndex, real }) = "OP_FIX_CLOSURE(" ^ Int.toString target ^ "," ^ Int.toString freeIndex ^ "," ^ Int.toString real ^ ")"
  | toString (OP_JUMP_IF_FALSE offset) = "OP_JUMP_IF_FALSE(" ^ Int.toString offset ^ ")"
  | toString (OP_JUMP offset) = "OP_JUMP(" ^ Int.toString offset ^ ")"
  | toString OP_PLUS = "OP_PLUS"
  | toString OP_MINUS = "OP_MINUS"
  | toString OP_LT = "OP_LT"
  | toString OP_LE = "OP_LE"
  | toString OP_PRINT = "OP_PRINT"
end;
