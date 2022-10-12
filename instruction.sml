structure Instruction = struct
datatype instruction = OP_POP
                     | OP_PUSH_NIL
                     | OP_PUSH_INT of int
                     | OP_PUSH_LOCAL of int
                     | OP_CALL
                     | OP_RETURN
                     | OP_FUNCTION of instruction list
                     | OP_JUMP_IF_FALSE of int
                     | OP_JUMP of int
                     | OP_PLUS
                     | OP_MINUS
                     | OP_LT
                     | OP_LE
                     | OP_PRINT
                     (* TODO: closure *)
fun toString OP_POP = "OP_POP"
  | toString OP_PUSH_NIL = "OP_PUSH_NIL"
  | toString (OP_PUSH_INT n) = "OP_PUSH_INT(" ^ Int.toString n ^ ")"
  | toString (OP_PUSH_LOCAL n) = "OP_PUSH_LOCAL(" ^ Int.toString n ^ ")"
  | toString OP_CALL = "OP_CALL"
  | toString OP_RETURN = "OP_RETURN"
  | toString (OP_FUNCTION insns) = "OP_FUNCTION[" ^ String.concatWith ";" (map toString insns) ^ "]"
  | toString (OP_JUMP_IF_FALSE offset) = "OP_JUMP_IF_FALSE(" ^ Int.toString offset ^ ")"
  | toString (OP_JUMP offset) = "OP_JUMP(" ^ Int.toString offset ^ ")"
  | toString OP_PLUS = "OP_PLUS"
  | toString OP_MINUS = "OP_MINUS"
  | toString OP_LT = "OP_LT"
  | toString OP_LE = "OP_LE"
  | toString OP_PRINT = "OP_PRINT"
end;
