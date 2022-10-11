structure Compiler = struct
local open Syntax
      open Instruction
in
(* compileExp : Syntax.exp -> Instruction.instruction list *)
fun compileExp NIL = [OP_PUSH_NIL]
  | compileExp (INT n) = [OP_PUSH_INT n]
  | compileExp (VAR name) = raise Fail "variable not supported yet"
  | compileExp (LAMBDA (name, body)) = raise Fail "lambda not supported yet"
  | compileExp (IF (a, b, c)) = let val b' = compileExp b
                                    val c' = compileExp c
                                in compileExp a @ [OP_JUMP_IF_FALSE (length b' + 1)] @ b' @ [OP_JUMP (length c')] @ c'
                                end
  | compileExp (APP (a, b)) = raise Fail "application not supported yet"
  | compileExp (PLUS (a, b)) = compileExp a @ compileExp b @ [OP_PLUS]
  | compileExp (MINUS (a, b)) = compileExp a @ compileExp b @ [OP_MINUS]
  | compileExp (LT (a, b)) = compileExp a @ compileExp b @ [OP_LT]
  | compileExp (LE (a, b)) = compileExp a @ compileExp b @ [OP_LE]
  | compileExp (PRINT a) = compileExp a @ [OP_PRINT]
fun compileProgram [] = []
  | compileProgram (x :: xs) = compileExp x @ OP_POP :: compileProgram xs
end
end;
