structure Syntax = struct
datatype exp = NIL
             | INT of int
             | VAR of string
             | LAMBDA of string * exp
             | IF of exp * exp * exp
             | APP of exp * exp
             | PLUS of exp * exp
             | MINUS of exp * exp
             | LT of exp * exp
             | LE of exp * exp
             | PRINT of exp
fun toString NIL = "nil"
  | toString (INT n) = Int.toString n
  | toString (VAR name) = name
  | toString (LAMBDA (name, body)) = "(lambda (" ^ name ^ ") " ^ toString body ^ ")"
  | toString (IF (a, b, c)) = "(if " ^ toString a ^ " " ^ toString b ^ " " ^ toString c ^ ")"
  | toString (APP (a, b)) = "(" ^ toString a ^ " " ^ toString b ^ ")"
  | toString (PLUS (a, b)) = "(+ " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (MINUS (a, b)) = "(- " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (LT (a, b)) = "(< " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (LE (a, b)) = "(<= " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (PRINT a) = "(print " ^ toString a ^ ")"
end;
