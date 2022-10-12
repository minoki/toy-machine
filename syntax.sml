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
structure StringSet = RedBlackSetFn (open String; type ord_key = string)
(* freeVars : StringSet.set * exp -> StringSet.set *)
fun freeVars (bound, NIL) = StringSet.empty
  | freeVars (bound, INT _) = StringSet.empty
  | freeVars (bound, VAR name) = if StringSet.member (bound, name)
                                 then StringSet.empty
                                 else StringSet.singleton name
  | freeVars (bound, LAMBDA (name, body)) = freeVars (StringSet.add (bound, name), body)
  | freeVars (bound, IF (a, b, c)) = StringSet.union (freeVars (bound, a), StringSet.union (freeVars (bound, b), freeVars (bound, c)))
  | freeVars (bound, APP (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, PLUS (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, MINUS (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, LT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, LE (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, PRINT a) = freeVars (bound, a)
end;
