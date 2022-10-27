structure Syntax = struct
datatype exp = NIL
             | INT of int
             | BOOL of bool
             | VAR of string
             | LAMBDA of string * exp
             | LET of (string * exp) list * exp
             | LETREC of (string * exp) list * exp
             | IF of exp * exp * exp
             | APP of exp * exp
             | PLUS of exp * exp
             | MINUS of exp * exp
             | TIMES of exp * exp
             | EQ of exp * exp
             | LT of exp * exp
             | LE of exp * exp
             | PRINT of exp
             | NEW_PROMPT
             | PUSH_PROMPT of exp * exp
             | WITH_SUBCONT of exp * exp
             | PUSH_SUBCONT of exp * exp
             | ABORT of exp * exp
             | SEQUENCE of exp list
             | CONS of exp * exp
             | CAR of exp
             | CDR of exp
             | IS_PAIR of exp
     and stmt = DEFINE_LAMBDA of string * string * exp
              | DEFINE of string * exp
              | EXP of exp
fun toString NIL = "nil"
  | toString (INT n) = Int.toString n
  | toString (BOOL n) = Bool.toString n
  | toString (VAR name) = name
  | toString (LAMBDA (name, body)) = "(lambda (" ^ name ^ ") " ^ toString body ^ ")"
  | toString (LET (bindings, body)) = "(let (" ^ String.concatWith " " (List.map (fn (name, exp) => "(" ^ name ^ " " ^ toString exp ^ ")") bindings) ^ ") " ^ toString body ^ ")"
  | toString (LETREC (bindings, body)) = "(letrec (" ^ String.concatWith " " (List.map (fn (name, exp) => "(" ^ name ^ " " ^ toString exp ^ ")") bindings) ^ ") " ^ toString body ^ ")"
  | toString (IF (a, b, c)) = "(if " ^ toString a ^ " " ^ toString b ^ " " ^ toString c ^ ")"
  | toString (APP (a, b)) = "(" ^ toString a ^ " " ^ toString b ^ ")"
  | toString (PLUS (a, b)) = "(+ " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (MINUS (a, b)) = "(- " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (TIMES (a, b)) = "(* " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (EQ (a, b)) = "(= " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (LT (a, b)) = "(< " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (LE (a, b)) = "(<= " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (PRINT a) = "(display " ^ toString a ^ ")"
  | toString NEW_PROMPT = "(new-prompt)"
  | toString (PUSH_PROMPT (a, b)) = "(push-prompt " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (WITH_SUBCONT (a, b)) = "(with-subcont " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (PUSH_SUBCONT (a, b)) = "(push-subcont " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (ABORT (a, b)) = "(abort " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (SEQUENCE xs) = "(begin " ^ String.concatWith " " (List.map toString xs) ^ ")"
  | toString (CONS (a, b)) = "(cons " ^ toString a ^ " " ^ toString b ^ ")"
  | toString (CAR a) = "(car " ^ toString a ^ ")"
  | toString (CDR a) = "(cdr " ^ toString a ^ ")"
  | toString (IS_PAIR a) = "(pair? " ^ toString a ^ ")"
fun stmtToString (DEFINE_LAMBDA (v, w, body)) = "(define (" ^ v ^ " " ^ w ^ ") " ^ toString body ^ ")"
  | stmtToString (DEFINE (v, x)) = "(define " ^ v ^ " " ^ toString x ^ ")"
  | stmtToString (EXP x) = toString x
structure StringSet = RedBlackSetFn (open String; type ord_key = string)
(* freeVars : StringSet.set * exp -> StringSet.set *)
fun freeVars (bound, NIL) = StringSet.empty
  | freeVars (bound, INT _) = StringSet.empty
  | freeVars (bound, BOOL _) = StringSet.empty
  | freeVars (bound, VAR name) = if StringSet.member (bound, name)
                                 then StringSet.empty
                                 else StringSet.singleton name
  | freeVars (bound, LAMBDA (name, body)) = freeVars (StringSet.add (bound, name), body)
  | freeVars (bound, LET (bindings, body)) = let val (bound, free) = List.foldl (fn ((name, exp), (b, s)) => (StringSet.add (b, name), StringSet.union (freeVars (bound, exp), s)) ) (bound, StringSet.empty) bindings
                                             in StringSet.union (free, freeVars (bound, body))
                                             end
  | freeVars (bound, LETREC (bindings, body)) = let val bound = List.foldl (fn ((name, exp), bound) => StringSet.add (bound, name)) bound bindings
                                                in List.foldl (fn ((name, exp), s) => StringSet.union (freeVars (bound, exp), s)) (freeVars (bound, body)) bindings
                                                end
  | freeVars (bound, IF (a, b, c)) = StringSet.union (freeVars (bound, a), StringSet.union (freeVars (bound, b), freeVars (bound, c)))
  | freeVars (bound, APP (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, PLUS (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, MINUS (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, TIMES (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, EQ (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, LT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, LE (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, PRINT a) = freeVars (bound, a)
  | freeVars (bound, NEW_PROMPT) = StringSet.empty
  | freeVars (bound, PUSH_PROMPT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, WITH_SUBCONT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, PUSH_SUBCONT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, ABORT (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, SEQUENCE xs) = List.foldl (fn (x, s) => StringSet.union (freeVars (bound, x), s)) StringSet.empty xs
  | freeVars (bound, CONS (a, b)) = StringSet.union (freeVars (bound, a), freeVars (bound, b))
  | freeVars (bound, CAR a) = freeVars (bound, a)
  | freeVars (bound, CDR a) = freeVars (bound, a)
  | freeVars (bound, IS_PAIR a) = freeVars (bound, a)
end;
