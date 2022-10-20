structure ParseSExp = struct
local structure X = SExp
      structure S = Syntax
in
fun parseExp (X.INT x) = S.INT x
  | parseExp (X.BOOL x) = S.BOOL x
  | parseExp (X.STRING x) = raise Fail "string not supported yet"
  | parseExp (X.LIST [X.ID "quote", X.LIST []]) = S.NIL
  | parseExp (X.LIST [X.ID "lambda", X.LIST [X.ID v], body]) = S.LAMBDA (v, parseExp body)
  | parseExp (X.LIST [X.ID "let", X.LIST bindings, body]) = S.LET (List.map parseBinding bindings, parseExp body)
  | parseExp (X.LIST [X.ID "let*", X.LIST bindings, body]) = List.foldr (fn (binding, body) => S.LET ([parseBinding binding], body)) (parseExp body) bindings
  | parseExp (X.LIST [X.ID "letrec", X.LIST bindings, body]) = S.LETREC (List.map parseBinding bindings, parseExp body)
  | parseExp (X.LIST [X.ID "if", cond, then_, else_]) = S.IF (parseExp cond, parseExp then_, parseExp else_)
  | parseExp (X.LIST [X.ID "+", a, b]) = S.PLUS (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "-", a, b]) = S.MINUS (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "=", a, b]) = S.EQ (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "<", a, b]) = S.LT (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "<=", a, b]) = S.LE (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "display", a]) = S.PRINT (parseExp a)
  | parseExp (X.LIST [X.ID "new-prompt"]) = S.NEW_PROMPT
  | parseExp (X.LIST [X.ID "push-prompt", a, b]) = S.PUSH_PROMPT (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "with-subcont", a, b]) = S.WITH_SUBCONT (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "push-subcont", a, b]) = S.PUSH_SUBCONT (parseExp a, parseExp b)
  | parseExp (X.LIST [X.ID "abort", a, b]) = S.ABORT (parseExp a, parseExp b)
  | parseExp (X.LIST [a, b]) = S.APP (parseExp a, parseExp b)
  | parseExp (X.ID x) = S.VAR x
  | parseExp x = raise Fail ("unparsed expression: " ^ SExp.toString x)
and parseBinding (X.LIST [X.ID v, x]) = (v, parseExp x)
  | parseBinding x = raise Fail ("unparsed binding: " ^ SExp.toString x)
fun parseProgram xs = List.map parseExp xs
end
end;
