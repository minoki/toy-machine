fun print_error (s, p1, p2) = print (s ^ "\n");
print "> ";
val content = case TextIO.inputLine TextIO.stdIn of
                  SOME ln => ln
                | NONE => "";
val lexer = let val i = ref 0
            in ToyLangParser.makeLexer (fn _ => if !i = 0
                                                then (i := 1; content)
                                                else "")
            end
val (program, _) = ToyLangParser.parse (0, lexer, print_error, ());
List.app (fn exp => print (Syntax.toString exp ^ "\n")) program;
