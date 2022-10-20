fun print_error (s, p1, p2) = print (s ^ "\n");
val content = case CommandLine.arguments () of
                  [filename] => let val instream = TextIO.openIn filename
                                in TextIO.inputAll instream before TextIO.closeIn instream
                                end
                | _ => ( print "> "
                       ; case TextIO.inputLine TextIO.stdIn of
                             SOME ln => ln
                           | NONE => ""
                       );
val lexer = let val i = ref 0
            in SExpParser.makeLexer (fn _ => if !i = 0
                                             then (i := 1; content)
                                             else "")
            end
val (program, _) = SExpParser.parse (0, lexer, print_error, ());
val program = ParseSExp.parseProgram program;
List.app (fn exp => print (Syntax.toString exp ^ "\n")) program;
print "---\n";
val insns = Compiler.compileProgram program;
List.app (fn insn => print (Instruction.toString insn ^ "\n")) insns;
print "---\n";
Interpreter.runProgram insns;
