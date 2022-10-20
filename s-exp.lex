(* -*- mode: sml-lex -*- *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

fun error x = print x
fun eof () = Tokens.EOF (0, 0)
fun stringValue s = let fun go (t, acc)
                            = case Substring.getc t of
                                  NONE => String.implode (List.rev acc)
                                | SOME (#"\\", t) => (case Substring.getc t of
                                                          NONE => raise Fail "invalid string"
                                                        | SOME (#"a", t) => go (t, #"\a" :: acc)
                                                        | SOME (#"b", t) => go (t, #"\b" :: acc)
                                                        | SOME (#"t", t) => go (t, #"\t" :: acc)
                                                        | SOME (#"n", t) => go (t, #"\n" :: acc)
                                                        | SOME (#"r", t) => go (t, #"\r" :: acc)
                                                        | SOME (#"\"", t) => go (t, #"\"" :: acc)
                                                        | SOME (#"|", t) => go (t, #"|" :: acc)
                                                        | SOME (c, t) => go (t, c :: acc)
                                                     )
                                | SOME (c, t) => go (t, c :: acc)
                    in go (Substring.substring (s, 1, String.size s - 2), [])
                    end

%%
%header (functor SExpLexFun (structure Tokens: SExp_TOKENS));

digit = [0-9];
ws = [\ \t\n];
identifierPrefixChar = [a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\~];
identifierChar = [a-zA-Z0-9\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~];
stringChar = [^\"\\]|\\[abtnr\"\\\|];

%%

{ws}+ => (lex ());
";"[^\n]* => (lex ());
"(" => (Tokens.LPAREN (0, 0));
")" => (Tokens.RPAREN (0, 0));
"#t" => (Tokens.TRUE (0, 0));
"#f" => (Tokens.FALSE (0, 0));
"." => (Tokens.DOT (0, 0));
"'" => (Tokens.QUOTE (0, 0));
\"{stringChar}*\" => (Tokens.STRING (stringValue yytext, 0, 0));
"#(" => (Tokens.HASH_LPAREN (0, 0));
{digit}+ => (Tokens.INT (CharVector.foldl (fn (a,r) => ord a - ord #"0" + 10 * r) 0 yytext, 0, 0));
{identifierPrefixChar}{identifierPrefixChar}* => (Tokens.ID (yytext, 0, 0));
. => (error ("toylang: ignoring bad character " ^ yytext); lex ());
