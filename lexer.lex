(* -*- mode: sml-lex -*- *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

fun error x = print x
fun eof () = Tokens.EOF (0, 0)

%%
%header (functor ToyLangLexFun (structure Tokens: ToyLang_TOKENS));

digit = [0-9];
ws = [\ \t\n];
alpha = [a-zA-Z];
alnum = [a-zA-Z0-9];

%%

{ws}+ => (lex ());
"(" => (Tokens.LPAREN (0, 0));
")" => (Tokens.RPAREN (0, 0));
"lambda" => (Tokens.LAMBDA (0, 0));
"let" => (Tokens.LET (0, 0));
"let*" => (Tokens.LET_STAR (0, 0));
"letrec" => (Tokens.LETREC (0, 0));
"if" => (Tokens.IF (0, 0));
"nil" => (Tokens.NIL (0, 0));
"print" => (Tokens.PRINT (0, 0));
"+" => (Tokens.PLUS (0, 0));
"-" => (Tokens.MINUS (0, 0));
"<" => (Tokens.LT (0, 0));
"<=" => (Tokens.LE (0, 0));
{digit}+ => (Tokens.INT (CharVector.foldl (fn (a,r) => ord a - ord #"0" + 10 * r) 0 yytext, 0, 0));
{alpha}({alnum}|_|\-|\*)* => (Tokens.NAME (yytext, 0, 0));
. => (error ("toylang: ignoring bad character " ^ yytext); lex ());
