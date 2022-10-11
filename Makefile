all: toylang

lexer.lex.sml: lexer.lex
	mllex $<

parser.grm.sml parser.grm.sig: parser.grm
	mlyacc $<

SOURCES = \
  syntax.sml \
  parser.grm.sig \
  lexer.lex.sml \
  parser.grm.sml \
  parser.sml \
  instruction.sml \
  compiler.sml \
  interpreter.sml \
  main.sml

toylang: toylang.mlb $(SOURCES)
	mlton -output $@ toylang.mlb
