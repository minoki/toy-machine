all: toylang

s-exp.lex.sml: s-exp.lex
	mllex $<

s-exp.grm.sml s-exp.grm.sig: s-exp.grm
	mlyacc $<

SOURCES = \
  s-exp.grm.sig \
  s-exp.lex.sml \
  s-exp.grm.sml \
  s-exp-parser.sml \
  syntax.sml \
  parse-s-exp.sml \
  instruction.sml \
  compiler.sml \
  interpreter.sml \
  main.sml

toylang: toylang.mlb $(SOURCES)
	mlton -output $@ toylang.mlb
