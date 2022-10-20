structure SExpLrVals = SExpLrValsFun (structure Token = LrParser.Token)
structure SExpLex = SExpLexFun (structure Tokens = SExpLrVals.Tokens)
structure SExpParser = Join (structure Lex = SExpLex
                             structure ParserData = SExpLrVals.ParserData
                             structure LrParser = LrParser);
