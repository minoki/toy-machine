structure ToyLangLrVals = ToyLangLrValsFun (structure Token = LrParser.Token)
structure ToyLangLex = ToyLangLexFun (structure Tokens = ToyLangLrVals.Tokens)
structure ToyLangParser = Join (structure Lex = ToyLangLex
                                structure ParserData = ToyLangLrVals.ParserData
                                structure LrParser = LrParser)
