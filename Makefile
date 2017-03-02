all: retina

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

retina : TokenInfo.hs Tree.hs Parser.hs Lexer.hs
	ghc --make retina

