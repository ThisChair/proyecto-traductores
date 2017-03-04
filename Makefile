all: retina

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

retina : TokenInfo.hs Tree.hs Parser.hs Lexer.hs RetMonad.hs
	ghc --make retina

clean :
	rm -f retina Lexer.hs Parser.hs *.o *.hi


