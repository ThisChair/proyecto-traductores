all: retina

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

retina : Lexer.hs Parser.hs TokenInfo.hs Tree.hs RetMonad.hs Output.hs Funciones.hs RunMonad.hs RunFunciones.hs
	ghc --make retina

clean :
	rm -f retina Lexer.hs Parser.hs *.o *.hi *~
