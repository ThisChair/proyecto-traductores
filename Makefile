all: retina

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

retina : TokenInfo.hs Tree.hs Parser.hs Lexer.hs Calc.hs
	ghc --make retina

clean:
    rm -f Calc Lexer.hs Parser.hs *.o *.hi