all:
	flex -o gpp_lexer.c gpp_lexer.l
	gcc -o gpp_lexer gpp_lexer.c -lfl

input: gpp_lexer
	./gpp_lexer input.txt

test: gpp_lexer
	./gpp_lexer test.txt

re:
	make clean
	make all

clean:
	rm -f gpp_lexer.c gpp_lexer

.PHONY: all input test re clean