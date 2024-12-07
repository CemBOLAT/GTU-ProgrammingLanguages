all:
	@echo "'make flex' to generate the gpp_lexer.c file"
	@echo "'make c' to compile the gpp_lexer.c file"
	@echo "'make input' to run the lexer with input.txt"
	@echo "'make clean' to remove the gpp_lexer.c and gpp_lexer files"
	@echo "'make re' to remove the gpp_lexer.c and gpp_lexer files and recompile"

flex:
	flex -o gpp_lexer.c gpp_lexer.l

gpp_lexer: gpp_lexer.c
	gcc -o gpp_lexer gpp_lexer.c -lfl

c: gpp_lexer

input: gpp_lexer
	./gpp_lexer input.txt

re: clean gpp_lexer

clean:
	rm -f gpp_lexer.c gpp_lexer

.PHONY: all input test re clean
