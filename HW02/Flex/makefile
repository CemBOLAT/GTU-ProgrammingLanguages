
all:
	flex -o gpp_lexer.c gpp_lexer.l
	gcc -o gpp_lexer gpp_lexer.c -L/opt/homebrew/opt/flex/lib -lfl

input:
	./gpp_lexer input.txt