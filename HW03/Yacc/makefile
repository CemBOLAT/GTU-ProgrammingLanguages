

all:
	@echo "'make yacc' to generate the gpp_parser.c file"
	@echo "'make c' to compile the gpp_parser.c file"
	@echo "'make input' to run the lexer with input.txt"
	@echo "'make clean' to remove the gpp_lexer.c and gpp_lexer files"
	@echo "'make re' to remove the gpp_lexer.c and gpp_lexer files and recompile"

yacc: yacc_helper.h gpp_interpreter.y
	bison -o gpp_parser.c gpp_interpreter.y
	flex -o gpp_lexer.c gpp_lexer.l
	gcc -o gpp_interpreter gpp_lexer.c gpp_parser.c -ll -w

gpp_lexer.c: gpp_lexer.l
	flex -o gpp_lexer.c gpp_lexer.l

gpp_parser.c: gpp_interpreter.y
	yacc -d -o gpp_parser.c gpp_interpreter.y

c: gpp_lexer.c gpp_parser.c
	gcc -o gpp_interpreter gpp_lexer.c gpp_parser.c -ll -w

input: gpp_interpreter
	./gpp_interpreter input.txt

re: clean c

clean:
	rm -f gpp_lexer.c
	rm -f gpp_parser.c gpp_parser.h gpp_interpreter

.PHONY: all input test re clean
