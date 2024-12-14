all: 
	yacc -d gpp_interpreter.y
	flex gpp_lexer.l
	gcc -o gpp_interpreter y.tab.c lex.yy.c
	./gpp_interpreter

clean:
	rm -f gpp_interpreter y.tab.c y.tab.h lex.yy.c

output:
	yacc -d -v gpp_interpreter.y

re: clean all

.PHONY: all clean re