# Makefile for Flex and Yacc project

# Compiler and Flags
CC = gcc
CFLAGS = -Wall -g

# Flex and Yacc files
LEX_FILE = gpp_lexer.l
YACC_FILE = gpp_interpreter.y
HEADER_FILE = yacc_helper.h

# Output files
LEX_OUTPUT = gpp_lexer.c
YACC_OUTPUT = gpp_parser.c
YACC_HEADER = gpp_parser.h
EXEC_OUTPUT = gpp_interpreter

# Targets
all: $(EXEC_OUTPUT)

$(EXEC_OUTPUT): $(LEX_OUTPUT) $(YACC_OUTPUT)
	$(CC) $(CFLAGS) -o $(EXEC_OUTPUT) $(LEX_OUTPUT) $(YACC_OUTPUT) -ll -w

$(LEX_OUTPUT): $(LEX_FILE)
	flex -o $(LEX_OUTPUT) $(LEX_FILE)

$(YACC_OUTPUT): $(YACC_FILE) $(HEADER_FILE)
	yacc -d -o $(YACC_OUTPUT) $(YACC_FILE)

clean:
	rm -f $(LEX_OUTPUT) $(YACC_OUTPUT) $(YACC_HEADER) $(EXEC_OUTPUT)

re: clean all
