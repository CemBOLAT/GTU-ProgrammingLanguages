%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

extern FILE *yyin;
int yylex();
void yyerror(const char *s);
void yy_scan_string(const char *str);
int load_file(char *filename);
int yydebug = 0;

%}

%union {
    char *sval;
}

%token <sval> VALUEI VALUEF
%token <sval> IDENTIFIER
%token KW_AND KW_OR KW_NOT KW_LESS KW_LIST KW_CONCAT KW_DEFFUN KW_IF KW_LOAD KW_TRUE KW_EQUAL KW_NIL KW_APPEND KW_SET KW_FOR KW_EXIT KW_PRINT KW_FALSE KW_DEFVAR KW_WHILE
%token OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP OP_COMMA OP_APOSTROPHE OP_QUOTE COMMENT

%type <sval> EXPLIST
%type <sval> EXP EXPB LIST_INPUT LIST VALUES SET PARAMLIST FCALL 

%%

// Parser yapıları
START:
    /* empty */ { ; } |
    INPUT { ; };

INPUT:
    EXPLIST { printf("Syntax Correct: %s\n", $1); };

EXPLIST:
    EXP { $$ = $1; }
    | EXPLIST EXP { $$ = $2; };

EXP:
    OP_OP OP_PLUS EXP EXP OP_CP { asprintf(&$$, "(+ %s %s)", $3, $4); } |
    OP_OP OP_MINUS EXP EXP OP_CP { asprintf(&$$, "(- %s %s)", $3, $4); } |
    OP_OP OP_MULT EXP EXP OP_CP { asprintf(&$$, "(* %s %s)", $3, $4); } |
    OP_OP OP_DIV EXP EXP OP_CP { asprintf(&$$, "(/ %s %s)", $3, $4); } |
    OP_OP KW_IF EXPB EXP EXP OP_CP { asprintf(&$$, "(if %s %s %s)", $3, $4, $5); } |
    OP_OP KW_IF EXPB EXP OP_CP { asprintf(&$$, "(if %s %s)", $3, $4); } |
    OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP { asprintf(&$$, "(for (%s %s %s) %s)", $4, $5, $6, $8); } |
    OP_OP KW_WHILE EXPB EXP OP_CP { asprintf(&$$, "(while %s %s)", $3, $4); } |
    OP_OP KW_PRINT EXP OP_CP { asprintf(&$$, "(print %s)", $3); } |
    OP_OP KW_EXIT OP_CP { asprintf(&$$, "(exit)"); exit(0); } |
    OP_OP KW_DEFFUN IDENTIFIER OP_OP PARAMLIST OP_CP EXPLIST OP_CP { asprintf(&$$, "(deffun %s (%s) %s)", $3, $5, $7); } |
    EXPB { $$ = $1; } |
    LIST_INPUT { $$ = $1; } |
    SET { $$ = $1; } |
    OP_OP KW_DEFVAR IDENTIFIER EXP OP_CP { asprintf(&$$, "(defvar %s %s)", $3, $4); } |
    FCALL { $$ = $1; } |
    OP_OP KW_LOAD OP_QUOTE IDENTIFIER OP_QUOTE OP_CP { asprintf(&$$, "(load \"%s\")", $4); } |
    COMMENT { $$ = ""; } |
    VALUEF { $$ = $1; } |
    VALUEI { $$ = $1; };

LIST_INPUT:
    OP_OP KW_APPEND LIST_INPUT LIST_INPUT OP_CP { asprintf(&$$, "(append %s %s)", $3, $4); } |
    OP_OP KW_CONCAT LIST_INPUT LIST_INPUT OP_CP { asprintf(&$$, "(concat %s %s)", $3, $4); } |
    LIST { $$ = $1; };

SET:
    OP_OP KW_SET IDENTIFIER EXP OP_CP { asprintf(&$$, "(set %s %s)", $3, $4); };

LIST:
    OP_OP KW_LIST VALUES OP_CP { asprintf(&$$, "(list %s)", $3); } |
    OP_APOSTROPHE OP_OP OP_CP { asprintf(&$$, "\'()"); } |
    KW_NIL { asprintf(&$$, "nil"); } |
    OP_APOSTROPHE OP_OP VALUES OP_CP { asprintf(&$$, "\'(%s)", $3); };

VALUES:
    VALUEI { $$ = $1; } |
    VALUEF { $$ = $1; } |
    IDENTIFIER { $$ = $1; } |
    VALUES OP_COMMA VALUEF { asprintf(&$$, "%s, %s", $1, $3); } |
    VALUES OP_COMMA VALUEI { asprintf(&$$, "%s, %s", $1, $3); } |
    VALUES OP_COMMA IDENTIFIER { asprintf(&$$, "%s, %s", $1, $3); };

FCALL:
    OP_OP IDENTIFIER PARAMLIST OP_CP { 
        if (strcmp($3, "") == 0)
            asprintf(&$$, "(%s)", $2);
        else
            asprintf(&$$, "(%s%s)", $2, $3);
        
    };

PARAMLIST:
    /* empty */ { $$ = ""; } |
    EXP { $$ = $1; } |
    PARAMLIST EXP { asprintf(&$$, "%s %s", $1, $2); };

EXPB:
    OP_OP KW_AND EXPB EXPB OP_CP { asprintf(&$$, "(and %s %s)", $3, $4); } |
    OP_OP KW_OR EXPB EXPB OP_CP { asprintf(&$$, "(or %s %s)", $3, $4); } |
    OP_OP KW_NOT EXPB OP_CP { asprintf(&$$, "(not %s)", $3); } |
    OP_OP KW_LESS EXP EXP OP_CP { asprintf(&$$, "(< %s %s)", $3, $4); } |
    OP_OP KW_EQUAL EXP EXP OP_CP { asprintf(&$$, "(= %s %s)", $3, $4); } |
    KW_TRUE { asprintf(&$$, "true"); } |
    KW_FALSE { asprintf(&$$, "false"); } |
    IDENTIFIER { $$ = $1; };


%%

int main(int argc, char **argv) {

    if (argc > 2)
        printf("Too many arguments\n");
    if (argc == 2)
    {
        int is_opened = load_file(argv[1]);
        if (is_opened)
        {
            printf("Error: File not found\n");
            return 1;
        }
        yyparse();
        fclose(yyin);
    }
    printf("Interpreter continues until (exit) input\n");

    while (1)
    {
        char input[256];
        printf("> ");
        if (fgets(input, sizeof(input), stdin) == NULL)
            break;
        
        yy_scan_string(input);
        yyparse();
    }

    return 0;
}

int load_file(char *filename)
{
    yyin = fopen(filename, "r");
    if (yyin == NULL) {
        fprintf(stderr, "Cannot open file %s\n", filename);
        return 1;
    }
    return 0;
}


void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}
