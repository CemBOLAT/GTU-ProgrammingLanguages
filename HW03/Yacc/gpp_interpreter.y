%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdbool.h>
    #include "yacc_helper.h"


    extern void yy_scan_string(const char *str);
    int b_exp = -1;

%} 


%union {
    float value;
    bool boolean;
    char *id;
}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_PRINT KW_TRUE KW_FALSE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA
%token COMMENT
%token <value> VALUEF VALUEI
%token <id> IDENTIFIER

%type <value> INPUT EXP EXPLIST SET START
%type <boolean> EXPB


%start START

%%

START: 
    INPUT { 
        if (b_exp == 1)
        {
            printf("True\n");
            b_exp = -1;
        }
        else if (b_exp == 0)
        {
            printf("False\n");
            b_exp = -1;
        }
        else{
            /* dont print unnecessary zeros */
            printf("%g\n", $1);
        }
     }

INPUT:
    COMMENT { ; } |
    OP_OP KW_EXIT OP_CP { printf("Terminating the program\n"); exit(0); } |
    OP_OP KW_PRINT EXP OP_CP { $$ = $3; } |
    EXPLIST  { $$ = $1; }; 

EXP:
    OP_OP OP_PLUS EXP EXP OP_CP { $$ = $3 + $4; } |
    OP_OP OP_MINUS EXP EXP OP_CP { $$ = $3 - $4; } |
    OP_OP OP_MULT EXP EXP OP_CP { $$ = $3 * $4; } |
    OP_OP OP_DIV EXP EXP OP_CP { $$ = $3 / $4; } |
    VALUEF { $$ = $1; } |
    VALUEI { $$ = $1; } |
    IDENTIFIER { $$ = get_value($1); } |
    SET { $$ = $1; } |
    OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP { if ($3) $$ = $4; else $$ = $5; } |
    OP_OP KW_IF EXPB EXPLIST OP_CP { if ($3) $$ = $4; else $$ = 0; } |
    OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP { $$ = $6; }; |
    EXPB { if ($1) b_exp = 1; else b_exp = 0; $$ = $1; };

SET:
    OP_OP KW_SET IDENTIFIER EXPLIST OP_CP { $$ = set_value($3, $4); };

EXPB:
    OP_OP KW_EQUAL EXP EXP OP_CP { $$ = ($3 == $4); } |
    OP_OP KW_LESS EXP EXP OP_CP { $$ = ($3 < $4); } |
    OP_OP KW_AND EXPB EXPB OP_CP { $$ = $3 && $4; } |
    OP_OP KW_OR EXPB EXPB OP_CP { $$ = $3 || $4; } |
    OP_OP KW_NOT EXPB OP_CP { $$ = !$3; } |
    KW_TRUE { $$ = true; } |
    KW_FALSE { $$ = false; } |
    OP_OP KW_NIL OP_CP { $$ = 0; };

EXPLIST:
    EXP { $$ = $1; } |
    EXPLIST EXP  { $$ = $2; };

%% 

int main(int argc , char** argv)
{
    if (argc > 2)
        printf("Too many arguments\n");
    if (argc == 2)
    {
        int is_opened = load_file(argv[1]);
        if (!is_opened)
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
        char input[256];  // Giriş için buffer
        printf("> ");     // Her giriş satırından önce > yazılır
        if (fgets(input, sizeof(input), stdin) == NULL)  // Giriş al
            break;  // EOF ya da hata durumunda döngüyü kır
        
        yy_scan_string(input);  // Lexer için input'u ayarla
        yyparse();              // Parse et
    }

    return 0;
}
