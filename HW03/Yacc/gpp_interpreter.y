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
    char *string;
    float value;
    bool boolean;
    char *id;
}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_PRINT KW_TRUE KW_FALSE KW_DEFVAR
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA OP_APOSTROPHE
%token COMMENT
%token <value> VALUEF VALUEI
%token <id> IDENTIFIER

%type <value> INPUT EXP EXPLIST SET START PARAMLIST FCALL
%type <boolean> EXPB

%type <string> LIST_INPUT VALUES LIST


%start START

%%

START: 
    /* empty */ { $$ = 0; } |
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
        else if (b_exp != -2){
            /* dont print unnecessary zeros */
            printf("%g\n", $1);
        }
        b_exp = -1;
        $$ = $1;
     }

INPUT:
    COMMENT { ; } |
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
    OP_OP KW_LOAD IDENTIFIER OP_CP { $$ = load_file($3); } |
    OP_OP KW_DEFFUN IDENTIFIER OP_OP PARAMLIST OP_CP EXPLIST OP_CP {function_add($3, $5, $7); $$ = 0.0; } |
    EXPB { if ($1) b_exp = 1; else b_exp = 0; $$ = $1; } |
    OP_OP KW_EXIT OP_CP { printf("Terminating the program\n"); exit(0); } |
    OP_OP KW_PRINT EXP OP_CP { $$ = $3; } |
    LIST_INPUT {
        b_exp = -2;
        char *temp = $1;
        if (temp[0] == '\0')
            printf("nil");
        else{
            printf("%s" , temp);
        }
        printf("\n");
        $$ = 0;
    } |
    FCALL { $$ = $1; };


LIST_INPUT:
    OP_OP KW_APPEND LIST LIST OP_CP { $$ = append_list($3, $4); } |
    OP_OP KW_LIST VALUES OP_CP { $$ = $3; } |
    OP_OP KW_CONCAT LIST LIST OP_CP { $$ = concat_list($3, $4); };

LIST:
    OP_APOSTROPHE OP_OP VALUES OP_CP { $$ = $3; } |
    OP_APOSTROPHE OP_OP OP_CP { $$ = make_empty_list(); } |
    OP_OP KW_LIST VALUES OP_CP { $$ = $3; } |
    KW_NIL { $$ = make_empty_list(); };

VALUES:
    VALUES VALUEF { $$ = append($1, $2); } |
    VALUES VALUEI { $$ = append($1, $2); } |
    VALUEF { $$ = append(make_empty_list(), $1); } |
    VALUEI { $$ = append(make_empty_list(), $1); };

FCALL:
    OP_OP IDENTIFIER OP_CP {
        $$ = function_call($2, 0.0);
    } |
    OP_OP IDENTIFIER EXP OP_CP {
        program_array.value_table[0] = $3;
        $$ = function_call($2, 1.0);
    } |
    OP_OP IDENTIFIER EXP EXP OP_CP {
        program_array.value_table[0] = $3;
        program_array.value_table[1] = $4;
        $$ = function_call($2, 2.0);
    } |
    OP_OP IDENTIFIER EXP EXP EXP OP_CP {
        program_array.value_table[0] = $3;
        program_array.value_table[1] = $4;
        program_array.value_table[2] = $5;
        $$ = function_call($2, 3.0);
    };
    

PARAMLIST:
    /* empty */ { $$ = 0; } |
    IDENTIFIER { 
        define_id($1, 0, true); 
        strcpy(program_array.name_table[0], $1); 
        $$ = 1.0; } |
    IDENTIFIER IDENTIFIER { 
        define_id($1, 0, true); 
        strcpy(program_array.name_table[0], $1); 
        define_id($2, 0, true); 
        strcpy(program_array.name_table[1], $2); 
        $$ = 2.0; } |
    IDENTIFIER IDENTIFIER IDENTIFIER{ 
        define_id($1, 0, true); 
        strcpy(program_array.name_table[0], $1); 
        define_id($2, 0, true); 
        strcpy(program_array.name_table[1], $2); 
        define_id($3, 0, true); 
        strcpy(program_array.name_table[2], $3); 
        $$ = 3.0; };

SET:
    OP_OP KW_SET IDENTIFIER EXP OP_CP { $$ = set_value($3, $4); }; |
    OP_OP KW_DEFVAR IDENTIFIER EXP OP_CP { $$ = define_value($3, $4); };

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