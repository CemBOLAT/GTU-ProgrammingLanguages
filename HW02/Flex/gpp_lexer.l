%{
    /* Definitions */
    #include <stdio.h>
    #include <string.h>
%}

/* Comment Delimeter is ';;' */
COMMENT ";;".*
/* unsigned integers or unsigned fractions with : or f  in between 2:3 or 2: or :3 */
VALUEI [0]|[1-9][0-9]*
VALUEF {VALUEI}[:f]{VALUEI}|{VALUEI}[:f]|[:]{VALUEI}
/* identifier: Any combination of alphabetical characters, digits and “_” with only leading alphabetical characters. */
IDENTIFIER  [a-zA-Z][a-zA-Z0-9_]*

/* Invalid characters for identifier */
INVALID_CHAR [^a-zA-Z0-9_() \t\n\r\b\f\v;]
INVALID_NUMBER [^0-9() \t\n\r\b\f\v;]

%%
    /* Pattern Action */
    /* KEYWORDS */

and      { printf("KW_AND\n");}
or       { printf("KW_OR\n");}
not      { printf("KW_NOT\n");}
equal    { printf("KW_EQUAL\n");}
less     { printf("KW_LESS\n");}
nil      { printf("KW_NIL\n");}
list     { printf("KW_LIST\n");}
append   { printf("KW_APPEND\n");}
concat   { printf("KW_CONCAT\n");}
set      { printf("KW_SET\n");}
deffun   { printf("KW_DEFFUN\n");}
for      { printf("KW_FOR\n");}
if       { printf("KW_IF\n");}
exit     { printf("KW_EXIT\n");}
load     { printf("KW_LOAD\n");}
print    { printf("KW_DISP\n");}
true     { printf("KW_TRUE\n");}
false    { printf("KW_FALSE\n");}

    /* COMMENT VALUEs IDENTIFER */

"+"             {printf("OP_PLUS\n");}
"-"             {printf("OP_MINUS\n");}
"/"             {printf("OP_DIV\n");}
"*"             {printf("OP_MULT\n");}
"("             {printf("OP_OP\n");}
")"             {printf("OP_CP\n");}
","             {printf("OP_COMMA\n");}

    /* COMMENT VALUEs IDENTIFER */
{COMMENT}       {printf ("COMMENT\n");}
{VALUEF}        {printf("VALUEF\n");}
{VALUEI}        {printf("VALUEI\n");}
{IDENTIFIER}    {printf("IDENTIFIER\n");}

    /* WHITESPACE */

[ \t\n\r\b\f\v] {;}

    /* INVALID TOKENS */

{IDENTIFIER}{INVALID_CHAR}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}
{VALUEI}{INVALID_NUMBER}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}
{VALUEF}{INVALID_NUMBER}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}

    /* Other syntax gives erros */

. { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}

%% 

int yywrap() {}

int main(int argc , char** argv)
{
    if (argc > 2)
        printf("Too many arguments\n");
    else if (argc == 1)
    {
        printf("Interpreter continues until \"quit\" input\n");
        while (1)
        {
            char input[256];
            printf("> ");
            fgets(input, sizeof(input), stdin);
            if (strcmp(input, "quit\n") == 0)
                break;
            yy_scan_string(input);
            yylex();
        }
    }
    else if (argc == 2)
    {
        FILE *fp;

        fp = fopen(argv[1], "r");
        if (fp == NULL)
        {
            printf("File not found\n");
            return 1;
        }
        yyin = fp;

        yylex();
        fclose(fp);
    }
    return 0;
}