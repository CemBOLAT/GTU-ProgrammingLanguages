
%{
    #include <stdio.h>
    #include <string.h>
    #include <stdbool.h>

    #include "y.tab.h"

    float custom_atof(char *input);
%}

COMMENT ";;".*
VALUEI [0-9]+
VALUEF {VALUEI}[:f]{VALUEI}|{VALUEI}[:f]|[:]{VALUEI}
IDENTIFIER  [a-zA-Z][a-zA-Z0-9_]*
STRING  \"[^\"]*\"

INVALID_CHAR [^a-zA-Z0-9_() \t\n\r\b\f\v;,]
INVALID_NUMBER [^0-9() \t\n\r\b\f\v;,]

%%
    /* Pattern Action */
    /* KEYWORDS */

and      { return KW_AND; }
or       { return KW_OR; }
not      { return KW_NOT; }
equal    { return KW_EQUAL; }
less     { return KW_LESS; }
nil      { return KW_NIL; }
list     { return KW_LIST; }
append   { return KW_APPEND; }
concat   { return KW_CONCAT; }
set      { return KW_SET; }
deffun   { return KW_DEFFUN; }
for      { return KW_FOR; }
if       { return KW_IF; }
exit     { return KW_EXIT; }
load     { return KW_LOAD; }
print    { return KW_PRINT; }
true     { return KW_TRUE; }
false    { return KW_FALSE; }
defvar   { return KW_DEFVAR; }
while    { return KW_WHILE; }

    /* COMMENT VALUEs IDENTIFER */

"+"             {return OP_PLUS;}
"-"             {return OP_MINUS;}
"/"             {return OP_DIV;}
"*"             {return OP_MULT;}
"("             {return OP_OP;}
")"             {return OP_CP;}
","             {return OP_COMMA;}
"'"             {return OP_APOSTROPHE;}

    /* COMMENT VALUEs IDENTIFER */

{COMMENT}       { return COMMENT; }
{VALUEF}        { 
                    float result = custom_atof(yytext);
                    yylval.sval = (char*)calloc(16, sizeof(char));
                    snprintf(yylval.sval, sizeof(yylval.sval), "%g", result);
                    return VALUEF;
                }
{VALUEI}        { 
                    float result = custom_atof(yytext);
                    yylval.sval = (char*)calloc(16, sizeof(char));
                    snprintf(yylval.sval, sizeof(yylval.sval), "%g", result);
                    return VALUEI;
                }
{IDENTIFIER}    {yylval.sval = strdup(yytext); return IDENTIFIER;}

{STRING}        {yylval.sval = strdup(yytext); return STRING;}

    /* WHITESPACE */

[ \t\n\r\b\f\v] {;}

    /* INVALID TOKENS */

{IDENTIFIER}{INVALID_CHAR}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}
{VALUEI}{INVALID_NUMBER}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}
{VALUEF}{INVALID_NUMBER}+ { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}

    /* Other syntax gives erros */

. { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext); yyterminate();}

%% 

int yywrap(){
    return 1;
}

float custom_atof(char *input){
    if (strchr(input, ':') != NULL){
        char *first_part = strtok(input, ":");
        char *second_part = strtok(NULL, ":");
        
        if (first_part && second_part) {
            char combined[256];
            snprintf(combined, sizeof(combined), "%s.%s", first_part, second_part);
            return atof(combined);
        } else {
            return atof(first_part);
        }
    }
    else if (strchr(input, 'f') != NULL){
        char *first_part = strtok(input, "f");
        char *second_part = strtok(NULL, "f");

        if (first_part && second_part) {
            char combined[256];
            snprintf(combined, sizeof(combined), "%s.%s", first_part, second_part);
            return atof(combined);
        } else {
            return atof(first_part);
        }
    }
    else {
        return atof(input);
    }
}