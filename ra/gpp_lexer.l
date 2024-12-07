%{
#include <stdio.h>
%}


%%
        /* keywords */
and           { printf("KW_AND\n");     }
or            { printf("KW_OR\n");      }
not           { printf("KW_NOT\n");     }
equal         { printf("KW_EQUAL\n");   }
less          { printf("KW_LESS\n");    }
nil           { printf("KW_NIL\n");     }   
list          { printf("KW_LIST\n");    }
append        { printf("KW_APPEND\n");  }
concat        { printf("KW_CONCAT\n");  }
set           { printf("KW_SET\n");     }
deffun        { printf("KW_DEFFUN\n");     }
for           { printf("KW_FOR\n");     }
if            { printf("KW_IF\n");      }
exit          { printf("KW_EXIT\n"); 
                printf("Exiting...\n");
                return 0;               }
load          { printf("KW_LOAD\n");    }
display       { printf("KW_DISPLAY\n"); }
true          { printf("KW_TRUE\n");    }
false         { printf("KW_FALSE\n");   }
        /* operators */
[+]           { printf("OP_PLUS\n");    }
[-]           { printf("OP_MINUS\n");   }
[/]           { printf("OP_DIV\n");     }
[*]           { printf("OP_MULT\n");    }
[(]           { printf("OP_OP\n");      }
[)]           { printf("OP_CP\n");      }
[,]           { printf("OP_COMMA\n");   }

        /* literal comment identifier */
[0]|[1-9][0-9]*         { printf("VALUEI\n");     }
[0-9]+f[0-9]+           { printf("VALUEF\n");     }

[;]{2}.*                { printf("COMMENT\n");    }

[a-zA-Z][a-zA-Z0-9]*    { printf("IDENTIFIER\n"); }

[ \t\n]+                { /* ignore tab, whitespace and newline */ ;}

[0-9]+[a-zA-Z]+         { printf("SYNTAX_ERROR cannot tokenize %s\n", yytext); yyterminate(); }

.                       { printf("SYNTAX_ERROR invalid character %s\n", yytext); yyterminate(); }

%%
/* */
int yywrap(void) {
    return 1;
}


int main(int argc, char **argv) {
    if (argc == 1) {
        printf("Type exit to quit,\n");
        printf("Enter the string to be parsed: ");
        yylex();
    }
    else if (argc == 2) {
        yyin = fopen(argv[1], "r");
        yylex();
        fclose(yyin);
    }
    else {
        printf("Too many arguments!\n");
        return 0;
    }
    return 0;
}