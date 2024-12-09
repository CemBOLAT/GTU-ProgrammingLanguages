
#ifndef YACC_HELPER_H
#define YACC_HELPER_H


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>


#define ID_SIZE 256
#define FUNCTION_SIZE 16
#define PARAMETER_SIZE 3

extern FILE *yyin;

int yylex();
int yyerror(const char *s)
{
    fprintf(stderr, "Syntax Error: %s\n", s);
    exit(EXIT_FAILURE);
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

typedef struct s_variable {
    char    id[ID_SIZE];
    float   value;
} t_variable;

typedef struct s_function {
    int     parameter_number;
    float   parameter_values[PARAMETER_SIZE];
    char    parameter_names[PARAMETER_SIZE][ID_SIZE];
    char    name[FUNCTION_SIZE];
    float   return_value;
} t_function;

typedef struct s_program_array {
    int         func_size;
    t_function  *functions;

    int         var_size;
    t_variable  *variables;

    float       value_table[PARAMETER_SIZE];
    char        name_table[PARAMETER_SIZE][ID_SIZE];

} t_program_array;

t_program_array program_array;


float get_value(char *id){
    printf("Getting value of %s\n", id);
    for (int i = 0; i < program_array.var_size; i++){
        if (strcmp(program_array.variables[i].id, id) == 0){
            return program_array.variables[i].value;
        }
    }
    fprintf(stderr, "Variable %s is not defined.\n", id);
    exit(EXIT_FAILURE);
}

float define_id(char *id, float value, int is_function){
    if (!is_function){
        for (int i = 0; i < program_array.var_size; i++){
            if (strcmp(program_array.variables[i].id, id) == 0){
                fprintf(stderr, "Variable %s is already defined.\n", id);
                exit(EXIT_FAILURE);
            }
        }
    }
    if (program_array.var_size != 0){
        program_array.variables = (t_variable*)realloc(program_array.variables, sizeof(t_variable)*(program_array.var_size + 1));
    }
    else{
        program_array.variables = (t_variable*)calloc(1, sizeof(t_variable));
    }
    strcpy(program_array.variables[program_array.var_size].id, id);
    program_array.variables[program_array.var_size].value = value;
    program_array.var_size++;
    return value;
}

float set_value(char *id, float value){
    for (int i = 0; i < program_array.var_size; i++){
        if (strcmp(program_array.variables[i].id, id) == 0){
            program_array.variables[i].value = value;
            return value;
        }
    }
    fprintf(stderr, "Variable %s is not defined.\n", id);
    exit(EXIT_FAILURE);
}

float function_call(char *name, int parameter_number){
    for (int i = 0; i < program_array.func_size; i++){
        if (strcmp(program_array.functions[i].name, name) == 0){
            if (program_array.functions[i].parameter_number != parameter_number){
                fprintf(stderr, "Function %s requires %d parameters.\n", name, program_array.functions[i].parameter_number);
                exit(EXIT_FAILURE);
            }
            for (int j = 0; j < parameter_number; j++){
                program_array.functions[i].parameter_values[j] = program_array.value_table[j];
            }
            return program_array.functions[i].return_value;
        }
    }
    fprintf(stderr, "Function %s is not defined.\n", name);
    exit(EXIT_FAILURE);
}

void function_add(char *name, int parameter_number, float return_value){
    for (int i = 0; i < program_array.func_size; i++){
        if (strcmp(program_array.functions[i].name, name) == 0){
            fprintf(stderr, "Function %s is already defined.\n", name);
            exit(EXIT_FAILURE);
        }
    }
    if (program_array.func_size != 0){
        program_array.functions = (t_function*)realloc(program_array.functions, sizeof(t_function)*(program_array.func_size + 1));
    }
    else{
        program_array.functions = (t_function*)calloc(1, sizeof(t_function));
    }
    for (int i = 0; i < parameter_number; i++){
        strcpy(program_array.functions[program_array.func_size].parameter_names[i], program_array.name_table[i]);
    }

    printf("Function %s added with %d parameters.\n", name, parameter_number);
    printf("Return value: %g\n", return_value);
    program_array.functions[program_array.func_size].parameter_number = parameter_number;
    program_array.functions[program_array.func_size].return_value = return_value;
    strcpy(program_array.functions[program_array.func_size].name, name);
    program_array.func_size++;
    program_array.var_size -= parameter_number;
}

float define_value(char *id, float value){
    for (int i = 0; i < program_array.var_size; i++){
        if (strcmp(program_array.variables[i].id, id) == 0){
            fprintf(stderr, "Variable %s is already defined.\n", id);
            exit(EXIT_FAILURE);
        }
    }
    if (program_array.var_size != 0){
        program_array.variables = (t_variable*)realloc(program_array.variables, sizeof(t_variable)*(program_array.var_size + 1));
    }
    else{
        program_array.variables = (t_variable*)calloc(1, sizeof(t_variable));
    }
    strcpy(program_array.variables[program_array.var_size].id, id);
    program_array.variables[program_array.var_size].value = value;
    program_array.var_size++;
    return value;
}

char *append_list(char *list1, char *list2){
    char *result = (char*)calloc(strlen(list1) + strlen(list2) + 2, sizeof(char));
    if (list1[0] == '\0'){
        strcpy(result, list2);
        return result;
    }
    if (list2[0] == '\0'){
        strcpy(result, list1);
        return result;
    }
    strcpy(result, list1);
    strcat(result, ",");
    strcat(result, list2);
    return result;
}

char *concat_list(char *list1, char *list2){
    char *result = (char*)calloc(strlen(list1) + strlen(list2) + 2, sizeof(char));
    
    if (list1[0] == '\0'){
        strcpy(result, list2);
        return result;
    }
    if (list2[0] == '\0'){
        strcpy(result, list1);
        return result;
    }
    strcpy(result, list1);
    strcat(result, ",");
    strcat(result, list2);
    return result;
}

char *make_empty_list(){
    char *result = (char*)calloc(1, sizeof(char));
    result[0] = '\0';
    return result;
}

char *append(char *list, float value){
    char *result = (char*)calloc(strlen(list) + 16, sizeof(char));
    if (list[0] == '\0'){
        sprintf(result, "%g", value);
        return result;
    }
    sprintf(result, "%s,%g", list, value);
    return result;
}

#endif // YACC_HELPER_H