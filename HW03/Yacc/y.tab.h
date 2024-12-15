/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     VALUEI = 258,
     VALUEF = 259,
     IDENTIFIER = 260,
     STRING = 261,
     KW_AND = 262,
     KW_OR = 263,
     KW_NOT = 264,
     KW_LESS = 265,
     KW_LIST = 266,
     KW_CONCAT = 267,
     KW_DEFFUN = 268,
     KW_IF = 269,
     KW_LOAD = 270,
     KW_TRUE = 271,
     KW_EQUAL = 272,
     KW_NIL = 273,
     KW_APPEND = 274,
     KW_SET = 275,
     KW_FOR = 276,
     KW_EXIT = 277,
     KW_PRINT = 278,
     KW_FALSE = 279,
     KW_DEFVAR = 280,
     KW_WHILE = 281,
     OP_PLUS = 282,
     OP_MINUS = 283,
     OP_MULT = 284,
     OP_DIV = 285,
     OP_OP = 286,
     OP_CP = 287,
     OP_COMMA = 288,
     OP_APOSTROPHE = 289,
     COMMENT = 290
   };
#endif
/* Tokens.  */
#define VALUEI 258
#define VALUEF 259
#define IDENTIFIER 260
#define STRING 261
#define KW_AND 262
#define KW_OR 263
#define KW_NOT 264
#define KW_LESS 265
#define KW_LIST 266
#define KW_CONCAT 267
#define KW_DEFFUN 268
#define KW_IF 269
#define KW_LOAD 270
#define KW_TRUE 271
#define KW_EQUAL 272
#define KW_NIL 273
#define KW_APPEND 274
#define KW_SET 275
#define KW_FOR 276
#define KW_EXIT 277
#define KW_PRINT 278
#define KW_FALSE 279
#define KW_DEFVAR 280
#define KW_WHILE 281
#define OP_PLUS 282
#define OP_MINUS 283
#define OP_MULT 284
#define OP_DIV 285
#define OP_OP 286
#define OP_CP 287
#define OP_COMMA 288
#define OP_APOSTROPHE 289
#define COMMENT 290




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 16 "gpp_interpreter.y"
{
    char *sval;
}
/* Line 1529 of yacc.c.  */
#line 123 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

