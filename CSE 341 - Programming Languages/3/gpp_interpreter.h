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
     KW_PROGN = 258,
     KW_GT = 259,
     KW_AND = 260,
     KW_OR = 261,
     KW_NOT = 262,
     KW_EQUAL = 263,
     KW_LESS = 264,
     KW_NIL = 265,
     KW_LIST = 266,
     KW_APPEND = 267,
     KW_CONCAT = 268,
     KW_SET = 269,
     KW_DEFFUN = 270,
     KW_FOR = 271,
     KW_IF = 272,
     KW_EXIT = 273,
     KW_LOAD = 274,
     KW_DISP = 275,
     KW_TRUE = 276,
     KW_FALSE = 277,
     KW_WHILE = 278,
     DEFV = 279,
     OP_PLUS = 280,
     OP_MINUS = 281,
     OP_DIV = 282,
     OP_MULT = 283,
     OP_OP = 284,
     OP_CP = 285,
     OP_DBLMULT = 286,
     OP_OC = 287,
     OP_CC = 288,
     OP_COMMA = 289,
     COMMENT = 290,
     IDENTIFIER = 291,
     VALUEF = 292,
     VALUEI = 293
   };
#endif
/* Tokens.  */
#define KW_PROGN 258
#define KW_GT 259
#define KW_AND 260
#define KW_OR 261
#define KW_NOT 262
#define KW_EQUAL 263
#define KW_LESS 264
#define KW_NIL 265
#define KW_LIST 266
#define KW_APPEND 267
#define KW_CONCAT 268
#define KW_SET 269
#define KW_DEFFUN 270
#define KW_FOR 271
#define KW_IF 272
#define KW_EXIT 273
#define KW_LOAD 274
#define KW_DISP 275
#define KW_TRUE 276
#define KW_FALSE 277
#define KW_WHILE 278
#define DEFV 279
#define OP_PLUS 280
#define OP_MINUS 281
#define OP_DIV 282
#define OP_MULT 283
#define OP_OP 284
#define OP_CP 285
#define OP_DBLMULT 286
#define OP_OC 287
#define OP_CC 288
#define OP_COMMA 289
#define COMMENT 290
#define IDENTIFIER 291
#define VALUEF 292
#define VALUEI 293




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 74 "gpp_interpreter.y"
{
    float value;
    bool boolean;
    char ID[256];
}
/* Line 1529 of yacc.c.  */
#line 131 "gpp_interpreter.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

