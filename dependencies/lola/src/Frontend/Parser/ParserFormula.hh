/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_PTFORMULA_FRONTEND_PARSER_PARSERFORMULA_HH_INCLUDED
# define YY_PTFORMULA_FRONTEND_PARSER_PARSERFORMULA_HH_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ptformula_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    END = 0,
    _CTLOPERATOR_ = 258,
    IDENTIFIER = 259,
    NUMBER = 260,
    _RELEASE_ = 261,
    _NEXTSTATE_ = 262,
    _INITIAL_ = 263,
    _DEADLOCK_ = 264,
    _FORMULA_ = 265,
    _MAX_ = 266,
    _AND_ = 267,
    _NOT_ = 268,
    _OR_ = 269,
    _XOR_ = 270,
    _iff_ = 271,
    _ALLPATH_ = 272,
    _ALWAYS_ = 273,
    _EVENTUALLY_ = 274,
    _EXPATH_ = 275,
    _UNTIL_ = 276,
    _REACHABLE_ = 277,
    _INVARIANT_ = 278,
    _IMPOSSIBLE_ = 279,
    _notequal_ = 280,
    _implies_ = 281,
    _equals_ = 282,
    _plus_ = 283,
    _minus_ = 284,
    _times_ = 285,
    _leftparenthesis_ = 286,
    _rightparenthesis_ = 287,
    _greaterthan_ = 288,
    _lessthan_ = 289,
    _greaterorequal_ = 290,
    _lessorequal_ = 291,
    _semicolon_ = 292,
    _TRUE_ = 293,
    _FALSE_ = 294,
    _FIREABLE_ = 295,
    _omega_ = 296,
    _colon_ = 297
  };
#endif
/* Tokens.  */
#define END 0
#define _CTLOPERATOR_ 258
#define IDENTIFIER 259
#define NUMBER 260
#define _RELEASE_ 261
#define _NEXTSTATE_ 262
#define _INITIAL_ 263
#define _DEADLOCK_ 264
#define _FORMULA_ 265
#define _MAX_ 266
#define _AND_ 267
#define _NOT_ 268
#define _OR_ 269
#define _XOR_ 270
#define _iff_ 271
#define _ALLPATH_ 272
#define _ALWAYS_ 273
#define _EVENTUALLY_ 274
#define _EXPATH_ 275
#define _UNTIL_ 276
#define _REACHABLE_ 277
#define _INVARIANT_ 278
#define _IMPOSSIBLE_ 279
#define _notequal_ 280
#define _implies_ 281
#define _equals_ 282
#define _plus_ 283
#define _minus_ 284
#define _times_ 285
#define _leftparenthesis_ 286
#define _rightparenthesis_ 287
#define _greaterthan_ 288
#define _lessthan_ 289
#define _greaterorequal_ 290
#define _lessorequal_ 291
#define _semicolon_ 292
#define _TRUE_ 293
#define _FALSE_ 294
#define _FIREABLE_ 295
#define _omega_ 296
#define _colon_ 297

/* Value type.  */

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE ptformula_lval;
extern YYLTYPE ptformula_lloc;
int ptformula_parse (void);

#endif /* !YY_PTFORMULA_FRONTEND_PARSER_PARSERFORMULA_HH_INCLUDED  */
