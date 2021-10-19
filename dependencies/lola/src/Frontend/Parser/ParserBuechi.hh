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

#ifndef YY_PTBUECHI_FRONTEND_PARSER_PARSERBUECHI_HH_INCLUDED
# define YY_PTBUECHI_FRONTEND_PARSER_PARSERBUECHI_HH_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ptbuechi_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    END = 0,
    IDENTIFIER = 258,
    NUMBER = 259,
    _accept_ = 260,
    _buechi_ = 261,
    _braceleft_ = 262,
    _braceright_ = 263,
    _comma_ = 264,
    _then_ = 265,
    _colon_ = 266,
    _INITIAL_ = 267,
    _AND_ = 268,
    _NOT_ = 269,
    _OR_ = 270,
    _XOR_ = 271,
    _iff_ = 272,
    _notequal_ = 273,
    _implies_ = 274,
    _equals_ = 275,
    _plus_ = 276,
    _minus_ = 277,
    _times_ = 278,
    _leftparenthesis_ = 279,
    _rightparenthesis_ = 280,
    _greaterthan_ = 281,
    _lessthan_ = 282,
    _greaterorequal_ = 283,
    _lessorequal_ = 284,
    _semicolon_ = 285,
    _TRUE_ = 286,
    _FALSE_ = 287,
    _FIREABLE_ = 288,
    _ALWAYS_ = 289,
    _EVENTUALLY_ = 290,
    _NEXTSTATE_ = 291,
    _UNTIL_ = 292,
    _ALLPATH_ = 293,
    _EXPATH_ = 294,
    _REACHABLE_ = 295,
    _INVARIANT_ = 296,
    _IMPOSSIBLE_ = 297
  };
#endif
/* Tokens.  */
#define END 0
#define IDENTIFIER 258
#define NUMBER 259
#define _accept_ 260
#define _buechi_ 261
#define _braceleft_ 262
#define _braceright_ 263
#define _comma_ 264
#define _then_ 265
#define _colon_ 266
#define _INITIAL_ 267
#define _AND_ 268
#define _NOT_ 269
#define _OR_ 270
#define _XOR_ 271
#define _iff_ 272
#define _notequal_ 273
#define _implies_ 274
#define _equals_ 275
#define _plus_ 276
#define _minus_ 277
#define _times_ 278
#define _leftparenthesis_ 279
#define _rightparenthesis_ 280
#define _greaterthan_ 281
#define _lessthan_ 282
#define _greaterorequal_ 283
#define _lessorequal_ 284
#define _semicolon_ 285
#define _TRUE_ 286
#define _FALSE_ 287
#define _FIREABLE_ 288
#define _ALWAYS_ 289
#define _EVENTUALLY_ 290
#define _NEXTSTATE_ 291
#define _UNTIL_ 292
#define _ALLPATH_ 293
#define _EXPATH_ 294
#define _REACHABLE_ 295
#define _INVARIANT_ 296
#define _IMPOSSIBLE_ 297

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


extern YYSTYPE ptbuechi_lval;
extern YYLTYPE ptbuechi_lloc;
int ptbuechi_parse (void);

#endif /* !YY_PTBUECHI_FRONTEND_PARSER_PARSERBUECHI_HH_INCLUDED  */
