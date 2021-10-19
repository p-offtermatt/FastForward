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

#ifndef YY_PTNETLOLA_FRONTEND_PARSER_PARSERNET_HH_INCLUDED
# define YY_PTNETLOLA_FRONTEND_PARSER_PARSERNET_HH_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ptnetlola_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    END = 0,
    IDENTIFIER = 258,
    NUMBER = 259,
    KEY_CONSUME = 260,
    KEY_FAIR = 261,
    KEY_PLACE = 262,
    KEY_MARKING = 263,
    KEY_PRODUCE = 264,
    KEY_SAFE = 265,
    KEY_STRONG = 266,
    KEY_TRANSITION = 267,
    KEY_WEAK = 268,
    COLON = 269,
    COMMA = 270,
    SEMICOLON = 271
  };
#endif
/* Tokens.  */
#define END 0
#define IDENTIFIER 258
#define NUMBER 259
#define KEY_CONSUME 260
#define KEY_FAIR 261
#define KEY_PLACE 262
#define KEY_MARKING 263
#define KEY_PRODUCE 264
#define KEY_SAFE 265
#define KEY_STRONG 266
#define KEY_TRANSITION 267
#define KEY_WEAK 268
#define COLON 269
#define COMMA 270
#define SEMICOLON 271

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 47 "Frontend/Parser/ParserNet.yy" /* yacc.c:1909  */

    char* attributeString;
    fairnessAssumption_t attributeFairness;
    ArcList* attributeArcList;

#line 94 "Frontend/Parser/ParserNet.hh" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

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


extern YYSTYPE ptnetlola_lval;
extern YYLTYPE ptnetlola_lloc;
int ptnetlola_parse (void);

#endif /* !YY_PTNETLOLA_FRONTEND_PARSER_PARSERNET_HH_INCLUDED  */
