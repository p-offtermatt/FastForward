/****************************************************************************
  This file is part of LoLA.

  LoLA is free software: you can redistribute it and/or modify it under the
  terms of the GNU Affero General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  LoLA is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
  more details.

  You should have received a copy of the GNU Affero General Public License
  along with LoLA. If not, see <http://www.gnu.org/licenses/>.
****************************************************************************/

/*!
\file
\brief lexic for LoLA low-level Petri nets
\author Karsten
\status approved 25.01.2012
\ingroup g_frontend

Mainly copied from LoLA1

\todo Herausfinden, ob es Probleme bei zu langen Kommentaren/Bezeichnern gibt.
Idee: Maximallänge angeben.
\todo Präfix hinzufügen?
*/

/* yylineno: we want line numbering
   nounput: we don't need yyunput() */
%option yylineno
%option nounput
%option noyywrap
%option outfile="lex.yy.c"
%option prefix="ptnetlola_"

%{
#include <Core/Runtime.h>
#include <Core/Dimensions.h>            // for yylval union
#include <Frontend/SymbolTable/ArcList.h> // for yylval union
#include <Frontend/Parser/ParserNet.hh>
#include <InputOutput/InputOutput.h>

extern void ptnetlola_error(char const* mess);
unsigned int ptnetlola_colno = 1;

/*!
\brief This macro is executed prior to the matched rule's action.

We use this macro to set set #ptnetlola_lloc to the positions of #ptnetlola_text. It further
manages the current column number #ptnetlola_colno. See Flex's manual
http://flex.sourceforge.net/manual/Misc-Macros.html for more information on
the macro.
*/
#define YY_USER_ACTION \
  ptnetlola_lloc.first_line = ptnetlola_lloc.last_line = ptnetlola_lineno; \
  ptnetlola_lloc.first_column = ptnetlola_colno; \
  ptnetlola_lloc.last_column = ptnetlola_colno+ptnetlola_leng-1; \
  ptnetlola_colno += ptnetlola_leng;

%}

%s IN_COMMENT

%%

 /* from http://flex.sourceforge.net/manual/How-can-I-match-C_002dstyle-comments_003f.html */
"/*"                   { BEGIN(IN_COMMENT); }
<IN_COMMENT>"*/"       { BEGIN(INITIAL); }
<IN_COMMENT>[^*\n\r]+  { /* comments */ }
<IN_COMMENT>"*"        { /* comments */ }
<IN_COMMENT>[\n\r]     { /* comments */ }
"{"[^\n\r]*"}"         { /* comments */ }

CONSUME                { return KEY_CONSUME; }
FAIR                   { return KEY_FAIR; }
MARKING                { return KEY_MARKING; }
PLACE                  { return KEY_PLACE; }
PRODUCE                { return KEY_PRODUCE; }
SAFE                   { return KEY_SAFE; }
STRONG                 { return KEY_STRONG; }
TRANSITION             { return KEY_TRANSITION; }
WEAK                   { return KEY_WEAK; }

\:                     { return COLON; }
,                      { return COMMA; }
\;                     { return SEMICOLON; }

[\n\r]                 { ptnetlola_colno = 1; /* whitespace */ }
[\t ]                  { /* whitespace */ }

[0-9]+                 { ptnetlola_lval.attributeString = strdup(ptnetlola_text); return NUMBER; }
[^,;:()\t \n\r\{\}]+   { ptnetlola_lval.attributeString = strdup(ptnetlola_text); return IDENTIFIER; }

.                      { ptnetlola_error("lexical error"); }
