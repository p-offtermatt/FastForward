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
\author Niels
\status new

\ingroup g_frontend

\brief Definition of the frontend error reporting function.
*/

#pragma once

// define the struct YYLTYPE if it is not already known
#if !defined YYLTYPE && !defined YYLTYPE_IS_DECLARED

/*!
\brief a structure for token locations as used by Flex and Bison.

\note This definition is not user-defined, but has to follow exactly this
notation. This code has been taken from the generated headers of Bison.
*/
typedef struct YYLTYPE
{
    int first_line;       ///< the line where the token begins
    int first_column;     ///< the column where the token begins
    int last_line;        ///< the line where the token ends
    int last_column;      ///< the column where the token ends
} YYLTYPE;

// make sure this struct is not defined twice
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

/// error function for lexers and parsers
extern void yyerrors(const char *token, YYLTYPE, const char *format, ...) __attribute__((noreturn));
