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

\brief Implementation of the frontend error RT::rep->orting function.
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Frontend/Parser/error.h>
#include <Core/Runtime.h>
#include <InputOutput/InputOutput.h>
#include <InputOutput/vasprintf.h>

/*!
This generic error RT::rep->orting function is used in all lexers and parsers.
It displays an error message together with the location of the error token. If
the input was read from a file, the error is further printed with some context
to help the user locate and better understand the error message.

\param[in] token   The token that was last read. Hopefully close to the error.
\param[in] loc     The location of the read token in the error file.
\param[in] format  An error message in printf format.

\note Assumes that #RT::currentInputFile is the file that causes this error. If
#RT::currentInputFile is NULL, the parser was called from a string
(command-line parameter) and no detailed error location can be printed.
\post Errors with EXIT_ERROR.

\ingroup g_frontend
*/
void yyerrors(const char *token, YYLTYPE loc, const char *format, ...) __attribute__((noreturn));
void yyerrors(const char *token, YYLTYPE loc, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    char *errormessage = NULL;
    const int res = vasprintf(&errormessage, format, args);
    assert(res != -1);
    RT::rep->status(errormessage);
    free(errormessage);
    va_end(args);

    // only print filename and excerpt if we read from a file/stdin
    if (RT::currentInputFile)
    {
        RT::rep->status("%s:%d:%d - error near '%s'",
                        RT::rep->markup(MARKUP_FILE, basename(const_cast<char *>
                                        (RT::currentInputFile->getFilename()))).str(),
                        loc.first_line, loc.first_column, token);

        RT::currentInputFile->printExcerpt(loc.first_line, loc.first_column, loc.last_line,
                                           loc.last_column);
    }
    else
    {
        RT::rep->status("%d:%d - error near '%s'", loc.first_line, loc.first_column, token);
    }

    RT::rep->abort(ERROR_SYNTAX);
    exit(EXIT_ERROR); // LCOV_EXCL_LINE - needed to corrently recognize noreturn since RT::rep->abort is virtual
}
