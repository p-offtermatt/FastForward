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
\brief implementation of class Reporter, ReporterSocket, and ReporterStream
\author Niels
\status approved 25.01.2012
\ingroup g_reporting
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <InputOutput/Reporter.h>
#include <InputOutput/vasprintf.h>

#include <unistd.h>
#include <cstdio>
#include <cstdarg>
#include <cstdlib>
#include <cerrno>
#include <cstring>

// initialize error messages
const char *Reporter::error_messages[] =
{
    "",
    "syntax error",
    "command line error",
    "file input/output error",
    "thread error",
    "network error"
};


/*---------------------------------------------------------------------------*/

/*!
\param port     the port to use
\param[in] ip   the target IP address
\param verbose  whether to display verbose messages
*/
ReporterSocket::ReporterSocket(u_short port, const char *ip, bool verbose)
    : verbose(verbose), mySocket(Socket(port, ip))
{
}

/*!
\param[in] format  the status message formatted as printf string
*/
void ReporterSocket::message(const char *format, ...) const
{
    char buffer[UDP_BUFFER_SIZE];
    va_list args;
    va_start(args, format);
    vsprintf(buffer, format, args);
    mySocket.send(buffer);
    va_end(args);
}

/*!
\param[in] format  the status message formatted as printf string
\post The message is not printed unless the #verbose member is true.
*/
void ReporterSocket::status(const char *format, ...) const
{
    if (not verbose)
    {
        return;
    }

    char buffer[UDP_BUFFER_SIZE];
    va_list args;
    va_start(args, format);
    vsprintf(buffer, format, args);
    mySocket.send(buffer);
    va_end(args);
}

/*!
\param code    the error code
\todo Handle premature termination with signals.
*/
void ReporterSocket::abort(errorcode_t code) const
{
    char buffer[UDP_BUFFER_SIZE];
    sprintf(buffer, "%s: %s -- aborting [#%02d]", PACKAGE, error_messages[code], code);
    mySocket.send(buffer);

    status("see manual for a documentation of this error");

    if (errno != 0)
    {
        status("last error message: %s", strerror(errno));
    }

    exit(EXIT_ERROR);
}

ReporterSocket::~ReporterSocket()
{
    status("done");
}

/*!
\param format  the string to format
\post Passed string format is formatted according to markup.
\note Memory for res is released by Reporter::~String().
*/
String ReporterSocket::markup(markup_t, const char *format, ...) const
{
    va_list args;
    va_start(args, format);
    char *res = NULL;
    const int r = vasprintf(&res, format, args);
    assert(r != -1);
    va_end(args);
    return String(res);
}

void ReporterSocket::bar(const unsigned int, const unsigned int) const
{
    return;
}

/*---------------------------------------------------------------------------*/

/*!
\param verbose  whether to display verbose messages
\post All member variables for colors are initialited according to the value
      of #useColor.
*/
ReporterStream::ReporterStream(bool verbose) :
    verbose(verbose),
#if !defined(__MINGW32__)
    useColor(isatty(fileno(stderr))  &&(                      // LCOV_EXCL_LINE
                 !strcmp(getenv("TERM"), "linux") ||          // LCOV_EXCL_LINE
                 !strcmp(getenv("TERM"), "cygwin") ||         // LCOV_EXCL_LINE
                 !strcmp(getenv("TERM"), "xterm") ||          // LCOV_EXCL_LINE
                 !strcmp(getenv("TERM"), "xterm-color") ||    // LCOV_EXCL_LINE
                 !strcmp(getenv("TERM"), "xterm-256color"))), // LCOV_EXCL_LINE
#else
    useColor(false),
#endif
    _cr_(useColor ? "\033[0;31m" : ""),
    _cg_(useColor ? "\033[0;32m" : ""),
    _cy_(useColor ? "\033[0;33m" : ""),
    _cb_(useColor ? "\033[0;34m" : ""),
    _cm_(useColor ? "\033[0;35m" : ""),
    _cc_(useColor ? "\033[0;36m" : ""),
    _cl_(useColor ? "\033[0;37m" : ""),
    _cr__(useColor ? "\033[0;4;31m" : ""),
    _cg__(useColor ? "\033[0;4;32m" : ""),
    _cy__(useColor ? "\033[0;4;33m" : ""),
    _cb__(useColor ? "\033[0;4;34m" : ""),
    _cm__(useColor ? "\033[0;4;35m" : ""),
    _cc__(useColor ? "\033[0;4;36m" : ""),
    _cl__(useColor ? "\033[0;4;37m" : ""),
    _cR_(useColor ? "\033[0;1;31m" : ""),
    _cG_(useColor ? "\033[0;1;32m" : ""),
    _cY_(useColor ? "\033[0;1;33m" : ""),
    _cB_(useColor ? "\033[0;1;34m" : ""),
    _cM_(useColor ? "\033[0;1;35m" : ""),
    _cC_(useColor ? "\033[0;1;36m" : ""),
    _cL_(useColor ? "\033[0;1;37m" : ""),
    _c_(useColor ? "\033[0m" : ""),
    _bold_(useColor ? "\033[1m" : ""),
    _underline_(useColor ? "\033[4m" : "")
{
}


/*!
\param[in] format  the status message formatted as printf string
*/
void ReporterStream::message(const char *format, ...) const
{
    fprintf(stderr, "%s: ", markup(MARKUP_TOOL, PACKAGE).str());

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
}


/*!
\param[in] format  the status message formatted as printf string
\post The message is not printed unless the #verbose member is true.
*/
void ReporterStream::status(const char *format, ...) const
{
    if (not verbose)
    {
        return;
    }

    fprintf(stderr, "%s: ", markup(MARKUP_TOOL, PACKAGE).str());

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
}


/*!
\param code    the error code
\todo Handle premature termination with signals.
*/
void ReporterStream::abort(errorcode_t code) const
{
    fprintf(stderr, "%s: %s%s%s%s -- aborting [#%02d]%s\n",
            markup(MARKUP_TOOL, PACKAGE).str(), _cR_, error_messages[code], _c_, _bold_, code, _c_);

    status("see manual for a documentation of this error");

    if (errno != 0)
    {
        status("last error message: %s", markup(MARKUP_IMPORTANT, strerror(errno)).str());
    }

    exit(EXIT_ERROR);
}

/*!
\param markup  how to markup the string
\param[in] format  the string to format
\post Passed string format is formatted according to markup.
\note Memory for res is released by String::~String().
*/
String ReporterStream::markup(markup_t markup, const char *format, ...) const
{
    va_list args;
    va_start(args, format);
    char *message = NULL;
    const int r = vasprintf(&message, format, args);
    assert(r != -1);
    va_end(args);

    char *res = NULL;
    int bytes = -1;

    switch (markup)
    {
    case MARKUP_TOOL:
        bytes = asprintf(&res, "%s%s%s", _cm_, message, _c_);
        break;

    case MARKUP_FILE:
        bytes = asprintf(&res, "%s%s%s", _cb__, message, _c_);
        break;

    case MARKUP_OUTPUT:
        bytes = asprintf(&res, "%s%s%s", _cB_, message, _c_);
        break;

    case MARKUP_GOOD:
        bytes = asprintf(&res, "%s%s%s", _cG_, message, _c_);
        break;

    case MARKUP_BAD:
        bytes = asprintf(&res, "%s%s%s", _cR_, message, _c_);
        break;

    case MARKUP_WARNING:
        bytes = asprintf(&res, "%s%s%s", _cY_, message, _c_);
        break;

    case MARKUP_IMPORTANT:
        bytes = asprintf(&res, "%s%s%s", _bold_, message, _c_);
        break;

    case MARKUP_PARAMETER:
        bytes = asprintf(&res, "%s%s%s", _cC_, message, _c_);
        break;

    case MARKUP_UNIMPORTANT:
        bytes = asprintf(&res, "%s%s%s", _cl_, message, _c_);
        break;
    }

    assert(bytes != -1);
    assert(res);

    // vasprintf uses malloc
    free(message);

    return String(res);
}

/*!
\note Taken from http://www.rosshemsley.co.uk/2011/02/creating-a-progress-bar-in-c-or-any-other-console-app/

\param i  The number of already processed elements
\param n  The total number of elements to be processed.

\note The progress bar is only shown in case the terminal is able to process
ANSI codes. It hence is skipped if LoLA's output is piped to files. Therefore,
most of the output is skipped during testing.
*/
void ReporterStream::bar(const unsigned int i, const unsigned int n) const
{
    assert(i <= n);

    // no progress bar for nonverbose reporters
    if (not verbose)
    {
        return;
    }

    // useColor is a variable that indicates whether we work in a terminal
    // if not, we skip the display of the progress bar
    if (not useColor)
    {
        return;
    }

    // LCOV_EXCL_START

    // whether this function was called before
    static bool called = false;
    // the percentage of the last call
    static int old_p = 0;

    // the width of the progress bar
    static const int w = 72;

    // the ratio of complete-to-incomplete.
    float ratio = static_cast<float>(i) / static_cast<float>(n);

    // the width of the completed bar
    const int c = static_cast<int>(ratio * w);

    // if there is no change since the last call, return
    if (static_cast<int>(ratio * 100) - old_p == 0)
    {
        return;
    }

    // store this call's percentage
    old_p = static_cast<int>(ratio * 100);

    // in all but the first call, clear the line
    if (not called)
    {
        called = true;
    }
    else
    {
        // move cursor up and clear line
        fprintf(stderr, "\033[1A\033[2K");
    }

    // if the progress bar is at 100%, reset the bar
    if (called and i == n)
    {
        called = false;
        old_p = 0;
        return;
    }

    // show the percentage complete
    fprintf(stderr, "%3d%% [", old_p);

    // show the load bar
    for (int x = 0; x < c; x++)
    {
        fprintf(stderr, "%s#", _cl_);
    }

    // fill up the bar
    for (int x = c; x < w; x++)
    {
        fprintf(stderr, " ");
    }

    fprintf(stderr, "%s]\n", _c_);

    // LCOV_EXCL_STOP
}

/*---------------------------------------------------------------------------*/

void ReporterSilent::message(const char *, ...) const
{
    return;
}

void ReporterSilent::status(const char *, ...) const
{
    return;
}

/*!
\param code    the error code
\todo Handle premature termination with signals.
*/
void ReporterSilent::abort(errorcode_t code) const
{
    fprintf(stderr, "%s: %s -- aborting [#%02d]\n",
            PACKAGE, error_messages[code], code);

    status("see manual for a documentation of this error");

    if (errno != 0)
    {
        status("last error message: %s", strerror(errno));
    }

    exit(EXIT_ERROR);
}

/*!
\param format  the string to format
\post Passed string format is formatted according to markup.
\note Memory for res is released by Reporter::~String().
*/
String ReporterSilent::markup(markup_t, const char *format, ...) const
{
    va_list args;
    va_start(args, format);
    char *res = NULL;
    const int r = vasprintf(&res, format, args);
    assert(r != -1);
    va_end(args);
    return String(res);
}

void ReporterSilent::bar(const unsigned int, const unsigned int) const
{
    return;
}
