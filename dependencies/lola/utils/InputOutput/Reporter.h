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
\status approved 25.01.2012
\ingroup g_reporting

\brief declaration of class Reporter, ReporterSocket, and ReporterStream
*/

#pragma once

#include <InputOutput/SimpleString.h>
#include <InputOutput/Socket.h>

/*!
\brief error codes for the Reporter::abort function

These values are used to differentiate errors signaled with Reporter::abort.
The code is *not* used as exit code. In case of an error, EXIT_ERROR is always
used. Howerver, the error codes can be used as a reference to a more detailed
documentation of the error.

\note Can be used as index for array Reporter::error_messages.
\ingroup g_reporting
*/
typedef enum
{
    ERROR_SYNTAX = 1,   ///< syntax error
    ERROR_COMMANDLINE,  ///< wrong commandline parameter
    ERROR_FILE,         ///< file input/output error
    ERROR_THREADING,    ///< thread-related error
    ERROR_NETWORK       ///< network error
} errorcode_t;


/*!
\brief markup types for Reporter::markup function
\ingroup g_reporting
*/
typedef enum
{
    MARKUP_TOOL,        ///< markup the name of a tool
    MARKUP_FILE,        ///< markup the name of a file
    MARKUP_OUTPUT,      ///< markup the type of a file
    MARKUP_GOOD,        ///< markup some good output
    MARKUP_BAD,         ///< markup some bad output
    MARKUP_WARNING,     ///< markup a warning
    MARKUP_IMPORTANT,   ///< markup an important message
    MARKUP_PARAMETER,   ///< markup a command-line parameter
    MARKUP_UNIMPORTANT  ///< markup an unimportant message
} markup_t;


/*!
\brief Class to implement reporting functionality
\ingroup g_reporting
*/
class Reporter
{
protected:
    /// error messages (indexed by errorcode_t)
    static const char *error_messages[];

public:
    virtual ~Reporter() {}

    /// markup a string
    virtual String markup(markup_t, const char *, ...) const = 0;

    /// always report
    virtual void message(const char *, ...) const = 0;

    /// only report in verbose mode
    virtual void status(const char *, ...) const = 0;

    /// display error message and abort program
    virtual void abort(errorcode_t) const __attribute__((noreturn)) = 0;

    /// display a progress bar
    virtual void bar(const unsigned int, const unsigned int) const = 0;
};


/*!
\brief Realization of Reporter class to write to Berkeley sockets
\ingroup g_reporting
*/
class ReporterSocket : public Reporter
{
private:
    /// whether verbose reports are desired
    bool verbose;

    /// socket for this reporter
    Socket mySocket;

public:
    ReporterSocket(u_short port, const char *, bool = true);
    ~ReporterSocket();

    /// always report
    void message(const char *, ...) const;

    /// only report in verbose mode
    void status(const char *, ...) const;

    /// display error message and abort program
    void abort(errorcode_t) const __attribute__((noreturn));

    /// markup a string
    String markup(markup_t, const char *, ...) const;

    /// display a progress bar
    void bar(const unsigned int, const unsigned int) const;
};


/*!
\brief Realization of Reporter class to write to standard error
\ingroup g_reporting
*/
class ReporterStream : public Reporter
{
private:
    /// whether verbose reports are desired
    bool verbose;

    /// whether to use colored output
    bool useColor;

    /// set foreground color to red
    const char *_cr_;
    /// set foreground color to green
    const char *_cg_;
    /// set foreground color to yellow
    const char *_cy_;
    /// set foreground color to blue
    const char *_cb_;
    /// set foreground color to magenta
    const char *_cm_;
    /// set foreground color to cyan
    const char *_cc_;
    /// set foreground color to light grey
    const char *_cl_;

    /// set foreground color to red (underlined)
    const char *_cr__;
    /// set foreground color to green (underlined)
    const char *_cg__;
    /// set foreground color to yellow (underlined)
    const char *_cy__;
    /// set foreground color to blue (underlined)
    const char *_cb__;
    /// set foreground color to magenta (underlined)
    const char *_cm__;
    /// set foreground color to cyan (underlined)
    const char *_cc__;
    /// set foreground color to light grey (underlined)
    const char *_cl__;

    /// set foreground color to red (bold)
    const char *_cR_;
    /// set foreground color to green (bold)
    const char *_cG_;
    /// set foreground color to yellow (bold)
    const char *_cY_;
    /// set foreground color to blue (bold)
    const char *_cB_;
    /// set foreground color to magenta (bold)
    const char *_cM_;
    /// set foreground color to cyan (bold)
    const char *_cC_;
    /// set foreground color to light grey (bold)
    const char *_cL_;

    /// reset foreground color
    const char *_c_;
    /// make text bold
    const char *_bold_;
    /// unterline text
    const char *_underline_;

public:
    explicit ReporterStream(bool = true);

    /// always report
    void message(const char *, ...) const;

    /// only report in verbose mode
    void status(const char *, ...) const;

    /// display error message and abort program
    void abort(errorcode_t) const __attribute__((noreturn));

    /// markup a string
    String markup(markup_t, const char *, ...) const;

    /// display a progress bar
    void bar(const unsigned int, const unsigned int) const;
};

/*!
\brief Realization of Reporter class to suppress all messages
\ingroup g_reporting
*/
class ReporterSilent : public Reporter
{
    /// always report
    void message(const char *, ...) const;

    /// only report in verbose mode
    void status(const char *, ...) const;

    /// display error message and abort program
    void abort(errorcode_t) const __attribute__((noreturn));

    /// markup a string
    String markup(markup_t, const char *, ...) const;

    /// display a progress bar
    void bar(const unsigned int, const unsigned int) const;
};
