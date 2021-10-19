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
\ingroup g_io

\brief declaration of classes IO, Input, and Output
*/

#pragma once

#include <Core/Dimensions.h>
#include <string>

// forward declaration
class Reporter;

/*!
\brief wrapper class for file input and output

This class wraps convenience methods around a standard C-style FILE*. Its main
purpose is to automatically close files in the moment its IO object leaves the
scope.

\ingroup g_io
*/
class IO
{
protected:
    /// a reporter for status messages
    static Reporter *r;

    /// the filename (or empty in case of stdin/stdout)
    const std::string filename;

    /// the kind of the file
    const std::string kind;

    /// the filepointer
    FILE *fp;

    /// default constructor
    IO(FILE *, std::string, std::string);

    /// destructor
    ~IO();

public:
    /// a setter for the reporter to use
    static void setReporter(Reporter *);

    /// implicit cast to FILE* (return fp)
    operator FILE *() const;

    /// return filename
    const char *getFilename() const;
};


/*!
\brief wrapper class for output files
\ingroup g_io
*/
class Output : public IO
{
public:
    /// output to file (given kind and optional filename)
    Output(std::string, std::string = "-");
};


/*!
\brief wrapper class for input files
\ingroup g_io
*/
class Input : public IO
{
public:
    /// input from file (given kind and optional filename)
    Input(std::string, std::string = "-");

    /// given the location of some text prints an excerpt of the file
    void printExcerpt(int first_line, int first_column,
                      int last_line, int last_column) const;
};
