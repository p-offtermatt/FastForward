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
\ingroup g_reporting

\brief declaration of class String
*/

#pragma once

/*!
\brief string class to avoid STL's std::string

This class wraps a standard NULL-terminated string to have better control over
its memory deallocation.

\note An operator for `const char*` is not provided, as it would not be usable
inside printf-calls.

\ingroup g_reporting
*/
class String
{
private:
    /// payload - is freed in destructor
    char *s;

public:
    /// constructor (does only copy pointer, not content)
    explicit String(char *s);

    /// destructor - frees payload
    ~String();

    /// getter for s
    const char *str() const;
};
