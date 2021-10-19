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
\brief implementation of class String
\author Niels
\status new
\ingroup g_reporting
*/

#include <config.h>
#include <InputOutput/SimpleString.h>

#include <cstdlib>
#include <cstring>
#include <cstdio>

/*!
\param s  Pointer to NULL-terminated string to store.
\note The given string is NOT copied! Only the pointer is stored and it will be
freed by the constructor. DO NOT use this constructor with const char*, string
literals, or anything that should live longer than this object
*/
String::String(char *s) : s(s)
{
    assert(s);
}

/*!
\pre memory for member s was allocated outside this object using malloc
\post memory for member s is released
*/
String::~String()
{
    free(s);
}

/*!
\return the wrapped NULL-terminated string s
*/
const char *String::str() const
{
    return s;
}
