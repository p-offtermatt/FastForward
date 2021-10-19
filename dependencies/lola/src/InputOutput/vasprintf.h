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
This header provides functions that are not covered by the C/C++ standard, but
are extensions. Some exotic operating systems provide no support for this
functions, so we need to implement them on our own.
*/

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <config.h>

/*
Taken from http://everycity.co.uk/alasdair/2011/07/vasprintf-and-asprintf-on-solaris-10/
*/

/*
It is very difficult to find whether vasprintf and asprintf are actually
usable. The configure script's AC_CHECK_FUNC seems only to check whether the
compiler has no conflict, but not whether the function is actually callable. A
workaround is to use the function AC_FUNC_VPRINTF which checks whether vprintf
and vsprintf are usable. If this fails, HAVE_DOPRNT is defined and it is safe
to assume that vasprintf and asprintf are not present either.

Note this whole mess is only due to Solaris' broken support for GNU
extensions...
*/
#ifdef HAVE_DOPRNT

#include <stdarg.h>
int vasprintf(char **ret, const char *format, va_list args);
int asprintf(char **strp, const char *fmt, ...);
#endif

/*
Taken from https://www.redhat.com/archives/open-scap-list/2011-May/msg00021.html
*/

#ifndef HAVE_GETLINE

#include <stdio.h>
#include <stddef.h>
#include <unistd.h>

ssize_t getline(char **lineptr, size_t *n, FILE *stream);

#endif

#ifdef __cplusplus
}
#endif
