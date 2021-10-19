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

#include <InputOutput/vasprintf.h>

#ifdef HAVE_DOPRNT

#include <stdlib.h>

#warning Using user-defined implementation of 'vasprintf', as this function is not available on this system.

int vasprintf(char **ret, const char *format, va_list args)
{
    va_list copy;
    va_copy(copy, args);

    /* Make sure it is determinate, despite manuals indicating otherwise */
    *ret = 0;

    int count = vsnprintf(NULL, 0, format, args);
    if (count >= 0)
    {
        char *buffer = (char *)malloc(count + 1);
        if (buffer != NULL)
        {
            count = vsnprintf(buffer, count + 1, format, copy);
            if (count < 0)
            {
                free(buffer);
            }
            else
            {
                *ret = buffer;
            }
        }
    }
    va_end(args);  // Each va_start() or va_copy() needs a va_end()

    return count;
}

#warning Using user-defined implementation of 'asprintf', as this function is not available on this system.

int asprintf(char **strp, const char *fmt, ...)
{
    int size;
    va_list args;
    va_start(args, fmt);
    size = vasprintf(strp, fmt, args);
    va_end(args);
    return size;
}
#endif

#ifndef HAVE_GETLINE

#define _GETLINE_BUFLEN 255

#include <stdlib.h>

#warning Using user-defined implementation of 'getline', as this function is not available on this system.

ssize_t getline(char **lineptr, size_t *n, FILE *stream)
{
    int c;
    size_t alloced = 0;
    char *linebuf;

    if (*lineptr == NULL)
    {
        linebuf = (char *)malloc(sizeof(char) * (_GETLINE_BUFLEN + 1));
        alloced = _GETLINE_BUFLEN + 1;
    }
    else
    {
        linebuf = *lineptr;
        alloced = *n;
    }
    ssize_t linelen = 0;

    do
    {
        c = fgetc(stream);
        if (c == EOF)
        {
            break;
        }
        if (linelen >= alloced)
        {
            linebuf = (char *)realloc(linebuf, sizeof(char) * (alloced + _GETLINE_BUFLEN + 1));
            assert(linebuf);
            alloced += (_GETLINE_BUFLEN + 1);
        }
        *(linebuf + linelen) = (unsigned char)c;
        linelen++;
    }
    while (c != '\n');

    /* empty line means EOF or some other error */
    if (linelen == 0)
    {
        if (linebuf != NULL && *lineptr == NULL)
        {
            free(linebuf);
            linebuf = NULL;
        }
        linelen = -1;
        *n = alloced;
    }
    else
    {
        if (linebuf != NULL)
        {
            linebuf[linelen] = '\0';
        }
        *n = alloced;
        *lineptr = linebuf;
    }

    return linelen;
}

#endif
