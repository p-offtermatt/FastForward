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
\status approved 25.01.2012
\todo comment me
*/

#include <config.h>
#include <InputOutput/SimpleString.h>
#include <InputOutput/Socket.h>
#include <InputOutput/Reporter.h>

#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <csignal>

Reporter *rep;

void signal_callback_handler(int signum) __attribute__((noreturn));
void signal_callback_handler(int signum)
{
    rep->message("caught signal: '%s'\n", strsignal(signum));
    exit(EXIT_SUCCESS);
}

int main(int argc, char **argv)
{
    if (UNLIKELY(argc == 2 and !strcmp(argv[1], "--help")))
    {
        printf("No help\n");
        return EXIT_SUCCESS;
    }

    if (UNLIKELY(argc == 2 and !strcmp(argv[1], "--version")))
    {
        printf("No version\n");
        return EXIT_SUCCESS;
    }

    rep = new ReporterStream();

    const int port = 5555;
    signal(SIGTERM, signal_callback_handler);

    Socket s(port);
    rep->message("listening on port %s", rep->markup(MARKUP_FILE, "%d", port).str());
    s.receive();

    return EXIT_SUCCESS;  // LCOV_EXCL_LINE
}
