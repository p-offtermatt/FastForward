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

\brief declaration of class Socket
*/

#pragma once

#include <config.h>
#include <Core/Dimensions.h>

#ifdef WIN32
#include <winsock.h>
#include <windows.h>
#include <stdint.h>
typedef uint32_t socklen_t;
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

#include <sys/types.h>
#include <cstddef>

/*!
\brief class encapsulating Berkely Sockets (using UDP datagrams)
\ingroup g_reporting
*/
class Socket
{
private:
    /// the socket
    const int sock;

    /// the address
    sockaddr_in address;

    /// whether we are listening (server) or sending (client)
    const unsigned listening : 1;

    /// whether to fail if an error occurs
    const bool failonerror;

public:
    /// create a socket - port is mandatory, destination address optional
    Socket(u_short port, const char *destination = NULL, bool failonerror = true);

    /// close the socket
    ~Socket();

    /// receive incoming messages (does not return)
    void receive() const;

    /// wait for a specific message
    char *waitFor(const char *message) const;

    /// send a message
    void send(const char *message) const;
};
