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
\ingroup g_runtime

\brief Definition of exit and termination handlers.
*/

#pragma once

#include <Core/Dimensions.h>

/*!
\brief Collection of event handlers

This struct encapsulates functionality to process events while running LoLA.

\ingroup g_runtime
*/
struct Handlers
{
private:
    /// timestamp of the start of LoLA
    static time_t start_time;

    /// a thread that runs the termination handler
    static pthread_t terminationHandler_thread;

    /// remote termination handler
    static void *remoteTerminationHandler(void *);

public:
    /// exit handler
    static void exitHandler();

    /// print statistics
    static void statistics();

    /// handler for new
    static void newHandler() __attribute__((noreturn));

    /// install the exit handler
    static void installExitHandler();

    /// install the termination handlers
    static void installTerminationHandlers();

    /// signal termination handler
    static void signalTerminationHandler(int) __attribute__((noreturn));
};
