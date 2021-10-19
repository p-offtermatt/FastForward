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
\author Karsten
\status approved 21.02.2012
\ingroup g_io

\brief declaration of compressed file input/output methods

Input and outout from/to a file in compressed format. We generate two separate
files that can be read in arbitrary order. In this version, we use an ASCII
file where data are separated by spaces and newlines.

\todo These functions should be moved into a struct -- maybe even Net.
*/

#pragma once

#include <Core/Dimensions.h>

class ParserPTNet;

/// write names to file
void WriteNameFile(FILE *const);

/// write compressed net structure to file
void WriteNetFile(FILE *const);

/// read names from file and annotated parsed net
void ReadNameFile(FILE *const, ParserPTNet *const);

/// read compressed net structure from file
void ReadNetFile(FILE *const);
