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
\status new

\brief We translate the net into a boolean formula that represents the
siphon/trap property of the net. We let minisat solve the formula.
*/

#pragma once

#include <Core/Dimensions.h>

// if you change the values, adjust them in minisat2(..)!
typedef enum { SIPHON_PROPERTY_TRUE = 0, SIPHON_PROPERTY_FALSE = 1, SIPHON_INHOMOGENIOUS = 2, SIPHON_INCONCLUSIVE = 3, SIPHON_INDETERMINATE = 4} siphon_result_t;

// 0 = all siphons contain marked trap
// 1 = there is a siphon without marked trap
// 2 = the net does not satisfy the requirements for applying the check
// 3 = there is a siphon where the formula is too short for finding the max trap
// 4 = minisat returned an indeterminate value, for instance, by reaching limits

siphon_result_t lola2minisat();

