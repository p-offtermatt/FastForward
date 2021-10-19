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
\author Christian Koch
\status new
\ingroup ctl

\brief Implementation of the CTL base class.
*/

#include <Formula/CTL/CTLFormula.h>

CTLFormulaResult CTLFormula::getCachedResult(void *payload) const
{
    return static_cast<CTLFormulaResult>(((*(reinterpret_cast<uint8_t *>(payload) + (index >> 3))) >>
                                          (index & 7)) & 3);
}

void CTLFormula::setCachedResult(void *payload, CTLFormulaResult result)
{
    uint8_t cachedValue = *(reinterpret_cast<uint8_t *>(payload) + (index >> 3));
    if (result & 1)
    {
        cachedValue |= (1 << index);
    }
    else
    {
        cachedValue &= ~(1 << index);
    }

    if (result & 2)
    {
        cachedValue |= (1 << (index + 1));
    }
    else
    {
        cachedValue &= ~(1 << (index + 1));
    }

    *(reinterpret_cast<uint8_t *>(payload) + (index >> 3)) = cachedValue;
}
