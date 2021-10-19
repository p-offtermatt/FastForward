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

\brief Randomly selects a transition from a fire list.
*/

#pragma once

class ChooseTransition;

/*!
\brief randomly chooses a transition

In contrast to reachability, the stubborn sets for the deadlock check are not
ordered in the sense that transitions listed earlier should be fired with a
higher probability. Hence, this class implements a choose function that randomly
selects one transition from a given firelist. This should be more efficient and
without bias.
*/
class ChooseTransitionRandomly : public ChooseTransition
{
public:
    virtual arrayindex_t choose(NetState &, arrayindex_t cardfirelist, arrayindex_t *firelist);
};
