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

#pragma once

#include <config.h>

class AutomataTree;

/*!
 \brief one element on the stack for LTL checks
 */
class LTLStackEntry
{
public:
    /// ordinary constructor for entry
    LTLStackEntry(arrayindex_t *f, arrayindex_t cf, arrayindex_t *_states, arrayindex_t cs,
                  arrayindex_t ls, arrayindex_t _lowlink, AutomataTree *_dfs)
    {
        assert(f); //firelist, first processed in firelist
        fl = f;
        current_on_firelist = cf;
        states = _states;
        current_on_statelist = cs;
        length_of_statelists = ls;
        dfs = _dfs;
        lowlink = _lowlink;
    }
    /// copy constructor used by the search stack
    LTLStackEntry(LTLStackEntry &src)
    {
        // copy the states
        current_on_statelist = src.current_on_statelist;
        length_of_statelists = src.length_of_statelists;
        states = new arrayindex_t[current_on_statelist + 1];
        assert(states);
        assert(src.states);
        if (current_on_firelist == 0)
            memcpy(states, src.states,
                   (current_on_statelist + 1) * SIZEOF_ARRAYINDEX_T);
        else
        {
            memcpy(states, src.states, (length_of_statelists) * SIZEOF_ARRAYINDEX_T);
        }

        // copy the firelist
        current_on_firelist = src.current_on_firelist;
        fl = new arrayindex_t[current_on_firelist + 1];
        assert(fl);
        assert(src.fl);
        memcpy(fl, src.fl, (current_on_firelist + 1) * SIZEOF_ARRAYINDEX_T);

        dfs = src.dfs;
        lowlink = src.lowlink;
    }

    ~LTLStackEntry()
    {
        if (states)
        {
            delete[] states;
        }
        fl = NULL;
        dfs = 0;
        states = NULL;
    }
    arrayindex_t *fl;  // array to take a firelist
    arrayindex_t current_on_firelist; // index of first processed element of fl
    arrayindex_t *states;  // array to take a statelist
    arrayindex_t current_on_statelist; // index of first processed element of states
    arrayindex_t length_of_statelists; // number on statelist, needed to reset after one iteration of
    // value of the dfs and lowlink numbers
    AutomataTree *dfs;
    arrayindex_t lowlink;
};


/*!
 \brief one element on the stack for LTL fairness checks
 */
class LTLFairnessStackEntry
{
public:
    /// ordinary constructor for entry
    LTLFairnessStackEntry(arrayindex_t *f, arrayindex_t cf, arrayindex_t *_states, arrayindex_t cs,
                          arrayindex_t ls)
    {
        assert(f); //firelist, first processed in firelist
        fl = f;
        current_on_firelist = cf;
        states = _states;
        current_on_statelist = cs;
        length_of_statelists = ls;
    }
    /// copy constructor used by the search stack
    LTLFairnessStackEntry(LTLFairnessStackEntry &src)
    {
        // copy the states
        current_on_statelist = src.current_on_statelist;
        length_of_statelists = src.length_of_statelists;
        states = new arrayindex_t[current_on_statelist + 1];
        assert(states);
        assert(src.states);
        if (current_on_firelist == 0)
            memcpy(states, src.states,
                   (current_on_statelist + 1) * SIZEOF_ARRAYINDEX_T);
        else
        {
            memcpy(states, src.states, (length_of_statelists) * SIZEOF_ARRAYINDEX_T);
        }

        // copy the firelist
        current_on_firelist = src.current_on_firelist;
        fl = new arrayindex_t[current_on_firelist + 1];
        assert(fl);
        assert(src.fl);
        memcpy(fl, src.fl, (current_on_firelist + 1) * SIZEOF_ARRAYINDEX_T);
    }
    ~LTLFairnessStackEntry()
    {
        if (states)
        {
            delete[] states;
        }
        fl = NULL;
        states = NULL;
    }
    arrayindex_t *fl;  // array to take a firelist
    arrayindex_t current_on_firelist; // index of first processed element of fl
    arrayindex_t *states;  // array to take a statelist
    arrayindex_t current_on_statelist; // index of first processed element of states
    arrayindex_t length_of_statelists; // number on statelist, needed to reset after one iteration of
    // value of the dfs and lowlink numbers
};
