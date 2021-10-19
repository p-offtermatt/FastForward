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

\brief Implementation of a class to organize witness paths.
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <InputOutput/JSON.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Witness/Condition.h>
#include <Witness/Event.h>
#include <Witness/Path.h>

/// we encode the beginnig of a cycle with an index no transition can have
#define BEGIN_CYCLE Net::Card[TR]+1
/// we encode the end of a cycle with an index no transition can have
#define END_CYCLE Net::Card[TR]+2

Path::Path() : initialized(false) {}

/*!
\param transition  index of a transition to be added to the path
\param add_to_front  whether the transition should be added to the fron of the
path - if false, the transition is added to the end of the path (default)
*/
void Path::addTransition(const arrayindex_t transition, bool add_to_front)
{
    if (add_to_front)
    {
        transition_sequence.push_front(transition);
    }
    else
    {
        transition_sequence.push_back(transition);
    }
}

/*!
\param add_to_front  whether the cycle should be added to the fron of the path
- if false, the cycle is added to the end of the path (default)
*/
void Path::beginCycle(bool add_to_front)
{
    if (add_to_front)
    {
        transition_sequence.push_front(BEGIN_CYCLE);
    }
    else
    {
        transition_sequence.push_back(BEGIN_CYCLE);
    }
}

/*!
\param add_to_front  whether the cycle should be added to the fron of the path
 - if false, the cycle is added to the end of the path (default)
*/
void Path::endCycle(bool add_to_front)
{
    if (add_to_front)
    {
        transition_sequence.push_front(END_CYCLE);
    }
    else
    {
        transition_sequence.push_back(END_CYCLE);
    }
}

/*!
\param[in,out] o  file to write the path to
\pre The path must be initialized.
\pre The given file must be opened.
*/
void Path::print(FILE *const o) const
{
    assert(initialized);
    assert(o);

    for (std::list<arrayindex_t>::const_iterator i = transition_sequence.begin();
            i != transition_sequence.end(); ++i)
    {
        if (*i == BEGIN_CYCLE)
        {
            fprintf(o, "===begin of cycle===\n");
        }
        else if (*i == END_CYCLE)
        {
            fprintf(o, "===end of cycle===\n");
        }
        else
        {
            fprintf(o, "%s\n", Net::Name[TR][*i]);
        }
    }
}

/*!
\pre The path must be initialized.
\todo We need to properly take care about cycles
*/
JSON Path::json() const
{
    assert(initialized);
    // the result object
    JSON j;
    // temporary object needed to deal with cycles
    JSON temp;

    for (std::list<arrayindex_t>::const_iterator i = transition_sequence.begin();
            i != transition_sequence.end(); ++i)
    {
        if (*i == BEGIN_CYCLE)
        {
            // Remember current JSON object in temp and add new transitions to
            // fresh array.
            temp = j;
            j = JSON(JSON::array);
        }
        else if (*i == END_CYCLE)
        {
            // Add completed cycle to old JSON object and restore j.
            temp += j;
            j = temp;
        }
        else
        {
            j += Net::Name[TR][*i];
        }
    }

    return j;
}

/*!
\param[in,out] o  file to write the distributed run to
\pre The path must be initialized.
\pre The given file must be opened.
*/
void Path::printRun(FILE *const o) const
{
    assert(initialized);
    assert(o);

    // add conditions for initial marking
    for (arrayindex_t i = 0; i < Net::Card[PL]; ++i)
    {
        if (Marking::Initial[i] > 0)
        {
            new Condition(i);
        }
    }

    // add all events (with surrounding conditions)
    for (std::list<arrayindex_t>::const_iterator it = transition_sequence.begin();
            it != transition_sequence.end(); ++it)
    {
        new Event(*it);
    }

    // if a tranision is the target, add it
    extern std::set<arrayindex_t> target_transition;
    for (std::set<arrayindex_t>::iterator it = target_transition.begin();
            it != target_transition.end(); ++it)
    {
        Event *e = new Event(*it);
        e->target = true;
    }

    // actual dot output
    fprintf(o, "digraph d {\n");
    fprintf(o, "  rankdir=LR;\n\n");
    Event::dot(o);
    Condition::dot(o);
    fprintf(o, "}\n");
}
