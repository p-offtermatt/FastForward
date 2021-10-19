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

\brief Implementation of a class to store events of a distributed run.
*/

#include <config.h>
#include <Net/Net.h>
#include <Witness/Condition.h>
#include <Witness/Event.h>

std::map<arrayindex_t, Event *> Event::current;
std::vector<Event *> Event::events;

/*!
\param t  the transition this event is associated to
\pre The given transition is actually enabled and there exist conditions that
are associated with the input places.
\post For the output places of the transition, new conditions are created.
*/
Event::Event(const arrayindex_t t) : transition(t), target(false)
{
    current[t] = this;
    events.push_back(this);

    // connect this event to the conditions modeling the preplaces of the associated transition; these conditions must be "current"; meaning they are on the last cut of the distributed run
    for (arrayindex_t pre = 0; pre < Net::CardArcs[TR][PRE][t]; ++pre)
    {
        const arrayindex_t p = Net::Arc[TR][PRE][t][pre];
        assert(Condition::current[p]);
        assert(Condition::current[p]->out == NULL);
        Condition::current[p]->out = this;
    }

    // create conditions for the postplaces of the associated transition
    for (arrayindex_t post = 0; post < Net::CardArcs[TR][POST][t]; ++post)
    {
        const arrayindex_t p = Net::Arc[TR][POST][t][post];
        new Condition(p, this);
    }
}

/*!
\param[in,out] o  file to write the events to
\note Pointer addresses are used to identify nodes in the graph.
\pre The given file must be opened.
*/
void Event::dot(FILE *const o)
{
    assert(o);

    for (std::vector<Event *>::iterator e = events.begin(); e != events.end(); ++e)
    {
        if ((*e)->target)
        {
            fprintf(o,
                    "  e%p [label=\"%s\" shape=box width=.5 fixedsize=true color=green style=filled fillcolor=white]\n",
                    reinterpret_cast<void *>(*e), Net::Name[TR][(*e)->transition]);
        }
        else
        {
            fprintf(o, "  e%p [label=\"%s\" shape=box width=.5 fixedsize=true style=filled fillcolor=white]\n",
                    reinterpret_cast<void *>(*e), Net::Name[TR][(*e)->transition]);
        }
    }
}
