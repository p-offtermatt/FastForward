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

\brief Implementation of a class to store conditions of a distributed run.
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Net/Net.h>
#include <Witness/Condition.h>

std::map<arrayindex_t, Condition *> Condition::current;
std::vector<Condition *> Condition::conditions;

/*!
\param p  the place this condition is associated to
\param e  the input event of the condition

\note This constructor is either called directly to create conditions for the
initial marking (then, parameter e is left NULL), or it is called by the
constructor of the Event class to create conditions after firing a transition.
*/
Condition::Condition(const arrayindex_t p, Event *e) : place(p), in(e), out(NULL)
{
    current[p] = this;
    conditions.push_back(this);
}

/*!
\param[in,out] o  file to write the conditions to
\note Pointer addresses are used to identify nodes in the graph.
\pre The given file must be opened.
*/
void Condition::dot(FILE *const o)
{
    assert(o);
    extern std::set<arrayindex_t> target_place;

    for (std::vector<Condition *>::iterator b = conditions.begin(); b != conditions.end(); ++b)
    {
        // event structure
        if (RT::args.pathshape_arg == pathshape_arg_eventstructure)
        {
            if ((*b)->in != NULL and (*b)->out != NULL)
            {
                fprintf(o, "  e%p -> e%p\n", reinterpret_cast<void *>((*b)->in),
                        reinterpret_cast<void *>((*b)->out));
            }
            continue;
        }

        // whether the condition is initial
        bool initial = ((*b)->in == NULL);

        // whether the condition is the target
        bool target = ((*b)->out == NULL && target_place.find((*b)->place) != target_place.end());

        // set the condition as target if it has no postset and we are looking for deadlocks
        if (target_place.empty() and (*b)->out == NULL)
        {
            if (Condition::current[(*b)->place] == *b)
            {
                target = true;
            }
        }

        // we filter conditions with empty postset to reduce clutter
        if ((*b)->out == NULL and (not target) and (RT::args.pathshape_arg != pathshape_arg_fullrun))
        {
            continue;
        }

        if (target)
        {
            fprintf(o,
                    "  c%p [label=\"%s\" shape=circle width=.5 fixedsize=true color=green style=filled fillcolor=white]\n",
                    reinterpret_cast<void *>(*b), Net::Name[PL][(*b)->place]);
        }
        else
        {
            if (initial)
            {
                fprintf(o,
                        "  c%p [label=\"%s\" shape=circle width=.5 fixedsize=true color=blue style=filled fillcolor=white]\n",
                        reinterpret_cast<void *>(*b), Net::Name[PL][(*b)->place]);
            }
            else
            {
                fprintf(o,
                        "  c%p [label=\"%s\" shape=circle width=.5 fixedsize=true style=filled fillcolor=white]\n",
                        reinterpret_cast<void *>(*b), Net::Name[PL][(*b)->place]);
            }
        }

        if ((*b)->in != NULL)
        {
            fprintf(o, "  e%p -> c%p\n", reinterpret_cast<void *>((*b)->in),
                    reinterpret_cast<void *>(*b));
        }

        if ((*b)->out != NULL)
        {
            fprintf(o, "  c%p -> e%p\n", reinterpret_cast<void *>(*b),
                    reinterpret_cast<void *>((*b)->out));
        }
    }
}
