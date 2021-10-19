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

#include <Core/Dimensions.h>
#include <Symmetry/AutomorphismGraph.h>
#include <Core/Runtime.h>
#include <Symmetry/Constraints.h>

arrayindex_t Vertex::card = 0;

Vertex::Vertex() : succ(NULL), colorSucc(NULL), pre(NULL), colorPre(NULL)
{
    ++card;

    // the following variables are needed once for every thread and
    // in addition for the main thread that descends to id

    property[DOM] = new int64_t [RT::args.threads_arg + 1];
    property[COD] = new int64_t [RT::args.threads_arg + 1];
    inConstraint[DOM] = new Constraint * [RT::args.threads_arg + 1];
    inConstraint[COD] = new Constraint * [RT::args.threads_arg + 1];
}

/*!
Quicksort

\param arcs  array of nodes
\param color  arc inscriptions
\param from  start index of the area to sort
\param to  end index of the area to sort
*/
void Vertex::sortArcs(Vertex **arcs, arrayindex_t *color, arrayindex_t from, arrayindex_t to)
{
    arrayindex_t blue = from;
    arrayindex_t white = from + 1;
    arrayindex_t red = to;

    arrayindex_t pivot = color[0];

    arrayindex_t tmpcolor;
    Vertex *tmpvertex;
    while (red > white)
    {
        if (color[white] > pivot)
        {
            // swap element at white with element left of red
            tmpcolor = color[white];
            tmpvertex = arcs[white];
            color[white] = color[--red];
            arcs[white] = arcs[red];
            color[red] = tmpcolor;
            arcs[red] = tmpvertex;
        }
        else
        {
            if (color[white] == pivot)
            {
                ++white;
            }
            else
            {
                // swap element at white with element at blue
                tmpcolor = color[white];
                tmpvertex = arcs[white];
                color[white] = color[blue];
                arcs[white++] = arcs[blue];
                color[blue] = tmpcolor;
                arcs[blue++] = tmpvertex;
            }
        }
    }
    if (blue > from)
    {
        sortArcs(arcs, color, from, blue);
    }
    if (red < to)
    {
        sortArcs(arcs, color, red, to);
    }
}

Vertex::~Vertex()
{
    delete[] colorPre;
    delete[] pre;
    delete[] colorSucc;
    delete[] succ;
    delete[] property[DOM];
    delete[] property[COD];
    delete[] inConstraint[DOM];
    delete[] inConstraint[COD];
}
