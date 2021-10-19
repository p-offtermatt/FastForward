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

\brief simple property (only SET of states needs to be computed to check it).
*/

#pragma once

#include <config.h>
#include <Exploration/SearchStack.h>
#include <Stores/Store.h>
#include <Witness/Path.h>

class NetState;
class Firelist;

/*!
\brief one element on the stack for simple properties

A simple stack entry contains two elements
- a firelist (as an array of numbers)
- the current index on the firelist
 */
class SimpleStackEntry
{
public:
    /// ordinary constructor for entry
    SimpleStackEntry(arrayindex_t *f, arrayindex_t c)
    {
        assert(f);    //firelist, first processed in firelist
        current = c;
        fl = f;
    }
    /// copy constructor used by the search stack
    SimpleStackEntry(const SimpleStackEntry &src)
    {
        current = src.current;
        fl = new arrayindex_t[current + 1];
        assert(fl);
        assert(src.fl);
        memcpy(fl, src.fl, (current + 1)*SIZEOF_ARRAYINDEX_T);
    }
    ~SimpleStackEntry()
    {
        if (fl)
        {
            delete[] fl;
        }
        fl = NULL;
    }
    arrayindex_t *fl;  // array to take a firelist
    arrayindex_t current; // index of first processed element of fl
};


/*!
\brief simple property, being always false

This property is one of the simplest possible properties, being false for every marking.
Also this class is the base class for all property, that only need to be evaluated on one single marking.
*/
class SimpleProperty
{
public:
    virtual ~SimpleProperty() { }

    /// the witness path
    SearchStack<SimpleStackEntry> stack;

    /// value of property in current state
    bool value;

    /// return a witness path
    virtual Path path()
    {
        static Path p;

        if (not p.initialized)
        {
            p.initialized = true;
            while (stack.StackPointer > 0)
            {
                SimpleStackEntry &s = stack.top();
                p.addTransition(s.fl[s.current], true);
                stack.pop();
            }
        }

        return p;
    }

    /// prepare for search
    virtual bool initProperty(NetState &)
    {
        return false;
    }

    /// check property in Marking::Current, use after fire. Argument is transition just fired.
    virtual bool checkProperty(NetState &, arrayindex_t)
    {
        return false;
    }

    /// check property in Marking::Current, use after backfire. Argument is transition just backfired.
    virtual bool updateProperty(NetState &, arrayindex_t)
    {
        return false;
    }

    /// create a new simple property exactly as the current one
    virtual SimpleProperty *copy()
    {
        return new SimpleProperty();
    }

    /// check property in Marking::Current with Omegas.
    virtual bool checkOmegaProperty(NetState &)
    {
        return false;
    }

    /// detect if checkOmegaProperty returned with unknown result
    virtual bool isUnknown()
    {
        return false;
    }
};
