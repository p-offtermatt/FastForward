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

\brief class implementation for atomic state predicates
*/

#include <config.h>
#include <Core/Handlers.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>
#include <Net/LinearAlgebra.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Marking.h>
#include <CoverGraph/CoverGraph.h>
#include <Formula/FormulaInfo.h>

/*!
\brief creates a state predicate with a formal sum of p places with positive
factor, n places with negative factor, and constant k particular places are
added using addPos and addNeg

\param p  number of places with positive factor
\param n  number of places with negative factor
\param k  constant

\todo Schleifen behandeln - können evtl. rausgenommen werden
*/
AtomicStatePredicate::AtomicStatePredicate(arrayindex_t p, arrayindex_t n, int k) :
    posPlaces(new arrayindex_t[p]), negPlaces(new arrayindex_t[n]),
    posMult(new capacity_t[p]), negMult(new capacity_t[n]), cardPos(p),
    cardNeg(n), up(NULL), cardUp(0), threshold(k), sum(0), original(true)
{
    parent = NULL;
}

AtomicStatePredicate::AtomicStatePredicate() :
    posPlaces(NULL), negPlaces(NULL), posMult(NULL), negMult(NULL), cardPos(0),
    cardNeg(0), up(NULL), cardUp(0), threshold(0), sum(0), original(true)
{}

AtomicStatePredicate::~AtomicStatePredicate()
{
    if (!original)
    {
        return;
    }
    delete[] posPlaces;
    delete[] negPlaces;
    delete[] posMult;
    delete[] negMult;
    free(up);
}

/*!
\param i  position in atomic state predicate
\param p  place
\param m  positive factor
*/
void AtomicStatePredicate::addPos(arrayindex_t i, arrayindex_t p, capacity_t m)
{
    assert(i < cardPos);
    posPlaces[i] = p;
    posMult[i] = m;
}

/*!
\param i  position in atomic state predicate
\param p  place
\param m  negative factor
*/
void AtomicStatePredicate::addNeg(arrayindex_t i, arrayindex_t p, capacity_t m)
{
    assert(i < cardNeg);
    negPlaces[i] = p;
    negMult[i] = m;
}

arrayindex_t AtomicStatePredicate::getUpSet(arrayindex_t *stack, bool *onstack, bool *) const
{
    assert(onstack);
    arrayindex_t stackpointer = 0;
    for (arrayindex_t i = 0; i < cardUp; i++)
    {
        arrayindex_t element;
        if (!onstack[element = up[i]])
        {
            onstack[element] = true;
            stack[stackpointer++] = element;
        }
    }
    return stackpointer;
}

arrayindex_t AtomicStatePredicate::getDownSet(arrayindex_t *stack, bool *onstack, bool *) const
{
    assert(onstack);
    arrayindex_t stackpointer = 0;
    for (arrayindex_t i = 0; i < cardDown; i++)
    {
        arrayindex_t element;
        if (!onstack[element = down[i]])
        {
            onstack[element] = true;
            stack[stackpointer++] = element;
        }
    }
    return stackpointer;
}

/*!
If value of this changes, it needs to be propagated to its parent. The
parameter is the change in the formal sum k_1 p_1 + ... + k_n p_n between the
previously considered marking and the current marking. Having a precomputed
value for this change, evaluation of the formula is accelerated.
*/
void AtomicStatePredicate::update(NetState &, int delta)
{
    sum += delta;
    if (sum <= threshold && !value)
    {
        value = true;
        if (parent)
        {
            parent->updateFT(position);
        }
        return;
    }
    if (sum > threshold && value)
    {
        value = false;
        if (parent)
        {
            parent->updateTF(position);
        }
        return;
    }
}

/*!
Evaluation starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. Marking::Current.

\param ns  net state to evaluate the formula
*/
void AtomicStatePredicate::evaluate(NetState &ns)
{
    sum = 0;
    for (arrayindex_t i = 0; i < cardPos; ++i)
    {
        sum += ns.Current[posPlaces[i]] * posMult[i];
    }
    for (arrayindex_t i = 0; i < cardNeg; ++i)
    {
        sum -= ns.Current[negPlaces[i]] * negMult[i];
    }

    value = (sum <= threshold);
}

/*!
Evaluation with Omega starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. Marking::Current.

\param ns  net state to evaluate the formula
*/
void AtomicStatePredicate::evaluateOmega(NetState &ns)
{
    sum = 0;
    unknown = false;
    for (arrayindex_t i = 0; i < cardPos; ++i)
    {
        if (ns.Current[posPlaces[i]] == OMEGA)
        {
            sum = OMEGA;
        }
        else if (sum < OMEGA)
        {
            sum += ns.Current[posPlaces[i]] * posMult[i];
        }
    }
    assert(sum >= 0);
    for (arrayindex_t i = 0; i < cardNeg; ++i)
    {
        if (ns.Current[negPlaces[i]] == OMEGA)
        {
            if (sum == OMEGA && threshold < OMEGA)
            {
                unknown = true;
            }
            else
            {
                sum = -OMEGA;
            }
        }
        else if (sum > -OMEGA && sum < OMEGA)
        {
            sum -= ns.Current[negPlaces[i]] * negMult[i];
        }
    }

    if ((sum == OMEGA || sum == -OMEGA) && threshold < OMEGA && threshold > -OMEGA)
    {
        unknown = true;
    }
    value = (sum <= threshold);
}

arrayindex_t AtomicStatePredicate::countAtomic() const
{
    return 1;
}

arrayindex_t AtomicStatePredicate::collectAtomic(AtomicStatePredicate **c)
{
    c[0] = this;
    return 1;
}


arrayindex_t AtomicStatePredicate::countDeadlock() const
{
    return 0;
}

arrayindex_t AtomicStatePredicate::collectDeadlock(DeadlockPredicate **)
{
    return 0;
}

arrayindex_t AtomicStatePredicate::countFireable() const
{
    return 0;
}

arrayindex_t AtomicStatePredicate::collectFireable(FireablePredicate **)
{
    return 0;
}

void AtomicStatePredicate::initUpSet()
{
	cardUp = 0;
        up = reinterpret_cast<arrayindex_t *>(malloc(Net::Card[TR] * SIZEOF_ARRAYINDEX_T));
}

void AtomicStatePredicate::finitUpSet()
{
    // shrink up array to size actually needed
    up = reinterpret_cast<arrayindex_t *>(realloc(up, cardUp * SIZEOF_ARRAYINDEX_T));
}

void AtomicStatePredicate::addToUpSet(arrayindex_t t)
{
	up[cardUp++] = t;
}

void AtomicStatePredicate::initDownSet()
{
	cardDown = 0;
        down = reinterpret_cast<arrayindex_t *>(malloc(Net::Card[TR] * SIZEOF_ARRAYINDEX_T));
}

void AtomicStatePredicate::finitDownSet()
{
    // shrink up array to size actually needed
    down = reinterpret_cast<arrayindex_t *>(realloc(down, cardDown * SIZEOF_ARRAYINDEX_T));
}

void AtomicStatePredicate::addToDownSet(arrayindex_t t)
{
	down[cardDown++] = t;
}

// LCOV_EXCL_START
bool AtomicStatePredicate::DEBUG__consistency(NetState &ns)
{
    // 1. check sum
    int s = 0;
    for (arrayindex_t i = 0; i < cardPos; i++)
    {
        s += posMult[i] * ns.Current[posPlaces[i]];
    }
    for (arrayindex_t i = 0; i < cardNeg; i++)
    {
        s -= negMult[i] * ns.Current[negPlaces[i]];
    }
    assert(s == sum);
    if (value)
    {
        assert(sum <= threshold);
    }
    else
    {
        assert(sum > threshold);
    }
    /* if (this != top)
     {
         assert(parent);
     }*/
    return true;
}
// LCOV_EXCL_STOP

/*!
\param parent  the parent predicate for the new, copied, object
*/
StatePredicate *AtomicStatePredicate::copy(StatePredicate *parent)
{
    AtomicStatePredicate *af = new AtomicStatePredicate(0, 0, 0);
    af->value = value;
    af->position = position;
    af->parent = parent;
    // we can copy the pointers, so use the same arrays as they are not changed!
    af->posPlaces = posPlaces;
    af->negPlaces = negPlaces;
    af->posMult = posMult;
    af->negMult = negMult;
    af->cardPos = cardPos;
    af->cardNeg = cardNeg;
    af->threshold = threshold;
    af->sum = sum;
    af->up = up;
    af->cardUp = cardUp;
    af->original = false;
    return af;
}

arrayindex_t AtomicStatePredicate::getSubs(const StatePredicate *const **)
const
{
    return 0;
}

StatePredicate *AtomicStatePredicate::negate()
{
    AtomicStatePredicate *af = new AtomicStatePredicate(cardNeg, cardPos, - threshold - 1);
    for (arrayindex_t i = 0; i < cardPos; i++)
    {
        af->addNeg(i, posPlaces[i], posMult[i]);
    }
    for (arrayindex_t i = 0; i < cardNeg; i++)
    {
        af->addPos(i, negPlaces[i], negMult[i]);
    }
    return af;
}

FormulaInfo *AtomicStatePredicate::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_atomic;
    Info->cardChildren = 0;
    Info->f = const_cast<AtomicStatePredicate *>(this);
    return Info;
}

int AtomicStatePredicate::countSubFormulas() const
{
    return 1;
}

/*!
Reduces all factors and the threshold with the gcd thereof.

\note This function needs to be called after all addPos/addNeg calls are
complete.
*/
void AtomicStatePredicate::reduceFactors()
{
    // make sure there is at least one multiplicity
    assert(cardPos + cardNeg > 0);

    // initialize result value
    int64_t gcd = (cardPos > 0) ? posMult[0] : negMult[0];

    // find gcd of threshold and all multiplicities
    for (arrayindex_t i = 0; i < cardPos; i++)
    {
        gcd = ggt(gcd, posMult[i]);
    }
    for (arrayindex_t i = 0; i < cardNeg; i++)
    {
        gcd = ggt(gcd, negMult[i]);
    }

    // add threshold to the result
    if (threshold != 0)
    {
        gcd = ggt(gcd, threshold);
    }

    assert(gcd);

    // make sure the gcd is positive
    gcd = (gcd < 0) ? -gcd : gcd;

    // apply ggt
    threshold /= gcd;
    for (arrayindex_t i = 0; i < cardPos; i++)
    {
        assert(posMult[i] % gcd == 0);
        posMult[i] /= gcd;
    }
    for (arrayindex_t i = 0; i < cardNeg; i++)
    {
        assert(negMult[i] % gcd == 0);
        negMult[i] /= gcd;
    }
}

char * AtomicStatePredicate::toString()
{
	int size = 1;
	char * result = (char *) malloc(sizeof(char));
	result[0] = '\0';

	// positive terms
	if(cardPos == 0)
	{
		size +=2;
		// Use a tmp pointer to avoid memleakOnRealloc
		char * resultTmp = (char *) realloc(result, size * sizeof(char));
		if (resultTmp == NULL)
		{
			// Could not realloc - free and exit
			free(result);
			RT::rep->status("realloc failed");
                        RT::rep->abort(ERROR_MEMORY);
		}
		result = resultTmp;
		sprintf(result+strlen(result),"0 ");
	}
	for(int i = 0; i < cardPos; i++)
	{
		size +=16 + strlen(Net::Name[PL][posPlaces[i]]);
		// Use a tmp pointer to avoid memleakOnRealloc
		char * resultTmp = (char *) realloc(result, size * sizeof(char));
		if (resultTmp == NULL)
		{
			// Could not realloc - free and exit
			free(result);
			RT::rep->status("realloc failed");
                        RT::rep->abort(ERROR_MEMORY);
		}
		result = resultTmp;
		if(i!=0)
		{
			sprintf(result+strlen(result)," + ");
		}
		sprintf(result + strlen(result), "%d * %s", posMult[i],Net::Name[PL][posPlaces[i]]);
	}
	
	// negative terms
	for(int i = 0; i < cardNeg; i++)
	{
		size +=16 + strlen(Net::Name[PL][negPlaces[i]]);
		// Use a tmp pointer to avoid memleakOnRealloc
		char * resultTmp = (char *) realloc(result, size * sizeof(char));
		if (resultTmp == NULL)
		{
			// Could not realloc - free and exit
			free(result);
			RT::rep->status("realloc failed");
                        RT::rep->abort(ERROR_MEMORY);
		}
		result = resultTmp;
		sprintf(result+strlen(result)," - ");
		sprintf(result + strlen(result), "%d * %s", negMult[i],Net::Name[PL][negPlaces[i]]);
	}
	// constant
	if(threshold >= 0)
	{
		size += 14;
		// Use a tmp pointer to avoid memleakOnRealloc
		char * resultTmp = (char *) realloc(result, size * sizeof(char));
		if (resultTmp == NULL)
		{
			// Could not realloc - free and exit
			free(result);
			RT::rep->status("realloc failed");
                        RT::rep->abort(ERROR_MEMORY);
		}
		result = resultTmp;
		sprintf(result + strlen(result), " <= %d", threshold);
	}
	else
	{
		size += 19;
		// Use a tmp pointer to avoid memleakOnRealloc
		char * resultTmp = (char *) realloc(result, size * sizeof(char));
		if (resultTmp == NULL)
		{
			// Could not realloc - free and exit
			free(result);
			RT::rep->status("realloc failed");
                        RT::rep->abort(ERROR_MEMORY);
		}
		result = resultTmp;
		sprintf(result + strlen(result), " <= 0 - %d", -threshold);
	}
	return result;
	
}


