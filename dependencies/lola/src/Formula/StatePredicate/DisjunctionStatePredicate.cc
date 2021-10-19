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

\brief class implementation for disjunction state predicates
*/

#include <Core/Dimensions.h>
#include <Formula/StatePredicate/StatePredicate.h>
#include <Formula/StatePredicate/DisjunctionStatePredicate.h>
#include <Formula/FormulaInfo.h>
#include <Net/Net.h>


DisjunctionStatePredicate::DisjunctionStatePredicate(arrayindex_t n) :
    sub(new StatePredicate*[n]), cardSub(n)
{
    parent = NULL;
}

DisjunctionStatePredicate::~DisjunctionStatePredicate()
{
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        delete sub[i];
    }
    delete[] sub;
}

void DisjunctionStatePredicate::addSub(arrayindex_t i, StatePredicate *f)
{
    assert(i < cardSub);
    sub[i] = f;
    sub[i]->position = i;
    sub[i]->parent = this;
}

arrayindex_t DisjunctionStatePredicate::getUpSet(arrayindex_t *stack, bool *onstack,
        bool *needEnabled) const
{
    // call only if this formula is false
    assert(cardSat == 0);

    arrayindex_t stackpointer = 0;
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        bool needEn;
        stackpointer += sub[i]->getUpSet(stack + stackpointer, onstack, &needEn);
        *needEnabled = *needEnabled || needEn;
        assert(stackpointer <= Net::Card[TR]);
    }
    return stackpointer;
}

/*!
If value of this changes, the parent formula is triggered for updating. This
means that updating is started at the leafs of the formula tree.  Parts of the
formula that did not change are not examined.

\param i  position of this formula in the parent's subformula list
*/
void DisjunctionStatePredicate::updateTF(arrayindex_t i)
{
    // assumption: satisfied left, unsatisfied right

    // --> sub[cardSat] is first unsatisfied
    if (cardSat-- == 1)
    {
        value = false;
        if (parent)
        {
            parent->updateTF(position);
        }
    }

    StatePredicate *tmp = sub[cardSat];
    sub[cardSat] = sub[i];
    sub[i] = tmp;
    sub[i]->position = i;
    sub[cardSat]->position = cardSat;
}

/*!
If value of this changes, the parent formula is triggered for updating. This
means that updating is started at the leafs of the formula tree. Parts of the
formula that did not change are not examined.

\param i  position of this formula in the parent's subformula list
*/
void DisjunctionStatePredicate::updateFT(arrayindex_t i)
{
    // assumption: satisfied left, unsatisfied right

    // --> sub[cardSat] is first satisfied
    StatePredicate *tmp = sub[cardSat];
    sub[cardSat] = sub[i];
    sub[i] = tmp;
    sub[i]->position = i;
    sub[cardSat]->position = cardSat;

    if (++cardSat == 1)
    {
        value = true;
        if (parent)
        {
            parent->updateFT(position);
        }
    }
}



/*!
Evaluation starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. Marking::Current.

\param ns  net state to evaluate the formula
*/
void DisjunctionStatePredicate::evaluate(NetState &ns)
{
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        sub[i]->evaluate(ns);
    }
    arrayindex_t left = 0;
    arrayindex_t right = cardSub;

    // sort satisfied to left, unsat to right of sub list
    // loop invariant: formulas left of left (not including left) are satisfied,
    // formulas right of right (including right) are unsatisfied
    while (true)
    {
        while (left < cardSub && sub[left]->value)
        {
            ++left;
        }
        while (right > 0 && !sub[right - 1]->value)
        {
            --right;
        }
        if (left >= right) // array sorted
        {
            break;
        }
        assert(left < cardSub);
        assert(right > 0);
        assert(right <= cardSub);
        StatePredicate *tmp = sub[left];
        sub[left++] = sub[--right];
        sub[right] = tmp;
    }
    cardSat = left;

    value = (cardSat > 0);

    // update position in sub formulas
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        sub[i]->position = i;
    }
}

/*!
Evaluation with Omega starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. Marking::Current.

\param ns  net state to evaluate the formula including omega values
*/
void DisjunctionStatePredicate::evaluateOmega(NetState &ns)
{
    bool false_unknown(false);
    arrayindex_t true_unknown(0);
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        sub[i]->evaluateOmega(ns);
        if (sub[i]->unknown)
        {
            if (sub[i]->value)
            {
                ++true_unknown;
            }
            else
            {
                false_unknown = true;
            }
        }
    }
    arrayindex_t left = 0;
    arrayindex_t right = cardSub;

    // sort satisfied to left, unsat to right of sub list
    // loop invariant: formulas left of left (not including left) are satisfied,
    // formulas right of right (including right) are unsatisfied
    while (true)
    {
        while (left < cardSub && sub[left]->value)
        {
            ++left;
        }
        while (right > 0 && !sub[right - 1]->value)
        {
            --right;
        }
        if (left >= right) // array sorted
        {
            break;
        }
        assert(left < cardSub);
        assert(right > 0);
        assert(right <= cardSub);
        StatePredicate *tmp = sub[left];
        sub[left++] = sub[--right];
        sub[right] = tmp;
    }
    cardSat = left;

    value = (cardSat > 0);
    unknown = false;
    if (!value && false_unknown)
    {
        unknown = true;
    }
    if (value && cardSat == true_unknown)
    {
        unknown = true;
    }

    // update position in sub formulas
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        sub[i]->position = i;
    }
}

arrayindex_t DisjunctionStatePredicate::countAtomic() const
{
    arrayindex_t result = 0;

    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        result += sub[i]->countAtomic();
    }
    return result;
}

arrayindex_t DisjunctionStatePredicate::collectAtomic(AtomicStatePredicate **p)
{
    arrayindex_t offset = 0;
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        offset += sub[i]->collectAtomic(p + offset);
    }
    return offset;
}

arrayindex_t DisjunctionStatePredicate::countDeadlock() const
{
    arrayindex_t result = 0;

    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        result += sub[i]->countDeadlock();
    }
    return result;
}

arrayindex_t DisjunctionStatePredicate::countFireable() const
{
    arrayindex_t result = 0;

    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        result += sub[i]->countFireable();
    }
    return result;
}

arrayindex_t DisjunctionStatePredicate::collectDeadlock(DeadlockPredicate **p)
{
    arrayindex_t offset = 0;
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        offset += sub[i]->collectDeadlock(p + offset);
    }
    return offset;
}

arrayindex_t DisjunctionStatePredicate::collectFireable(FireablePredicate **p)
{
    arrayindex_t offset = 0;
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        offset += sub[i]->collectFireable(p + offset);
    }
    return offset;
}

// LCOV_EXCL_START
bool DisjunctionStatePredicate::DEBUG__consistency(NetState &ns)
{
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        assert(sub[i]->DEBUG__consistency(ns));
        assert(sub[i]->position == i);
        assert(sub[i]->parent == this);
        assert(sub[i] != this);
        for (arrayindex_t j = 0; j < cardSub; j++)
        {
            if (i != j)
            {
                assert(sub[i] != sub[j]);
            }
        }
        if (i < cardSat)
        {
            assert(sub[i]->value);
        }
        if (i >= cardSat)
        {
            assert(!sub[i]->value);
        }
    }
    assert(cardSat <= cardSub);
    if (cardSat)
    {
        assert(value);
    }
    else
    {
        assert(!value);
    }
    /*if (this != top)
    {
        assert(parent);
    }*/
    return true;
}

// LCOV_EXCL_STOP

/*!
\param parent  the parent predicate for the new, copied, object
*/
StatePredicate *DisjunctionStatePredicate::copy(StatePredicate *parent)
{
    DisjunctionStatePredicate *dsp = new DisjunctionStatePredicate(cardSub);
    dsp->cardSub = cardSub;
    dsp->cardSat = cardSat;
    dsp->value = value;
    dsp->position = position;
    dsp->parent = parent;
    // copy all sub-predicates, and give them the _new_ disjunction as parent
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        dsp->sub[i] = sub[i]->copy(dsp);
    }
    return dsp;
}

arrayindex_t DisjunctionStatePredicate::getSubs(const StatePredicate *const **subs) const
{
    *subs = sub;
    return cardSub;
}

bool DisjunctionStatePredicate::isOrNode() const
{
    return true;
}

FormulaInfo *DisjunctionStatePredicate::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();

    Info->tag = formula_or;
    Info->cardChildren = cardSub;
    Info->statePredicateChildren = new StatePredicate * [cardSub];
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        Info->statePredicateChildren[i] = sub[i];
    }
    return Info;
}

int DisjunctionStatePredicate::countSubFormulas() const
{
    int sum = 1; // 1 for the root node
    for (arrayindex_t i = 0; i < cardSub; i++)
    {
        sum += sub[i]->countSubFormulas();
    }
    return sum;
}


char * DisjunctionStatePredicate::toString()
{
	int size = 3;
	char * result = (char *) malloc(size * sizeof(char));
	char * subresult;
	result[0] = '(';
	result[1] = '\0';

	for(int i = 0; i < cardSub; i++)
	{
		if(i!=0)
		{
			size += 4;
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
			sprintf(result+strlen(result)," OR ");
		}
		subresult = sub[i] -> toString();
		size += strlen(subresult);
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
		sprintf(result+strlen(result),"%s",subresult);
		free(subresult);
	}
	sprintf(result+strlen(result),")");
	return result;
}
