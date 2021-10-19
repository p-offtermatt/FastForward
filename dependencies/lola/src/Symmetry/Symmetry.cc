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

\brief This data structure stores the generating set for all symmetries
which is in Schreier/Sims form.

Upon insertion of a symmetry, it combines it with existing ones, to find other
members of the generating set. It supports application of the symmetries to a
marking.
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Net/Net.h>
#include <Symmetry/Symmetry.h>

pthread_mutex_t GeneratingSet::mx = PTHREAD_MUTEX_INITIALIZER;

/*!
initially, data structures are allocated with length card(P). Later, memory can
be released using the condense methods Hence, we use malloc/calloc
*/
GeneratingSet::GeneratingSet() :
    possibleGenerators(((Net::Card[PL]) * (Net::Card[PL] - 1)) / 2),
    knownGenerators(0),
    realComputed(0),
    store(static_cast<arrayindex_t ** *>(calloc(Net::Card[PL], SIZEOF_VOIDP))),
    cardLevels(0),
    depth(static_cast<arrayindex_t *>(calloc(Net::Card[PL], SIZEOF_ARRAYINDEX_T))),
    card(static_cast<arrayindex_t *>(calloc(Net::Card[PL], SIZEOF_ARRAYINDEX_T))),
    reserve_marking(new capacity_t [Net::Card[PL]])
{
    // maybe replace by memset
    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        store[i] = NULL;
        depth[i] = i;
        card[i] = 0;
    }
}

/*!
\param d  The generator is the identity for 0..d-1
\param images  All images from d..|places|
*/
void GeneratingSet::add(arrayindex_t d, arrayindex_t *images)
{
    assert(store[d] != NULL);

    // before condensation, the value of i and depth coincide
    // After condensation, the values may differ and no symmetry
    // should be added then

    assert(depth[d] == d);
    assert(images[0] > d);

    store[d][images[0] - d - 1] = images;
    ++card[d];

    pthread_mutex_lock(&mx);
    ++knownGenerators;
    ++realComputed;
    pthread_mutex_unlock(&mx);

    combine_all(d, images[0] - d - 1);
}

/*!
shift all symmetries to front; order does not matter any more
*/
void GeneratingSet::condense_level(arrayindex_t d)
{
    if (card[d])
    {
        arrayindex_t i = 0;
        arrayindex_t j = Net::Card[PL] - d - 2;
        while (i < j)
        {
            if (store[d][i])
            {
                ++i;
                continue;
            }
            if (!store[d][j])
            {
                --j;
                continue;
            }
            store[d][i++] = store[d][j--];
        }
        // shrink this level
        store[d] = static_cast<arrayindex_t **>(realloc(store[d], card[d] * SIZEOF_VOIDP));
        assert(store[d]);
    }
    else
    {
        // there are no generators in this level -> free this level completely
        free(store[d]);
        store[d] = NULL;
        pthread_mutex_lock(&mx);
        --cardLevels;
        pthread_mutex_unlock(&mx);
    }
}

/*!
\note Order does matter here.
*/
void GeneratingSet::condense()
{
    // target index
    arrayindex_t newlevel = 0;
    // source index
    arrayindex_t oldlevel = 0;

    // proceed to first empty level
    for (; oldlevel < Net::Card[PL]; oldlevel++)
    {
        if (!card[oldlevel])
        {
            break;
        }
    }
    newlevel = oldlevel++;
    for (; oldlevel < Net::Card[PL]; oldlevel++)
    {
        if (!card[oldlevel])
        {
            continue;
        }
        card[newlevel] = card[oldlevel];
        depth[newlevel] = depth[oldlevel];
        store[newlevel++] = store[oldlevel];
    }
    assert(newlevel == cardLevels);

    // shrink data structures (card and depth have the same length as store)
    store = static_cast<arrayindex_t ** *>(realloc(store, cardLevels * SIZEOF_VOIDP));
    card = static_cast<arrayindex_t *>(realloc(card, cardLevels * SIZEOF_ARRAYINDEX_T));
    depth = static_cast<arrayindex_t *>(realloc(depth, cardLevels * SIZEOF_ARRAYINDEX_T));
}

void GeneratingSet::openLevel(arrayindex_t d)
{
    pthread_mutex_lock(&mx);
    ++cardLevels;
    pthread_mutex_unlock(&mx);
    store[d] = static_cast<arrayindex_t **>(calloc(Net::Card[PL] - d - 1, SIZEOF_VOIDP));
    assert(store[d]);
}

GeneratingSet::~GeneratingSet()
{
    for (arrayindex_t i = 0; i < cardLevels; i++)
    {
        for (arrayindex_t j = 0; j < card[i]; j ++)
        {
            free(store[i][j]);
        }
        free(store[i]);
    }
    free(store);
    free(card);
    free(depth);
    delete[] reserve_marking;
}

/*!
\param m  marking to canonize
\return canonized marking

\todo bestcandidate testen nur bis zum nächsten Level.
*/
capacity_t *GeneratingSet::canonize(capacity_t *m)
{
    // iterate all levels
    for (arrayindex_t d = 0; d < cardLevels; d++)
    {
        // within each level, select best symmetry
        arrayindex_t bestcandidate = 0;
        for (arrayindex_t currentcandidate = 1; currentcandidate < card[d]; currentcandidate++)
        {
            arrayindex_t i;
            // compare currentcandidate with bestcandidate
            for (i = depth[d]; i < Net::Card[PL]; i++)
            {
                // check if we find a mismatch
                if (m[store[d][currentcandidate][i - depth[d]]] != m[store[d][bestcandidate][i - depth[d]]])
                {
                    break;
                }
            }
            // evaluate the mismatch to decide which vector is better
            // if we did not find a mismatch, then i == Net::Card[PL], so we
            // stick with the old bestcandidate
            if (i < Net::Card[PL]
                    && m[store[d][currentcandidate][i - depth[d]]] < m[store[d][bestcandidate][i - depth[d]]])
            {
                // current better than best
                bestcandidate = currentcandidate;
            }
        }

        // here: bestcandidate is best symmetry in level d,
        // but not necessarily better than not applying
        // any symmetry at all (or applying identity)
        // hence: compare best candidate to m itself
        arrayindex_t i;
        for (i = depth[d]; i < Net::Card[PL]; i++)
        {
            if (m[i] != m[store[d][bestcandidate][i - depth[d]]])
            {
                break;
            }
        }
        if (i < Net::Card[PL] && m[store[d][bestcandidate][i - depth[d]]] < m[i])
        {
            // the best symmetry is actually better than the identity -> now
            // apply best symmetry to given marking

            // the markings are the same for the first depth[d] places
            for (arrayindex_t i = 0; i < depth[d]; i ++)
            {
                reserve_marking[i] = m[i];
            }

            // from depth[d] on, apply symmetry
            for (arrayindex_t i = depth[d]; i < Net::Card[PL]; i++)
            {
                reserve_marking[i] = m[store[d][bestcandidate][i - depth[d]]];
            }

            // swap reserve_marking with m
            capacity_t *swap = reserve_marking;
            reserve_marking = m;
            m = swap;
        }
    }

    // return old m or m with applied best candiate
    return m;
}

// LCOV_EXCL_START
/*!
For the mapping

1 -> 2
2 -> 3
3 -> 1
4 -> 5
5 -> 4
6 -> 6

print

(1 2 3) (4 5)

\note Singletons are not printed.
*/
void GeneratingSet::printGeneratingSet() const
{
    bool *used;
    // debugging function
    // output generating set to stdout
    for (arrayindex_t d = 0; d < cardLevels; d++) // for all levels do
    {
        std::cout << "Level " << d << " at depth " << depth[d] << std::endl;
        for (arrayindex_t i = 0; i < card[d]; i++)  // for all generators in level d
        {
            // array to cache which nodes have been printed so far
            used = static_cast<bool *>(calloc(Net::Card[PL], SIZEOF_BOOL));

            std::cout << "\t# " << i << ": ";
            for (arrayindex_t j = depth[d]; j < Net::Card[PL]; j++)
            {
                if (used[j])
                {
                    // place contained in other cycle
                    continue;
                }

                if (store[d][i][j - depth[d]] == j)
                {
                    // skip singleton cycle
                    continue;
                }

                // print a cycle
                std::cout << "(" << Net::Name[PL][j];
                used[j] = true;

                // in the loop, jump from k to sigma(k)
                for (arrayindex_t k = store[d][i][j - depth[d]]; k != j; k = store[d][i][k - depth[d]])
                {
                    std::cout << " " << Net::Name[PL][k];
                    used[k] = true;
                }

                std::cout << ") ";
            }
            std::cout << std::endl;
            free(used);
        }
    }
}
// LCOV_EXCL_STOP

/*!
Combine two generators of the same level.

\param d  level
\param sigma  index of the first generator
\param tau  index of the second generator
*/
void GeneratingSet::combine(arrayindex_t d, arrayindex_t sigma, arrayindex_t tau)
{
    assert(d < Net::Card[PL]);
     assert(sigma < Net::Card[PL] - d);
     assert(tau < Net::Card[PL] - d);
    assert(store[d][sigma] != NULL);
    assert(store[d][tau] != NULL);

    // index of the result of the combination of sigma and tau
    arrayindex_t psi = store[d][tau][store[d][sigma][0] - d];

    // if we already have a symmetry at this position, we don't care and return
    if (psi == d || store[d][psi - d - 1] != NULL)
    {
        return;
    }

    // create an array for psi
    arrayindex_t *result = static_cast<arrayindex_t *>(calloc(Net::Card[PL] - d, SIZEOF_ARRAYINDEX_T));
    store[d][psi - d - 1] = result;

    // create psi (= tau(sigma))
    for (arrayindex_t i = 0; i < Net::Card[PL] - d; i++)
    {
        result[i] = store[d][tau][store[d][sigma][i] - d];
    }

    pthread_mutex_lock(&mx);
    ++knownGenerators;
    pthread_mutex_unlock(&mx);

    ++card[d];

    // check if psi gives us new symmetries
    combine_all(d, psi - d - 1);
}

/*!
Combine a new generator with all generators of a level

\param d  level
\param fresh  the index of the new generator
*/
void GeneratingSet::combine_all(arrayindex_t d, arrayindex_t fresh)
{
    assert(store[d][fresh] != NULL);

    // traverse the whole level
    for (arrayindex_t i = 0; i < Net::Card[PL] - d - 1; i++)
    {
        // we found a generator
        if (store[d][i])
        {
            // try to add i(fresh) to level
            combine(d, fresh, i);
            // try to add fresh(i) to level
            combine(d, i, fresh);
        }
    }
}

long double GeneratingSet::sizeofSymmetryGroup() const
{
    long double s = 1;

    for (arrayindex_t d = 0; d < cardLevels; d++)
    {
        // +1 for the (not explicitly stored) identity
        s *= card[d] + 1;
    }
    return s;
}
