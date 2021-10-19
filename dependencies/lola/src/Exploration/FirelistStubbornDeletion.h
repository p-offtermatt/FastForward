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
\author Harro
\status new

\brief Class for firelist generation by the deletion algorithm for stubborn
sets.
*/

#pragma once

#include <Core/Dimensions.h>

class Firelist;
class StatePredicate;

/// a stubborn firelist for the search for deadlocks and state predicates
class FirelistStubbornDeletion : public Firelist
{
public:
    /// constructor for deadlock search
    FirelistStubbornDeletion();

    /// constructor for state predicates
    explicit FirelistStubbornDeletion(StatePredicate *sp);

    /// destructor
    ~FirelistStubbornDeletion();

    /// return value contains number of elements in fire list, argument is reference
    /// parameter for actual list
    virtual arrayindex_t getFirelist(NetState &ns, arrayindex_t **);

    /// to make a copy of this object
    virtual Firelist *createNewFireList(SimpleProperty *property);

private:
    /// build the unchanging part of the deletion graph
    void buildStaticGraph();

    /// execute the deletion algorithm
    arrayindex_t deletion(NetState &ns, arrayindex_t **);

    /// initialise the deletion graph
    void init();

    /// make a single run of the deletion algorithm
    void cnstr(NetState &ns, arrayindex_t *prt, arrayindex_t prtnr);

    /// temporarily remove a node from the graph and calculate the consequences
    void speculate(arrayindex_t node);

    /// put a node back into the graph and check the consequences
    void rehabilitate(arrayindex_t node);

    /// colors of nodes
    enum { WHITE = 0, GRAY, BLACK };

    /// protection
    enum { UNPROTECTED = 0, PROTECTED };

    /// root flag
    enum { PENDING = 0, DONE };

    /// and-or-nodes
    enum { ANDNODE = 0, ORNODE };

    /// max number of <t,s>-pairs
    arrayindex_t fieldsize;

    /// max number of nodes in the and-or-graph
    arrayindex_t size;

    /// lists of predecessors for all nodes
    arrayindex_t **predecessor;

    /// number of predecessors for each node
    arrayindex_t *predcnt;

    /// counter for white successors of or-nodes
    arrayindex_t *counter;

    /// maximum value for the counter for white successors of or-nodes for a given state
    arrayindex_t *maxcounter;

    /// offset of the first <t,s>-pair for a given t
    arrayindex_t *offset;

    /// the transition of a <t,s>-pair (given by offset)
    arrayindex_t *transition;

    /// for each place a list of transitions decreasing its token count
    std::vector<arrayindex_t> *consumer;

    /// for each place the token decrease by firing the corresponding transition in the consumer list
    std::vector<arrayindex_t> *consumption;

    /// for each place a set of transitions decreasing its token count
    std::set<arrayindex_t> *producer;

    /// color of a node (0=white, 1=gray, 2=black)
    unsigned char *color;

    /// if a node is protected from deletion
    unsigned char *protect;

    /// if a node has already been tried for deletion
    unsigned char *root;

    /// if a node is an or-node (true) or and-node (false)
    unsigned char *andor;

    /// pointer to the state predicate if it exists
    StatePredicate *sp;

    /// offset to where the formula nodes start
    arrayindex_t formula_offset;

    /// list of formula nodes in the same order as in the graph
    std::vector<const StatePredicate *> f_nodes;

    /// indices of formula and-nodes (= graph or-nodes) with successors
    std::map<arrayindex_t, std::vector<arrayindex_t> > f_and_nodes;

    /// indices of formula leafs (atomic, TRUE, FALSE)
    std::vector<arrayindex_t> f_leaf_nodes;

    /// vector of all enabled transitions
    arrayindex_t *enabled;
};

