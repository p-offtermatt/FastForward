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
#include <Exploration/StatePredicateProperty.h>
#include <Formula/LTL/BuechiAutomata.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>
#include <Net/Net.h>
#include <Net/NetState.h>

int BuechiAutomata::getSuccessors(arrayindex_t **list,
        arrayindex_t currentState)
{
    arrayindex_t cardTransitionList = cardTransitions[currentState];
    uint32_t **transitionsList = transitions[currentState];

    *list = new arrayindex_t[cardEnabled[currentState]];
    int curCard = 0;

    //RT::rep->message("SIZE %d (curState %d)",cardEnabled[currentState], currentState);
    for (arrayindex_t i = 0; i < cardTransitionList; i++)
    {
        //RT::rep->message("checking %d (%d) -> %d",currentState, 
        //    atomicPropositions[transitionsList[i][0]]->getPredicate()->value,transitionsList[i][1]);
        if (atomicPropositions[transitionsList[i][0]]->getPredicate()->value)
        {
            //RT::rep->message("NOW PROP %d (p:%d)--> TRUE",i,atomicPropositions[transitionsList[i][0]]);

            //RT::rep->message("List(%d) %d = %d @ %d/%d [%d] (p: %d)",i,curCard,transitionsList[i][1], 
            //    cardEnabled[currentState],cardTransitionList,currentState,atomicPropositions[transitionsList[i][0]]);
            (*list)[curCard++] = transitionsList[i][1];
        } //else
    }
    //RT::rep->message("END");
    return curCard;
}

void BuechiAutomata::updateProperties(NetState &ns, arrayindex_t transition)
{
    //RT::rep->message("UPDATE");
    for (arrayindex_t i = 0; i < cardStates; i++)
    {
        cardEnabled[i] = 0;
    }
    for (arrayindex_t i = 0; i < cardAtomicPropositions; i++)
        if (atomicPropositions[i]->checkProperty(ns, transition))
        {
            //RT::rep->message("CHECK PROP %d (s = %d, p:%d) --> TRUE",i,atomicPropotions_backlist[i],atomicPropositions[i]);
            cardEnabled[atomicPropotions_backlist[i]]++;
        } //else
    ////RT::rep->message("CHECK PROP %d (s = %d, p:%d)--> FALSE",i,atomicPropotions_backlist[i],atomicPropositions[i]);
}

void BuechiAutomata::initProperties(NetState &ns)
{
    //RT::rep->message("INIT");
    for (arrayindex_t i = 0; i < cardAtomicPropositions; i++)
    {
        //RT::rep->message("INIT %d",i);
        if (atomicPropositions[i]->initProperty(ns))
        {
            cardEnabled[atomicPropotions_backlist[i]]++;
            //RT::rep->message("TRUE %d", cardEnabled[atomicPropotions_backlist[i]]);
        } //else
        //RT::rep->message("FALSE");
    }
}

void BuechiAutomata::revertProperties(NetState &ns, arrayindex_t transition)
{
    ////RT::rep->message("REVERT");
    for (arrayindex_t i = 0; i < cardStates; i++)
    {
        cardEnabled[i] = 0;
    }
    for (arrayindex_t i = 0; i < cardAtomicPropositions; i++)
        if (atomicPropositions[i]->updateProperty(ns, transition))
        {
            cardEnabled[atomicPropotions_backlist[i]]++;
        }
}

bool BuechiAutomata::isAcceptingState(arrayindex_t state)
{
    return isStateAccepting[state];
}

arrayindex_t BuechiAutomata::getNumberOfStates()
{
    return cardStates;
}

BuechiAutomata::~BuechiAutomata()
{
    for (arrayindex_t i = 0; i < cardAtomicPropositions; i++)
    {
        //delete atomicPropositions[i]->getPredicate();
        //delete atomicPropositions[i];
    }

    // allocated in Task::setFormula()
    delete[] atomicPropositions;

    for (arrayindex_t i = 0; i < cardStates; i++)
    {
        for (arrayindex_t j = 0; j < cardTransitions[i]; j++)
        {
            delete[] transitions[i][j];
        }
        delete[] transitions[i];
    }

    delete[] transitions;
    delete[] cardTransitions;
    delete[] isStateAccepting;
    delete[] atomicPropotions_backlist;
}


int current_next_string_index_number = 1;

char *produce_next_string(int *val)
{
    current_next_string_index_number++;
    int length = (int) log10(current_next_string_index_number) + 2;
    char *buf = new char[length]();
    sprintf(buf, "%d", current_next_string_index_number);
    *val = current_next_string_index_number;
    return buf;
}

void BuechiAutomata::writeBuechi()
{
    Output buechifile("Buechi automaton", std::string(RT::args.writeBuechi_arg) + ".buechi");
    fprintf(buechifile, "buechi\n{\n");
    for (int i = 0; i < cardStates; i++)
    {
        fprintf(buechifile, "\tState%d :\n", i);
        for (int j = 0; j < cardTransitions[i]; j++)
        {
            fprintf(buechifile, "\t\t%s => State%d\n", atomicPropositions[transitions[i][j][0]]->getPredicate()->toString(), transitions[i][j][1]);
        }
        if (i < cardStates - 1)
        {
            fprintf(buechifile, "\t\t,\n");
        }
    }
    fprintf(buechifile, "}\naccept\n{\n");
    bool notfirst = false;
    for (int k = 0; k < cardStates; k++)
    {
        if (isStateAccepting[k])
        {
            if (notfirst)
            {
                fprintf(buechifile, " ,\n");
            }
            notfirst = true;
            fprintf(buechifile, "\tState%d", k);
        }
    }
    fprintf(buechifile, "\n};\n");
}
