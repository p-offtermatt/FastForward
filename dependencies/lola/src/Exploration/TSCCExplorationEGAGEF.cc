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
\author Markus
\status new

\brief do a depth_first search an find tscc's to evaluate a property
*/

#include <Core/Dimensions.h>
#include <Exploration/TSCCExploration.h>
#include <Exploration/ChooseTransition.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>

bool TSCCExplorationEGAGEF::depth_first(SimpleProperty &property, NetState &ns,
                                        Store<statenumber_t> &myStore, Firelist &myFirelist, __attribute__((__unused__))int threadNumber)
{
    fprintf(stderr, "TsccexplorationEGAGEF gets used\n");

    property.value = property.initProperty(ns);

    // add initial marking to store
    statenumber_t *payload;
    myStore.searchAndInsert(ns, &payload, 0);

    // get initial firelist
    arrayindex_t *currentFirelist;
    arrayindex_t currentEntry = myFirelist.getFirelist(ns, &currentFirelist);


    //last dfs where property was true
    arrayindex_t lasttrue = 0;

    //initialise dfsnumber,lowlink and highest_lowlink
    statenumber_t currentDFSNumber = 0;
    statenumber_t highest_lowlink = 0;
    statenumber_t currentLowlink = currentDFSNumber;

    //set initial dfs to zero
    setDFS(payload, ++currentDFSNumber);

    while (true)
    {
        if (currentEntry-- > 0)
        {
            //fire the next enabled transition
            Transition::fire(ns, currentFirelist[currentEntry]);

            //already in store eg visited?
            statenumber_t *newPayload;

            // search and insert the current netstate
            if (myStore.searchAndInsert(ns, &newPayload, 0))
            {
                //already visited
                //get the dfs number from the found payload
                statenumber_t newDFS = getDFS(newPayload);
                //backfire the transition because already known
                Transition::backfire(ns, currentFirelist[currentEntry]);

                //set the lowlink of the top element on the stack to this
                //because we have reached it from this state from the
                //topelement... (only if its smaller)
                if (dfsstack.StackPointer != 0)
                {
                    if (newDFS < dfsstack.top().lowlink)
                    {
                        dfsstack.top().lowlink = newDFS;
                    }
                }
            }
            else
            {
                //not yet visited
                //set the dfs number
                setDFS(newPayload, ++currentDFSNumber);
                //set the currentlowlink
                currentLowlink = currentDFSNumber;

                //put a entry on the searchstack
                //with the current dfs number and the current lowlink
                new(dfsstack.push()) DFSStackEntry(currentFirelist, currentEntry, newPayload, currentLowlink);

                //check the given property and
                //save lasttrue and lastfalse dfs number for the given property
                if (property.checkProperty(ns, currentFirelist[currentEntry]))
                {
                    lasttrue = currentDFSNumber;
                }

                //update enabled transitions
                Transition::updateEnabled(ns, currentFirelist[currentEntry]);
                //update the firelist
                currentEntry = myFirelist.getFirelist(ns, &currentFirelist);
            }
        }
        else
        {
            //delete the finished firelist
            delete[] currentFirelist;
            //if the stack is empty we are finished
            if (dfsstack.StackPointer == 0)
            {
                return property.value;
            }

            //getting v'
            DFSStackEntry &stackentry = dfsstack.top();
            currentEntry = stackentry.flIndex;
            currentFirelist = stackentry.fl;
            stackentry.fl = NULL;

            //check if this is the root of a tscc
            if (getDFS( (statenumber_t *) stackentry.payload) == stackentry.lowlink
                    && highest_lowlink < stackentry.lowlink)
            {
                //valid for egagef :)
                if ( lasttrue >= stackentry.lowlink )
                {
                    //for the current tscc "add" one more true
                    //just for clarification of the code...
                    property.value = true;
                    return true;
                }
                //update the highest lowlink to the current
                highest_lowlink = stackentry.lowlink;
            }
            // pop the top element on the stack
            dfsstack.pop();
            assert(currentEntry < Net::Card[TR]);
            //backfire and revert the enabledness
            Transition::backfire(ns, currentFirelist[currentEntry]);
            Transition::revertEnabled(ns, currentFirelist[currentEntry]);
            //update the property
            property.updateProperty(ns, currentFirelist[currentEntry]);
        }
    }
}
