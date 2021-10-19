# Copyright 2017 CNRS & Universite de Bordeaux

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import numpy as np
import z3
import time
from petri import get_transitions,get_transitions_sparse



# set true to activate the computation of omega places
def set_omega(bool_omega):
    global omega
    omega = bool_omega



    
# example: init = p1 >= 0, p2 = 1, p3 = 0, res is the list ("omega",">0","=0")
def triplet_init(init):
    res = []
    for e in init:
        if e[0] == ">=":
            if omega: 
                res.append("omega")
            else:
                res.append(">0")
        elif e[0] == '=' and e[1] == 0:
            res.append("=0")
        else:
            res.append('>0')

    
    return res



def triplet_target(target):
        
    res = []

    for e in target:
        if e[0] == ">=" and e[1] > 0:
            res.append(">0")
        elif e[0] == ">=" and e[1] == 0:
            res.append("=0")
        elif e[0] == '=' and e[1] == 0:
            res.append("=0")
        else:
            print "e :", e
            exit(1)
        
    
    return res        



# can the transition be fire "a little" (>0) , as much as we want (omega) or not at all (=0)?
def fireable_sparse(transition,triplet):
    size = len(triplet)
    if omega:
        res = "omega"
    else:
        res = ">0"        
    i = 0

    for place in transition[0]:
        if triplet[place] == "=0":
            return "=0"
        elif triplet[place] == ">0":
            res = ">0"
        
    return res

# fixpoint computation. At the end we have omega places, >0 places and places =0 that can't be reach
def compute_triplet_sparse(init,transitions):
    size = len(init)    
    triplet = triplet_init(init)
    
    W = range(len(transitions))
    
    change = True
        
    while change:
        Wprime = []
        change = False
        for t in W:
            transition = transitions[t]
            res = fireable_sparse(transition,triplet)
            if res == ">0" or res == "omega":
                change = True
                nonzero = transition[1]
                for place in nonzero:
                    if not (triplet[place] == "omega" and res == ">0"):
                        triplet[place] = res
            elif res == "=0":
                Wprime.append(t)
                        
        W = Wprime

    return triplet


# if triplet = [omega, >0, = 0] we know that 0,0,n (with n > 0) is uncoverable.
def eliminate_target(triplet,target):
    # is target cover ?
    size = len(triplet)
    res = True # at first, with size = 0, it is coverable
    for i in range(0,size):
        # if target is more than 0 et our computation is "=0", we know this is uncoverable
        if (triplet[i] == "=0") and ((target[i][0] == ">=" or target[i][0] == "=") and target[i][1] > 0):
            res = False
            return False
        # if target is more than 0 et our computation is ">0", we don't know if it coverable or not
        elif (triplet[i] == ">0") and ((target[i][0] == ">=" or target[i][0] == "=") and target[i][1] > 0):
            res = None

        # if triplet [i] == "omega" it doesn't change anything

        
        elif (not target[i][0] == ">=") and (not target[i][0] == "="):
            print target[i]
            print "error: target different that >="
            exit(1)
    return res


# find places that are either unreachable (=0) or omega place
def find_useless_places(triplet):
    res = []
    size = len(triplet)
    
    for i in range(0,size):
        if triplet[i] == "=0" or triplet[i] == "omega":            
            res.append(i)

    return res

# a transition is "useful" if, and only if, the transition put a token in a place
def is_usefull(transition):
    for i in range(0,len(transition[0])):
        if transition[1][i] > 0:
            return True        
    return False



# reduce the petri net, the init vector and the targets
def reduce_petrinet(petrinet,transitions,init,triplet,targets):

    time_before = time.time()
    useless_places = find_useless_places(triplet)    
    time_after = time.time()
    #print "time to find useless places", time_after - time_before

    print len(triplet), "places"
    if not omega:
        print len(useless_places), "places not reachable(s)"
    else:
        print len(useless_places), "places not useful (either not reachable or omega place)"

    time_before = time.time()
    new_transitions = []

    useless_transitions = []
    t =0
    only_took = 0
    for transition in transitions:
        cut = 0
        res = fireable_sparse(transition,triplet)
        if not (res == "=0" or res == "omega"):
            if len(transitions[0]) == 0:
                useless_transitions.append(t)
                only_took +=1
        else:
            useless_transitions.append(t)
        t += 1

    dimension = len(init)
    useful_places = [place for place in range(dimension) if not place in useless_places]
    print "useful places:", len(useful_places)
    really_useless = []
    for t in range(len(transitions)):
        if not t in useless_transitions:
            useful = False
            for place in transitions[t][1]:
                if place in useful_places:
                    useful = True
                    break
            if not useful:
                really_useless.append(t)


    print  len(transitions), "transitions"
    print len(useless_transitions), "transition(s) has been removed"
    
    pre,post = petrinet

    
    red_petrinet = np.delete(petrinet[0],useless_places,axis=0),np.delete(petrinet[1],useless_places,axis=0)
    
    
    red_petrinet = np.delete(red_petrinet[0],useless_transitions,axis=1),np.delete(red_petrinet[1],useless_transitions,axis=1)
        
    res = [np.asmatrix(np.array(red_petrinet[0])),np.asmatrix(np.array(red_petrinet[1]))]

    cut = 0
    for place in useless_places:       
        init.pop(place-cut)
        cut += 1

    # and the targets

    for target in targets:
        cut = 0
        for place in useless_places:
            target.pop(place-cut)
            cut += 1


    return res,init,targets
            

# return true if there exists a target m such that for all place p, we have m(p) <= init(p)
# in that case there is no need for backward algorithm.
# The answer to the coverability question is yes
def unsafe_easy_targets(targets,init):

    new_targets = []
    res = False
    for target in targets:
        cover =  True
        for place in range(len(init)):
            if (init[place][0] == ">=" or init[place][0] == "=") and (init[place][1] < target[place]):
                cover = False
                break
        if cover:
            res = True
            break
            

    return res




# main function. Called from main.py
def pre_processing(petrinet,init,targets):   
    # find >0 and = 0 place on init
    # find which targets are UNCOVER (or COVER if w place)
    # reduce size of petrinet and init    

    transitions_sparse = get_transitions_sparse(petrinet)

    triplet  = compute_triplet_sparse(init,transitions_sparse)

    time_before_end = time.time()

    # we try to eliminate some targets, or find a cover target quickly
    newtargets = []


    #time_before = time.time()

    # pre_processing_res :
    # False: uncoverable (all targets are uncoverable)
    # True: coverable (at least one target is coverable)
    # None: we don't know yet
    pre_processing_res = False    
    for target in targets:
        res = eliminate_target(triplet,target)
        if res:
            print "target coverable"
            print "one target eliminated"
            # the coverability question was: is there a coverable target in the set of all targets
            pre_processing_res = True
            break
            #exit(0) 
        elif res == False:
            print "target not coverable"
            print "one target eliminated"            
        else:
            pre_processing_res = None
            newtargets.append(target)
    
    targets = newtargets
    
    # now we reduce the size of the petrinet (and therefore init)
    reduced_petrinet, reduced_init,red_targets = reduce_petrinet(petrinet,transitions_sparse,init,triplet,targets)
    
    # is there easy target eliminated ?
    if not (pre_processing_res == False):
        easy_res_uncoverable = unsafe_easy_targets(red_targets,init)
        if easy_res_uncoverable:
            pre_processing_res = True

    time_after_end = time.time()
    
    return reduced_petrinet,reduced_init,red_targets,pre_processing_res
    

