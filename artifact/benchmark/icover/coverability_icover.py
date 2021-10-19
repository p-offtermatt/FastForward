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

import z3
from petri import load_petrinet, constraint_vector, petrinet_lossy
from cpn import reachable, build_cpn_solver
from solver_utils import set_markings_constrained
from upward_sets import update_upward, in_upward, merge_upward
import numpy as np


from petri import get_transitions
from solver_icover import build_limit_solver,add_limit_equations,build_limit_solver_petrinet
from coverability import _omega_marking,pre_upward,sum_norm,non_coverable,check_cpn_coverability_z3
from exploration_heuristic import heuristic_initialize_array,heuristic_get_elements,heuristic_add_elements

import time 


total_build_time = 0
total_check_time = 0


def limit_non_coverable(transitions, init, targets):
    MAX_TARGETS_SINGLE_TEST = 10
    

    solver,_ = build_limit_solver(transitions,init)
                
    i = 0
    for target in targets:        
        newtarget = []
        for t in target:
            newtarget.append(t[1])
            
        if check_limit_coverability_z3(solver,[newtarget]) == True:

            return False
      
    return True


def check_limit_coverability_z3(solver,target):
    global total_check_time
    global total_build_time
    
    solver.push()                
    
    time_before = time.time()
    add_limit_equations(solver,target)    
    
    time_after = time.time()
    total_build_time += time_after - time_before
    time_before = time.time()
    result = solver.check()

    time_after = time.time()
    total_check_time += time_after - time_before        

    solver.pop()
    
    if result == z3.sat:
        return True
    elif result == z3.unsat:
        return False
    else:
        return None

def limit_coverability(petrinet, init, targets, prune=False, max_iter=None):
    # use state equation
    # add by GLS
    # Verify if non coverable in CPN first
    transitions = get_transitions(petrinet)
    if prune and limit_non_coverable(transitions, init, targets):
        print "result find with non_coverable"
        return False

    # Otherwise, proceed with backward coverability
    def smallest_elems(x):
        return set(sorted(x, key=sum_norm)[:int(10 + 0.2 * len(x))])            
    
    solver,target_vars = build_limit_solver(transitions,init)
                    

    def limit_coverable(markings):        
        return check_limit_coverability_z3(solver, markings)

    
    init_marking = _omega_marking(init)
    basis = {tuple(constraint_vector(m)) for m in targets}
    precomputed = {}
    covered = in_upward(init_marking, basis)        
    
    num_iter = 0

   
    while not covered:
        if max_iter is not None and num_iter >= max_iter:
            return None # Unknown result
        else:
            num_iter += 1

        # Compute prebasis                

        #time_before = time.time()
        prebasis = pre_upward(petrinet, basis, precomputed)

        #time_after = time.time()
        #print "time pre upward :", time_after - time_before


        # Coverability pruning        
        #time_before = time.time()
        pruned = {x for x in prebasis if prune and not limit_coverable([x])}
        nbpruned = len(pruned)

        #time_after = time.time()
        #print "time to prune:", time_after - time_before
        
        
        prebasis.difference_update(pruned)

  

        # Continue?
        if len(prebasis) == 0:
            break
        else:        

            prebasis = smallest_elems(prebasis)
            merge_upward(basis, prebasis)
            covered = in_upward(init_marking, basis)
         

        #print "numbers",num_iter,len(prebasis)+nbpruned,nbpruned




    #print "total build time :", total_build_time
    #print "total check time :", total_check_time
    #print "numbers",num_iter,len(prebasis)+nbpruned,nbpruned

    
    return covered



glitch = 0
UUcomp = 0

nbcomp = 0
error = 0

def comparable_coverability(petrinet, init, targets, prune=False, max_iter=None):

    global total_check_time
    global total_build_time
    global UUcomp
    global nbcomp
    global error


    # Verify if non coverable in CPN first
    if prune and non_coverable(petrinet, init, targets):
        print "non_coverable in Q"
        print "result find with non_coverable"
        return False
    
    # Otherwise, proceed with backward coverability
    def smallest_elems(x):
        return set(sorted(x, key=sum_norm)[:int(10 + 0.2 * len(x))])

    solverQ, variables = build_cpn_solver(petrinet, init, targets=None,
                                         domain='N')

    transitions = get_transitions(petrinet)
    solverL,_ = build_limit_solver(transitions,init)
    _, _, target_vars = variables

   

    def comparaison_coverable(markings):
        global glitch
        global UUcomp
        global nbcomp
        global error

        nbcomp += 1
        #print solverQ
        #print solverL
        resQ = check_cpn_coverability_z3(solverQ, target_vars, markings)
        resL = check_limit_coverability_z3(solverL, markings)
    

        if resQ and resL == False:
            print "qcover solver say cover and limit solver say not cover"
            print "impossible"
            print "error"
            #print markings
            
            error +=1
            print
            exit(1)

        if resQ == False and resL:

            glitch +=1
        
            
        return resQ


    
    

    init_marking = _omega_marking(init)

    basis = {tuple(constraint_vector(m)) for m in targets}
    precomputed = {}
    covered = in_upward(init_marking, basis)    

    num_iter = 0

    while not covered:
        if max_iter is not None and num_iter >= max_iter:
            return None # Unknown result
        else:
            num_iter += 1

        # Compute prebasis

        print "step :",num_iter


        prebasis = pre_upward(petrinet, basis, precomputed)


        # Coverability pruning
        nbover = glitch
        pruned = {x for x in prebasis if prune and not comparaison_coverable([x])}
        print "nb over ", glitch-nbover
        print "prebasis size : ", len(prebasis)
        print "size of pruned :", len(pruned)
        prebasis.difference_update(pruned)
        

        
        for x in pruned:
            solverQ.add(z3.Or([target_vars[p] < x[p] for p in
                              range(len(x))]))

        # Continue?
        if len(prebasis) == 0:
            break
        else:
            prebasis = smallest_elems(prebasis)
            merge_upward(basis, prebasis)
            covered = in_upward(init_marking, basis)
         
        
        print 


    print "total build time :", total_build_time
    print "total check time :", total_check_time
    print "glitch: ", glitch
    print "error", error
    print "comparison", nbcomp

    return covered





######### exploration heuristic #############



def pre_upward_for_one_element(petrinet,element,transitions,basis):   

    pre_matrix, post_matrix = petrinet
    num_places, num_transitions = pre_matrix.shape
    res = []

    for t in range(num_transitions):
        pre_m = [0] * num_places

        for p in range(num_places):
            pre, post = int(pre_matrix[p, t]), int(post_matrix[p, t])
            pre_m[p] = max(pre, element[p] + pre - post)

        pre_m = tuple(pre_m)



        if not in_upward(pre_m,basis):
            #print "pre_m",pre_m
            res.append(pre_m)
                          
    return res


def is_already_seen(basis,element):
    #print "already seen ? basis:",basis
    return in_upward(element,basis)

def leq(x, y):
    return all(x[i] <= y[i] for i in range(len(x)))


def limit_coverability_exploration_heuristic(petrinet, init, targets, type_of_array,prune=False, max_iter=None):
    # type_of_array: "fifos","stacks"
    
    transitions = get_transitions(petrinet)
    
    if prune and limit_non_coverable(transitions, init, targets):
        print "result find with non_coverable"
        return False

      
    solver,target_vars = build_limit_solver(transitions,init)
    
    def limit_coverable(markings):
        return check_limit_coverability_z3(solver, markings)    
    
    init_marking = _omega_marking(init)
    size_priority = 1000
    targets_markings = {tuple(constraint_vector(m)) for m in targets}    
    queue = heuristic_initialize_array(targets_markings,size_priority,type_of_array)

    #print "targets",targets_markings
    basis = set()
    #basis = {tuple(constraint_vector(m)) for m in targets} # tmp1
    num_iter = 0

    nb_tested = 0
    nb_pruned = 0
    
    while True:
        num_iter += 1
      
        element = heuristic_get_elements(queue,size_priority)

        if element == None:
            print "numbers", -1, nb_pruned, nb_tested, len(basis)
            #outside = verify_basis(petrinet,basis)
            # for element in outside:
            #     if limit_coverable([element]):
            #         print "problem not an invariant"
            #         print element
            #         exit(1)

            #print "verified"
            #for e in basis:
            #    print "in basis:",e
            return False# UNCOVER

        #print "element", element
        #if True or not is_already_seen(basis,element): # TMP
        if not is_already_seen(basis,element): # TMP
            #print "not seen!"
            pre_elements = pre_upward_for_one_element(petrinet,element,transitions,basis)

            #print pre_elements
            if prune:
                nb_elements = len(pre_elements)
                nb_tested  += nb_elements
                
                pre_elements = {x for x in pre_elements if limit_coverable([x])}
                
                nb_not_pruned = len(pre_elements)
                #print "pruned:", (nb_elements - nb_not_pruned) , " / " , nb_elements
                nb_pruned += nb_elements - nb_not_pruned



                #merge_upward(basis,pre_elements) # TMP

                    
            queue = heuristic_add_elements(queue,pre_elements,size_priority)


            # size_q = 0
            # for q in queue:
            #     size_q += len(q)

            # print "queue",size_q
            for pre_element in pre_elements:
                if leq(pre_element,init_marking):
                    #print "COVER because:"
                    #print pre_element
                    #print "<="
                    #print init_marking
                    #for m in basis:
                    #    print "in basis:", m
                   
                    #print "numbers", -1, nb_pruned, nb_tested, "basis:",len(basis)
                    return True # COVER !
                    
            basis.update([element]) #TMP
            
      
            #print
                
            
    print "error"
    exit(1)
    return None
