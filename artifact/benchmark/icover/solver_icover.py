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

from fractions import Fraction
import numpy as np
import z3
import config
from petri import places_set, places_preset, places_postset, transitions_preset, transitions_postset, constraint_vector



DOMAINS = {'R': z3.Real, 'N': z3.Int,'B' : z3.Bool}


def delta(transition,i):
    return transition[1][i]-transition[0][i]
    


def interisation(init):
    size = len(init)
    res = []
   
    for place in range(0,size):
        if not init[place][0] == "=":
            print "not =?"
            exit(1)
        else:
            res.append(init[place][1])


    return res



def build_limit_solver(transitions,init):
    dimension = len(init)
    nb_transitions = len(transitions)

    solver = z3.Solver()
    domain = 'R'
    x_variables = [DOMAINS[domain]('x_{}'.format(t)) for t in range(nb_transitions)]
    
    target_variables = [DOMAINS[domain]('t_{}'.format(t)) for t in range(dimension)]
    place_variables = [DOMAINS[domain]('p_{}'.format(t)) for t in range(dimension)]
        
    solver.add([x_variables[i] >= 0 for i in range(nb_transitions)])

    for place in range(dimension):
        if not init[place][0] == ">=":
            if len(transitions) > 0 and not delta(transitions[0],place) == 0:
                sum_vector = init[place][1] + x_variables[0]*delta(transitions[0],place)
            else:
                sum_vector = init[place][1]


            t = 0

            for transition in transitions:
                if t > 0:
                    if not delta(transition,place) == 0:
                      

                        sum_vector = sum_vector + (+ x_variables[t]*(delta(transition,place)))
                
                    
                t +=1
    
          
            solver.add(place_variables[place] == sum_vector)
            solver.add(place_variables[place] >= target_variables[place])
          

    for place in range(dimension):
        solver.add(target_variables[place] >= 0)

        
  
    return solver,target_variables


def deltas(petrinet):
    res = []


def build_limit_solver_petrinet(petrinet,init):
    # same as build_limit_solver but without using transitions in list (and then eliminate the need for get_transitions)
    
    dimension = len(init)

    nb_transitions = petrinet[0].shape[1]

    solver = z3.Solver()
    domain = 'R'
    x_variables = [DOMAINS[domain]('x_{}'.format(t)) for t in range(nb_transitions)]
    
    target_variables = [DOMAINS[domain]('t_{}'.format(t)) for t in range(dimension)]
    place_variables = [DOMAINS[domain]('p_{}'.format(t)) for t in range(dimension)]
        
    solver.add([x_variables[i] >= 0 for i in range(nb_transitions)])

    for place in range(dimension):
    
        if not init[place][0] == ">=":
          
            delta_t = petrinet[1][place,0] - petrinet[0][place,0]
          
            if not delta_t == 0:
                sum_vector = init[place][1] + x_variables[0]*delta_t
            else:
                sum_vector = init[place][1]


            for t in range(1,nb_transitions):
               
                delta_t = petrinet[1][place,t] - petrinet[0][place,t]
                if not delta_t == 0:
                   
                    sum_vector = sum_vector + (+ x_variables[t]*delta_t)
                
                    
                t +=1
    

            solver.add(place_variables[place] == sum_vector)
            solver.add(place_variables[place] >= target_variables[place])
           

    for place in range(dimension):
        solver.add(target_variables[place] >= 0)

  
    return solver,target_variables


def add_limit_equations(solver,target):

  

    dimension = len(target[0])
    
    
    domain = 'R'
    target_variables = [DOMAINS[domain]('t_{}'.format(t)) for t in range(dimension)]

    for place in range(dimension):                
        solver.add(target_variables[place] == target[0][place])
      
    return



