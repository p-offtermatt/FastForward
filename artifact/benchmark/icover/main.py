# Modifications Copyright 2017 CNRS & Universite de Bordeaux

# Copyright 2017 Michael Blondin, Alain Finkel, Christoph Haase, Serge Haddad

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import argparse
from petri import load_petrinet
from coverability import coverability

# for ICover
from solver_icover import *
from petri import get_transitions
from preprocessing import pre_processing, set_omega
from coverability_icover import limit_coverability,comparable_coverability, limit_coverability_exploration_heuristic


import time


def main(path,pre,mode):
    
    petrinet, init, targets = load_petrinet(path)            
    
    red_petrinet = petrinet
    red_init = init
    
    pre_processing_res = None # if no pre-preprocessing
    if pre:
        time_before = time.time()
        print "**** pre processing ***********"
        red_petrinet, red_init, targets,pre_processing_res = pre_processing(petrinet,init,targets)
        petrinet = red_petrinet
        init = red_init
        time_after = time.time()
        print "pre_processing finished in", time_after - time_before
        print "*******************************"
        
    if pre_processing_res:
        print "The pre-processing found that one target was coverable"
        print "Unsafe"
        exit(0)
    elif pre_processing_res == False:
        print "The pre-processing found that all targets were uncoverable"
        print "Safe"
        exit(0)        
    
    # know we will launch the backward coverability algorithm
    if mode == "qcover":
        result = coverability(petrinet, init, targets, prune=True,max_iter=None)
    elif mode == "limit":
        result = limit_coverability(petrinet, init, targets, prune=True,max_iter=None)
    elif mode  == "comp":
        result = comparable_coverability(petrinet, init, targets,prune=True,max_iter=None)
    elif mode == "hfifos":
        result =  limit_coverability_exploration_heuristic(petrinet, init, targets, "fifos",prune=True,max_iter=None)
    elif mode == "hstacks":
        result =  limit_coverability_exploration_heuristic(petrinet, init, targets, "stacks",prune=True,max_iter=None)
    else:
        print "wrong mode"
        print "choose qcover, limit or comp, hfifos or hstacks"
        exit(2)

    if result is None:
        print 'Unknown'
    elif result:
        print 'Unsafe'
    else:
        print 'Safe'

# Entry point
if __name__ == '__main__':
   
        
    parser = argparse.ArgumentParser(description='Performs coverability safety verification.')

    parser.add_argument('path', metavar='Filename', type=str,
                        help='File (.spec) to verify.')

    
    parser.add_argument('--pre',
                        action = 'store_true',
                        default=False,
                        help='activate the pre-processessing of reachable places')


    parser.add_argument('--omega',
                        action = 'store_true',
                        default=False,
                        help='activate the pre-processessing of omega places (not possible without --pre)')
    parser.add_argument('mode', type=str,default="",
                        help="""mode for the backward coverability algorithm:\n
                        limit (for limit-reachability invariant of ICover),
                        qcover (for continous invariant of QCover),
                        comp (for comparison of pruning between the two),
						 hfifos (heuristique: for changing the order of the computation
                        based on an array of priority fifos)
                        hstack (heuristique: for changing the order of the computation
                        based on an array of priority stacks)
						""")

    args = parser.parse_args()
    
    if (not args.pre) and args.omega:
        print "The pre-processessing of omega places is not possible without --pre"
        exit(2)
    
    set_omega(args.omega) # set if we perform the pre-processing with omega places

    main(args.path, args.pre, args.mode)

