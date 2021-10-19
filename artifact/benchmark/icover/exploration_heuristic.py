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


def priority(element):    
    return sum(element)
    #return 0



def heuristic_initialize_array(targets,size,type_of_array):
    global type_str
    type_str = type_of_array


    if type_str == "fifos" or type_str == "stacks":
        array = [[] for _ in xrange(size)]    
        array = heuristic_add_elements(array,targets,size)
    else:
        print "type of queue not support (", type_str, ")"
        exit(1)

        
    return array


def heuristic_add_elements(array,elements,size):
    for element in elements:
        if type_str == "fifos":
            array[priority(element)].append(element)
        elif type_str == "stacks":
            array[priority(element)].insert(0,element)


    return array


def heuristic_get_elements(array,size):


    for i in range(size):
        if len(array[i]) > 0:
            return array[i].pop(0)
        else:
            
            i+=1

    return None



    
