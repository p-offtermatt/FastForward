#!/usr/bin/python3

import sys
import random

def make_net(n,order):
    def previous(i):
        return (((i-2) % n) + 1)
    print('petri net "leader election %i" {' % n)
    print('  places {')
    for i in range(1,n+1):
        print('    ', end='')
        for j in range(1,n+1):
            print('s%in%i ' % (i,j), end='')
        print()
        print('    ', end='')
        for j in range(1,n+1):
            print('s%im%i ' % (i,j), end='')
        print()
        print()
    print('    lead')
    print('  }')
    print('  transitions {')
    for i in range(1,n+1):
        print('    ', end='')
        for j in range(1,n+1):
            print('s%isend%i ' % (i,j), end='')
        print()
        print('    ', end='')
        for j in range(1,n+1):
            if j < i:
                print('s%idisc%i ' % (i,j), end='')
            elif i == j:
                print('s%iacpt%i ' % (i,j), end='')
            else:
                print('s%ipass%i ' % (i,j), end='')
        print()
        print()
    print('    newleader')
    print('  }')
    print('  arcs {')
    for i in range(1,n+1):
        for j in range(1,n+1):
            print('    s%in%i -> s%isend%i -> s%im%i' % (i,j,i,j,i,j))
        print()
        for j in range(1,n+1):
            print('    s%im%i -> ' % (previous(i),j), end='')
            if j < i:
                print('s%idisc%i ' % (i,j))
            elif i == j:
                print('s%iacpt%i -> lead' % (i,j))
            else:
                print('s%ipass%i -> s%im%i' % (i,j,i,j))
        print()
        print()
    print('    lead -> newleader -> { ', end='')
    for i in range(1,n+1):
        print('s%in%i ' % (i,order[i-1]), end='')
    print('}')
    print('  }')
    print('  initial { ', end='')
    for i in range(1,n+1):
        print('s%in%i ' % (i,order[i-1]), end='')
    print('}')
    print('}')
    #print('safety property {')
    #print('  lead >= 2')
    #print('}')
    print('liveness property {')
    print('  newleader = 0')
    print('}')

n = int(sys.argv[1])
o = sys.argv[2]

order = list(range(1,n+1))
if o == 'rand':
    random.shuffle(order)
elif o == 'rev':
    order.reverse()
make_net(n,order)
