#!/usr/bin/python3

import sys
import random

def make_net(n,comm):
    def previous(i):
        return (((i-2) % n) + 1)
    def next(i):
        return ((i % n) + 1)
    print('petri net "snapshot %i" {' % n)
    print('  places {')
    for i in range(1,n+1):
        print('    white%i red%i sent%i notsent%i sample%i nosample%i' % (i,i,i,i,i,i))
        print()
    print('  }')
    print('  transitions {')
    for i in range(1,n+1):
        print('    init%i send%iwhite%i send%ired%i' % (i,i,next(i),i,next(i)), end='')
        if comm:
            print(' comm%ired' % i)
        else:
            print()
    print()
    print('    snapshot ', end='')
    for i in range(1,n+1):
        print('nosnapshot%i ' % (i), end='')
    print()
    print('  }')
    print('  arcs {')
    for i in range(1,n+1):
        if comm:
            print('    { red%i sent%i } -> comm%ired -> { red%i sent%i }' % (i,i,i,i,i))
        print('    { white%i nosample%i } -> init%i -> { red%i sample%i }' % (i,i,i,i,i))
        print('    { red%i notsent%i white%i nosample%i } -> send%iwhite%i -> { red%i sent%i red%i sample%i }' % (i,i,next(i),next(i),i,next(i),i,i,next(i),next(i)))
        print('    { red%i notsent%i red%i } -> send%ired%i -> { red%i sent%i red%i }' % (i,i,next(i),i,next(i),i,i,next(i)))
        print()
    print('    { ', end='')
    for i in range(1,n+1):
        print('sample%i red%i sent%i ' % (i,i,i), end='')
    print('} -> snapshot -> { ', end='')
    for i in range(1,n+1):
        print('nosample%i white%i notsent%i ' % (i,i,i), end='')
    print('}')
    for i in range(1,n+1):
        print('    nosample%i -> nosnapshot%i -> nosample%i' % (i,i,i))
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    white%i notsent%i nosample%i ' % (i,i,i))
    print('  }')
    print('}')
    print('liveness property {')
    for i in range(1,n+1):
        print('  init%i + send%iwhite%i + send%ired%i' % (i,i,next(i),i,next(i)), end='')
        if comm:
            print(' + comm%ired' % i, end='')
        print(' > 0 &&')
    print('  snapshot', end='')
    for i in range(1,n+1):
        print(' + nosnapshot%i' % i, end='')
    print(' > 0 &&')
    print('  snapshot = 0')
    print('}')

n = int(sys.argv[1])
o = sys.argv[2]

if o == 'nocomm':
    make_net(n,False)
else:
    make_net(n,True)
