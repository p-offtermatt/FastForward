#!/usr/bin/python3

import sys
import random


def make_net(n,order):
    def previous(i):
        return (((i-2) % n) + 1)
    print('petri net "improved leader election %i" {' % n)
    print('  places {')
    for i in range(1,n+1):
        print('    s%ia0 s%ia1 s%ia2 s%iaH s%iaP s%iaD' % (i,i,i,i,i,i))
        print('    ', end='')
        for j in range(1,n+1):
            print('s%imax%i ' % (i,j), end='')
        print()
        print('    ', end='')
        for j in range(0,n+1):
            print('s%ileft%i ' % (i,j), end='')
        print()
        print('    s%im1_n ' % i, end='')
        for j in range(1,n+1):
            print('s%im1_%i ' % (i,j), end='')
        print()
        print('    s%im2_n ' % i, end='')
        for j in range(1,n+1):
            print('s%im2_%i ' % (i,j), end='')
        print()
        print()
    print('  }')
    print('  transitions {')
    for i in range(1,n+1):
        print('    ', end='')
        for j in range(1,n+1):
            print('s%ia0send%i ' % (i,j), end='')
        print()
        print()
        for j in range(1,n+1):
            print('    ', end='')
            for vmax in range(1,n+1):
                if j == vmax:
                    print('s%ia1rec%imax%i ' % (i,j,vmax), end='')
                else:
                    for left in range(0,n+1):
                        print('s%ia1rec%imax%ileft%i ' % (i,j,vmax,left), end='')
            print()
        print()
        for j in range(1,n+1):
            print('    ', end='')
            for left in range(0,n+1):
                if left <= j:
                    print('s%ia2rec%ileft%i ' % (i,j,left), end='')
                else:
                    for vmax in range(1,n+1):
                        print('s%ia2rec%imax%ileft%i ' % (i,j,vmax,left), end='')
            print()
        print()
        print('    ', end='')
        for j in range(1,n+1):
            print('s%iaPrec1_%i s%iaPrec2_%i ' % (i,j,i,j), end='')
        print()
        print()
        print('    ', end='')
        for vmax in range(1,n+1):
            for left in range(0,n+1):
                print('s%iaDmax%ileft%i ' % (i,vmax,left), end='')
        print()
        print()
        print()
    print('    ', end='')
    for i in range(1,n+1):
        print('s%idone ' % i, end='')
    print()
    print('  }')
    print('  arcs {')
    for i in range(1,n+1):
        for j in range(1,n+1):
            print('    { s%ia0 s%imax%i s%im1_n } -> s%ia0send%i -> { s%ia1 s%imax%i s%im1_%i }' % (i,i,j,i,i,j,i,i,j,i,j))
        print()
        for j in range(1,n+1):
            for vmax in range(1,n+1):
                if j == vmax:
                    print('    { s%ia1 s%imax%i s%im1_%i } -> s%ia1rec%imax%i -> { s%iaH s%imax%i s%im1_n }' % (i,i,vmax,previous(i),j,i,j,vmax,i,i,vmax,previous(i)))
                else:
                    for left in range(0,n+1):
                        print('    { s%ia1 s%imax%i s%ileft%i s%im1_%i s%im2_n } -> s%ia1rec%imax%ileft%i -> { s%ia2 s%imax%i s%ileft%i s%im1_n s%im2_%i }' % (i,i,vmax,i,left,previous(i),j,i,i,j,vmax,left,i,i,vmax,i,j,previous(i),i,j))
        print()
        for j in range(1,n+1):
            for left in range(0,n+1):
                if left <= j:
                    print('    { s%ia2 s%ileft%i s%im2_%i } -> s%ia2rec%ileft%i -> { s%iaP s%ileft%i s%im2_n }' % (i,i,left,previous(i),j,i,j,left,i,i,left,previous(i)))
                else:
                    for vmax in range(1,n+1):
                        if left <= vmax:
                            print('    { s%ia2 s%imax%i s%ileft%i s%im2_%i } -> s%ia2rec%imax%ileft%i -> { s%iaP s%imax%i s%ileft%i s%im2_n }' % (i,i,vmax,i,left,previous(i),j,i,j,vmax,left,i,i,vmax,i,left,previous(i)))
                        else:
                            print('    { s%ia2 s%imax%i s%ileft%i s%im2_%i s%im1_n } -> s%ia2rec%imax%ileft%i -> { s%ia1 s%imax%i s%ileft%i s%im2_n s%im1_%i }' % (i,i,vmax,i,left,previous(i),j,i,i,j,vmax,left,i,i,left,i,left,previous(i),i,left))
        print()
        for j in range(1,n+1):
            print('    { s%iaP s%im1_%i s%im1_n } -> s%iaPrec1_%i -> { s%iaP s%im1_n s%im1_%i }' % (i,previous(i),j,i,i,j,i,previous(i),i,j))
            print('    { s%iaP s%im2_%i s%im2_n } -> s%iaPrec2_%i -> { s%iaP s%im2_n s%im2_%i }' % (i,previous(i),j,i,i,j,i,previous(i),i,j))
        print()
        for vmax in range(1,n+1):
            for left in range(0,n+1):
                print('    { s%iaD s%imax%i s%ileft%i } -> s%iaDmax%ileft%i -> { s%ia0 s%imax%i s%ileft0 }' % (i,i,vmax,i,left,i,vmax,left,i,i,order[i-1],i))
        print()
        print()
    for i in range(1,n+1):
        print('    { ', end='')
        for j in range(1,i):
            print('s%iaP ' % j, end='')
        print('s%iaH ' % i, end='')
        for j in range(i+1,n+1):
            print('s%iaP ' % j, end='')
        print('} -> s%idone -> { ' % i, end='')
        for j in range(1,n+1):
            print('s%iaD ' % j, end='')
        print('}')
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    s%ia0 s%imax%i s%ileft0 s%im1_n s%im2_n' % (i,i,order[i-1],i,i,i))
    print('  }')
    print('}')
    print('liveness property {')
    print('    ', end='')
    for i in range(1,n+1):
        print('s%idone = 0' % i, end='')
        if i < n:
            print(' && ', end='')
    print()
    print('}')

n = int(sys.argv[1])
o = sys.argv[2]

order = list(range(1,n+1))
if o == 'rand':
    random.shuffle(order)
elif o == 'rev':
    order.reverse()
make_net(n,order)
