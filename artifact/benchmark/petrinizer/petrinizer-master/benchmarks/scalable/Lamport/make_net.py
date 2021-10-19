#!/usr/bin/python3

import sys

# x_i := false;

#s1: x_i = true;
#s2: for j := 1 until i - 1 do
#s2_j_1: if x_j then
#s2_j_2:     x_i := false;
#s2_j_3:     while x_j do od;
#s2_j_4:     goto s1;
#        fi
#    od
#s3: for j := i + 1 until N do
#s3_j:   while x_j do od;
#    od
#s4: critical section
#s5: x_i := false

def make_net(n, prop, p):
    print('petri net "Lamport\'s 1-bit mutual exclusion algorithm for n=%i" {' % n)
    print('  places {', end='')
    for i in range(1,n+1):
        print()
        print('    p%is1' % i)
        print('    p%is2' % i, end='')
        for j in range(1,i):
            print(' p%is2j%is1' % (i,j), end='')
            print(' p%is2j%is2' % (i,j), end='')
            print(' p%is2j%is3' % (i,j), end='')
            print(' p%is2j%is4' % (i,j), end='')
        print()
        print('    p%is3' % i, end='')
        for j in range(i+1,n+1):
            print(' p%is3j%i' % (i,j), end='')
        print()
        print('    p%is4' % i)
        print('    p%is5' % i)
        print('    b%it b%if' % (i,i))
    print('  }')
    print('  transitions {', end='')
    for i in range(1,n+1):
        print()
        print('    t%is1' % i)
        print('    t%is2' % i, end='')
        for j in range(1,i):
            print(' t%is2j%is1b%it' % (i,j,j), end='')
            print(' t%is2j%is1b%if' % (i,j,j), end='')
            print(' t%is2j%is2' % (i,j), end='')
            print(' t%is2j%is3b%it' % (i,j,j), end='')
            print(' t%is2j%is3b%if' % (i,j,j), end='')
            print(' t%is2j%is4' % (i,j), end='')
        print()
        print('    t%is3' % i, end='')
        for j in range(i+1,n+1):
            print(' t%is3j%ib%it' % (i,j,j), end='')
            print(' t%is3j%ib%if' % (i,j,j), end='')
        print()
        print('    t%is4' % i)
        print('    t%is5' % i)
    print('  }')
    print('  arcs {', end='')
    for i in range(1,n+1):
        print()
        print()
        print('    { p%is1 b%if } -> t%is1 -> { p%is2 b%it }' % (i,i,i,i,i))
        print()
        if i == 1:
            print('    { p%is2 } -> t%is2 -> { p%is3 }' % (i,i,i))
        else:
            print('    { p%is2 } -> t%is2 -> { p%is2j1s1 }' % (i,i,i))
        for j in range(1,i):
            print('    { p%is2j%is1 b%it } -> t%is2j%is1b%it -> { p%is2j%is2 b%it }'
                            % (i,j,j,i,j,j,i,j,j))
            if j == i - 1:
                print('    { p%is2j%is1 b%if } -> t%is2j%is1b%if -> { p%is3 b%if }'
                            % (i,j,j,i,j,j,i,j))
            else:
                print('    { p%is2j%is1 b%if } -> t%is2j%is1b%if -> { p%is2j%is1 b%if }'
                            % (i,j,j,i,j,j,i,j+1,j))
            print('    { p%is2j%is2 b%it } -> t%is2j%is2 -> { p%is2j%is3 b%if }'
                            % (i,j,i,i,j,i,j,i))
            print('    { p%is2j%is3 b%it } -> t%is2j%is3b%it -> { p%is2j%is3 b%it }'
                            % (i,j,j,i,j,j,i,j,j))
            print('    { p%is2j%is3 b%if } -> t%is2j%is3b%if -> { p%is2j%is4 b%if }'
                            % (i,j,j,i,j,j,i,j,j))
            print('    { p%is2j%is4 } -> t%is2j%is4 -> { p%is1 }'
                            % (i,j,i,j,i))
        print()
        if i == n:
            print('    { p%is3 } -> t%is3 -> { p%is4 }' % (i,i,i))
        else:
            print('    { p%is3 } -> t%is3 -> { p%is3j%i }' % (i,i,i,i+1))
        for j in range(i+1,n+1):
            print('    { p%is3j%i b%it } -> t%is3j%ib%it -> { p%is3j%i b%it }'
                            % (i,j,j,i,j,j,i,j,j))
            if j == n:
                print('    { p%is3j%i b%if } -> t%is3j%ib%if -> { p%is4 b%if }'
                                % (i,j,j,i,j,j,i,j))
            else:
                print('    { p%is3j%i b%if } -> t%is3j%ib%if -> { p%is3j%i b%if }'
                                % (i,j,j,i,j,j,i,j+1,j))
        print()
        print('    { p%is4 } -> t%is4 -> { p%is5 }' % (i,i,i))
        print()
        print('    { p%is5 b%it } -> t%is5 -> { p%is1 b%if }' % (i,i,i,i,i))
        print()
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    p%is1 b%if' % (i,i))
    print('  }')
    print('}')
    if prop == 'fairness':
        print('liveness property "starvation freedom for process %i" {' % p)
        for i in range(1,n+1):
            print('    t%is1 + ' % i, end='')
            print('t%is2 + ' % i, end='')
            for j in range(1,i):
                print('t%is2j%is1b%it + ' % (i,j,j), end='')
                print('t%is2j%is1b%if + ' % (i,j,j), end='')
                print('t%is2j%is2 + ' % (i,j), end='')
                print('t%is2j%is3b%it + ' % (i,j,j), end='')
                print('t%is2j%is3b%if + ' % (i,j,j), end='')
                print('t%is2j%is4 + ' % (i,j), end='')
            print('t%is3 + ' % i, end='')
            for j in range(i+1,n+1):
                print('t%is3j%ib%it + ' % (i,j,j), end='')
                print('t%is3j%ib%if + ' % (i,j,j), end='')
            print('t%is4 + ' % i, end='')
            print('t%is5 > 0 &&' % i)
        print('    t%is4 = 0' % p)
        print('}')
    elif prop == 'safety':
        print('safety property "mutual exclusion" {')
        print('    p1s4', end='')
        for i in range(2,n+1):
            print(' + p%is4' % i, end='')
        print(' >= 2')
        print('}')

n = int(sys.argv[1])
prop = 'fairness'
process = 1

make_net(n, prop, process)
