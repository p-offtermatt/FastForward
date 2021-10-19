#!/usr/bin/python3

import sys
from itertools import chain

#q[i]  := 0 for 1 ≤ i ≤ n
#turn[i] := 0 for 1 ≤ i ≤ n - 1

# s1: for j = 1 to n - 1 do
# s2_j: q[i] = j
# s3_j: turn[j] = i
# s4_j  for k=1 to i-1, i+1 to n do
# s5_j_k: if ( q[k] >= j and
# s6_j_k       turn[j] = i ) then
# s7_j_k:   goto s4_j
#     fi
# s8_j_k: next
#     done
# s9_j: next
#   done
#s10: q[i] = n
#s11: critical section
#s12: q[i] = 0
#s13: goto s1

def make_net(n, prop, p):
    print('petri net "Peterson\'s mutual exclusion algorithm with levels for n=%i" {' % n)
    print('  places {', end='')
    for i in range(1,n+1):
        print()
        print('    p%is1' % i)
        print('   ', end='')
        for j in range(1,n):
            print(' p%is2j%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            print(' p%is3j%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            print(' p%is4j%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' p%is5j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' p%is6j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' p%is7j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' p%is8j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            print(' p%is9j%i' % (i,j), end='')
        print()
        print('    p%is10' % i)
        print('    p%is11' % i)
        print('    p%is12' % i)
        print('    p%is13' % i)
        print('    q%iv0' % i, end='')
        for j in range(1,n+1):
            print(' q%iv%i' % (i,j), end='')
        print()
    print()
    for j in range(1,n):
        print('   ', end='')
        for v in range(0,n+1):
            print(' turn%iv%i' % (j,v), end='')
        print()
    print('  }')
    print('  transitions {', end='')
    for i in range(1,n+1):
        print()
        print('    t%is1' % i)
        print('   ', end='')
        for j in range(1,n):
            print(' t%is2j%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in range(0,n+1):
                print(' t%is3j%iturn%iv%i' % (i,j,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            print(' t%is4j%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                for v in range(0,n+1):
                    print(' t%is5j%ik%iq%iv%i' % (i,j,k,k,v), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                for v in range(0,n+1):
                    print(' t%is6j%ik%iturn%iv%i' % (i,j,k,j,v), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' t%is7j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print(' t%is8j%ik%i' % (i,j,k), end='')
        print()
        print('   ', end='')
        for j in range(1,n):
            print(' t%is9j%i' % (i,j), end='')
        print()
        print('    t%is10' % i)
        print('    t%is11' % i)
        print('    t%is12' % i)
        print('    t%is13' % i)
    print('  }')
    print('  arcs {', end='')
    for i in range(1,n+1):
        print()
        print()
        print('    { p%is1 } -> t%is1 -> { p%is2j1 }' % (i,i,i))
        print()
        for j in range(1,n):
            print('    { p%is2j%i q%iv%i } -> t%is2j%i -> { p%is3j%i q%iv%i }'
                        % (i,j,i,j-1,i,j,i,j,i,j))
        print()
        for j in range(1,n):
            for v in range(0,n+1):
                print('    { p%is3j%i turn%iv%i } -> t%is3j%iturn%iv%i -> { p%is4j%i turn%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j,j,i))
        print()
        for j in range(1,n):
            if i == 1:
                print('    { p%is4j%i } -> t%is4j%i -> { p%is5j%ik2 }' % (i,j,i,j,i,j))
            else:
                print('    { p%is4j%i } -> t%is4j%i -> { p%is5j%ik1 }' % (i,j,i,j,i,j))
        print()
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                for v in range(0,n+1):
                    if v >= j:
                        print('    { p%is5j%ik%i q%iv%i } -> t%is5j%ik%iq%iv%i -> { p%is6j%ik%i q%iv%i }' 
                                % (i,j,k,k,v,i,j,k,k,v,i,j,k,k,v))
                    else:
                        print('    { p%is5j%ik%i q%iv%i } -> t%is5j%ik%iq%iv%i -> { p%is8j%ik%i q%iv%i }' 
                                % (i,j,k,k,v,i,j,k,k,v,i,j,k,k,v))
        print()
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                for v in range(0,n+1):
                    if v == i:
                        print('    { p%is6j%ik%i turn%iv%i } -> t%is6j%ik%iturn%iv%i -> { p%is7j%ik%i turn%iv%i }' 
                                % (i,j,k,j,v,i,j,k,j,v,i,j,k,j,v))
                    else:
                        print('    { p%is6j%ik%i turn%iv%i } -> t%is6j%ik%iturn%iv%i -> { p%is8j%ik%i turn%iv%i }' 
                                % (i,j,k,j,v,i,j,k,j,v,i,j,k,j,v))
        print()
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                print('    { p%is7j%ik%i } -> t%is7j%ik%i -> { p%is4j%i }' % (i,j,k,i,j,k,i,j))
        print()
        for j in range(1,n):
            for k in chain(range(1,i), range(i+1,n+1)):
                if k == i - 1 and i == n or k == n:
                    print('    { p%is8j%ik%i } -> t%is8j%ik%i -> { p%is9j%i }'
                                % (i,j,k,i,j,k,i,j))
                elif k == i - 1:
                    print('    { p%is8j%ik%i } -> t%is8j%ik%i -> { p%is5j%ik%i }'
                                % (i,j,k,i,j,k,i,j,i+1))
                else:
                    print('    { p%is8j%ik%i } -> t%is8j%ik%i -> { p%is5j%ik%i }'
                                % (i,j,k,i,j,k,i,j,k+1))
        print()
        for j in range(1,n):
            if j == n - 1:
                print('    { p%is9j%i } -> t%is9j%i -> { p%is10 }' % (i,j,i,j,i))
            else:
                print('    { p%is9j%i } -> t%is9j%i -> { p%is2j%i }' % (i,j,i,j,i,j+1))
        print()
        print('    { p%is10 q%iv%i } -> t%is10 -> { p%is11 q%iv%i }' % (i,i,n-1,i,i,i,n))
        print()
        print('    { p%is11 } -> t%is11 -> { p%is12 }' % (i,i,i))
        print()
        print('    { p%is12 q%iv%i } -> t%is12 -> { p%is13 q%iv0 }' % (i,i,n,i,i,i))
        print()
        print('    { p%is13 } -> t%is13 -> { p%is1 }' % (i,i,i))
        print()
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    p%is1 q%iv0' % (i,i))
    for j in range(1,n):
        print('    turn%iv0' % j)
    print('  }')
    print('}')
    if prop == 'fairness':
        print('liveness property "starvation freedom for process %i" {' % p)
        for i in range(1,n+1):
            print('    t%is1 + ' % i, end='')
            for j in range(1,n):
                print('t%is2j%i + ' % (i,j), end='')
            for j in range(1,n):
                for k in range(0,n+1):
                    print('t%is3j%iturn%iv%i + ' % (i,j,j,k), end='')
            for j in range(1,n):
                print('t%is4j%i + ' % (i,j), end='')
            for j in range(1,n):
                for k in chain(range(1,i), range(i+1,n+1)):
                    for v in range(0,n+1):
                        print('t%is5j%ik%iq%iv%i + ' % (i,j,k,k,v), end='')
            for j in range(1,n):
                for k in chain(range(1,i), range(i+1,n+1)):
                    for v in range(0,n+1):
                        print('t%is6j%ik%iturn%iv%i + ' % (i,j,k,j,v), end='')
            for j in range(1,n):
                for k in chain(range(1,i), range(i+1,n+1)):
                    print('t%is7j%ik%i + ' % (i,j,k), end='')
            for j in range(1,n):
                for k in chain(range(1,i), range(i+1,n+1)):
                    print('t%is8j%ik%i + ' % (i,j,k), end='')
            for j in range(1,n):
                print('t%is9j%i + ' % (i,j), end='')
            print('t%is10 + ' % i, end='')
            print('t%is11 + ' % i, end='')
            print('t%is12 + ' % i, end='')
            print('t%is13 > 0 &&' % i)
        print('    t%is11 = 0' % p)
        print('}')
    elif prop == 'safety':
        print('safety property "mutual exclusion" {')
        print('    p1s11', end='')
        for i in range(2,n+1):
            print(' + p%is11' % i, end='')
        print(' >= 2')
        print('}')

n = int(sys.argv[1])
prop = 'fairness'
process = 1

make_net(n, prop, process)
