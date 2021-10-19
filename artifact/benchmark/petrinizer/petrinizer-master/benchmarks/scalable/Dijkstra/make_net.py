#!/usr/bin/python3

import sys

#c[i] := 1
#b[i] := 1
#turn := 0

# s1: b[i] := 0;
# s2: if turn /= i then
# s3:   c[i] := 1
# s4:   if turn = 0 or b[turn] = 1 then
# s5:     turn := i
#     fi
# s6:   goto s2
#   fi
# s7: c[i] := 0
# s8: for j:= 1 to N do
# s9_j:  if j != i and c[j] = 0 then
#s10:    goto s2
#     fi
#   done
#s11: critical section
#s12: turn := 0
#s13: c[i] := 1
#s14: b[i] := 1
#s15: goto s1

def make_net(n, prop, p):
    print('petri net "Dijstra\'s version for n processes of Dekker\'s mutual exclusion algorithm for n=%i" {' % n)
    print('  places {', end='')
    for i in range(1,n+1):
        print()
        print('    p%is1' % i)
        print('    p%is2' % i)
        print('    p%is3' % i)
        print('    p%is4' % i)
        print('    p%is4turn0' % i, end='')
        for j in range(1,n+1):
            print(' p%is4turn%i' % (i,j), end='')
        print()
        print('    p%is5' % i)
        print('    p%is6' % i)
        print('    p%is7' % i)
        print('    p%is8' % i)
        print('   ', end='')
        for j in range(1,n+1):
            print(' p%is9j%i' % (i,j), end='')
        print()
        print('    p%is10' % i)
        print('    p%is11' % i)
        print('    p%is12' % i)
        print('    p%is13' % i)
        print('    p%is14' % i)
        print('    p%is15' % i)
        print('    c%iv0 c%iv1 b%iv0 b%iv1' % (i,i,i,i))
    print()
    print('    turn0', end='')
    for i in range(1,n+1):
        print(' turn%i' % i, end='')
    print()
    print('  }')
    print('  transitions {', end='')
    for i in range(1,n+1):
        print()
        print('    t%is1b%iv0 t%is1b%iv1' % (i,i,i,i))
        print('    t%is2turn0' % i, end='')
        for j in range(1,n+1):
            print(' t%is2turn%i' % (i,j), end='')
        print()
        print('    t%is3c%iv0 t%is3c%iv1' % (i,i,i,i))
        print('    t%is4turn0' % i, end='')
        for j in range(1,n+1):
            print(' t%is4turn%i' % (i,j), end='')
        print()
        print('   ', end='')
        for j in range(1,n+1):
            print(' t%is4turn%ib%iv0 t%is4turn%ib%iv1' % (i,j,j,i,j,j), end='')
        print()
        print('    t%is5turn0' % i, end='')
        for j in range(1,n+1):
            print(' t%is5turn%i' % (i,j), end='')
        print()
        print('    t%is6' % i)
        print('    t%is7c%iv0 t%is7c%iv1' % (i,i,i,i))
        print('    t%is8' % i)
        print('   ', end='')
        for j in range(1,n+1):
            if i == j:
                print(' t%is9j%i' % (i,j), end='')
            else:
                print(' t%is9j%ic%iv0 t%is9j%ic%iv1' % (i,j,j,i,j,j), end='')
        print()
        print('    t%is10' % i)
        print('    t%is11' % i)
        print('    t%is12turn0' % i, end='')
        for j in range(1,n+1):
            print(' t%is12turn%i' % (i,j), end='')
        print()
        print('    t%is13c%iv0 t%is13c%iv1' % (i,i,i,i))
        print('    t%is14b%iv0 t%is14b%iv1' % (i,i,i,i))
        print('    t%is15' % i)
    print('  }')
    print('  arcs {', end='')
    for i in range(1,n+1):
        print()
        print()
        print('    { p%is1 b%iv0 } -> t%is1b%iv0 -> { p%is2 b%iv0 }' % (i,i,i,i,i,i))
        print('    { p%is1 b%iv1 } -> t%is1b%iv1 -> { p%is2 b%iv0 }' % (i,i,i,i,i,i))
        print('    { p%is2 turn0 } -> t%is2turn0 -> { p%is3 turn0 }' % (i,i,i))
        for j in range(1,n+1):
            if i == j:
                print('    { p%is2 turn%i } -> t%is2turn%i -> { p%is7 turn%i }' % (i,j,i,j,i,j))
            else:
                print('    { p%is2 turn%i } -> t%is2turn%i -> { p%is3 turn%i }' % (i,j,i,j,i,j))
        print('    { p%is3 c%iv0 } -> t%is3c%iv0 -> { p%is4 c%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is3 c%iv1 } -> t%is3c%iv1 -> { p%is4 c%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is4 turn0 } -> t%is4turn0 -> { p%is5 turn0 }' % (i,i,i))
        for j in range(1,n+1):
            print('    { p%is4 turn%i } -> t%is4turn%i -> { p%is4turn%i turn%i }' % (i,j,i,j,i,j,j))
        for j in range(1,n+1):
            print('    { p%is4turn%i b%iv0 } -> t%is4turn%ib%iv0 -> { p%is6 b%iv0 }' % (i,j,j,i,j,j,i,j))
            print('    { p%is4turn%i b%iv1 } -> t%is4turn%ib%iv1 -> { p%is5 b%iv1 }' % (i,j,j,i,j,j,i,j))
        for j in range(0,n+1):
            print('    { p%is5 turn%i } -> t%is5turn%i -> { p%is6 turn%i }' % (i,j,i,j,i,i))
        print('    { p%is6 } -> t%is6 -> { p%is2 }' % (i,i,i))
        print('    { p%is7 c%iv0 } -> t%is7c%iv0 -> { p%is8 c%iv0 }' % (i,i,i,i,i,i))
        print('    { p%is7 c%iv1 } -> t%is7c%iv1 -> { p%is8 c%iv0 }' % (i,i,i,i,i,i))
        print('    { p%is8 } -> t%is8 -> { p%is9j1 }' % (i,i,i))
        for j in range(1,n+1):
            if i == j:
                if j == n:
                    print('    { p%is9j%i } -> t%is9j%i -> { p%is11 }' % (i,j,i,j,i))
                else:
                    print('    { p%is9j%i } -> t%is9j%i -> { p%is9j%i }' % (i,j,i,j,i,j+1))
            else:
                print('    { p%is9j%i c%iv0 } -> t%is9j%ic%iv0 -> { p%is10 c%iv0 }' % (i,j,j,i,j,j,i,j))
                if j == n:
                    print('    { p%is9j%i c%iv1 } -> t%is9j%ic%iv1 -> { p%is11 c%iv1 }' % (i,j,j,i,j,j,i,j))
                else:
                    print('    { p%is9j%i c%iv1 } -> t%is9j%ic%iv1 -> { p%is9j%i c%iv1 }' % (i,j,j,i,j,j,i,j+1,j))
        print('    { p%is10 } -> t%is10 -> { p%is2 }' % (i,i,i))
        print('    { p%is11 } -> t%is11 -> { p%is12 }' % (i,i,i))
        for j in range(0,n+1):
            print('    { p%is12 turn%i } -> t%is12turn%i -> { p%is13 turn0 }' % (i,j,i,j,i))
        print('    { p%is13 c%iv0 } -> t%is13c%iv0 -> { p%is14 c%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is13 c%iv1 } -> t%is13c%iv1 -> { p%is14 c%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is14 b%iv0 } -> t%is14b%iv0 -> { p%is15 b%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is14 b%iv1 } -> t%is14b%iv1 -> { p%is15 b%iv1 }' % (i,i,i,i,i,i))
        print('    { p%is15 } -> t%is15 -> { p%is1 }' % (i,i,i))
        print()
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    p%is1 b%iv1 c%iv1' % (i,i,i))
    print('    turn0')
    print('  }')
    print('}')
    if prop == 'fairness':
        print('liveness property "starvation freedom for process %i" {' % p)
        for i in range(1,n+1):
            print('    t%is1b%iv0 + t%is1b%iv1 + ' % (i,i,i,i), end='')
            print('t%is2turn0 + ' % i, end='')
            for j in range(1,n+1):
                print('t%is2turn%i + ' % (i,j), end='')
            print('t%is3c%iv0 + t%is3c%iv1 + ' % (i,i,i,i), end='')
            print('t%is4turn0 + ' % i, end='')
            for j in range(1,n+1):
                print('t%is4turn%i + ' % (i,j), end='')
            for j in range(1,n+1):
                print('t%is4turn%ib%iv0 + t%is4turn%ib%iv1 + ' % (i,j,j,i,j,j), end='')
            print('t%is5turn0 + ' % i, end='')
            for j in range(1,n+1):
                print('t%is5turn%i + ' % (i,j), end='')
            print('t%is6 + ' % i, end='')
            print('t%is7c%iv0 + t%is7c%iv1 + ' % (i,i,i,i), end='')
            print('t%is8 + ' % i, end='')
            for j in range(1,n+1):
                if i == j:
                    print('t%is9j%i + ' % (i,j), end='')
                else:
                    print('t%is9j%ic%iv0 + t%is9j%ic%iv1 + ' % (i,j,j,i,j,j), end='')
            print('t%is10 + ' % i, end='')
            print('t%is11 + ' % i, end='')
            print('t%is12turn0 + ' % i, end='')
            for j in range(1,n+1):
                print('t%is12turn%i + ' % (i,j), end='')
            print('t%is13c%iv0 + t%is13c%iv1 + ' % (i,i,i,i), end='')
            print('t%is14b%iv0 + t%is14b%iv1 + ' % (i,i,i,i), end='')
            print('t%is15 > 0 &&' % i)
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
