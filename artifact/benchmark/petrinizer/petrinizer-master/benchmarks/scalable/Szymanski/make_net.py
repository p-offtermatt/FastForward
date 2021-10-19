#!/usr/bin/python3

import sys

#Entry protocol
#s1: non-critical section
#s2: flag[self] ← 1                    #Standing outside waiting room
#s3: await(all flag[1..N] ∈ {0, 1, 2}) #Wait for open door
#s4: flag[self] ← 3                    #Standing in doorway
#s5: if any flag[1..N] = 1:            #Another process is waiting to enter
#s6:   flag[self] ← 2                #Waiting for other processes to enter
#s7:   await(any flag[1..N] = 4)     #Wait for a process to enter and close the door

#s8: flag[self] ← 4                    #The door is closed
#s9: await(all flag[1..self-1] ∈ {0, 1})   #Wait for everyone of lower ID to finish exit protocol

#s10: critical section

#Exit protocol
#s11: await(all flag[self+1..N] ∈ {0, 1, 4}) #Ensure everyone in the waiting room has
                                       #realized that the door is supposed to be closed
#s12:flag[self] ← 0 #Leave. Reopen door if nobody is still in the waiting room

def make_net(n, prop, p):
    print('petri net "Szymanski\'s mutual exclusion algorithm for n=%i" {' % n)
    print('  places {', end='')
    for i in range(1,n+1):
        print()
        print('    p%is1' % i)
        print('    p%is2' % i)
        print('    p%is3' % i, end='')
        for j in range(1,n+1):
            print(' p%is3j%i' % (i,j), end='')
        print()
        print('    p%is4' % i)
        print('    p%is5' % i, end='')
        for j in range(1,n+1):
            print(' p%is5j%i' % (i,j), end='')
        print()
        print('    p%is6' % i)
        print('    p%is7' % i, end='')
        for j in range(1,n+1):
            print(' p%is7j%i' % (i,j), end='')
        print()
        print('    p%is8' % i)
        print('    p%is9' % i, end='')
        for j in range(1,i):
            print(' p%is9j%i' % (i,j), end='')
        print()
        print('    p%is10' % i)
        print('    p%is11' % i, end='')
        for j in range(i+1,n+1):
            print(' p%is11j%i' % (i,j), end='')
        print()
        print('    p%is12' % i)
        print('   ', end='')
        for j in range(0,5):
            print(' flag%iv%i' % (i,j), end='')
        print()
    print('  }')
    print('  transitions {', end='')
    for i in range(1,n+1):
        print()
        print('    t%is1' % i)
        print('    t%is2' % i)
        print('    t%is3' % i, end='')
        for j in range(1,n+1):
            if i == j:
                print(' t%is3j%iflag%i' % (i,i,i), end='')
            else:
                for k in range(0,5):
                    print(' t%is3j%iflag%iv%i' % (i,j,j,k), end='')
        print()
        print('    t%is4' % i)
        print('    t%is5' % i, end='')
        for j in range(1,n+1):
            if i == j:
                print(' t%is5j%iflag%i' % (i,i,i), end='')
            else:
                for k in range(0,5):
                    print(' t%is5j%iflag%iv%i' % (i,j,j,k), end='')
        print()
        print('    t%is6' % i)
        print('    t%is7' % i, end='')
        for j in range(1,n+1):
            if i == j:
                print(' t%is7j%iflag%i' % (i,i,i), end='')
            else:
                for k in range(0,5):
                    print(' t%is7j%iflag%iv%i' % (i,j,j,k), end='')
        print()
        print('    t%is8flag%iv2 t%is8flag%iv3' % (i,i,i,i))
        print('    t%is9' % i, end='')
        for j in range(1,i):
            for k in range(0,5):
                print(' t%is9j%iflag%iv%i' % (i,j,j,k), end='')
        print()
        print('    t%is10' % i)
        print('    t%is11' % i, end='')
        for j in range(i+1,n+1):
            for k in range(0,5):
                print(' t%is11j%iflag%iv%i' % (i,j,j,k), end='')
        print()
        print('    t%is12' % i)
    print('  }')
    print('  arcs {', end='')
    for i in range(1,n+1):
        print()
        print()
        print('    { p%is1 } -> t%is1 -> { p%is2 }' % (i,i,i))
        print()
        print('    { p%is2 flag%iv0 } -> t%is2 -> { p%is3 flag%iv1 }' % (i,i,i,i,i))
        print()
        print('    { p%is3 } -> t%is3 -> { p%is3j1 }' % (i,i,i))
        for j in range(1,n+1):
            if i == j:
                if j < n:
                    print('    { p%is3j%i } -> t%is3j%iflag%i -> { p%is3j%i }'
                        % (i,j,i,j,j,i,j+1))
                else:
                    print('    { p%is3j%i } -> t%is3j%iflag%i -> { p%is4 }'
                        % (i,j,i,j,j,i))
            else:
                for v in range(0,5):
                    if v < 3:
                        if j < n:
                            print('    { p%is3j%i flag%iv%i } -> t%is3j%iflag%iv%i -> { p%is3j%i flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j+1,j,v))
                        else:
                            print('    { p%is3j%i flag%iv%i } -> t%is3j%iflag%iv%i -> { p%is4 flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,v))
                    else:
                        print('    { p%is3j%i flag%iv%i } -> t%is3j%iflag%iv%i -> { p%is3j%i flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,j,v))
        print()
        print('    { p%is4 flag%iv1 } -> t%is4 -> { p%is5 flag%iv3 }' % (i,i,i,i,i))
        print()
        print('    { p%is5 } -> t%is5 -> { p%is5j1 }' % (i,i,i))
        for j in range(1,n+1):
            if i == j:
                if j < n:
                    print('    { p%is5j%i } -> t%is5j%iflag%i -> { p%is5j%i }'
                        % (i,j,i,j,j,i,j+1))
                else:
                    print('    { p%is5j%i } -> t%is5j%iflag%i -> { p%is8 }'
                        % (i,j,i,j,j,i))
            else:
                for v in range(0,5):
                    if v == 1:
                        print('    { p%is5j%i flag%iv%i } -> t%is5j%iflag%iv%i -> { p%is6 flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,v))
                    else:
                        if j < n:
                            print('    { p%is5j%i flag%iv%i } -> t%is5j%iflag%iv%i -> { p%is5j%i flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j+1,j,v))
                        else:
                            print('    { p%is5j%i flag%iv%i } -> t%is5j%iflag%iv%i -> { p%is8 flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,v))
        print()
        print('    { p%is6 flag%iv3 } -> t%is6 -> { p%is7 flag%iv2 }' % (i,i,i,i,i))
        print()
        print('    { p%is7 } -> t%is7 -> { p%is7j1 }' % (i,i,i))
        for j in range(1,n+1):
            if i == j:
                if j < n:
                    print('    { p%is7j%i } -> t%is7j%iflag%i -> { p%is7j%i }'
                        % (i,j,i,j,j,i,j+1))
                else:
                    print('    { p%is7j%i } -> t%is7j%iflag%i -> { p%is7 }'
                        % (i,j,i,j,j,i))
            else:
                for v in range(0,5):
                    if v == 4:
                        print('    { p%is7j%i flag%iv%i } -> t%is7j%iflag%iv%i -> { p%is8 flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,v))
                    else:
                        if j < n:
                            print('    { p%is7j%i flag%iv%i } -> t%is7j%iflag%iv%i -> { p%is7j%i flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j+1,j,v))
                        else:
                            print('    { p%is7j%i flag%iv%i } -> t%is7j%iflag%iv%i -> { p%is7 flag%iv%i }'
                                % (i,j,j,v,i,j,j,v,i,j,v))
        print()
        print('    { p%is8 flag%iv2 } -> t%is8flag%iv2 -> { p%is9 flag%iv4 }' % (i,i,i,i,i,i))
        print('    { p%is8 flag%iv3 } -> t%is8flag%iv3 -> { p%is9 flag%iv4 }' % (i,i,i,i,i,i))
        print()
        if i == 1:
            print('    { p%is9 } -> t%is9 -> { p%is10 }' % (i,i,i))
        else:
            print('    { p%is9 } -> t%is9 -> { p%is9j1 }' % (i,i,i))
        for j in range(1,i):
            for v in range(0,5):
                if v < 2:
                    if j < i - 1:
                        print('    { p%is9j%i flag%iv%i } -> t%is9j%iflag%iv%i -> { p%is9j%i flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j+1,j,v))
                    else:
                        print('    { p%is9j%i flag%iv%i } -> t%is9j%iflag%iv%i -> { p%is10 flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j,v))
                else:
                    print('    { p%is9j%i flag%iv%i } -> t%is9j%iflag%iv%i -> { p%is9j%i flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j,j,v))
        print()
        print('    { p%is10 } -> t%is10 -> { p%is11 }' % (i,i,i))
        print()
        if i == n:
            print('    { p%is11 } -> t%is11 -> { p%is12 }' % (i,i,i))
        else:
            print('    { p%is11 } -> t%is11 -> { p%is11j%i }' % (i,i,i,i+1))
        for j in range(i+1,n+1):
            for v in range(0,5):
                if v < 2 or v > 3:
                    if j < n:
                        print('    { p%is11j%i flag%iv%i } -> t%is11j%iflag%iv%i -> { p%is11j%i flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j+1,j,v))
                    else:
                        print('    { p%is11j%i flag%iv%i } -> t%is11j%iflag%iv%i -> { p%is12 flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j,v))
                else:
                    print('    { p%is11j%i flag%iv%i } -> t%is11j%iflag%iv%i -> { p%is11j%i flag%iv%i }'
                            % (i,j,j,v,i,j,j,v,i,j,j,v))
        print()
        print('    { p%is12 flag%iv4 } -> t%is12 -> { p%is1 flag%iv0 }' % (i,i,i,i,i))
        print()
    print('  }')
    print('  initial {')
    for i in range(1,n+1):
        print('    p%is1 flag%iv0' % (i,i))
    print('  }')
    print('}')
    if prop == 'fairness':
        print('liveness property "starvation freedom for process %i" {' % p)
        for i in range(1,n+1):
            print('    t%is1 + ' % i, end='')
            print('t%is2 + ' % i, end='')
            print('t%is3 + ' % i, end='')
            for j in range(1,n+1):
                if i == j:
                    print('t%is3j%iflag%i + ' % (i,i,i), end='')
                else:
                    for k in range(0,5):
                        print('t%is3j%iflag%iv%i + ' % (i,j,j,k), end='')
            print('t%is4 + ' % i, end='')
            print('t%is5 + ' % i, end='')
            for j in range(1,n+1):
                if i == j:
                    print('t%is5j%iflag%i + ' % (i,i,i), end='')
                else:
                    for k in range(0,5):
                        print('t%is5j%iflag%iv%i + ' % (i,j,j,k), end='')
            print('t%is6 + ' % i, end='')
            print('t%is7 + ' % i, end='')
            for j in range(1,n+1):
                if i == j:
                    print('t%is7j%iflag%i + ' % (i,i,i), end='')
                else:
                    for k in range(0,5):
                        print('t%is7j%iflag%iv%i + ' % (i,j,j,k), end='')
            print('t%is8flag%iv2 + t%is8flag%iv3 + ' % (i,i,i,i), end='')
            print('t%is9 + ' % i, end='')
            for j in range(1,i):
                for k in range(0,5):
                    print('t%is9j%iflag%iv%i + ' % (i,j,j,k), end='')
            print('t%is10 + ' % i, end='')
            print('t%is11 + ' % i, end='')
            for j in range(i+1,n+1):
                for k in range(0,5):
                    print('t%is11j%iflag%iv%i + ' % (i,j,j,k), end='')
            print('t%is12 > 0 &&' % i)
        print('    t%is10 = 0' % p)
        print('}')
    elif prop == 'safety':
        print('safety property "mutual exclusion" {')
        print('    p1s10', end='')
        for i in range(2,n+1):
            print(' + p%is10' % i, end='')
        print(' >= 2')
        print('}')

n = int(sys.argv[1])
prop = 'fairness'
process = 1

make_net(n, prop, process)
