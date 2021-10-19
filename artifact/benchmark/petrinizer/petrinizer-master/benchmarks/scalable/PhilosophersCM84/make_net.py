#!/usr/bin/python3

import sys

print("""
petri net "The drinking philosophers for n=2" {
    places {
        p1h p1e
        p2h p2e

        req1p1 req1p2
        req2p1 req2p2

        fork1p1 fork1p2
        fork2p1 fork2p2

        fork1clean fork1dirty
        fork2clean fork2dirty
    }
    transitions {
        p1req1 p1req2 p1give1 p1give2 p1eat p1done
        p2req1 p2req2 p2give1 p2give2 p2eat p2done
        //p1done(2),p1eat(2),p1give1,p1give2,p1req1,p1req2,
        //p2give1,p2give2,p2req1,p2req2
    }
    arcs {
        { p1h req1p1 fork1p2 } -> p1req1  -> { p1h req1p2 fork1p2 }
        { p1h req2p1 fork2p2 } -> p1req2  -> { p1h req2p2 fork2p2 }
        { p1h req1p1 fork1p1 fork1dirty } -> p1give1  -> { p1h req1p1 fork1p2 fork1clean }
        { p1h req2p1 fork2p1 fork2dirty } -> p1give2  -> { p1h req2p1 fork2p2 fork2clean }
        { p1h fork1p1 fork2p1 fork1clean fork2clean } -> p1eat  -> { p1e fork1p1 fork2p1 fork1dirty fork2dirty }
        { p1e } -> p1done  -> { p1h }

        { p2h req1p2 fork1p1 } -> p2req1  -> { p2h req1p1 fork1p1 }
        { p2h req2p2 fork2p1 } -> p2req2  -> { p2h req2p1 fork2p1 }
        { p2h req1p2 fork1p2 fork1dirty } -> p2give1  -> { p2h req1p2 fork1p1 fork1clean }
        { p2h req2p2 fork2p2 fork2dirty } -> p2give2  -> { p2h req2p2 fork2p1 fork2clean }
        { p2h fork1p2 fork2p2 fork1clean fork2clean } -> p2eat  -> { p2e fork1p2 fork2p2 fork1dirty fork2dirty }
        { p2e } -> p2done  -> { p2h }
    }
    initial {
        p1h p2h
        fork1dirty fork2dirty
        fork1p1 fork2p1 req1p2 req2p2
    }
}
liveness property "philosopher 1 does not starve" {
    p1req1 + p1req2 + p1give1 + p1give2 + p1eat + p1done > 0 &&
    p2req1 + p2req2 + p2give1 + p2give2 + p2eat + p2done > 0 &&
    p1eat = 0
}
liveness property "philosopher 2 does not starve" {
    p1req1 + p1req2 + p1give1 + p1give2 + p1eat + p1done > 0 &&
    p2req1 + p2req2 + p2give1 + p2give2 + p2eat + p2done > 0 &&
    p2eat = 0
}
safety property "mutual exclusion" {
    p1e >= 1 && p2e >= 1
}
""")
