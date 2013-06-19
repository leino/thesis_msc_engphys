#!/bin/bash

echo adding hypergraphs...
time runhaskell addhypergraphs.hs strangeclass.db 3 2 -d1:2 -D2:2

for NUM_ITERATIONS in {10..300..10}
do
    echo adding experiments for UCT$NUM_ITERATIONS...
    time runhaskell addexperiments.hs strangeclass.db UCT$NUM_ITERATIONS Perfect 10
done

echo planning...
time runhaskell plan.hs strangeclass.db
echo playing...
time runhaskell -i../poga play.hs strangeclass.db