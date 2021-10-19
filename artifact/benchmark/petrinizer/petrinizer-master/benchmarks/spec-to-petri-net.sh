#!/bin/bash

sed '/#/D' >/tmp/pp-spec.pp
echo "EOF" >>/tmp/pp-spec.pp

echo "petri net {"
echo "places {"

# convert places
</tmp/pp-spec.pp \
sed -e '/vars/,/rules/!D' \
    -e '/vars/D' \
    -e '/rules/D' \
    -e 's/[[:blank:]][[:blank:]]*/ /g' \
    -e '/^[[:blank:]]*$/D' \
    -e 's/^[[:blank:]]*//'

echo "}"
echo "transitions {"

# convert transitions
</tmp/pp-spec.pp \
sed -e '/rules/,/init/!D' \
    -e '/rules/D' \
    -e '/init/D' | \
tr '\n' ' ' | \
sed -e 's/;/, \
/g' | \
(
    n=0
    while read line; do
        if [[ $line =~ "->" ]]; then
            n=$((n+1))
            echo -n "t$n "
        fi
    done
    echo
)

echo "}"
echo "arcs {"

# convert arcs
</tmp/pp-spec.pp \
sed -e '/rules/,/init/!D' \
    -e '/rules/D' \
    -e '/init/D' | \
tr '\n' ' ' | \
sed -e 's/;/\
/g' | \
(
    n=0
    while read line; do
        if [[ $line =~ "->" ]]; then
            n=$((n+1))
            echo "t$n : $line"
        else
            echo $line
        fi
    done
) | \
sed -e 's/,/ /g' \
    -e '/^[[:blank:]]*$/D' \
    -e 's/^[[:blank:]]*//' \
    -e 's/[[:blank:]]$*//' \
    -e 's/[[:alpha:]_][[:alnum:]_]*[[:blank:]]*>=[[:blank:]]*0[[:blank:]]*//' \
    -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*>=[[:blank:]]*\([[:digit:]][[:digit:]]*\)/\1[\2]/g" \
    -e "s/[[:alpha:]_][[:alnum:]_]*'[[:blank:]]*=//g" \
    -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*+[[:blank:]]*\([[:digit:]][[:digit:]]*\)/\1[\2]/g" \
    -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*-[[:blank:]]*\([[:digit:]][[:digit:]]*\)/\1[-\2]/g" \
    -e "s/\([[:blank:]]*:[[:blank:]]*\([[:alpha:]_].*\)->[[:blank:]]*\)/\1\2/" | \
(
    while read line; do
      re="(^.*->.*[[:blank:]])([[:alpha:]_][[:alnum:]_]*)[[:blank:]]*\[([[:digit:]]+)\][[:blank:]]*(.*[[:blank:]])\2[[:blank:]]*\[(-?[[:digit:]]+)\][[:blank:]]*(.*)$"
      while [[ $line =~ $re ]]; do
        out_weight=$((BASH_REMATCH[3] + BASH_REMATCH[5]))
        if [[ $out_weight -gt 0 ]]; then
          line=${BASH_REMATCH[1]}${BASH_REMATCH[2]}"["$out_weight"] "${BASH_REMATCH[4]}${BASH_REMATCH[6]}
        else
          line=${BASH_REMATCH[1]}${BASH_REMATCH[4]}${BASH_REMATCH[6]}
        fi
      done
      echo $line
    done
) | \
(
    while read line; do
      re="^[[:blank:]]*([[:alpha:]_][[:alnum:]_]*)[[:blank:]]*:[[:blank:]]*(.*)[[:blank:]]*->[[:blank:]]*(.*)[[:blank:]]*$"
      if [[ $line =~ $re ]]; then
        t=${BASH_REMATCH[1]}
        in=${BASH_REMATCH[2]}
        out=${BASH_REMATCH[3]}
        nl='\n'
        echo -n $in | \
            sed -e "s/[[:blank:]]*\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*\[\([[:digit:]]\)*\][[:blank:]]*/\1 ->[\2] $t$nl/g"
        echo $out | \
            sed -e "s/[[:blank:]]*\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*\[\([[:digit:]]\)*\][[:blank:]]*/$t ->[\2] \1$nl/g"
      fi
    done
)

echo "}"
echo "initial {"

# initial conditions
</tmp/pp-spec.pp \
sed -e '/init/,/target/!D' \
    -e '/init/D' \
    -e '/target/D' | \
tr '\n' ' ' | \
sed -e 's/$/\
/' | \
sed -e 's/,[[:blank:]]*/\
/g' | \
sed -e 's/^[[:blank:]]*//' \
    -e 's/[[:blank:]]*$//' \
    -e '/[[:alpha:]_][[:alnum:]_]*[[:blank:]]*=[[:blank:]]*0/D' \
    -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*=[[:blank:]]*\([[:digit:]][[:digit:]_]*\)/\1[\2]/" | \
(
    n=0
    while read line; do
        if [[ $line =~ (>=[[:blank:]]*[[:digit:]]) ]]; then
            n=$((n+1))
            echo $line | \
              sed -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*>=[[:blank:]]*\
\([[:digit:]][[:digit:]_]*\)/\1[\2]=}=transitions { init$n }=\
arcs { init$n -> \1 }=initial {/" | tr '=' '\n'
        else
            echo $line
        fi
    done
)

echo "}"
echo "}"

echo "safety property {"

# target conditions
</tmp/pp-spec.pp \
sed -e '/target/,/invariants/!D' \
    -e '/target/,/EOF/!D' \
    -e '/target/D' \
    -e '/invariants/D' \
    -e '/EOF/D' \
    -e '/^[[:blank:]]*$/D' \
    -e 's/[[:blank:]]*,[[:blank:]]*/ \&\& /g' | \
#    -e "s/\([[:alpha:]_][[:alnum:]_]*\)[[:blank:]]*>=[[:blank:]]*\([[:alnum:]_][[:alnum:]_]*\)/(['\1'],\2)/g"
(
    read line
    echo -n "($line)"
    while read line; do
        echo -n " || ($line)"
    done
    echo
)

echo "}"
