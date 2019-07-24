#!/bin/bash

filename=$(basename ${@: -1})
output=$(mktemp)

textidote \
    --ignore sh:seclen \
    --no-color \
    --check en \
    --dict ~/.config/languagetool/ignore-words.txt \
    "$@" > $output

exit_code=$?

cat $output | \
    perl -pe "s/\* L(\d+)C(\d+)[^ ]+/$filename:\1:\2 --/" | \
    perl -pe 's/([^]]) \n$/\1 /' | \
    perl -pe 's/[ ]+/ /g' | \
    perl -pe 's/(.*) -- (.*)\[(.*)\] $/\1: [\3] \2/' | \
    grep -v '^\s\+'

rm $output
