#!/usr/bin/env bash

set -e

examples="arraytest
extend_type_conversion
inherit
testdll
def_example
hello
wrap"

for folder in $examples; do
    pushd $folder
    $1 ../../genDubConfig.py $folder > dub.json
    [[ $# = 1 ]] && dub build -v
    [[ $# = 2 ]] && dub build -v --compiler=$2
    [[ $# = 3 ]] && dub build -v --compiler=$2 --arch=$3
    $1 test.py
    popd
done
