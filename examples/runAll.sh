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
    $TEST_PYTHON ../../genDubConfig.py $folder > dub.json
    [[ $# = 0 ]] && dub build
    [[ $# = 1 ]] && dub build --compiler=$1
    [[ $# = 2 ]] && dub build --compiler=$1 --arch=$2
    $TEST_PYTHON test.py
    popd
done
