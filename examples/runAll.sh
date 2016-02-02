#!/usr/bin/env bash

set -e

extend_examples="arraytest
extend_type_conversion
inherit
testdll
def_example
hello
wrap"

embed_examples="pyind
simple_embedded
interpcontext
pydobject"

for folder in $extend_examples; do
    pushd $folder
    $1 ../../genDubConfig.py $folder > dub.json
    [[ $# = 1 ]] && dub build -v
    [[ $# = 2 ]] && dub build -v --compiler=$2
    [[ $# = 3 ]] && dub build -v --compiler=$2 --arch=$3
    $1 test.py
    popd
done

for folder in $embed_examples; do
    pushd $folder
    $1 ../../genDubConfig.py $folder embed > dub.json
    [[ $# = 1 ]] && dub build -v
    [[ $# = 2 ]] && dub build -v --compiler=$2
    [[ $# = 3 ]] && dub build -v --compiler=$2 --arch=$3
    ./$folder
    popd
done
