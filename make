#!/bin/bash
stack build
if [[ $? != 0 ]] ; then
    echo "Build failed"
    exit 1
fi

stack exec Qommunist-exe
STACK_PATH=$(stack path --local-install-root)
cp "$STACK_PATH/bin/Qommunist-exe" .