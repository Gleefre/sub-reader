#!/usr/bin/env bash

# find out a directory where .sh lies
WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/bin"

cd $WORK_DIR
./sub-reader
