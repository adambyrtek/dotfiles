#!/bin/bash

cd $1
DISPATCH="$(pwd)/public/dispatch.fcgi"

if [ ! -f $DISPATCH ]; then
    echo "Dispatcher $DISPATCH doesn't exist"
    exit 1
fi

PIDS=$(pgrep -f $DISPATCH)
echo "Killing the following processes: $PIDS"
kill $PIDS
