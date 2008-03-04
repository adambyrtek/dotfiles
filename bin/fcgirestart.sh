#!/bin/bash

if [[ -z $1 ]]; then
    echo "First argument should be a full path to the application"
    exit 1
fi

DISPATCH="$1/public/dispatch.fcgi"
PIDS=$(pgrep -f $DISPATCH)

if [[ -z $PIDS ]]; then
    echo "No processes for dispatcher $DISPATCH found"
    echo "Are you sure you've provided the full path?"
    exit 1
fi

echo "Killing the following processes: $PIDS"
kill $PIDS
