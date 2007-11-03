#!/bin/sh

# Set correct FactCGI permissions for Rails app

PROGNAME=`basename $0`
if [ $# != 1 ]; then
    echo "Usage: $PROGNAME railsdir"
    exit 1
fi

APPDIR=$1
if [ ! -d "$APPDIR" ]; then
    echo "$PROGNAME: $APPDIR is not a directory"
    exit 1
fi
if [ ! -r "$APPDIR/Rakefile" ]; then
    echo "$PROGNAME: $APPDIR doesn't look like Rails application"
    exit 1
fi

REQFILES="public/dispatch.fcgi config/database.yml tmp log"
for FILE in $REQFILES; do
    if [ ! -e $FILE ]; then
        echo "$PROGNAME: Required file $FILE not exists"
        exit 1
    fi
done

cd $APPDIR

chgrp -R www-data tmp log config/database.yml
chmod -R g+w tmp log
chmod 640 config/database.yml
chmod a+x public/dispatch.fcgi
if head -1 public/dispatch.fcgi | grep -v '^#!/usr/bin/env ruby$' > /dev/null 2>&1; then
    echo "Please fix bang line in dispatch.fcgi, should be:"
    echo "#!/usr/bin/env ruby"
fi
