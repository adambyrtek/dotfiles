#!/bin/sh

# svndiff -- svn diff with vimdiff.
#
# Written by Erik C. Thauvin (erik@thauvin.net)
# May 11, 2006
#
# Copyright (C) 2006 Erik C. Thauvin. All rights reserved.
#
# This software is provided "as is" without express or implied warranties.
#
# Permission is granted to use, copy, modify and distribute this software,
# provided this disclaimer and copyright are preserved on all copies. This
# software may not, however, be sold or distributed for profit, or included
# with other software which is sold or distributed for profit, without the
# permission of the author.
#
# $Id$

PROGNAME=`basename $0`

if [ $# != 1 ]
then
    echo "Usage: $PROGNAME <file>"
    exit 2
else
    TEMP=/tmp/tmp.$$.`basename $1`
    svn cat $1 > $TEMP
    vimdiff $TEMP $1
    rm -f $TEMP
fi
