#!/bin/sh

# Backup del.icio.us items
# (c)2007 Adam Byrtek

DATE=`date "+%Y%m%d"`

curl -s https://alpha.pl:harakiri@api.del.icio.us/v1/posts/all > ~/backup/delicious-backup-$DATE.xml
