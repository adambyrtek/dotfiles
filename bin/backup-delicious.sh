#!/bin/sh

# Backup del.icio.us items

DATE=$(date "+%Y%m%d")
OUTPUT="$HOME/backup/delicious-backup-$DATE.xml"

curl -s https://alpha.pl:harakiri@api.del.icio.us/v1/posts/all > $OUTPUT
