#!/bin/sh

# Backup del.icio.us items
# first argument should be username, second a password

DATE=$(date "+%Y%m%d")
OUTPUT="$HOME/backup/delicious-backup-$DATE.xml"

curl -s https://$1:$2@api.del.icio.us/v1/posts/all > $OUTPUT
