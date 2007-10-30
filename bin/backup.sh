#!/bin/sh

rsync -avz -e ssh --exclude='Movies' --exclude='Pictures' --exclude='Music' --exclude='.Trash' --delete-after ~ adambyrtek.net:backup/macbook/
