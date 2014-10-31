#!/bin/sh
lckfile=/tmp/rsync-backup.lock
if shlock -f ${lckfile} -p $$
then
echo "rsync transfer starting at $(date)"
rsync -avz -r ~/popcycle/* /Volumes/seaflow_backup
rm ${lckfile}
else
echo "rsync did not run, lock ${lckfile} already held by $(cat ${lckfile})"
fi