#!/bin/sh
export PATH=$PATH:/usr/local/bin:/usr/bin
lckfile=/tmp/SeaFlow_RT.lock
if shlock -f ${lckfile} -p $$
then
echo "cron job starting at $(date)"
Rscript ~/popcycle-master/executable_scripts/copy_file.R > ~/cron_job.out 2>&1
python ~/popcycle-master/executable_scripts/fix_sfl_newlines.py >> ~/cron_job.out 2>&1
python ~/popcycle-master/executable_scripts/import_sfl.py -d ~/popcycle/sqlite/popcycle.db -c realtime -e ~/SeaFlow/datafiles/evt/ >> ~/cron_job.out 2>&1
Rscript ~/popcycle-master/executable_scripts/cron_job.R >> ~/cron_job.out 2>&1
rm ${lckfile}
else
echo "cron job did not run, lock ${lckfile} already held by $(cat ${lckfile})"
fi
