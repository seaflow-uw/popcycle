#!/bin/bash
# Reset realtime analysis data.  This script should be run if you want to
# completely restart a realtime analysis.
# CAUTION: It will erase all current work in the project!

# This script will erase
#  ~/stat.csv
#  ~/sfl.csv
#  ~/RT_analysis.log
#  ~/cron_job.out
#  ~/SeaFlow/datafiles/evt/*
#  ~/popcycle/
# 
# This script will also recreate ~/popcycle/ with no filtering or gating
# parameters and an popcycle sqlite3 database

rm ~/stat.csv
rm ~/sfl.csv
rm ~/RT_analysis.log
rm ~/cron_job.out
rm -rf ~/SeaFlow/datafiles/evt/*
rm -rf ~/popcycle

Rscript ~/popcycle-master/executable_scripts/only_load_library.R
