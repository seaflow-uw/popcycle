#!/bin/bash

# Exit with failure when any command in this file fails
set -e

# Setup SeaFlow directories
mkdir ~/SeaFlow
mkdir ~/SeaFlow/datafiles
mkdir ~/SeaFlow/datafiles/evt

# Setup popcycle directories
mkdir ~/popcycle
mkdir ~/popcycle/sqlite
mkdir ~/popcycle/params
mkdir ~/popcycle/params/gates
mkdir ~/popcycle/params/filter

mkdir ~/popcycle/logs
mkdir ~/popcycle/logs/gates
mkdir ~/popcycle/logs/filter

# Create the initial database
sqlite3 ~/popcycle/sqlite/popcycle.db < sql/popcycle.sql
# Run the R setup scripts
Rscript setup.R
