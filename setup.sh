#!/bin/bash

mkdir ~/popcycle
mkdir ~/popcycle/sqlite
mkdir ~/popcycle/params
mkdir ~/popcycle/params/gates
mkdir ~/popcycle/params/filter

mkdir ~/popcycle/logs
mkdir ~/popcycle/logs/gates
mkdir ~/popcycle/logs/filter

sqlite3 ~/popcycle/sqlite/popcycle.db < sql/popcycle.sql
Rscript setup.R
