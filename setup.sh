#!/bin/bash

mkdir ~/popcycle
mkdir ~/popcycle/sqlite
mkdir ~/popcycle/params
mkdir ~/popcycle/params/gates
mkdir ~/popcycle/params/filter
mkdir ~/popcycle/params/gates_archived
mkdir ~/popcycle/params/filter_archived

mkdir ~/popcycle/logs
mkdir ~/popcycle/logs/gates

sqlite3 ~/popcycle/sqlite/popcycle.db < sql/popcycle.sql
