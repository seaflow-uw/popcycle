#!/bin/bash

mkdir ~/popcycle
mkdir ~/popcycle/sqlite
mkdir ~/popcycle/params
mkdir ~/popcycle/params/gates
mkdir ~/popcycle/logs

sqlite3 ~/popcycle/sqlite/popcycle.db < sql/popcycle.sql
