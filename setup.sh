#!/bin/bash

mkdir ~/popcycle
mkdir ~/popcycle/sqlite
mkdir ~/popcycle/params
mkdir ~/popcycle/params/gates
mkdir ~/popcycle/logs
mkdir ~/popcycle/logs/gates

sqlite3 ~/popcycle/sqlite/popcycle.db < sql/popcycle.sql
