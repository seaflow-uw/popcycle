#!/bin/sh

# Add the current user to the `staff` group so it can install R packages
sudo useradd -a -G staff $USER

# Install R
sudo apt-get install r-base
