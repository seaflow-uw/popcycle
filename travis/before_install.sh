#!/bin/bash

# Add the current user to the `staff` group so it can install R packages
sudo adduser $USER staff

# Install R
sudo apt-get install r-base
