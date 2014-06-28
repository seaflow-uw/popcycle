#!/bin/bash

# Exit immediately when any command fails
set -e

# Add the current user to the `staff` group so it can install R packages
sudo adduser $USER staff
newgrp -

# Install R
sudo apt-get install r-base
