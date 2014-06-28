#!/bin/bash

# Exit immediately when any command fails
set -e

# Add the current user to the `staff` group so it can install R packages
sudo adduser $USER staff

# Install R
# .. but we need a new R, so first update the ubuntu mirror
# .. see http://cran.rstudio.com/bin/linux/ubuntu/
echo 'deb http://cran.us.r-project.org/bin/linux/ubuntu trusty/' | sudo tee -a /etc/apt/sources.list
sudo apt-get update
# Install R
sudo apt-get install r-base
