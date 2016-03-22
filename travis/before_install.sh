#!/bin/bash

# Exit immediately when any command fails
set -e

# Add the current user to the `staff` group so it can install R packages
sudo adduser $USER staff

# Update Ubuntu's R version
# First, update the ubuntu mirror
# .. see http://cran.rstudio.com/bin/linux/ubuntu/
echo 'deb http://cran.us.r-project.org/bin/linux/ubuntu precise/' | sudo tee -a /etc/apt/sources.list
# .. also add the key of the signer
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# .. and update the packages
sudo apt-get update

# Install R for real, and sqlite3 just in case it's not available
sudo apt-get install -y r-base sqlite3 python-pip libssl-dev libcurl4-openssl-dev libssh2-1-dev

# Install Python dateutil library
sudo pip install python-dateutil
