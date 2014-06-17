PopCycle
========

PopCycle is a R/python package that process in real-time continuous flow cytometry data from SeaFlow.

Setup instructions: 
> bash setup.sh

This should create all the necessary directories, the popcycle database, and install popcycle as an R package. The setup.R
script should also install RSQLite and splancs packages if they are not already installed.

Uploading old files:

If you intend to upload old OPP and VCT files, you can use the template of executable_scripts/upload_old_opp_vct_test.R.
This script assumes that the old OPP files are stored in the following way:

/path/to/cruise/days/files.evt.opp
/path/to/cruise/days/files.evt.opp.*vct

If you point the full.path variable to point to the right location, and also provide the location of the database (change
the db variable) and the cruise name (the cruise.name variable), the script will upload all OPP and VCT files in this directory
structure. If the OPP and VCT files are stored in a different hierarchy, you may need to change the code to load the OPP and
VCT file paths. This script assumes that the files are the old seaflow style, and will name the files day/file_name.


