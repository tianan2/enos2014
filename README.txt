###readme.txt

This folder contains the replication files for "The causal effect of intergroup contact on exclusionary attitudes".

***To execute these scripts, run "analysis_master.r", this can be accomplished by typing "source('analysis_master.r')" into the R terminal
**This will call all other scripts in the folder.
*The scripts should output 3 tables and 5 plots.
*Prior to executing the scripts, the R terminal should be pointed to the folder "R_PNAS", which can be accomplished by typing "setwd('../R_PNAS')" where ".." is the local file structure where "R_PNAS" can be found

These files require the R programming language, which can be downloaded here http://www.r-project.org/

You may need to install the packages loaded in analysis_master.r, this can be accomplished by typing the following in the R terminal:
install.packages('ri')
install.packages('RItools')
install.packages('car')
install.packages('xtable')
install.packages('effects')
install.packages('RColorBrewer')

Questions? Please contact Ryan Enos (renos@gov.harvard.edu).
