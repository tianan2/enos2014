###analysis_master.r
###master train analysis script
###RdE October 2012

rm(list = ls())

###THE PACKAGES BELOW MAY NEED TO BE INSTALLED USING install.packages('x'), WHERE X IS THE PACKAGE NAME
library(ri)
library(RItools)
library(car)
library(xtable)
library(effects)
library(RColorBrewer)
############################

options(scipen = 999)  ##set for non-scientific notaion output

##Load data
dat.all = read.csv('pnas_dat/pnas_data.csv')
dat.t1 = read.csv('pnas_dat/t1_data.csv')
dat.all.prime = read.csv('pnas_dat/prime_data.csv')
###data loading for faces graphic
conf.dat = read.csv('pnas_dat/confederate_face_data.csv')
hisp.dat = read.csv('pnas_dat/hispanic_face_data.csv')
white.dat = read.csv('pnas_dat/white_face_data.csv')


##the following scripts will be executed
source('balance_check.r') ##executes balance tests
source('main_results.r')  ##produces ATE and car subanalysis
source('subset_inference.r')  ##produces dose effect and subgroup effects in supporting material
source('primetest.r') ##produces prime test in supporting material
source('output_create.r')  ##creates tables
source('outgraphic_single_pnas.r') ##produces graphics, expect face graphic
source('face_summary_pnas.r') ##creates face graphic