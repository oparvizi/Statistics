---
title: "Complex Surveys"
output: html_document
date: "2023-01-15"
source: https://www.wiley.com/en-us/Complex+Surveys%3A+A+Guide+to+Analysis+Using+R-p-9780470284308
---
# The analysis of complex survey samples, in contrast, is usually design-based
# The fundamental statistical concept in design-based inference is the probability sumple or random sample. 

# Sampling weights-------------------------------------
# The fundamental statistical idea behind all of design-based inference is that an individual sampled with a sampling probability of ni represents l/ni individuals in 
# the population. The value l/ni is called the sampling weight. 

# Other meanings of “weights” Statisticians and statistical software use the term ‘weight’ to mean at least three different things.--------
# sampling weights: A sampling weight of 1000 means that the observation represents 1000 individuals in the population. 
# precision weights: A precision (or inverse-variance) weight of 1000 means that the observation has 1000 times lower variance than an observation with a weight of 1. 
# frequency weights: A frequency weight of 1000 means that the sample contains 1000 identical observations and space is being saved by using only one record in the data set to represent them. 

# Design effects-----------------------------------------
# The design effect was defined by Kish (1965) as the ratio of a variance of an estimate in a complex sample to the variance of the same estimate in a simple random sample

# Real surveys:  
# Reading plain text data--------------------------------
nwts <- read. csv("C:/svybook/nwts/nwts-share. csvl')
summary(nwts); head(nwts); nrow(hwts); ncol(nwts);  

# Reading data from other packages-----------------------
# The functions readexport 0, read.dta0, and read.spss0 will read SAS XPORT, Stata, and SPSS files.
library(foreign) 
demo<-read.xport ("~/nhanes/demo_c.xpt") 
names(demo) 
# Simple computations------------------------------------
demo$RIDAGEYR [100 : 1501]                # Positive numbers
demo$RIDAGEYR [-c (1 : 10, 100 : 1OOO)]   # Negative numbers
demo$RIDAGEYR [demo$RIAGENDR==l]          # Logical (TRUe/FALSE) vectors
with(demo, RIDAGEYR [RIAGENDR==1]         # with (1 specifies a particular data set as the default place to look up variables.)
pbc[pbc$trt == -9] <- NA                  # Missing data

# SIMPLE AND STRATIFIED SAMPLING-------------------------
# ANALYZING SIMPLE RANDOM SAMPLES: 
# Horvitz-Thompson estimator, Confidence intervals 
library (survey) 
data(api) 
srs-design <- svydesign(id="l, fpc="fpc, 
srs-design 
# Independent Sampling design 
svydesign(id = ~1, fpc = ~fpc, data = apisrs) 
svytotal(~enrol1, srs_design) 
svymean(~enroll, srs_design) 
                                        
nofpc <- svydesign(id=~1, weights=~pw, data=apisrs) 
nofpc 
# Independent Sampling design (with replacement) 
svydesign(id = ~1, weights = ~pw, data = apisrs) 
svytotal(~enrol1, nofpc) 
svymean(~enrol1, nofpc) 

svycontrast(means, quote(api00-api99)) 

# STRATIFIED SAMPLING-----------------------------------
means <- svymean(~apiOO+api99, srs_design) 
svycontrast (means, c (api00=1, api99=-1)) 
srs-design <- update (srs_design, apidif f =apiOO-api99) 
srs-design <- update(srs_design, apipct = apidiff/api99) 
svymean(*apidiff+apipct, srs_design) 
 page 23





















