---
title: "StatisticsR"
output: html_document
date: "2023-01-15"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE}
#-------------------R Fundamentals
#---------------------------------
# Arithmetic Operators
# Operation Symbol
# Addition        +
# Subtraction     -
# Multiplication  *
# Division        /
# Exponentiation  ^

(3^2+6)/(3+2)
#-------------------Functions
date()
round(3.141593)
round(3.141593, digits=2)
round(3.141593, 2)
help(functionname)

# Useful Mathematical Functions
# Purpose          Function
# Exponential         exp
# Natural logarithm   log
# Log base 10         log10
# Square root         sqrt
# Cosine              cos
# Sine                sin
# Tangent             tan
# Arc cosine          acos
# Arc sine            asin
# Arc tangent         atan
# Round               round
# Absolute value      abs
# Factorial           factorial

#-------------------Objects
# Simple Objects
height<-72.95
heightcm<-round(height*2.54)

# Vectors
temperatures<-c(3, 3.76, -0.35, 1.2, -5)
temperatures
length(temperatures)
exp(temperatures)
mean(temperatures)

# Data Frames
Puromycin
Puromycin$rate
mean(Puromycin$rate)
dataset[r,c]
Puromycin[6,2]
Puromycin[6,]
Puromycin[,2]
Puromycin[2]
Puromycin[-1]  # to exclude the first column
Puromycin[6:10,]
Puromycin[,c(1,3)] # select two columns 

rownum<-c(6,8,14)
colnum<-2
Puromycin[rownum,colnum]
Puromycin$rate[10]
help(Puromycin)

# The Data Editor
fix(Puromycin) # an alternative to viewing datasets in the command window, R has a spreadsheet style viewer

#-------------------Workspaces
objects()
rm(height, string1, string2) # delete objects
rm(list=objects()) #  delete all of the objects
save.image("/home/Username/folder/filename.RData")
load("/home/Username/folder/filename.RData")

#-------------------Error Messages
#-------------------Script Files


#-------------------Working with Data File
#---------------------------------

# Entering Data Directly
Chain<-c("Morrisons", "Asda", "Tesco", "Sainsburys")
Stores<-c(439, NA, 2715, 934)
Sales.Area<-c(12261, NA, 36722, 19108)
Market.Share<-c(12.3, 16.9, 30.3, 16.5)
supermarkets<-data.frame(Chain, Stores, Sales.Area, Market.Share)
supermarkets
rm(Chain, Stores, Sales.Area, Market.Share)

# Importing Plain Text Files (Comma-separated values or comma-delimited (.csv) , Tab-delimited (.txt), Data interchange format (.dif))
dataset1<-read.csv("C:/folder/filename.csv")
dataset1<-read.delim("C:/folder/filename.txt")
dataset1<-read.csv("C:/folder/filename.csv", header=F)
dataset1<-read.csv("C:/folder/filename.csv", header=F, col.names=c("Name1", "Name2", "Name3"))
dataset1<-read.csv("C:/folder/filename.csv", na.strings=".")
# DIF Files
dataset1<-read.DIF("C:/folder/filename.dif")
dataset1<-read.DIF("C:/folder/filename.dif", header=T)

dataset1<-read.DIF("C:/folder/filename.dif", na.string="NULL") # na.strings argument to tell R about any values that it should interpret as missing data
dataset1<-read.DIF("C:/folder/filename.dif", transpose=T)
# Other Plain Text Files
dataset1<-read.table("C:/folder/supermarkets.txt", sep="/", header=T) # Plain Text Files
dataset1<-read.table("C:/folder/filename.txt", sep="/", header=T, na.strings="NULL")

# Importing Excel Files

# There is no more than one row of column headers
# There is no formatting such as bold, italic or colored text, cell borders or background colors
# There are no commas in large numbers (e.g., 1324157 is acceptable but 1,324,157 is not)
# If exponential (scientific) notation is used, the format is correct (e.g., 0.00312 can be expressed as 3.12e-3 or 3.12E-3)
# There are no currency, unit, or percent symbols in numeric variables (symbols in categorical variables or in the variable names are fine)
# The minus sign is used to indicate negative numbers (e.g., -5) and not brackets (parentheses) or red text
# The workbook has only one worksheet

# Importing Files from Other Software-------------------------------

#Some of the Functions Available in the Foreign Add-on Package

# File -type                            Extension      Function
# Database format file                 .dbf            read.dbf
# Stata versions 5 to 12 data file     .dta            read.dta
# Minitab portable worksheet file      .mtp            read.mtp
# SPSS data file                       .sav            read.spss
# SAS transfer format                  .xport          read.xport
# Epi Info data file                   .rec            read.epiinfo
# Octave text data file                .txt            read.octave
# Attribute-relation file              .arff           read.arff
# Systat file                          .sys, .syd      read.systat

dataset1<-read.dta("C:/folder/filename.dta")

# Using Relative File Paths
getwd()
setwd("C:/folder/subfolder")

# Exporting Datasets------------------------------------------------
write.csv(dataset, "filename.csv")
write.csv(Puromycin, "puromycin_data.csv")
write.csv(dataset, "C:/folder/filename.csv")
write.table(dataset, "filename.txt", sep="\t")
write.csv(dataset, "filename.csv", row.names=F)
write.table(dataset, "filename.txt", sep="\t", col.names=F)

# Summary---------
# Task                                    Command

# Create data frame                       dataset<-data.frame(vector1, vector2,   vector3)
# Import CSV file                         dataset<-read.csv("filepath")
# Import tab-delimited file &             dataset<-read.delim("filepath")
# Import DIF file                         dataset<-read.DIF("filepath")
# Import other text file                  dataset<-read.table("filepath, sep="?")
# Display working directory               getwd()
# Change working directory                setwd("C:/folder/subfolder")
# Export dataset to CSV file              write.csv(dataset, "filename.csv")
# Export dataset to tab-delimited file    write.table(dataset, "filename.txt",sep="\t")

#-------------------Preparing and Manipulating Your Data
#---------------------------------
# This chapter also uses the pulserates, fruit, flights, customers, and coffeeshop datasets, which are all available with the downloads for this book (www.apress.com/9781484201404) in CSV format or in an R workspace file. 

# Variables
# Rearranging and Removing Variables
people1<-subset(people, select=c(Hand.Span, Sex, Eye.Color))
people<-subset(people, select=c(Hand.Span, Sex, Eye.Color))
people1<-people[-c(1,3,6)]   # retain the variables and reorder
people1<-people[c(2,4,1)]

# Renaming Variables
names(people)
names(people)[5]<-"Gender"
names(people)[c(2,4,5)]<-c("Eyes", "Span.mm", "Gender")
names(people)<-c("Subject", "Eyes", "Height.cm", "Span.mm", "Gender", "Hand")

# Variable Classes
class(dataset$variable) # view the class of a variable
sapply(dataset, class) # To check the class of all the variables simultaneously

dataset$variable<-as.factor(dataset$variable)
dataset$variable<-as.numeric(dataset$variable)
dataset$variable<-as.character(dataset$variable)
dataset$variable<-as.Date(dataset$variable)

# Calculating New Numeric Variables
dataset$var2<-dataset$var1
people$Height.Inches<-round(people$Height/2.54)  # change to Inch
people$Height[people$Height<150]<-NA  # set all values of Height less than 150 cm to missing
pulserates$Mean.Pulse<-apply(pulserates[2:4], 1, mean)

# Dividing a Continuous Variable into Categories
people$Height.Cat<-cut(people$Height, c(150, 160, 180, 200), c("Short", "Medium", "Tall"))

# Working with Factor Variables
people$Sex<-as.factor(people$Sex)
levels(people$Sex)
levels(people$Sex)<-c("Male", "Female")
levels(people$Eye.Color)
levels(people$Eye.Color)[2]<-"Brown"
people$Eye.Color<-relevel(people$Eye.Color, "Brown")

# Manipulating Character Variables--------

# Concatenating Character Strings
fruit$Label<-paste(fruit$Product, ": £", format(fruit$Price, trim=T, digits=2), " ", fruit$Unit, sep="") # paste function allows you to create new character variables by pasting together existing variables (of any class) and other characters.

# Extracting a Substring
flights$Airline<-substring(flights$Flight.Number, 1, 2)
flights$Ref<-substring(flights$Flight.Number, 3)

# Searching a Character Variable
grep("reading", customers$Address) # searches the Address variable for the term “reading”
grep("reading", customers$Address, ignore.case=T)
matches<-grep("reading", customers$Address, ignore.case=T)
reading.customers<-customers[matches,]

# Working with Dates and Times
dataset$variable<-as.Date(dataset$variable, "format")

# The Most Commonly Used Symbols for Date-Time Formats. 
# Enter help(strptime) to View a Complete List
help(strptime) 
# Symbol  Meaning                               Possible values
# d       Day of the month                      01 to 31, or 1 to 31
# %m      Month number                          01 to 12
# %b      Three letter abbreviated month name   Jan, Feb, Mar, Apr, etc.
# %B      Full month name                       January, February, March, April, etc.
# %y      Two-digit year                        00 to 99, e.g., 10 for 2010
# %Y      Four-digit year                       e.g., 2004
# %H      Hour in 24-hour format                0 to 23, e.g. 19 for 7pm
# %M      Minute past the hour                  00 to 59
# %S      Seconds past the hour                 00 to 59
# %I      Hour in 12-hour format                01 to 12
# %p      AM or PM                              AM or PM
 
coffeeshop$Date<-as.Date(coffeeshop$Date, "%d/%b/%Y") # To convert the variable to the date class
flights$DateTime<-paste(flights$Date, flights$Time)
flights$DateTime<-strptime(flights$DateTime, "%d/%m/%Y %H:%M")

dataset$duration<-difftime(dataset$enddate, dataset$startdate, units="hours")
dataset$age<-difftime(Sys.Date(), dataset$dob)
dataset$newdatevar<-dataset$datevar-7
coffeeshop$Day<-weekdays(coffeeshop$Date)
round(flights$DateTime, units="hours")
dataset$charvar<-format(dataset$datevar, format="%d.%m.%Y")

# Adding and Removing Observations

fix(dataset) # Adding New Observations
dataset<-dataset[-c(2,4,7),] # Removing Specific Observations

# ***Be careful to include the comma ***before the closing bracket; otherwise, you will remove columns rather than rows. Remember that you can also use the colon symbol (:) to select a range of consecutive observations.------------
dataset<-dataset[-c(2:10),]
dataset<-unique(dataset)
dups<-dataset[duplicated(dataset),]

# Selecting a Subset of the Data-----------

# Selecting a Subset According to Selection Criteria
# subset(dataset, condition)
subset(people, Eye.Color=="Brown")
browneyes<-subset(people, Eye.Color=="Brown")
subset(people, Eye.Color %in% c("Brown", "Green"))
subset(people, Eye.Color!="Blue")
subset(people, Height==169)
subset(people, Height<165)
subset(people, Eye.Color=="Brown" & Height<165)
subset(people, Height<165, select=c(Hand.Span, Height))
people[people$Eye.Color=="Brown",]
plot(Height~Hand.Span, people, subset=Sex==2)

# Selecting a Random Sample from a Dataset
sampledata<-dataset[sample(1:nrow(dataset), 50),]
sampledata<-dataset[sample(1:nrow(dataset), 50, replace=T),]

# Sorting a Dataset
people<-people[order(people$Hand.Span),]
people<-people[order(people$Hand.Span, decreasing=T),]
people<-people[order(people$Sex, people$Height),]

# Summary
# Task                                    Command
# Rename variable                         names(dataset)[n]<-"Newname"
# View variable class                     class(dataset$variable)
# Change variable class to numeric        dataset$var1<-as.numeric(dataset$var1)
# Change variable class to factor         dataset$var1<-as.factor(dataset$var1)
# Change variable class to character      dataset$var1<-as.character(dataset$var1)
#Change variable class to date            dataset$var1<-as.Date(dataset$var1, "format")
# Copy variable                           dataset$var2<-dataset$var1
# Divide variable into categories         dataset$factor1<-cut(dataset$var1, c(1,2,3,4), c("Name1", "Name2","Name3"))
# Rename factor level                     dataset$variable)[n]<-"Newname"
# Reorder factor levels                   dataset$variable, "Level1")
# Join two character strings              dataset$var3<-paste(dataset$var1, dataset$var2)
# Extract a substring                     dataset$var2<-substring(dataset$var1, first, last)
# Search character variable               grep("search term", dataset$variable)
# Remove cases                            dataset<-dataset[-c(2,4,7),]
# Remove duplicates                       dataset<-unique(dataset)
# Select subset                           subset(dataset, variable=="value")
# Select random sample                    newdataset<-dataset[sample(1:nrow(dataset), samplesize),]
# Sort dataset                            dataset<-dataset[order(dataset$variable),]


#-------------------Combining and Restructuring Datasets
#---------------------------------

# Appending Rows: Similarity an Row names
newdataset<-rbind(dataset1, dataset2, dataset3)
CIAdata<-rbind(CIAdata1, CIAdata2) # rbind function allows you to attach one dataset on to the bottom of the other

# Appending Columns: Difference Row names
newdataset<-cbind(dataset1, dataset2, dataset3)
CIAWHOdata<-cbind(CIAdata1, WHOdata)

# Merging Datasets by Common Variables
CIACPIdata<-merge(CIAdata1, CPIdata) # merge function identifies variables with the same name and uses them to match up the observations
newdataset<-merge(dataset1, dataset2, by.x="var1", by.y="VAR1")
allCIACPIdata<-merge(CIAdata1, CPIdata, all=T)
# all.x and all.y arguments to include unmatched cases 
allCIAGPIdata<-merge(CIAdata1, CPIdata, all.x=T) 
allCIAGPIdata<-merge(CIAdata1, CPIdata, all.y=T)
# R automatically sorts the merged dataset by the same variables that were used to match the observations. If you want to prevent this, set the sort argument to F:
newdataset<-merge(dataset1, dataset2, sort=F)

# Stacking and Unstacking a Dataset
# Stacking Data
grades2<-stack(grades1)
newdataset<-stack(grades1, select=c("ClassA", "ClassC"))
names(grades2)<-c("Result", "Class")
# Unstacking Data
grades1<-unstack(grades2)
unstackeddata<-unstack(stackeddata, values~groups)
sepalwidths<-unstack(iris, Sepal.Width~Species)
unstackeddata$groupA
# Reshaping a Dataset
resistance2<-reshape(resistance, direction="long", varying=list(c("Day3", "Day7", "Day14")), times=c(3, 7, 14), idvar="Formula", v.names="Resistance", timevar="Day")

vitalsigns2<-reshape(vitalsigns, direction="wide", v.names="result", timevar="test", 
idvar="subject")

vitalsigns2<-reshape(vitalsigns, direction="wide", v.names="result", timevar="test", 
idvar="subject", varying=list(c("SysBP", "DiaBP", "Pulse")))

# Summary
# Task C                          ommand
# Append datasets vertically      rbind(dataset1, dataset2)
# Append datasets horizontally    cbind(dataset1, dataset2)
# Merge datasets                  merge(dataset1, dataset2)
# Stack dataset                   stack(dataset, select=c("var1", "var2", "var3"))
# Unstack dataset                 unstack(dataset, values~groups)
# Reshape (wide to long)          reshape(dataset, direction="long", varying=list(c                                     ("var1", "var2", "var3")), times=c(t1,t2,t3),                                         idvar="identifier")
# Reshape (long to wide)          reshape(dataset, direction="wide",v.names="values",                                         timevar="groups", idvar="identifier")


#-------------------Summary Statistics for Continuous Variables
#---------------------------------

# Univariate Statistics
summary(iris)
# Functions for Summarizing Continuous Variables; 
# Those Marked with an Asterisk Give a Single Value as Output

# Statistic                   Function
# Mean*                       mean
# Median*                     median
# Standard deviation*         sd
# Median absolute deviate*    mad
# Variance*                   var
# Maximum value*              max
# Minimum value*              min
# Interquartile range*        IQR
# Range                       range
# Quantiles                   quantile
# Tukey five-number summary   fivenum
# Sum*                        sum
# Product*                    prod
# Number of observations*     length

mean(trees$Height)

#R to ignore any missing values when calculating the statistic
mean(dataset$variable, na.rm=T)

sapply(trees, mean)
sapply(dataset, mean, na.rm=T)
# If any of the variables in your dataset are nonnumeric, the sapply function behaves inconsistently. For example, this command attempts to calculate the maximum value for each of the variables in the iris dataset. R returns an error message because the fifth variable in the dataset is a factor variable 
sapply(iris, max)
#Error in Summary.factor(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, :
#max not meaningful for factors
sapply(iris[-5], max)

# Statistics by Group
tapply(iris$Sepal.Width, iris$Species, mean)
tapply(dataset$variable, dataset$factor1, mean, na.rm=T)
tapply(warpbreaks$breaks, list(warpbreaks$wool, warpbreaks$tension), median)
aggregate(Sepal.Width~Species, iris, mean)  # calculate the mean sepal width for each species, use the aggregate function
aggregate(breaks~wool+tension, warpbreaks, median) # calculate the median number of breaks for each combination of wool and tension for the warpbreaks dataset
aggregate(cbind(Sepal.Width, Sepal.Length)~Species, iris, mean)  #  summarize two or more continuous variables simultaneously, nest them inside the cbind function as shown
sepalmeans<-aggregate(cbind(Sepal.Width, Sepal.Length)~Species, iris, mean)  #  save the output to a new data frame, as shown here

# Measures of Association-------------------------
# Covariance------------------
cov(trees$Height, trees$Volume)
cov(trees)

cov(iris) # Error: is.numeric(x) || is.logical(x) is not TRUE 
cov(iris[-5])

cov(dataset, use="pairwise")

# Pearson’s Correlation Coefficient--------------
# calculate the Pearson’s correlation coefficient between two variables, use the cor function
cor(trees$Girth, trees$Volume)
cor(trees)
cor(dataset, use="pairwise")

# Spearman’s Rank Correlation Coefficient------------
cor(trees$Girth, trees$Volume, method="spearman")
cor(trees, method="spearman")
cor(dataset$var1, dataset$var2, method="spearman", use="pairwise")

# Hypothesis Test of Correlation-------------
cor.test(dataset$var1, dataset$var2)  # perform a test of the correlation between two variables
cor.test(dataset$var1, dataset$var2, method="spearman")

cor.test(dataset$var1, dataset$var2, alternative="greater") # performs a two-sided test, but you can adjust this by setting the alternative argument to "less" or "greater"
cor.test(dataset$var1, dataset$var2, conf.level=0.99)  # output includes a 95% confidence interval for the correlation estimate

# EXAMPLE: HYPOTHESIS TEST OF CORRELATION USING THE TREES DATASET
#----------------------------------------------------------------
# perform a two-sided test of the Pearson's product moment correlation between tree girth and volume at the 5% significance level
cor.test(trees$Girth, trees$Volume)
 
# ***Pearson's product-moment correlation---------
# data: trees$Girth and trees$Volume
# t = 20.4783, df = 29, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9322519 0.9841887
# sample estimates: 
# cor
# 0.9671194

# SCIENTIFIC NOTATION------------
# 720000    7.2e5
# 0.000072  7.2e-5

# Comparing a Sample with a Specified Distribution---------------
# Shapiro-Wilk Test--------------
# EXAMPLE: SHAPIRO-WILK TEST USING THE TREES DATASET
#----------------------------------------------------------------
shapiro.test(dataset$variable)

shapiro.test(trees$Height)
# ***Shapiro-Wilk normality test
# data: trees$Height
# W = 0.9655, p-value = 0.4034

# Kolmogorov-Smirnov Test---------------
ks.test(dataset$variable, "pnorm", 100, 10) #  perform a one-sample test with the null hypothesis that the sample is drawn from a normal distribution with a mean of 100 and a standard deviation of 10
ks.test(dataset$sample1, dataset$sample2)

# EXAMPLE: ONE-SAMPLE KOLMOGOROV-SMIRNOV TEST USING BOTTLES DATA
#----------------------------------------------------------------
ks.test(bottles$Volume, "pnorm", 500, 25)
# ***One-sample Kolmogorov-Smirnov test data: bottles$Volume--------
# D = 0.2288, p-value = 0.2108
# alternative hypothesis: two-sided

# EXAMPLE: TWO-SAMPLE KOLMOGOROV-SMIRNOV TEST USING THE PLANTGROWTH DATA
#----------------------------------------------------------------
PlantGrowth2<-unstack(PlantGrowth)
ks.test(PlantGrowth2$trt1, PlantGrowth2$trt2)
# ***Two-sample Kolmogorov-Smirnov test----------
# data: PlantGrowth2$trt1 and PlantGrowth2$trt2
# D = 0.8, p-value = 0.002057
# alternative hypothesis: two-sided

# Confidence Intervals and Prediction Intervals
t.test(trees$Height)
# One Sample t-test----------
# data: trees$Height
# t = 66.4097, df = 30, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  73.6628 78.3372
# sample estimates:
# mean of x
#  76
t.test(trees$Height, conf.level=0.99)
t.test(dataset$variable, alternative="greater")
predict(lm(trees$Height~1), interval="prediction")[1,] # o calculate a prediction interval for the tree heights

predict(lm(dataset$variable~1), interval="prediction", level=0.99)[1,]

# Summary
# Task                                        Command
# Statistic for each variable                 sapply(dataset, statistic)
# Statistic by group                          tapply(dataset$var1, dataset$factor1, statistic)
# Statistic by group                          aggregate(variable~factor, dataset, statistic)
# Covariance                                  cov(dataset$var1, dataset$var2)
# Covariance matrix                           cov(dataset)
# Pearson’s correlation coefficient           cor(dataset$var1, dataset$var2)
# Correlation matrix cor(dataset)
# Spearman’s rank correlation coefficient     cor(dataset$var1, dataset$var2, method="spearman")
# Spearman’s correlation matrix               cor(dataset, method="spearman")
# Hypothesis test of correlation              cor.test(dataset$var1, dataset$var2)
# Shapiro-Wilk test                           shapiro.test(dataset$variable)
# One-sample Kolmogorov-Smirnov test          ks.test(dataset$sample1, "pnorm", mean, sd)
# Two-sample Kolmogorov-Smirnov test          ks.test(dataset$sample1, dataset$sample2)
# Confidence interval                         t.test(dataset$variable)
# Prediction interval                         predict(lm(dataset$variable~1), interval="prediction")[1,]


#-------------------Tabular Data
#---------------------------------
# Frequency Tables
table(people2$Eye.Color)
table(dataset$factor1, useNA="ifany") # add an additional column to the table showing the numbers of missing values (if there are any), set the useNA argument
table(people2$Eye.Color, people2$Sex)
table(people2$Eye.Color, people2$Height.Cat, people2$Sex)
sexeyetable<-table(people2$Eye.Color, people2$Sex)

# Displaying Tables
# ftable: displays your table in a more compact way, which is useful for tables with three or more dimensions
ftable(Titanic)
prop.table(sexeyetable) #  proportion of the total count
prop.table(sexeyetable, margin=2) # margin argument to 1 for rows, 2 for columns, and 3+ for higher dimensions
round(prop.table(sexeyetable)*100)
addmargins(sexeyetable)
addmargins(sexeyetable, margin=1)

# Creating Tables from Count Data
xtabs(breaks~wool+tension, warpbreaks)
#   tension
# wool L M H
# A 401 216 221
# B 254 259 169
xtabs(list(counts1, counts2)~factor1, dataset)
warpbreakstable<-xtabs(breaks~wool+tension, warpbreaks)
as.data.frame(Titanic)

# Creating a Table Directly
table1D<-as.table(c(5, 21, 17, 3, 1))
table1D
row.names(table1D)<-c("Category 1", "Category 2", "Category 3", "Category 4", "Category 5")
# Create a two-dimensional table----------
matrix1<-matrix(c(15, 10, 11, 15), nrow=2)
matrix1<- matrix(c(15, 9, 11, 17), nrow=2, dimnames=list(Group=c("Active", "Control"), Disease=c("Resolved", "Unresolved")))
treattable<-as.table(matrix1)
# Alternative
treattable<-as.table(matrix(c(15, 9, 11, 16), nrow=2, dimnames=list(Group=c("Active", "Control"), Disease=c("Resolved", "Unresolved"))))

#---------------------------------Chi-Square Goodness-of-Fit Test

chisq.test(tableobject) # one-dimensional table
# using raw data, nest the table function inside the chisq.test function
chisq.test(table(dataset$factor1))
#  test the data against a different theoretical distribution
chisq.test(tableobject, p=c(0.1, 0.4, 0.4, 0.1)) # hypothesis that 10 percent of the population belong to the first category, 40 percent to the second category, 40 percent to the third category, and 10 percent to the fourth category

# EXAMPLE: CHI-SQUARE GOODNESS-OF-FIT TEST USING THE APARTMENTS DATA
#----------------------------------------------------------------
table(apartments$Price.Cat)
chisq.test(table(apartments$Price.Cat), p=c(0.2, 0.3, 0.3, 0.2))

# Tests of Association Between Categorical Variables-------------
#Chi-Square Test of Association----------------------------------

summary(tableobject)
summary(table(dataset$var1, dataset$var2, dataset$var3))
# EXAMPLE: CHI-SQUARE TEST OF ASSOCIATION USING PEOPLE2 DATA
summary(sexeyetable)
summary(table(people2$Sex, people2$Eye.Colour))

# EXAMPLE: CHI-SQUARE TEST OF ASSOCIATION USING THE ESOPH DATA
tobacco<-xtabs(cbind(ncases, ncontrols)~tobgp, esoph)
tobacco
summary(tobacco)

# Fisher’s Exact Test--------------------------------------------
fisher.test(tableobject)
fisher.test(dataset$var1, dataset$var2)
fisher.test(dataset$var1, dataset$var2, conf.level=0.99)

# EXAMPLE: FISHER'S EXACT TEST USING PEOPLE2 DATA
table(people2$Sex, people2$Handedness)
fisher.test(people2$Sex, people2$Handedness)

# Fisher's Exact Test for Count Data
# data: people2$Sex and people2$Handedness
# p-value = 0.07692
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.000000 2.098087
# sample estimates:
# odds ratio
#  0

# Proportions Test--------------
prop.test(tableobject)
prop.test(t(tableobject))
prop.test(table(dataset$groups, dataset$outcome))
prop.test(c(success1, success2), c(n1, n2))

# XAMPLE: PROPORTIONS TEST---------------
treattable
prop.table(treattable, margin=1)
prop.test(treattable)
prop.test(c(15,8), c(26,25)) # Alternative


# Summary
# Task                                Command
# Contingency table                   table(dataset$factor1, dataset$factor2)
# Compact table                       ftable(tableobject)
# Proportions table                   prop.table(tableobject)
# Table with margins                  addmargins(tableobject)
# Chi-square goodness-of-fit test     chisq.test(tableobject, p=c(p1, p2, pN))                                             chisq.test(table(dataset$factor1), p=c(p1, p2, pN))
# Chi-square test of association      summary(tableobject)                                                                 summary(table(dataset$factor1, dataset$factor2))
# Fisher’s exact test                 fisher.test(tableobject)                                                             fisher.test(dataset$factor1, dataset$factor2)
# Proportions test                    prop.test(tableobject)                                                               prop.test(table(dataset$groups, dataset$outcome))


#-------------------Probability Distributions
#---------------------------------

# Probability Distributions in R
#Standard probability distributions included with R. Combine the distribution suffix with prefix: d for the pdf; p for the cdf; q for the inverse cdf; and r for the random number generator Distribution Function suffix.

# Distribution                    Function suffix
# Beta                            beta
# Binomial                        binom
# Cauchy                          cauchy
# Chi-square                      chisq
# Exponential                     exp
# F                               f
# Gamma                           gamma
# Geometric                       geom
# Hypergeometric                  hyper
# Logistic                        logis
# Log normal                      lnorm
# Multinomial                     multinom
# Negative binomial               nbinom
# Normal                          norm
# Poisson                         pois
# Student's t distribution        t
# Uniform                         unif
# Weibull                         weibull

# THE PROBABILITY DENSITY FUNCTION AND CUMULATIVE DISTRIBUTION FUNCTION--------

#EXAMPLE: PROBABILITY MASS FUNCTION FOR THE BINOMIAL DISTRIBUTION
#----------------------------------------------------------------
dbinom(2, 10, 1/6) # Suppose that a fair die is rolled 10 times. What is the probability of throwing exactly two sixes?

# EXAMPLE: PROBABILITY MASS FUNCTION FOR THE POISSON DISTRIBUTION
#----------------------------------------------------------------
dpois(18, 20) # The number of lobster ordered in a restaurant on a given day is known to follow a Poisson distribution with a mean of 20. What is the probability that exactly eighteen lobsters will be ordered tomorrow?

# EXAMPLE: PROBABILITY DENSITY FUNCTION FOR THE NORMAL DISTRIBUTION
#----------------------------------------------------------------
dnorm(2.5, mean=5, sd=2) # To find the value of the pdf at x=2.5 for a normal distribution with a mean of 5 and a standard deviation of 2.


# Finding Probabilities---------------------------------

pnorm(2.5) # probability that a randomly selected value will be less than or equal to 2.5
pnorm(6, mean=5, sd=2)
pnorm(6, 5, 2, lower.tail=F) # find the complementary probability that the value will be greater than 6
1-pnorm(6, 5, 2)

# EXAMPLE: FINDING PROBABILITIES FOR THE NORMAL DISTRIBUTION
#----------------------------------------------------------------
pnorm(200, 177, 10, lower.tail=F) # Suppose the height of men in the United Kingdom is known to be normally distributed with a mean of 177 centimeters and a standard deviation of 10 centimeters. If you were to select a man from the United Kingdom population at random, what is the probability that he would be more than 200 centimeters tall?

pnorm(150, 177, 10) # From the output, you can see that the probability is approximately 0.011, or 1.1 percent. What is the probability that he would be less than 150 centimeters tall?

# EXAMPLE: FINDING PROBABILITIES FOR THE BINOMIAL DISTRIBUTION
#----------------------------------------------------------------
pbinom(10, 100, 1/6) # If you were to roll a fair six-sided die 100 times, what is the probability of rolling a six no more than 10 times?

pbinom(20, 100, 1/6, lower.tail=F) # What is the probability of rolling a six more than 20 times?

# EXAMPLE: FINDING PROBABILITIES FOR THE EXPONENTIAL DISTRIBUTION
#----------------------------------------------------------------
pexp(6, 1/24) # Malfunctions in a particular type of electronic device are known to follow an exponential distribution with a mean time of 24 months until the device malfunctions. What is the probability that a randomly selected device will malfunction within the first 6 months?

pexp(60, 1/24, lower.tail=F)  # What is the probability that a randomly selected device will last more than 5 years (60 months) without malfunction?


#-------------------------------------------Finding Quantiles
qnorm(0.95) #  find quantiles for the normal distribution
qnorm(0.95, mean=5, sd=2) # nonstandard normal distributions, use the mean and sd arguments to specify the parameters for the distribution
qnorm(0.95, 5, 2, lower.tail=F) # find the value above which 95 percent of the population

# EXAMPLE: FINDING QUANTILES FOR THE NORMAL DISTRIBUTION
#----------------------------------------------------------------
qnorm(0.005, 195, 17) # A manufacturer of a special type of one-size glove wants to design the glove to fit at least 99 percent of the population. Hand span is known to be normally distributed with a mean of 195 millimeters and a standard deviation of 17 millimeters. What range of hand spans must the glove accommodate?
 qnorm(0.005, 195, 17, lower.tail=F) # find the value above which 0.5 percent of the population falls

# EXAMPLE: FINDING QUANTILES FOR THE EXPONENTIAL DISTRIBUTION
#----------------------------------------------------------------
 qexp(0.4, 1/24) # Malfunctions in a particular type of electronic device are known to follow an exponential distribution with a mean time of 24 months until the device malfunctions. After how many months will 40 percent of the devices already have malfunctioned?

# EXAMPLE; FINDING QUANTILES FOR THE POISSON DISTRIBUTION
#----------------------------------------------------------------
qpois(0.8, 20) # The number of lobsters ordered in a restaurant on a given day is known to follow a Poisson distribution with a mean of 20. If the manager wants to be able to satisfy all requests for lobster on at least 80 percent of days, how many lobster should they order each day?

#Generating Random Numbers----------------------------------------
vector1<-rnorm(100) # 100 random numbers from a standard normal distribution
vector2<-rnorm(100, 27.3, 4.1) # To  generate random numbers from a nonstandard normal distribution, add the mean and sd arguments
sample(1:100, 20) # sample function generate a simple random sample from a range of numbers

# EXAMPLE: GENERATING RANDOM NUMBERS FROM A NORMAL DISTRIBUTION
#----------------------------------------------------------------
rnorm(3, 195, 17) # simulate: Hand span in a particular population is known to be normally distributed with a mean of 195 millimeters and a standard deviation of 17 millimeters

# EXAMPLE: GENERATING RANDOM NUMBERS FROM A BINOMIAL DISTRIBUTION
#----------------------------------------------------------------
rbinom(1, 10, 1/6) #  simulate the number of sixes thrown in 10 rolls of a fair die, use the command

# EXAMPLE: GENERATING RANDOM NUMBERS FROM A POISSON DISTRIBUTION
#----------------------------------------------------------------
rpois(7, 20)  # The number of lobsters ordered on any given day in a restaurant follows a Poisson distribution with a mean of 20. To simulate the number of lobsters ordered over a seven-day period,

# EXAMPLE: GENERATING RANDOM NUMBERS FROM AN EXPONENTIAL DISTRIBUTION
#----------------------------------------------------------------
rexp(10, 1/24)  #Malfunctions in a particular type of electronic device are known to follow an exponential distribution with a mean time of 24 months until the device malfunctions. To simulate the time to malfunction for ten randomly selected devices,

# Summary
# Task                                                     Command
# Find the value of the density function at value x        dnorm(x, mean, sd)
# Obtain P(X £ x)                                          pnorm(x, mean, sd)
# Obtain x where P(X £ x)=p                                qnorm(p, mean, sd)
# Generate n random numbers from a normal distribution     rnorm(n, mean, sd)


#-------------------Creating Plots
#---------------------------------

# Simple Plots------------------
plot(trees$Height)
plot(trees$Height, type="l") # By default, R uses symbols to plot the data values. To use lines instead of symbols, set the type argument to "l".
# "b" for both lines and symbols
# "h" for vertical lines
# "s" for steps

# Histograms--------------------
hist(trees$Height)
hist(dataset$variable, breaks=15) # specify the number of bars with the breaks argument
hist(dataset$variable, freq=F) #  create a histogram of densities

hist(trees$Height, freq=F)
curve(dnorm(x, mean(trees$Height), sd(trees$Height)), add=T) # curve function to fit a normal distribution curve to the data

hist(dataset$variable, freq=F)
curve(dnorm(x, mean(dataset$variable, na.rm=T), sd(dataset$variable, na.rm=T)), add=T) # If the variable has any missing data values, remember to set the na.rm argument to T for the mean and sd functions

# Normal Probability Plots-----
qqnorm(trees$Height)
qqline(trees$Height) # add a reference line to the plot
qqplot(trees$Height) #  create quantile plots for comparing data with other standard probability distributions in addition to the normal distribution.

# INTERPRETING THE NORMAL PROBABILITY PLOT
# Please See Page 105

# Stem-and-Leaf Plots----------
stem(trees$Volume) # plot is actually a semigraphical technique rather than a true plot.
# The decimal point is 1 digit(s) to the right of the |
# 1 | 00066899
# 2 | 00111234567
# 3 | 24568
# 4 | 3
# 5 | 12568
# 6 |
# 7 | 7

# Bar Charts--------------------
plot(people2$Eye.Color)

barplot(tableobject) #  create a bar chart from a previously saved one-dimensional table object
plot(people2$Eye.Color, horiz=T) # a horizontal bar chart
barplot(sexeyetable, legend.text=T) # create from a two-dimensional table object
barplot(sexeyetable, beside=T, legend.text=T) #  displays the categories side-by-side
barplot(table(people2$Eye.Color, people2$Sex), legend.text=T) # create a two-dimensional table object

# Pie Charts-------------------
pie(tableobject) # a single categorical variable and is an alternative to the bar chart
pie(table(people2$Eye.Color)) #  create a pie chart from raw data, nest the table function inside the pie function as shown here.

pie(table(dataset$variable, useNA="ifany"))  #variable has missing data and you want this to appear as an additional section in the pie chart, set the useNA argument to "ifany".

# Scatter Plots----------------
plot(Volume~Girth, trees) # create a scatter plot with the plot function, by giving two numeric variables as input

plot(Volume~Girth, trees)
abline(coef(lm(Volume~Girth, trees))) # add a line of best fit (linear regression line), use the abline function directly after the plot function

plot(Volume~Girth, trees, type="l") # both symbols and lines, set it to "b"

# Scatterplot Matrices---------
pairs(iris)  # create a scatterplot matrix of the variables in a dataset
pairs(~Sepal.Length+Sepal.Width+Petal.Length, iris) # select a subset of variables to include in the matrix

# Box Plots---------------------
boxplot(Sepal.Length~Species, iris) # box plot (or box-and-whisker plot) presents summary statistics for a continuous variable in a graphical form.
boxplot(Sepal.Length~Species, iris, horizontal=T) # create a horizontal box plot
boxplot(iris$Sepal.Length) # create a single box plot for a continuous variable
boxplot(Sepal.Length~Species, iris, range=0)# By default, the whiskers extend to a maximum of 1.5 times the interquartile range of the data, with any values beyond this is shown as outliers. If you want the whiskers to extend to the minimum and maximum values, set the range argument to 0

# Plotting a Function-----------
curve(x^3) # plot a mathematical function
curve(dnorm(x))

# Exporting and Saving Plots----
savePlot("/home/Username/folder/filename.png", type="png")

# Summary
# Plot Type                 Command
# Basic plot                plot(dataset$variable)
# Line plot                 plot(dataset$variable, type="l")
# Histogram                 hist(dataset$variable)
# Normal probability plot   qqnorm(dataset$variable)
# Stem-and-leaf plot        stem(dataset$variable)
# Bar chart                 plot(dataset$factor1)                                                                barplot(tableobject1D)
# Stacked bar chart         barplot(tableobject2D)
# Grouped bar chart         barplot(tableobject2D, beside=T)
# Pie chart                 pie(tableobject1D)                                                                   pie(table(dataset$factor1))
# Scatter plot              plot(yvar~xvar, dataset)
# Scatterplot matrix        pairs(dataset)
# Grouped box plot          boxplot(variable~factor1, dataset)
# Function plot             curve(f(x))


#-------------------Customizing Your Plots
#---------------------------------

#Titles and Labels---------
plot(dataset$variable, main="This is the title")
plot(dataset$variable, sub="This is the subtitle")
plot(trees$Height, main="This is the first line of the title\n and this is the second line")
plot(dataset$variable, xlab="The x axis label", ylab="The y axis label")
plot(trees$Height, main="This is the title", family="serif", col.main="grey80", cex.main=3, font.main=3)
pie(table(dataset$variable), labels=c("Label1", "Label2", "Label3"))
plot(dataset$variable, names.arg=c("Label1", "Label2", "Label3"))
boxplot(variable~factor, dataset, names=c("Label1", "Label2", "Label3"))

# Axes---------------------
plot(Volume~Girth, trees, xlim=c(0, 30), ylim=c(0, 100))
plot(Volume~Girth, trees, las=1)

# Colors-------------------
plot(dataset$variable, col="red")
colors()
plot(dataset$variable, col=2)  # specify colors is with a number between 1 and 8
plot(dataset$variable, col="#FF0000")

plot(dataset$variable, col=c("red", "blue", "green"))
plot(dataset$variable, col=1:8)
plot(dataset$variable, col=rainbow(5)) # rainbow, which help to easily create a visually appealing set of colors.

# Arguments for changing the color of plot component 
# (*the background color must be changed with the par function)
# Component                                           Argument
# Plotting symbol, line, or area                      col
# Foreground (axis, tick marks, and other elements)   fg
# Background                                          bg*
# Title text                                          col.main
# Subtitle text                                       col.sub
# Axis label text                                     col.lab
# Axis numbers                                        col.axis

par(bg="red")
plot(dataset$variable)


# Plotting Symbols
plot(dataset$variable, pch=5) # numbers 1 to 25 correspond to the symbols
plot(dataset$variable, pch=21, col="red", bg="blue")
plot(dataset$variable, pch="$") # plotting symbols 5 times their normal size
plot(dataset$variable, cex=5)  # lotting symbols 5 times their normal size

# Plotting Lines---------------
plot(dataset$variable, type="l", lty=2) # numbers 1 to 6 correspond to the line types
plot(dataset$variable, type="l", lty="dashed")
plot(dataset$variable, type="l", lwd=3)

# Shaded Areas-----------------
barplot(tableobject, density=20, angle=30)
pie(table(people2$Eye.Color), density=c(10, 20, 40))

# Adding Items to Plots--------
# Adding Straight Lines--------
abline(v=5) # add a vertical line at x=5 
abline(h=2) # add a horizontal line at y=2
abline(a=2, b=3) # add a diagonal line with intercept 2 and slope 3 
segments(12,20,18,55) # draw a line segment (a line that extends from one point to another).

# Adding a Mathematical Function Curve------
curve(x^2, add=T)  # superimpose the curve over the current plot, set the add argument to T

# Adding Labels and Text---------
text(3, 4, "Text String") # add text to your plot with the text function
locator(1) #  locator function: coordinates of a given location on your plot,
text(3, 4, "Text String", col="red", font=3, cex=2)

# text function is useful if you want to add labels to all of the points in a scatter plot---------------
plot(pcGDP~urban, CIAdata, xlim=c(50, 100), ylim=c(0,40000))
text(pcGDP~urban, CIAdata, CIAdata$country, pos=1) # The pos argument tells R where to place the labels in relation to the coordinates: 1 is below; 2 is to the left, 3 is above; 4 is to the right. 
text(pcGDP~urban, CIAdata, CIAdata$country, pos=4, offset=0.3) # offset argument to adjust the distance (relative to the character width) between the coordinate and the label.

# label a few specific data points-------------
plot(pcGDP~urban, CIAdata, xlim=c(50, 100), ylim=c(0,40000))
identify(CIAdata$urban, CIAdata$pcGDP, label=CIAdata$country)

# Adding a Grid-----------
grid()
grid(3, 3)
grid(nx=NA, ny=NULL)
grid(ny=NA)

# Adding Arrows-----------
arrows(10,50,12.7,35)
arrows(10,50,12.7,35, code=3)
arrows(10,50,12.7,35, angle=25, length=0.1)

# Overlaying Plots--------

# EXAMPLE: OVERLAY PLOT USING FIVEYEARREPORT DATA
#----------------------------------------------------------------

# page 135





#-------------------chapter
#---------------------------------







































```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
