Source: https://link.springer.com/book/10.1007/978-1-4614-1302-8
## Biostatistics with R An Introduction to Statistics Through Biological Data 
 # Data Preprocessing-----------------------------------------------------
   # *Missing Data : NA (Not Available)   
       # up to the researcher to decide whether to remove the observations with missing values or impute (guess) the missing values in order to keep the observations.
   # *Outliers
     # Sometimes, an observed value of a variable is suspicious since it does not follow the overall patterns presented by the rest of the data.
   # *Data Transformation: Data transformation techniques (i.e., applying a function to the variable) to reduce the influence of extreme values in our analysis.
   # *Creating New Variable Based on Two or More ExistingVariables. i.e. BMI = weight ×703/(height)^2 .
   # *Creating Categories for Numerical Variables: Create categorical variables based on numerical variables. i.e. 0:18.5 = "Underweight"
 #-------------------------------------------------------------------------        
 # Coefficient of variation instead of standard deviation: CV = s/¯x .
   # Variable Standardization: a common linear transformation, where we subtract the sample mean ¯x from the observed values and divide the result by the sample
   #                           standard deviation s, in order to shift the mean to zero and make the standard deviation 1:
   #     yi = (xi − ¯x)/s
 # Scaling and Shifting Variables:      CV = s/¯x
 # Variable standardization:            yi = xi − ¯x/s . 
 
 # Data Exploration with R Programming--------------------------------------
 library(MASS)
 data(Pima.tr)
 head(Pima.tr)
 help(Pima.tr)
 type.freq <- table(Pima.tr$type)
 type.freq
 barplot(type.freq, xlab = "Type", ylab = "Frequency",
                + main = "Frequency Bar Graph of Type")
 n <- sum(type.freq)
 type.rel.freq <- type.freq/n
 round(type.rel.freq, 2)
 round(type.rel.freq, 2) * 100    
 barplot(type.rel.freq, xlab = "Type",
        + ylab = "Relative Frequency",
        + main = "Relative Frequency Bar Graph of Type")
 
data(birthwt) 
is.factor(birthwt$smoke)
birthwt$smoke <- factor(birthwt$smoke)
is.factor(birthwt$smoke)
table(birthwt$smoke)
hist(Pima.tr$age, freq = TRUE,
    + xlab = "Age", ylab = "Frequency",
    + col = "grey", main = "Frequency Histogram of Age")

hist(Pima.tr$age, freq = FALSE,
    + xlab = "Age", ylab = "Density",
    + col = "grey", main = "Density Histogram of Age")
    
mean(Pima.tr$npreg)
median(Pima.tr$bmi)
quantile(Pima.tr$bmi, probs = c(0.1, 0.25, 0.5, 0.9))
summary(Pima.tr$bmi)
boxplot(PIma.tr$bmi, ylab = "BMI")
boxplot(Pima.tr$bmi, ylab = "BMI", horizontal = TRUE)
IQR(Pima.tr$bmi)
minMax <- range(Pima.tr$bmi)
minMax[2] - minMax[1]
var(Pima.tr$bmi)
sd(Pima.tr$bmi)
Pima.tr$weight.status <- rep(NA, 200)

  for (i in 1:200) {
     if (Pima.tr$bmi[i] < 18.5) {
        Pima.tr$weight.status[i] <- "Underweight"
     }
     else if (Pima.tr$bmi[i] >= 18.5 &
         Pima.tr$bmi[i] < 24.9) {
         Pima.tr$weight.status[i] <- "Normal"
     }
     else if (Pima.tr$bmi[i] >= 24.9 &
         Pima.tr$bmi[i] < 29.9) {
         Pima.tr$weight.status[i] <- "Overweight"
     }
     else {
        Pima.tr$weight.status[i] <- "Obese"
     }
  }
    
head(Pima.tr)
Pima.tr$weight.status <- factor(Pima.tr$weight.status)    
levels(Pima.tr$weight.status)    
Pima.tr$weight.status <- factor(Pima.tr$weight.status,   
    + levels = c("Underweight", "Normal",
    + "Overweight", "Obese"))
levels(Pima.tr$weight.status)   
data(Pima.tr2)    
is.na(Pima.tr2$bp)
which(is.na(Pima.tr2$bp))    
complete.cases(Pima.tr2)    
Pima.complete <- na.omit(Pima.tr2)   
    
## Exploring Relationships------------------------------------------------    
  # *Visualizing and Summarizing Relationships Between Variables  
  # *Relationships Between Two Numerical Random Variables  
    
library(mfp)









                
                
