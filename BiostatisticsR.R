Source: https://link.springer.com/book/10.1007/978-1-4614-1302-8
## Biostatistics with R An Introduction to Statistics Through Biological Data 

## Data Preprocessing-----------------------------------------------------
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
  # *Relationships Between Two Numerical Random Variables: scatterplot., linear relationship, nonlinear relationship 
library(mfp)
  # *Relationships Between Categorical Variables : contingency tables, sample proportion, difference of proportions, relative proportion, relative risk, sample odds ratio
  # *Relationships Between Numerical and Categorical Variables: difference of means, plot of means

# Two Numerical Variables-------------------------------------------------
data(bodyfat)
plot(bodyfat$abdomen, bodyfat$siri,
  + xlab = "Abdomen", ylab = "Percent Body Fat")
cor(bodyfat$abdomen, bodyfat$siri)
cor.matrix <- cor(bodyfat[, c("siri", "weight",
  + "height", "abdomen")])
round(cor.matrix, 2)

# Two Categorical Variables------------------------------------------------
library(MASS)
data(birthwt)
table(birthwt$smoke, birthwt$low)
plot(birthwt$smoke, birthwt$bwt)
boxplot(bwt ~ smoke, ylab = "Birthweight",
  + data = birthwt, xlab = "Smoking Status",
  + main = "Birthweight by Smoking Status")

smoke.ind <- which(birthwt$smoke == 1)
summary(birthwt$bwt[smoke.ind])
by(birthwt$bwt, birthwt$smoke, summary)           # obtain summary statistics by group is to use the by function
by(birthwt$bwt, birthwt$ht, sd)

## Probability--------------------------------------------------------------
  # The Sample Space: *Randomness: A phenomenon is called random if its outcome (value) cannot be determined with certainty before it occurs.
  # Probability Measure
  # Complement, Union, and Intersection 
  # Joint vs. Marginal Probability
  # Disjoint Events
  # Conditional Probabilities
  # The Law of Total Probability
  # Independent Events
  # Bayes’ Theorem
  # Application of Bayes’ Theorem in Medical Diagnosis
  # Bayesian Statistics: prior probability, posterior probability.
  # Interpretation of Probability as the Relative Frequency
  # Using Tree Diagrams to Obtain Joint Probabilities
  # Making Decisions under Uncertainty

## Random Variables and Probability Distributions----------------------------
  # Random Variables
  # Discrete vs. Continuous
  # Probability Distributions: population mean, population variance
  # Discrete Probability Distributions: probability mass function (pmf).
  # Bernoulli Distribution
  # Binomial Distribution
  # Poisson Distribution
  # Continuous Probability Distributions: probability density functions (pdf), probability density curve
  # Probability Density Curves and Density Histograms
  # Normal Distribution
  # Student’s t-distribution
  # Cumulative Distribution Function and Quantiles
  # Scaling and Shifting Random Variables
  # Sum of Two Random Variables
  # Quantile–Quantile Plots
# Probability Distributions with R Programming---------------------------------
rbinom(5, size = 10, prob = 0.2)
rbinom(10, size = 1, prob = 0.2)
dbinom(3, size = 10, prob = 0.2)
x <- 0:10
Px <- dbinom(x, size = 10, prob = 0.2)
round(Px, 2)
plot(x, Px, type = "h", xlab = "Number of Successes",
  + ylab = "Probability Mass",
  + main = "Binomial(10, 0.2)")
points(x, Px, pch = 16)
abline(h = 0, col = "gray")
pbinom(3, size = 10, prob = 0.2, lower.tail = TRUE)
qbinom(0.879, size = 10, prob = 0.2,
  + lower.tail = TRUE)
rpois(12, 4)
dpois(6, 4)
ppois(6, 4)
qpois(0.889, 4)
rnorm(5, mean = 25, sd = 4)
x <- seq(from = 10, to = 40, length = 100)
fx <- dnorm(x, mean = 25, sd = 4)
plot(x, fx, type = "l", xlab = "BMI",
  + ylab = "Density", main = "N(25, 16)")
abline(h = 0, col = "gray")
pnorm(18.5, mean = 25, sd = 4,
  + lower.tail = TRUE)
qnorm(0.05, mean = 25, sd = 4,
  + lower.tail = T)
pnorm(30, mean = 25, sd = 4) -
  + pnorm(25, mean = 25, sd = 4)
Fx <- pnorm(x, mean = 25, sd = 4)
plot(x, Fx, type = "l", xlab = "BMI",
  + ylab = "Cumulative Probability",
  + main = "N(25, 16)")
abline(h = 0, col = "gray")

## Estimation---------------------------------------------------------------
# Parameter Estimation
# Point Estimation: Population Mean, Population Variance
# Sampling Distribution
# Confidence Intervals for the Population Mean
# Confidence Interval When the Population Variance Is Unknown 
# Using Central Limit Theorem for Confidence Interval
# Confidence Intervals for the Population Proportion, 
# Margin of Error 
# Deriving Confidence Intervals
# Sample Size Estimation


## Hypothesis Testing--------------------------------------------------------
# Hypothesis Testing for the Population Mean
# Statistical Significance: z-Tests of the Population Mean, Interpretation of p-value, One-Sided Hypothesis Testing, Two-Sided Hypothesis Testing
# Hypothesis Testing Using t -tests
# Hypothesis Testing for Population Proportion
# Test of Normality: Shapiro–Wilk
# Hypothesis Testing with R Programming--------------------------------------
pnorm(-1, mean = 0, sd = 1, lower.tail = TRUE)
pt(5.33, df = 199, lower.tail = FALSE)
t.test(x = Pima.tr$bmi, mu = 30,
  + alternative = "two.sided")
t.test(x = Pima.tr$bmi, mu = 30, conf.level = 0.9)
shapiro.test(x = Pima.tr$bmi)

## Statistical Inference for the Relationship Between Two Variables----------
# Relationship Between a Numerical Variable and a Binary Variable: Two-Sample t -tests for Comparing the Means, Pooled t -test, Paired t -test, 
# Inference about the Relationship Between Two Binary Variables
# Inference Regarding the Linear Relationship Between Two Numerical Variables
# Two-Sample t-test Using R--------------------------------------------------
t.test(bwt ~ smoke, mu = 0, alternative = "two.sided",
  + data = birthwt)
t.test(Platelet$Before, Platelet$After,
  + alternative = "less", paired = TRUE)
t.test(Platelet$Before, Platelet$After,
  + alternative = "less")
# Correlation Test Using R---------------------------------------------------
cor.test(bodyfat$siri, bodyfat$abdomen,
  + alternative = "greater") 

# Analysis of Variance (ANOVA)------------------------------------------------
# one-way ANOVA
# between-groups variation
# within-groups variation
# total variation
# The Assumptions of ANOVA
# Two-Way ANOVA
# ANOVA Using R---------------------------------------------------------------
pf(3.2, df1 = 3, df2 = 23, lower.tail = FALSE)
library(MASS)
data(Cushings)
aov1.out <- aov(Tetrahydrocortisone ~ Type,
  + data = Cushings)
summary(aov1.out)

library(MASS)
data(genotype)
aov2.out <- aov(Wt ~ Mother + Litter,
  + data = genotype)

library(MASS)
data(genotype)
aov2.int.out <- aov(Wt ~ Mother * Litter,
  + data = genotype)
summary(aov2.int.out)

## Analysis of Categorical Variables-------------------------------------------
# Pearson’s χ2 (chi-squared) test
# Pearson’s χ2 Test for One Categorical Variable: Binary Variables: expected frequencies, observed frequencies
# Categorical Variables with Multiple Categories
# Pearson’s χ2 Test of Independence
# Entering Contingency Tables into R-Commander
# Fisher’s Exact Test
# Pearson’s χ2 Test Using R
chisq.test(x = c(24, 16), p = c(0.7, 0.3))
chisq.test(x = c(13, 11, 16), p = c(0.5, 0.2, 0.3))
birthwt.tab <- table(birthwt$smoke, birthwt$low)
birthwt.tab
chisq.test(birthwt.tab, correct = FALSE)

contTable <- matrix(c(189, 10845, 104, 10933),
  + nrow = 2, ncol = 2, byrow = TRUE)
rownames(contTable) <- c("Placebo", "Aspirin")
colnames(contTable) <- c("No heart attack",
  + "Heart attack")
contTable

output <- chisq.test(contTable, correct = FALSE)
output

output$observed
output$expected

## Regression Analysis-----------------------------------------------------------
# Linear Regression Models with One Binary Explanatory Variable
# Statistical Inference Using Simple Linear Regression Models: Confidence Interval for Regression Coefficients, Hypothesis Testing with Simple Linear Regression Models
# Linear Regression Models with One Numerical Explanatory Variable
# Goodness of Fit
# Model Assumptions and Diagnostics
# Multiple Linear Regression
# Interaction
# Linear Regression Models in R--------------------------------------------------
install.packages("mfp", dependencies = TRUE)
library(mfp)
data(bodyfat)
fit <- lm(siri ~ height, data = bodyfat)
fit
summary(fit)
names(fit)
fit$coefficients
fit$fitted.values[1:5]
fit$residuals[1:5]
plot(bodyfat$height, bodyfat$siri,
  + main = "Scatterplot for Percent Body Fat by Height",
  + xlab = "Height", ylab = "Percent Body Fat")
abline(fit)

multReg <- lm(siri ~ height + abdomen, data = bodyfat)
summary(multReg)

## Clustering---------------------------------------------------------------------
# core concept in any cluster analysis is the notion of similarity and dissimilarity: squared distance, squared Euclidean distance
# K-means Clustering
# Hierarchical Clustering: Agglomerative (bottom-up), Divisive (top-down)
# Standardizing Variables Before Clustering
# Clustering in R
Protein <- read.table("Protein.txt",
  + header = TRUE, sep = "")
x <- Protein[, c("RedMeat", "Fish")]
x <- scale(x)
clus <- kmeans(x, centers = 3)
clus$cluster
clus$centers
Protein$ClusterId <- clus$cluster
plot(Protein$Fish, Protein$RedMeat,
  + type = "n", xlab = "Fish", ylab = "Red Meat",
  + xlim = c(0, 15), ylim = c(0, 20))

points(Protein$Fish[Protein$ClusterId ==
  + 1], Protein$RedMeat[Protein$ClusterId ==
  + 1], pch = 1, cex = 1.5)
points(Protein$Fish[Protein$ClusterId ==
  + 2], Protein$RedMeat[Protein$ClusterId ==
  + 2], pch = 2, cex = 1.5)
points(Protein$Fish[Protein$ClusterId ==
  + 3], Protein$RedMeat[Protein$ClusterId ==
  + 3], pch = 3, cex = 1.5)

legend("topright", legend = c("Cluster 1",
  + "Cluster 2", "Cluster 3"), pch = c(1,
  + 2, 3))
 
d <- dist(x)
clus.h <- hclust(d, method = "centroid")
plot(clus.h, labels = Protein$Country)
rect.hclust(clus.h, k = 3)
clus.h.id <- cutree(clus.h, k = 3)
Protein$HClusterId <- clus.h.id

# Bayesian Analysis-------------------------------------------------------------
# A Simple Case of Bayesian Analysis for Population Proportion
# Prior and Posterior Probabilities: prior probabilities, posterior probability 
# The General Form of Bayesian Analysis for Population Proportion
# Bayesian Inference: Estimation, Hypothesis Testing, 

mu <- seq(from = 0, to = 1, length.out = 100)
f <- dbeta(mu, shape1 = 8, shape = 2)
plot(mu, f, type = "l", xlab = expression(mu),
  + ylab = "Density")
qbeta(c(0.025, 0.975), shape1 = 26, shape2 = 4)
pbeta(0.8, shape1 = 26, shape2 = 4,
  + lower.tail = FALSE)

p.78 <- pbeta(0.78, shape1 = 26, shape2 = 4,
  + lower.tail = TRUE)
p.82 <- pbeta(0.82, shape1 = 26, shape2 = 4,
  + lower.tail = TRUE)
p.82 - p.78
