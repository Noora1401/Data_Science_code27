###S05: Data Preprocessing in R - Part 1---------
###Required Libraries----------------------------
library("moments")
library("MASS")
###Data Understanding----------------------------
  #Step 1: relevant  data?
  #Step 2: data sources?
#Data Quality: https://analica.ir/data-quality/
##Data set variables definition------------------ 
##Read Data from File----------------------------
data <- read.csv("CS_01.csv", header = TRUE)
dim(data)
head(data)
str(data)
##Data set variables definition------------------ 
colnames(data)
#"id":            Unique customer id
#"sex":           Gender type: F: Female, M: Male
#"is.employed":   Employment status: True, False 
#"income":        Income level     
#"marital.stat"   Marital status: Married, Never Married,
#                                 Divorced/Separated Widowed
#"health.ins"     Health insurance status: True, False  
#"housing.type"   Housing ownership status: 
#                                 Homeowner free and clear
#                                 Rented
#                                 Occupied with no rent
#                                 Homeowner with mortgage/loan
#"recent.move"    If recently move: True, False
#"num.vehicles"   Number of owned vehicles
#"age"            Age
#"state.of.res"   State of residency
##Uni-variate Profiling--------------------------
#customer ids------------------------------------
unique(data$id)
length(unique(data$id))
#sex---------------------------------------------
#Factor
#     It is used for fields that takes only predefined, 
#     finite number of values (categorical data)

data$sex <- factor(data$sex)
table(data$sex)

#missing values?
is.na(data$sex)
sum(is.na(data$sex))

#what percentage of our customers are female?

#is.employed-------------------------------------
table(data$is.employed)
table(data$is.employed, useNA = "ifany")

#Barplot
barplot(table(data$is.employed))

#Missing values?
sum(is.na(data$is.employed))

#Percentage of missing values in data?
sum(is.na(data$is.employed)) / nrow(data) * 100

#What percentage of customers are employed?
sum(data$is.employed == TRUE, na.rm = TRUE) / sum(!is.na(data$is.employed)) * 100

#income------------------------------------------
summary(data$income)
sum(is.na(data$income))

#zero?
sum(data$income == 0, na.rm = TRUE)/nrow(data) * 100

#Distribution of income
#histogram
hist(data$income)
hist(data$income, breaks = 20)

#boxplot
boxplot(data$income)

#skewed data
mean(data$income, na.rm = T)
median(data$income, na.rm = T)

#Test of Normality-------------------------------
#https://analica.ir/normality-test/
valid_income <- data$income[data$income > 0 &
                            !is.na(data$income)]
#Histogram
hist(valid_income, probability = T, breaks = 15)
lines(density(valid_income), col = "red")

#QQ-plot
qqnorm(valid_income, main = "QQ Plot of Income", pch = 20)
qqline(valid_income, col = "red")

#Shapiro-Wilk Test for Normality 
#p-value < 0.05 reject normality assumption
#Good for sample size <= 25
shapiro.test(valid_income)

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(valid_income)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(valid_income)
#Conclusion: reject normality assumption

#Transforming Skewed Data------------------------
    #For right-skewed data: square root, cube root, and log
    #For left-skewed data:  square root (constant - x), cube root (constant - x), and log (constant - x)
    #Box-Cox transformation
#Log transformation of income--------------------
log_income <- log(valid_income)

#Histogram
hist(log_income, probability = T, breaks = 15)
lines(density(log_income), col = "red")

#QQ-plot
qqnorm(log_income, main = "QQ Plot", pch = 20)
qqline(log_income, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(log_income)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(log_income)

#Box-Cox Transformation--------------------------
#transformed_x = (x ^ lambda - 1) /lambda  if lambda <> 0
#transformed_x =  log(x)                   if lambda = 0
#https://en.wikipedia.org/wiki/Power_transform
box_results <- boxcox(valid_income ~ 1, lambda = seq(-5, 5, 0.1))               
class(box_results)
str(box_results)
box_results <- data.frame(box_results$x, box_results$y)            # Create a data frame with the results
lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1]
lambda
boxcox_income <- (valid_income ^ lambda - 1) / lambda

#Histogram
hist(boxcox_income, probability = T, breaks = 15)
lines(density(boxcox_income), col = "red")

#QQ-plot
qqnorm(boxcox_income, main = "QQ Plot", pch = 20)
qqline(boxcox_income, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(boxcox_income)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(boxcox_income)

#Other data transformations----------------------
#Min-max scaling
summary(valid_income)
hist(valid_income, breaks = 20)

mn_scaling <- function(x) (x - min(x)) /(max(x)- min(x))
valid_income_mn_scaled <- mn_scaling(valid_income)
summary(valid_income_mn_scaled)
hist(valid_income_mn_scaled, breaks = 20)

#Z-score scaling
valid_income_z_scaled <- scale(valid_income)
summary(valid_income_z_scaled)
hist(valid_income_z_scaled, breaks = 20)

#marital.status----------------------------------
data$marital.stat <- factor(data$marital.stat)
table(data$marital.stat)

#missing value?
sum(is.na(data$marital.stat))

barplot(table(data$marital.stat))

barplot(table(data$marital.stat)/nrow(data) * 100)
#age---------------------------------------------
summary(data$age)
hist(data$age, breaks = 20)
boxplot(data$age)

sum(data$age > 100)
data[data$age > 100,] #Who are they?

#Questions
#What percentage of customers are between 25 and 35?
sum(data$age > 24 & data$age < 36)/nrow(data) * 100
sum(data$age > 24 & data$age < 36)/sum(data$age < 100) * 100 #more accurate

#What percentage of male customers are above 30?
sum(data$sex == "M" & data$age > 30 & data$age < 100)/sum(data$sex == "M" & data$age < 100) * 100

#What percentage of female customers above 30 are employed?
sum(data$sex == "F" & data$age > 30 & data$is.employed == "TRUE", na.rm = TRUE) / sum(data$sex == "F" & data$age > 30 & !is.na(data$is.employed)) * 100
sum(data$sex == "F" & data$age > 30 & data$is.employed == "TRUE" & !is.na(data$is.employed)) / sum(data$sex == "F" & data$age > 30 & !is.na(data$is.employed)) * 100 

#Discretize age----------------------------------
summary(data$age)

#5 Categories?[18 - 29, 30 - 39, 40 - 49, 50 - 59, 60 >= ]
cut(data$age[data$age <= 100], 
    breaks = c(18, 29, 39, 49, 59, 100),
    #labels = c("G1", "G2", "G3", "G4", "G5"),
    include.lowest = T)
#Use either a rationale or 
#             quantiles to define breaks
##Bivariate Profiling----------------------------
#Continuous variables----------------------------
#correlation: https://analica.ir/correlation/
plot(data$age[data$age <= 100], 
     data$income[data$age <= 100])
cor(data$age[data$age <= 100], 
    data$income[data$age <= 100], 
    use = "complete.obs",
    method = "pearson")

#Hypothesis test: Pearson Correlation
#Normality assumption for Pearson correlation
cor.test(data$age[data$age <= 100 & !is.na(data$income)], 
         data$income[data$age <= 100 & !is.na(data$income)],
         method = "spearman", exact = FALSE)
#exact: to compute exact p-value
#Categorical variables---------------------------
#Two Categorical Variables: Cross Tabulation Analysis
#health.ins vs. sex
cross_tab <- table(data$sex, data$health.ins)
cross_tab
prop.table(cross_tab, margin = 1) #over rows
prop.table(cross_tab, margin = 2) #over columns

#https://analica.ir/cross-tab-chi-square-test/

#Chi-Square Test
#H0: health.ins is independent of sex
#H1: health.ins is related to sex
#If p-value < 0.05 reject H0
chisq.test(cross_tab)

#Categorical vs Numerical Variables--------------
#health.ins vs. income
boxplot(income ~ health.ins, 
        data = data)

#Comparing means: t-test, ANOVA, Mann-Whitney test, Kruskal-Wallis test 
tapply(data$income, data$health.ins, mean, na.rm = TRUE)

#t-test needs normality assumption
#Comparing medians:
tapply(data$income, data$health.ins, median, na.rm = TRUE)
#independent 2-group Mann-Whitney U Test
wilcox.test(income ~ health.ins, 
            data = data, 
            alternative = "two.sided",
            paired = FALSE)
###Assignment: ----------------------------------
#Consider CS_01 dataset and answer these questions
  #Q1: What percentage of female customers are below 26?
  #Q2: What percentage of customers who have health insurance are employed with income above average income?
  #Q3: What percentage of customers who have health insurance are married males?
  #Q4: What percentage of married customers have health insurance?
  #Q5: Identify our top customers? (income > average , homeowner, and at least one vehicle)
  #Q6: Check if age is normally distributed. 
  #    Apply Boxcox transformation on age and check its impact on normality assumption.
  #Q7: Check these hypotheses: 
  #    a: relationship btw employment status and health insurance
  #    b: relationship btw income and sex
  #    c: relationship btw num.vehicles and housing.type
###End of the Code###----------------------------

