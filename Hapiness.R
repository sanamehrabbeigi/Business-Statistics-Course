########################################################
### Step 0.  Load any libraries and format output
########################################################
library(car)
library(ggplot2)
library(dplyr)
require(glmnet)
library(pROC)
library(MASS)


options(scipen = 5, # discourage scientific notation for better formatting of coefficients
        digits = 4)  # print 3 digits

########################################################
### Step 1.  Import the dataset 
########################################################

tempData <- read.csv(url("https://laurencipriano.github.io/IveyBusinessStatistics/Datasets/GSS_HappyData.csv"), header = TRUE)

summary(tempData)
View(tempData)
nrow(tempData)
ncol(tempData)

#Question:
#How does job satisfaction, unemployment, job security, or job mobility associate with happiness? 

Happiness_Job <- tempData[, c("VHAPPY","SATJOB","UNEMP","JobFind","JOBLOSE","WRKSTAT","PRESTG10","SEI10","SATFIN","YEAR","SEX","RACE","AGE","DEGREE","EDUC","CLASS_","MARITAL")]
View(Happiness_Job)
summary(Happiness_Job)
nrow(Happiness_Job)
ncol(Happiness_Job)

cor(Happiness_Job)

#percentage of NAs
na_percentage  <-colSums(is.na(Happiness_Job))/67588 * 100
na_percentage

get_mode <- function(x) {
  unique_x <- unique(na.omit(x)) # Remove NA values to avoid errors
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Replace NA values in all columns with the mode
Happiness_Job[] <- lapply(Happiness_Job, function(col) {
  if (any(is.na(col))) {
    col[is.na(col)] <- get_mode(col)
  }
  return(col)
})

#We should first make sure that there is a linear relationship b/w
#predictors and the log of odds of Happiness

windows()
par(mfrow = c(3, 3))
hist(Happiness_Job$VHAPPY)

hist(Happiness_Job$SATJOB)
hist(Happiness_Job$UNEMP)
hist(Happiness_Job$JOBLOSE)
hist(Happiness_Job$JobFind)

hist(Happiness_Job$WRKSTAT)
hist(Happiness_Job$PRESTG10)
hist(Happiness_Job$SEI10)
hist(Happiness_Job$SATFIN)
par(mfrow = c(1, 1))


#### SATJOB ####  categorical
sample_bySATJOB = table(Happiness_Job$SATJOB)
sample_bySATJOB

Hapiness_bySATJOB = table(Happiness_Job$SATJOB[Happiness_Job$VHAPPY == 1])
Hapiness_bySATJOB

data.SATJOB = data.frame(SATJOB = as.numeric(names(sample_bySATJOB)), 
                      counts = as.numeric(sample_bySATJOB),
                      Happy = as.numeric(Hapiness_bySATJOB))
data.SATJOB

# proportions:
data.SATJOB$prop = data.SATJOB$Happy/data.SATJOB$counts
#barplot(data.SATJOB$prop ~ data.SATJOB$SATJOB)
dev.off()

ggplot(data.SATJOB, aes(x = SATJOB, y = prop)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Job Satisfaction",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by Job Satisfaction"
  ) +
  theme_minimal()

#odds: p/(1-p)
data.SATJOB$odds = data.SATJOB$prop/(1 - data.SATJOB$prop)
data.SATJOB$logodds = log(data.SATJOB$odds)

plot(data.SATJOB$SATJOB, data.SATJOB$logodds, xlab = "Job Satisfaction",
     ylab = "Log(odds)")
abline(lm(data = data.SATJOB, logodds ~ SATJOB), col = "red")


#### UNEMP ####  binary # 1 = Yes
sample_byUNEMP= table(Happiness_Job$UNEMP)
sample_byUNEMP

Hapiness_byUNEMP = table(Happiness_Job$UNEMP[Happiness_Job$VHAPPY == 1])
Hapiness_byUNEMP

data.UNEMP = data.frame(UNEMP = as.numeric(names(sample_byUNEMP)), 
                         counts = as.numeric(sample_byUNEMP),
                         Happy = as.numeric(Hapiness_byUNEMP))
data.UNEMP


# proportions:
data.UNEMP$prop = data.UNEMP$Happy/data.UNEMP$counts
#barplot(data.UNEMP$prop ~ data.UNEMP$UNEMP)
ggplot(data.UNEMP, aes(x = UNEMP, y = prop)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    x = "Ever unemployed in last ten yrs",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by UNEMP"
  ) +
  theme_minimal()

#odds: p/(1-p)
data.UNEMP$odds = data.UNEMP$prop/(1 - data.UNEMP$prop)
data.UNEMP$logodds = log(data.UNEMP$odds)

plot(data.UNEMP$UNEMP, data.UNEMP$logodds, xlab = "Ever unemployed in last ten yrs",    ylab = "Log(odds)")
abline(lm(data = data.UNEMP, logodds ~ UNEMP), col = "red")

#chi-squared independence:
chisq_test_UNEMP <- chisq.test(table(Happiness_Job$UNEMP, Happiness_Job$VHAPPY))
print(chisq_test_UNEMP)

#### JOBLOSE ####  categorical
sample_byJOBLOSE = table(Happiness_Job$JOBLOSE)
sample_byJOBLOSE

Hapiness_byJOBLOSE = table(Happiness_Job$JOBLOSE[Happiness_Job$VHAPPY == 1])
Hapiness_byJOBLOSE

data.JOBLOSE = data.frame(JOBLOSE= as.numeric(names(sample_byJOBLOSE)), 
                         counts = as.numeric(sample_byJOBLOSE),
                         Happy = as.numeric(Hapiness_byJOBLOSE))
data.JOBLOSE

# proportions:
data.JOBLOSE$prop = data.JOBLOSE$Happy/data.JOBLOSE$counts
#barplot(data.JOBLOSE$prop ~ data.JOBLOSE$JOBLOSE)
ggplot(data.JOBLOSE, aes(x = JOBLOSE, y = prop)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    x = "Job Security",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by JOBLOSE"
  ) +
  theme_minimal()

#odds:
data.JOBLOSE$odds = data.JOBLOSE$prop/(1 - data.JOBLOSE$prop)
data.JOBLOSE$logodds = log(data.JOBLOSE$odds)

plot(data.JOBLOSE$JOBLOSE, data.JOBLOSE$logodds, xlab = "Job Security",
     ylab = "Log(odds)")
abline(lm(data = data.JOBLOSE, logodds ~ JOBLOSE), col = "red")

#### JobFind ####  categorical
sample_byJobFind = table(Happiness_Job$JobFind)
sample_byJobFind

Hapiness_byJobFind = table(Happiness_Job$JobFind[Happiness_Job$VHAPPY == 1])
Hapiness_byJobFind

data.JobFind = data.frame(JobFind= as.numeric(names(sample_byJobFind)), 
                          counts = as.numeric(sample_byJobFind),
                          Happy = as.numeric(Hapiness_byJobFind))
data.JobFind

# proportions:
data.JobFind$prop = data.JobFind$Happy/data.JobFind$counts
ggplot(data.JobFind, aes(x = JobFind, y = prop)) +
  geom_bar(stat = "identity", fill = "yellow") +
  labs(
    x = "Job Mobility",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by JobFind"
  ) +
  theme_minimal()
#odds:
data.JobFind$odds = data.JobFind$prop/(1 - data.JobFind$prop)
data.JobFind$logodds = log(data.JobFind$odds)

plot(data.JobFind$JobFind, data.JobFind$logodds, xlab = "Job Mobility",
     ylab = "Log(odds)")
abline(lm(data = data.JobFind, logodds ~ JobFind), col = "red")


#### WRKSTAT ####  categorical #not ordered
sample_byWRKSTAT = table(Happiness_Job$WRKSTAT)
sample_byWRKSTAT

Hapiness_byWRKSTAT = table(Happiness_Job$WRKSTAT[Happiness_Job$VHAPPY == 1])
Hapiness_byWRKSTAT

data.WRKSTAT = data.frame(WRKSTAT= as.factor(names(sample_byWRKSTAT)), 
                          counts = as.numeric(sample_byWRKSTAT),
                          Happy = as.numeric(Hapiness_byWRKSTAT))
data.WRKSTAT

# proportions:
data.WRKSTAT$prop = data.WRKSTAT$Happy/data.WRKSTAT$counts
#barplot(data.WRKSTAT$prop ~ data.WRKSTAT$WRKSTAT)
ggplot(data.WRKSTAT, aes(x = WRKSTAT, y = prop)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(
    x = "Labor Force Status",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by WRKSTAT"
  ) +
  theme_minimal()

#odds: p/(1-p)
data.WRKSTAT$odds = data.WRKSTAT$prop/(1 - data.WRKSTAT$prop)
data.WRKSTAT$logodds = log(data.WRKSTAT$odds)

#plot(data.WRKSTAT$WRKSTAT, data.WRKSTAT$logodds, xlab = "Labor Force Status",    ylab = "Log(odds)")
#abline(lm(data = data.WRKSTAT, logodds ~ WRKSTAT), col = "red")

#ANOVA ??????????? -> no
#chi-squared independence:
#chisq_test_WRKSTAT <- chisq.test(table(Happiness_Job$WRKSTAT, Happiness_Job$VHAPPY))
#print(chisq_test_WRKSTAT)

table(Happiness_Job$WRKSTAT, Happiness_Job$VHAPPY)

#### PRESTG10 ####  continuous
Happiness_Job$PRESTG10
Happiness_Job$PRESTG10[Happiness_Job$PRESTG10 == 80] <- 77
Happiness_Job$PRESTG10[Happiness_Job$PRESTG10 == 18] <- 19
Happiness_Job$PRESTG10[Happiness_Job$PRESTG10 == 17] <- 18
Happiness_Job$PRESTG10[Happiness_Job$PRESTG10 == 16] <- 17
Happiness_Job$PRESTG10 = Happiness_Job$PRESTG10 - 16

sample_byPRESTG10 = table(Happiness_Job$PRESTG10)
sample_byPRESTG10 

Hapiness_byPRESTG10 = table(Happiness_Job$PRESTG10[Happiness_Job$VHAPPY == 1])
Hapiness_byPRESTG10

data.PRESTG10 = data.frame(PRESTG10= as.numeric(names(sample_byPRESTG10)), 
                          counts = as.numeric(sample_byPRESTG10),
                          Happy = as.numeric(Hapiness_byPRESTG10))
data.PRESTG10

#proportions:
data.PRESTG10$prop = (data.PRESTG10$Happy + 0.5) / (data.PRESTG10$counts + 1)
#data.PRESTG10$prop = (data.PRESTG10$Happy) / (data.PRESTG10$counts)
#barplot(data.PRESTG10$prop ~ data.PRESTG10$PRESTG10)
ggplot(data.PRESTG10, aes(x = PRESTG10, y = prop)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(
    x = "Occupational Prestige Score",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by PRESTG10"
  ) +
  theme_minimal()

#odds:
data.PRESTG10$odds = data.PRESTG10$prop/(1 - data.PRESTG10$prop)
data.PRESTG10$logodds = log(data.PRESTG10$odds)

windows()
plot(data.PRESTG10$PRESTG10, data.PRESTG10$logodds, xlab = "Occupational Prestige Score",
     ylab = "Log(odds)")
abline(lm(data = data.PRESTG10, logodds ~ PRESTG10), col = "red")

#### SATFIN ####  categorical
sample_bySATFIN = table(Happiness_Job$SATFIN)
sample_bySATFIN

Hapiness_bySATFIN = table(Happiness_Job$SATFIN[Happiness_Job$VHAPPY == 1])
Hapiness_bySATFIN

data.SATFIN = data.frame(SATFIN = as.numeric(names(sample_bySATFIN)), 
                         counts = as.numeric(sample_bySATFIN),
                         Happy = as.numeric(Hapiness_bySATFIN))
data.SATFIN

# proportions:
data.SATFIN$prop = data.SATFIN$Happy/data.SATFIN$counts
#barplot(data.SATFIN$prop ~ data.SATFIN$SATFIN)
dev.off()

ggplot(data.SATFIN, aes(x = SATFIN, y = prop)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    x = "Financial Satisfaction",
    y = "Proportion of Happiness (Happy = 1)",
    title = "Proportion of Happiness by Financial Satisfaction"
  ) +
  theme_minimal()

#odds: p/(1-p)
data.SATFIN$odds = data.SATFIN$prop/(1 - data.SATFIN$prop)
data.SATFIN$logodds = log(data.SATFIN$odds)

windows()
plot(data.SATFIN$SATFIN, data.SATFIN$logodds, xlab = "Financial Satisfaction",
     ylab = "Log(odds)")
abline(lm(data = data.SATFIN, logodds ~ SATFIN), col = "red")





cor.matrix <- cor(Happiness_Job)
cor.matrix
print(ifelse(cor.matrix[] > 0.7,"X","-"))

#as factor of each predictors
Happiness_Job$F_SATJOB = factor(Happiness_Job$SATJOB, levels = c(1,2,3,4),labels = c("Very satisfied", "Moderately satisfied","A little dissatisfied","Very dissatisfied"))
Happiness_Job$F_UNEMP = factor(Happiness_Job$UNEMP, levels = c(1,2),labels = c("YES", "NO"))
Happiness_Job$F_JOBLOSE = factor(Happiness_Job$JOBLOSE, levels = c(1,2,3,4,5),labels = c("Very likely", "Fairly likely","Not too likely","Not likely","Leaving labor force"))
Happiness_Job$F_JobFind = factor(Happiness_Job$JobFind , levels = c(1,2,3),labels = c("Very easy", "Somewhat easy","Not easy"))
Happiness_Job$F_WRKSTAT= factor(Happiness_Job$WRKSTAT , levels = c(1,2,3,4,5,6,7,8),labels = c("Working full time", "Working part time","With a job,NW","Unemployed","Retired","In school","Keeping house","Other"))
Happiness_Job$F_SATFIN= factor(Happiness_Job$SATFIN , levels = c(1,2,3),labels = c("Pretty well satisfied", "More or less satisfied","Not satisfied at all"))

Happiness_Job$F_SEX = factor(Happiness_Job$SEX, levels = c(1,2),labels = c("MALE", "FEMALE"))
Happiness_Job$F_RACE = factor(Happiness_Job$RACE, levels = c(1,2,3),labels = c("White", "Black","Other"))
Happiness_Job$F_DEGREE = factor(Happiness_Job$DEGREE, levels = c(0,1,2,3,4),labels = c("Less than high school", "High school","Associate/junior college","Bachelor's","Graduate"))
Happiness_Job$F_CLASS= factor(Happiness_Job$CLASS_, levels = c(1,2,3,4,5),labels = c("Lower class", "Working class","Middle class","Upper class","No class"))
Happiness_Job$F_MARITAL= factor(Happiness_Job$MARITAL, levels = c(1,2,3,4,5),labels = c("Married", "Widowed","Divorced","Separated","Never married"))

#model
gl1 = glm(data = Happiness_Job, VHAPPY ~ F_SATJOB+F_UNEMP+F_JOBLOSE+F_JobFind+F_WRKSTAT+PRESTG10+F_SATFIN+AGE+F_SEX+F_RACE+F_DEGREE+F_CLASS+F_MARITAL, family = binomial)
summary(gl1)

exp_coeff <- exp(coef(gl1))
print(exp_coeff)

#is.factor(Happiness_Job$FACSATJOB)
#is.ordered(Happiness_Job$FACSATJOB)


#### Confusion Matrix #####
Happiness_Job$predicted.prob1 = predict(gl1, type = "response")
Happiness_Job$predicted.class1 = ifelse(Happiness_Job$predicted.prob1 >= 0.5, 1, 0)

confusion_matrix1 = table(predicted = Happiness_Job$predicted.class1, 
                         actual = Happiness_Job$VHAPPY)
confusion_matrix1

#accuracy: measures the percentage of true predictions as a whole:
accuracy1 = sum(diag(confusion_matrix1))/sum(confusion_matrix1)
accuracy1
#precision: this calculates the percentage of predicted positives:
precision1 = confusion_matrix1[2,2]/sum(confusion_matrix1[2, ])
precision1 
#sensitivity (recall) = the percentage of true positives in terms of the total positives
sensitivity1 = confusion_matrix1[2,2]/sum(confusion_matrix1[, 2])
sensitivity1
#specificity: the number of true negatives divided by TN + FP
specificty1 = confusion_matrix1[1, 1]/sum(confusion_matrix1[, 1])
specificty1
#F1 score: 2*precision*sensitivity/(precision + sensitivity)
f1_1 = 2*precision1*sensitivity1/(precision1 + sensitivity1)
f1_1


#### ROC and AUC
windows()
roc.obj1 = roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob1)
plot(roc.obj1, col = 'red',legacy.axes = TRUE)
auc(roc.obj1)

vif(gl1)

#without prestige
gl2 = glm(data = Happiness_Job, VHAPPY ~ F_UNEMP+F_JOBLOSE+F_JobFind+F_WRKSTAT+PRESTG10+F_SATFIN+AGE+F_SEX+F_RACE+F_DEGREE+F_CLASS+F_MARITAL, family = binomial)
summary(gl2)

Happiness_Job$predicted.prob2= predict(gl2, type = "response")
Happiness_Job$predicted.class2 = ifelse(Happiness_Job$predicted.prob2 >= 0.5, 1, 0)

confusion_matrix2 = table(predicted = Happiness_Job$predicted.class2, 
                          actual = Happiness_Job$VHAPPY)
confusion_matrix2

windows()
roc.obj2 = roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob2)
plot(roc.obj2, col = "blue",legacy.axes = TRUE)
auc(roc.obj2)

#step function
step_model <- stepAIC(gl1, direction = "both")
summary(step_model)

Happiness_Job$predicted.prob3= predict(step_model, type = "response")
Happiness_Job$predicted.class3 = ifelse(Happiness_Job$predicted.prob3 >= 0.5, 1, 0)

confusion_matrix3 = table(predicted = Happiness_Job$predicted.class3, 
                          actual = Happiness_Job$VHAPPY)
confusion_matrix3

windows()
roc.obj3 = roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob3)
plot(roc.obj3, col = "green",legacy.axes = TRUE)
auc(roc.obj3)




gl4 = glm(data = Happiness_Job, VHAPPY ~ F_SATJOB+F_UNEMP+F_JOBLOSE+F_JobFind+F_WRKSTAT+PRESTG10+F_SATFIN, family = binomial)
summary(gl4)

Happiness_Job$predicted.prob4= predict(gl4, type = "response")
Happiness_Job$predicted.class4 = ifelse(Happiness_Job$predicted.prob4 >= 0.5, 1, 0)

confusion_matrix4 = table(predicted = Happiness_Job$predicted.class4, 
                          actual = Happiness_Job$VHAPPY)
confusion_matrix4


# ROC curves
roc.obj1 <- roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob1)
roc.obj2 <- roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob2)
roc.obj3 <- roc(Happiness_Job$VHAPPY, Happiness_Job$predicted.prob3)

# Plots
plot(roc.obj1, col = 'red', main = "ROC Curves", print.auc = TRUE, legacy.axes = TRUE)
plot(roc.obj2, col = 'blue', add = TRUE, print.auc = TRUE, print.auc.y = 0.4)
plot(roc.obj3, col = 'green', add = TRUE, print.auc = TRUE, print.auc.y = 0.4)
