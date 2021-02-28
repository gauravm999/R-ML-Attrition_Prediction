#Practice Project - HR Attrition Prediction Analysis
#https://lms.simplilearn.com/courses/3813/PG-DS---Data-Science-with-R/syllabus

library(readxl)
library(plyr) #cleaning
library(dplyr) 
library(e1071)
library(caret)
library(mlbench)
library(ggplot2)
library(tidyverse)
library(funModeling) 
library(Hmisc)
library(car) #vif
library(tinytex)
library(pROC)
library(randomForest)

#Task 1: Load data 

setwd("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/3. Practice_Projects/HR Attrition_Prediction_IBM")
HR_data<- read_excel("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/3. Practice_Projects/HR Attrition_Prediction_IBM/HR_Attrition_IBM_Data.xlsx")

dim(HR_data)
names(HR_data)
str(HR_data)
summary(HR_data)
# View(HR_data)

#Covert Char variables to Categorical and Integer variables
HR_data$Attrition<- as.factor(HR_data$Attrition)
HR_data$Department<- as.factor(HR_data$Department)
HR_data$Education<- as.integer(HR_data$Education)
HR_data$EducationField<- as.factor(HR_data$EducationField)
HR_data$EnvironmentSatisfaction<- as.integer(HR_data$EnvironmentSatisfaction)
HR_data$JobSatisfaction<- as.integer(HR_data$JobSatisfaction)
HR_data$MaritalStatus<- as.factor(HR_data$MaritalStatus)
HR_data$NumCompaniesWorked<- as.integer(HR_data$NumCompaniesWorked)
HR_data$WorkLifeBalance<- as.integer(HR_data$WorkLifeBalance)
HR_data$YearsAtCompany<- as.integer(HR_data$YearsAtCompany)

str(HR_data)
summary(HR_data)
HR_data<- as.data.frame(HR_data)

#Task 2 - EDA (Part1- Age ditribution of Employees):
Age_dist<- hist(HR_data$Age, col = 'blue', breaks=10, labels = TRUE,
                main ="Employee distribution by Age",
                xlab = "Age", ylab = "Frequency")
Age_dist

# g<- ggplot(HR_data, aes(x=Age, fill=Attrition)) +
#   geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity')+
#   stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)
# g

# ?geom_histogram

Age_Attr_Dist<- table(HR_data$Attrition,HR_data$Age)
barplot(Age_Attr_Dist,legend=c("No","Yes"),main=" Distribution of Attrition by Age",
        xlab= "Age",ylab="Number of Employees", col=c('blue','red'))

#Insights:
# ~60% (882) of the Total employees (1470) are between 25-40 age group


#Task 2 - EDA (Part2 -Find outliers) :
par(mfrow=c(2,5))
OutAge<- boxplot(HR_data$Age, main="Age")
OutDistance<- boxplot(HR_data$DistanceFromHome, main="Distance from Home")
OutEdu<- boxplot(HR_data$Education, main="Education")
OutEnv<- boxplot(HR_data$EnvironmentSatisfaction, main="EnvironmentSatisfaction")
OutJobsat<- boxplot(HR_data$JobSatisfaction, main="JobSatisfaction")
OutMonthlyIncome<- boxplot(HR_data$MonthlyIncome, main="MonthlyIncome")
OutNumCoWrkd<- boxplot(HR_data$NumCompaniesWorked, main="NumCompaniesWorked")
OutWlbal<- boxplot(HR_data$WorkLifeBalance, main="WorkLifeBalance")
OutYrsAtCo<- boxplot(HR_data$YearsAtCompany, main="YearsAtCompany")

dev.off()

#Insights:
# There are outliers in following variables :  
# Lot of Outliers present in Monthly Income and Years at Company
# Few Outliers present in Number of COmpanies worked

# Task 2(c) - Attrition by Age

Attr_by_Age<- ggplot(HR_data, aes(x=Age, fill=Attrition)) +
  geom_histogram(color="#e9ecef", alpha=0.7, stat = "bin", position = "stack")+
  stat_bin(binwidth=5, geom="text", aes(label=..count..))
Attr_by_Age

# Insight 
# Majority of the attrition is between 30-40 age agroup


# Task2: Part3 (Explore data for Left employees):

#Attrition_by_Department:
Attr_by_Dept <- table(HR_data$Department,HR_data$Attrition)
Attr_by_Dept

Attr_by_Dept_plot<- table(HR_data$Attrition,HR_data$Department)
barplot(Attr_by_Dept_plot,legend=c("No","Yes"),main=" Attrition by Department",
        xlab= "Age",ylab="Number of Employees", col=c('blue','red'))


Attr_by_Dept_tab<- with(HR_data, table(Department, Attrition))
prop.table(Attr_by_Dept_tab, margin = 1)

#Attrition_by_ Educationt:

Attr_by_Edu <- table(HR_data$Education,HR_data$Attrition)
Attr_by_Edu

Attr_by_Edu_plot<- table(HR_data$Attrition,HR_data$Education)
barplot(Attr_by_Edu_plot,legend=c("No","Yes"),main=" Attrition by Education",
        xlab= "Education",ylab="Number of Employees", col=c('blue','red'))

Attr_by_Edu_tab<- with(HR_data, table(Education, Attrition))
prop.table(Attr_by_Edu_tab, margin = 1)

#Attrition_by_ Educationt:

Attr_by_Edufield <- table(HR_data$EducationField,HR_data$Attrition)
Attr_by_Edufield

Attr_by_Edufield_plot<- table(HR_data$Attrition,HR_data$EducationField)
barplot(Attr_by_Edufield_plot,legend=c("No","Yes"),main=" Attrition by Education Field",
        xlab= "Education Field",ylab="Number of Employees", col=c('blue','red'))

Attr_by_Edufield_tab<- with(HR_data, table(EducationField, Attrition))
prop.table(Attr_by_Edufield_tab, margin = 1)


#Attrition_by_ EnvironmentSatisfaction:

Attr_by_Envsat <- table(HR_data$EnvironmentSatisfaction,HR_data$Attrition)
Attr_by_Envsat

Attr_by_Envsat_plot<- table(HR_data$Attrition,HR_data$EnvironmentSatisfaction)
barplot(Attr_by_Envsat_plot,legend=c("No","Yes"),main=" Attrition by Environment Satisfaction",
        xlab= "Education Environment Satisfaction",ylab="Number of Employees", col=c('blue','red'))

Attr_by_Envsat_tab<- with(HR_data, table(EnvironmentSatisfaction, Attrition))
prop.table(Attr_by_Envsat_tab, margin = 1)


#Attrition_by_ JobSatisfaction:

Attr_by_Jobsat <- table(HR_data$JobSatisfaction,HR_data$Attrition)
Attr_by_Jobsat

Attr_by_Jobsat_plot<- table(HR_data$Attrition,HR_data$JobSatisfaction)
barplot(Attr_by_Envsat_plot,legend=c("No","Yes"),main=" Attrition by Environment Satisfaction",
        xlab= "Job Satisfaction",ylab="Number of Employees", col=c('blue','red'))

Attr_by_Jobsat_tab<- with(HR_data, table(JobSatisfaction, Attrition))
prop.table(Attr_by_Jobsat_tab, margin = 1)

#Attrition_by_ MaritalStatus:

Attr_by_MariStat<- table(HR_data$MaritalStatus,HR_data$Attrition)
Attr_by_MariStat

Attr_by_MariStat_plot<- table(HR_data$Attrition,HR_data$MaritalStatus)
barplot(Attr_by_MariStat_plot,legend=c("No","Yes"),main=" Attrition by Marital Status",
        xlab= "Marital Status",ylab="Number of Employees", col=c('blue','red'))

Attr_by_MariStat_tab<- with(HR_data, table(MaritalStatus, Attrition))
prop.table(Attr_by_MariStat_tab, margin = 1)

#Attrition_by_ NumCompaniesWorked:

Attr_by_NumCosWorked<- table(HR_data$NumCompaniesWorked,HR_data$Attrition)
Attr_by_NumCosWorked

Attr_by_NumCosWorked_plot<- table(HR_data$Attrition,HR_data$NumCompaniesWorked)
barplot(Attr_by_NumCosWorked_plot,legend=c("No","Yes"),main=" Attrition by Marital Status",
        xlab= "NumCompaniesWorked",ylab="Number of Employees", col=c('blue','red'))

Attr_by_NumCosWorked_tab<- with(HR_data, table(NumCompaniesWorked, Attrition))
prop.table(Attr_by_NumCosWorked_tab, margin = 1)


#Attrition_by_ WorkLifeBalance:

Attr_by_WLBal<- table(HR_data$WorkLifeBalance,HR_data$Attrition)
Attr_by_WLBal

Attr_by_WLBal_plot<- table(HR_data$Attrition,HR_data$WorkLifeBalance)
barplot(Attr_by_WLBal_plot,legend=c("No","Yes"),main=" Attrition by Work Life Balance",
        xlab= "WorkLifeBalance",ylab="Number of Employees", col=c('blue','red'))

Attr_by_WLBal_tab<- with(HR_data, table(WorkLifeBalance, Attrition))
prop.table(Attr_by_WLBal_tab, margin = 1)

#Attrition_by_ YearsAtCompany:

Attr_by_YearsAtCo<- table(HR_data$YearsAtCompany,HR_data$Attrition)
Attr_by_YearsAtCo

Attr_by_YearsAtCo_plot<- table(HR_data$Attrition,HR_data$YearsAtCompany)
barplot(Attr_by_YearsAtCo_plot,legend=c("No","Yes"),main=" Attrition by Years at Company",
        xlab= "YearsAtCompany",ylab="Number of Employees", col=c('blue','red'))

Attr_by_YearsAtCo_tab<- with(HR_data, table(YearsAtCompany, Attrition))
prop.table(Attr_by_YearsAtCo_tab, margin = 1)

names(HR_data)
#Attrition_by_ DistanceFromHome:

Attr_by_DistanceFromHome<- table(HR_data$DistanceFromHome,HR_data$Attrition)
Attr_by_DistanceFromHome

Attr_by_DistanceFromHome_plot<- table(HR_data$Attrition,HR_data$DistanceFromHome)
barplot(Attr_by_DistanceFromHome_plot,legend=c("No","Yes"),main=" Attrition by Years at Company",
        xlab= "DistanceFromHome",ylab="Number of Employees", col=c('blue','red'))

Attr_by_DistanceFromHome_tab<- with(HR_data, table(DistanceFromHome, Attrition))
prop.table(Attr_by_DistanceFromHome_tab, margin = 1)

#Attrition_by_ MonthlyIncome:
Attr_by_MonthlyIncome<- ggplot(HR_data, aes(x=MonthlyIncome, fill=Attrition)) +
  geom_histogram(color="#e9ecef", alpha=0.7, stat = "bin", position = "stack")+
  stat_bin(binwidth=1000, geom="text", aes(label=..count..), vjust=-0.5)
Attr_by_MonthlyIncome

# Insights
# Attrition rate is highest in Sales at ~21% and lowest in R&D ~14%
# Attrition rate is highest in employees with "Below collge" education at ~18%
# Attrition rate is highest in employees with "Human Resources" degree at ~26%
# Attrition rate is highest in employees who rated "Low Environment Satisfaction" at ~25%
# Attrition rate is highest in employees who rated "Low Job Satisfaction" at ~23%
# Attrition rate is highest in employees who are "Single" at ~26%
# Attrition rate is highest in employees who worked in "5 companies" at ~25% but its base is low, however employees who worked in 1 company (500+) has attrition of 19% which is a matter of concern
# Attrition rate is highest in employees who rated Work life bakace as "Bad" at ~31%, however there are 14% of the Total employees who rated Work Life balance as "Better".
# Attrition rate is highest in employees who have spent less than 2 years in the Company
# Attrition rate is highest in employees who lives in 12-13 miles distance from the Company
# Attrition rate is highest in employees with Monthly Income <3000


#Task 2 (part 5) Find out the distribution of employees by the education field:
Edufield_Attr_Dist<- table(HR_data$EducationField)
barplot(Edufield_Attr_Dist,main=" Distribution by Education Field",
        xlab= "EducationField",ylab="Number of Employees", col='blue')
#Insight:
# Majority of Employees are from Life Sciences

#Task 2 (part 5) Give a bar chart for the number of married and unmarried employees:
MaritalStatus_Dist<- table(HR_data$MaritalStatus)
barplot(MaritalStatus_Dist,main=" Distribution of Attrition by MaritalStatus",
        xlab= "MaritalStatus",ylab="Number of Employees", col='blue')
#Insight:
# Majority of Employees are Married

#Task 3: Model build (to Predict Attrition)

# Check if variables are Correlated - Correlation :
library(corrplot)
correlation_matrix <- cor(HR_data[,unlist(lapply(HR_data, is.numeric))])
correlation_matrix_df<- as.data.frame(correlation_matrix)
correlation_matrix_df
symnum(correlation_matrix) #variables are not correlared

# Correlation Plot:
cor = cor(HR_data[,unlist(lapply(HR_data, is.numeric))])
corrplot(cor, method="number", type = "upper", order = "hclust", 
         tl.col = "black", cl.cex=1)


# Split data: Train-Test:
## set the seed to make your partition reproducible
set.seed(123)
dt = sort(sample(nrow(HR_data), nrow(HR_data)*.7))
train<-HR_data[dt,]
test<-HR_data[-dt,]

dim(train)
dim(test)


#Model -Train data:

M1<- glm(Attrition ~ ., train, family=binomial())
summary(M1)
vif(M1) #VIF less than 5 for al variables

# Model with Significant variables only on Training data:
formula<- Attrition ~ Age + Department + DistanceFromHome + EnvironmentSatisfaction + JobSatisfaction +
  MaritalStatus + MonthlyIncome + NumCompaniesWorked + WorkLifeBalance

M2<- glm(formula, train, family="binomial")
summary(M2)
vif(M2) #VIF less than 5 for al variables

# M2 Model have all significant variables affecting Attrition and has lower AIC value, hence considered as final model.

# Let's say our null hypothesis is that second model is better than the first model. 
# p < 0.05 would reject our hypothesis and in case p > 0.05, we'll fail to reject the null hypothesis.

# Compare above 2 Models - ANOVA:
Models_comparision<- anova(M1,M2,test = "Chisq")
Models_comparision

# With p > 0.05, this ANOVA test also corroborates the fact that the second model is better than first model. 
# Let's predict on unseen data now.


#Prediction on Test data
# predict_train<- predict(M2,newdata=train,type="response") #predict on train data
predict_test<- predict(M2,newdata=test,type="response") #predict on test data

#Combine predictions
# train_combined_final<- cbind(train, predict_train)
# View(train_combined_final)

test_combined_final<- cbind(test, predict_test)
test_combined_final$score <- ifelse(test_combined_final$predict_test > 0.25, '1', '0')
# View(test_combined_final)

## Confusion Matrix for Training Data
# train$score <- ifelse(train_combined_final$predict_train > 0.5, '1', '0')

# Train_Conf_matrix<- prop.table(table(train$score,train$Attrition),2)
# Train_Conf_matrix
# 
# Train_Conf_matrix_ABSNumbers<- table(train$score, train$Attrition)
# Train_Conf_matrix_ABSNumbers

## Confusion Matrix - Test Data:
Test_Conf_matrix<- prop.table(table(test_combined_final$score,test_combined_final$Attrition),2)
Test_Conf_matrix

Test_Conf_matrix_ABSNumbers<- table(test_combined_final$score,test_combined_final$Attrition)
Test_Conf_matrix_ABSNumbers

# Check Model Accuracy:
TestNum <- test_combined_final  %>% mutate(predict_test = 1*(test_combined_final$predict_test > 0.25) + 0,
                         Attrition_binary = 1*(Attrition == "Yes") + 0)
# TestNum

TestNumFinal <- TestNum %>% mutate(accurate = 1*(predict_test == Attrition_binary))
Accuracy<- sum(TestNumFinal$accurate)/nrow(TestNumFinal)
Accuracy 
# Model accuracy - 0.7732426 (i.e. 77.32%)
# Precision : 50.7%
# Recall : 37.6%

#plot ROC 
library(ROCR) 
library(Metrics)
pr <- prediction(test_combined_final$predict_test,test_combined_final$Attrition)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf)
