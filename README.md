```{r}
data = read.csv("/Users/sung-alee/Desktop/141project/heart_disease.csv")
# colnames = HeartDiseaseorAttack, HighBP, HighChol, CholCheck, BMI, Smoker, Stroke, Diabetes, PhysActivity, Fruit, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, GenHlth, MentHlth, PhysHlth, DiffWalk
head(data)
#col_name = c(1,2,3,5,6,7,8,9)
#data = data[,col_name]
#interested in col: HeartDiseaseorAttack, HighBP, HighChol, BMI, Smoker, Stroke, Diabetes, PhysActivity (first 8 col)

#explanation of the columns 

# HighBP : Indicates if the person has been told by a health professional that they have High Blood Pressure.
# HighChol : Indicates if the person has been told by a health professional that they have High Blood Cholesterol.
# CholCheck : Cholesterol Check, if the person has their cholesterol levels checked within the last 5 years.
# BMI : Body Mass Index, calculated by dividing the persons weight (in kilogram) by the square of their height (in meters).
# Smoker : Indicates if the person has smoked at least 100 cigarettes.
# Stroke : Indicates if the person has a history of stroke.
# Diabetes : Indicates if the person has a history of diabetes, or currently in pre-diabetes, or suffers from either type of diabetes.
# PhysActivity : Indicates if the person has some form of physical activity in their day-to-day routine.
# Fruits : Indicates if the person consumes 1 or more fruit(s) daily.
# Veggies : Indicates if the person consumes 1 or more vegetable(s) daily.
# HvyAlcoholConsump : Indicates if the person has more than 14 drinks per week.
# AnyHealthcare : Indicates if the person has any form of health insurance.
# NoDocbcCost : Indicates if the person wanted to visit a doctor within the past 1 year but couldn’t, due to cost.
# GenHlth : Indicates the persons response to how well is their general health, ranging from 1 (excellent) to 5 (poor).
# Menthlth : Indicates the number of days, within the past 30 days that the person had bad mental health.
# PhysHlth : Indicates the number of days, within the past 30 days that the person had bad physical health.
# DiffWalk : Indicates if the person has difficulty while walking or climbing stairs.
# Sex : Indicates the gender of the person, where 0 is female and 1 is male.
# Age : Indicates the age class of the person, where 1 is 18 years to 24 years up till 13 which is 80 years or older, each interval between has a 5-year increment.
# Education : Indicates the highest year of school completed, with 0 being never attended or kindergarten only and 6 being, having attended 4 years of college or more.
# Income : Indicates the total household income, ranging from 1 (at least $10,000) to 6 ($75,000+)

# 0 is no diabetes, 1 is pre-diabetes, and 2 is diabetes

# 1 Never attended school or only kindergarten
# 2 Grades 1 through 8 (Elementary) 
# 3 Grades 9 through 11 (Some high school) 
# 4 Grade 12 or GED (High school graduate) 
# 5 College 1 year to 3 years (Some college or technical school)
# 6 College 4 years or more (College graduate)


# 1 ($10,000 to less than $20,000) 
# 2 ($20,000 to less than $25,000) 
# 3 ($25,000 to less than $35,000) 
# 4 ($35,000 to less than $50,000)
# 5 ($50,000 to less than $75,000) 
# 6  $75,000+ 


# 1 Age 18 to 24
# 2 Age 25 to 29
# 3 Age 30 to 34
# 4 Age 35 to 39
# 5 Age 40 to 44
# 6 Age 45 to 49
# 7 Age 50 to 54
# 8 Age 55 to 59
# 9 Age 60 to 64
# 10 Age 65 to 69
# 11 Age 70 to 74
# 12 Age 75 to 79
# 13 Age 80 or older

```

```{R}

fit_bml <- glm(HeartDiseaseorAttack~BMI, data = data_n, family = binomial) # fit the BMI
fit_income <- glm(HeartDiseaseorAttack~Income, data = data_n, family = binomial)# fit the Income 


prob= predict(fit_bml, type = "response") # predict based on training data 
prob2= predict(fit_income, type = "response")

#mean(prob) #0.09418559
##min(prob) # 0.06510333
#max(prob) # 0.3507381


#sum(prob>0.3)#82

#sum(predicted==1)#650
library(ggplot2)



# plot for probability of getting heart attact based on BMI and Income 
plot1 <- ggplot(data = data) +
geom_point(mapping = aes (x = BMI, y = prob, color = factor(HeartDiseaseorAttack,))) +
geom_hline(yintercept = 0.2, colour = "red") +
labs(y = "Probability of getting heart attact")+
  geom_smooth(mapping = aes (x = BMI, y = prob), method = "glm",
              method.args = list(family = "binomial"), se = FALSE)
plot2<- ggplot(data = data) +
geom_point(mapping = aes (x = Income, y = prob2, color = factor(HeartDiseaseorAttack,))) +
geom_hline(yintercept = 0.13, colour = "red") +
labs(y = "Probability of getting heart attact")+
  geom_smooth(mapping = aes (x = Income, y = prob2), method = "glm",
              method.args = list(family = "binomial"), se = FALSE)

# as long as your y is binary, then use the logistic model 

plot1
plot2


```


```{r}
# creating test and train data
set.seed(13)
train_index <- sample(1:nrow(data), 0.5 * nrow(data))  # 50% for training 
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
head(test_data)
max(test_data$Income)
#WILL CHANGE TO K-FOLD cv

```

```{r}
# LDA 

# Install and load the MASS package

library(MASS)
lda_model <- lda(HeartDiseaseorAttack ~ ., data = data) # fit the model using lda 


pred <- predict(lda_model, data)$class # predict based on training dataset 
confusion_mat <- table(data$HeartDiseaseorAttack, pred, dnn = c("True", "Predicted"))
confusion_mat

accuracy <- sum(diag(confusion_mat))/sum(confusion_mat)
accuracy 

```

```{r}
# Using train_data to create our model
logreg=glm(HeartDiseaseorAttack ~ HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + Diabetes + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age + Education + Income, data = train_data, family = binomial) #which variables are significant?

logregBMI = glm(HeartDiseaseorAttack ~ HighBP +BMI, data = train_data, family = binomial)
summary(logregBMI)

summary(logreg)
#significant variables = 
## (Intercept)       -7.7828371  0.1424933 -54.619  < 2e-16 ***
# HighBP             0.5238218  0.0249983  20.954  < 2e-16 ***
# HighChol           0.6099891  0.0231388  26.362  < 2e-16 ***
# CholCheck          0.4037878  0.0901655   4.478 7.52e-06
# Smoker             0.3481076  0.0221743  15.699  < 2e-16 ***
# Stroke             0.9670444  0.0349096  27.701  < 2e-16 ***
# Diabetes           0.1414394  0.0126559  11.176  < 2e-16 ***
# HvyAlcoholConsump -0.2474005  0.0546589  -4.526 6.00e-06 ***
# NoDocbcCost        0.2767829  0.0376501   7.351 1.96e-13 ***
# GenHlth            0.4770993  0.0133503  35.737  < 2e-16 ***
# DiffWalk           0.3139449  0.0272785  11.509  < 2e-16 ***
# Sex                0.7463776  0.0225979  33.029  < 2e-16 ***
# Age                0.2633630  0.0051587  51.052  < 2e-16 ***
# Income            -0.0461303  0.0059884  -7.703 1.33e-14 ***
logreg2 = glm(HeartDiseaseorAttack ~ HighBP + HighChol + CholCheck + Smoker + Stroke + Diabetes + HvyAlcoholConsump + NoDocbcCost + GenHlth + DiffWalk + Sex + Age + Income, data = train_data, family = binomial)
summary(logreg2)
```

```{r}
# using logreg2 to predict test_data
prediction <- predict(logreg2, test_data, type = "response")
predicted_classes <- ifelse(prediction > 0.5, 1, 0)
max(prediction)
unique(predicted_classes)
confusion_matrix <- print(table(test_data$HeartDiseaseorAttack, predicted_classes))
confusion_matrix <- table(predicted_classes, test_data$HeartDiseaseorAttack);confusion_matrix
print(sum(diag(confusion_matrix)) / sum(confusion_matrix))
```

```{r}
#plotting in R
logreg=glm(HeartDiseaseorAttack ~ HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + Diabetes + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age + Education + Income, data = train_data, family = binomial)
logreg2 = glm(HeartDiseaseorAttack ~ HighBP + HighChol + CholCheck + Smoker + Stroke + Diabetes + HvyAlcoholConsump + NoDocbcCost + GenHlth + DiffWalk + Sex + Age + Income, data = train_data, family = binomial)

# Set up a layout for multiple plots
par(mfrow = c(2,2))


#HIGH BP
barplot(table(train_data$HighBP), col = c("yellow", "green"), beside = T, names.arg = c("No High BP", "Yes High BP"), main = "High Blood Pressure", ylab = "Frequency")
#HIGH CHOL
barplot(table(train_data$HighChol), col = c("yellow", "green"), beside = T, names.arg = c("No High Chol", "Yes High Chol"), main = "High Cholestrol", ylab = "Frequency")
# CholCheck
barplot(table(train_data$CholCheck), col = c("yellow", "green"), beside = T, names.arg = c("No Check", "Yes Check"), main = "Cholesterol Check within 5 years ", ylab = "Frequency")
#Smoker
barplot(table(train_data$Smoker), col = c("yellow", "green"), beside = T, names.arg = c("No Smoker", "Smoker"), main = "Smoker", ylab = "Frequency")
#Stroke
barplot(table(train_data$Stroke), col = c("yellow", "green"), beside = T, names.arg = c("No Stroke", "Yes Stroke"), main = "Stroke", ylab = "Frequency")
# Diabetes
train_data$Diabetes <- as.factor(train_data$Diabetes)
barplot(table(train_data$Diabetes), col = c("yellow","orange", "green"), beside = T, names.arg = c("No Diabetes", "Pre-D", "diabetes"), main = "High Diabetes Distribution", ylab = "Frequency")
# HvyAlcoholConsump
barplot(table(train_data$HvyAlcoholConsump), col = c("yellow", "green"), beside = T, names.arg = c("No Heavy Alcohol Consump", "Yes Heavy Alcohol Consump"), main = "Heavy Alcohol Consumption", ylab = "Frequency")
# NoDocbcCost
## NoDocbcCost : Indicates if the person wanted to visit a doctor within the past 1 year but couldn’t, due to cost.
barplot(table(train_data$NoDocbcCost), col = c("yellow", "green"), beside = T, names.arg = c("No Doc bc Cost", "Visit Doc"), main = "Doctor visit in 1 year Distribution", ylab = "Frequency")
# DiffWalk
barplot(table(train_data$DiffWalk), col = c("yellow", "green"), beside = T, names.arg = c("Not DiffWalk", "Yes DiffWalk"), main = "Difficult to walk", ylab = "Frequency")
# GenHlth

# Sex
barplot(table(train_data$Sex), col = c("salmon", "skyblue"), beside = T, names.arg = c("Male", "Female"), main = "Sex", ylab = "Frequency")

#general health 
barplot(table(train_data$GenHlth), col = "lightpink", beside = T, names.arg = c("Excellent", "Good", "Moderate", "Below Moderate", "Poor"), main = "General Health", ylab = "Frequency")

# Age
barplot(table(train_data$Age), col = c("lightpink"), beside = T, names.arg = c("18years to 80 years or older"), main = "Age", ylab = "Frequency")
# Income
barplot(table(train_data$Income), col = c("lightpink"), beside = T,names.arg = c("$10,000 to $$75,000 or higher"), main = "Income", ylab = "Frequency")



# 1 ($10,000 to less than $20,000) 
# 2 ($20,000 to less than $25,000) 
# 3 ($25,000 to less than $35,000) 
# 4 ($35,000 to less than $50,000)
# 5 ($50,000 to less than $75,000) 
# 6  $75,000+ 
# Age : Indicates the age class of the person, where 1 is 18 years to 24 years up till 13 which is 80 years or older, each interval between has a 5-year increment.

```

```{r}

#age of people with heart attacks vs age of people without heart attacks
cols = names(df) %in% c('points')
data0 = subset(data, HeartDiseaseorAttack<1 )

data1 = subset(data, HeartDiseaseorAttack >= 1 )

boxplot(data1$Age, data0$Age, col ="yellow", main = "Age groups with heart attacks vs without heart attacks")



#come groups with heart attacks vs without heart attacks"

#INCOME of people with heart attacks vs INCOME of people without heart attacks

boxplot(data1$Income, data0$Income, col ="lightpink", main = "Income with heart attacks vs without heart attack")

boxplot(data1$GenHlth, data0$GenHlth, col ="lightblue", main = "General health with heart attact vs without heart attack")



#GEN Health of people with heart attacks vs GEN Health of people without heart attacks
```

```{r}
cor_matrix = cor(data[,c("Education", "Income")]);cor_matrix
cor_matrix = cor(data[,c("GenHlth", "MentHlth", "PhysHlth")]);cor_matrix

```

