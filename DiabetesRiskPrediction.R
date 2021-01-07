# Introduction

#This project was done for a course: HarvardX: PH125.9x Data Science: Capstone in HarvardX Professional Certificate Program in Data Science. The project can be found at: https://github.com/MagdalenaStefanowicz/DiabetesRiskPrediction
#World Health Organization (WHO) estimates that diabetes was the seventh leading cause of death in 2016. The number of people with diabetes rose from 108 million in 1980 to 422 million in 2014 [1]. Being able to predict diabetes among patients could help many to avoid severe consequences of the disease as well as help in diagnosing it in good time. 
#This project aims to predict a risk of diabetes based on early stage factors among patients. The prediction is based on various diagnostic measurements included in the dataset: Early stage diabetes risk prediction dataset [2]. The dataset contains symptoms data of newly diabetic or would be diabetic patients. The data was collected with direct questionnaires from the patients of Sylhet Diabetes Hospital in Sylhet, Bangladesh. Dataset size: 520 observations (patients) and 17 variables.
#The following key steps are performed in order to achieve classification of diabetes diagnosis: 1. dataset is analyzed in order to find the appropriate machine learning model 2. train set and test set are created 3. diabetes diagnosis are predicted using models with logisting regression and decision tree 4. confusion matrix is used to evaluate the approaches
#Linear regression model can achieve accuracy of 95%. 

# Analysis
# Read data

#The following packages and the dataset are downloaded and installed.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(readxl)
library(rpart)
library(knitr)
library(corrplot)

set.seed(1234)

url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/00529/diabetes_data_upload.csv"
dest_file <- "data/diabetes_data_upload.csv"
download.file(url, destfile = dest_file)

diabetes <- read_csv("data/diabetes_data_upload.csv")

#Let's analyze the available data: variables and their classes. 
#The variable "class" represents diabetes diagnosis with result values: Negative or Positive. 

diabetes %>% as_tibble() %>% head()

#Diabetes dataset has 520 observations and 17 variables.

dim(diabetes)

#There are no NA's values in the dataset. No cleaning is needed for further analyzis.  
sum(is.na(diabetes))

# Exploratory data analyzis

#First, Let's start by checking distribution of positive diabetes diagnosis among the patients.
diabetes %>% 
  group_by(class) %>%
  summarize(count = n()) %>%
  table()

#Now we're going to analyze affect of each available parameters on the diabetes diagnosis - two by two. Starting from Age and Gender. 
age_plot <- diabetes %>%
  ggplot(aes(x = class, y = Age)) +
  geom_boxplot(aes(fill = class), show.legend = FALSE)+
  ylab("Age")+
  xlab("Diabetes Diagnosis")+
  ggtitle("Diabetes vs Age")

gender_plot <- diabetes %>%
  group_by(Gender) %>%
  ggplot(aes(x = Gender)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Gender")+
  ggtitle("Diabetes vs Gender")

library("gridExtra")
grid.arrange(age_plot, gender_plot,
             ncol= 2, nrow = 1)

#The boxplot for Age show little variation with diabetes. No clear correlation among the patients. 
#The barplot for Gender shows that there are far more male participants then female. Female in the dataset are also far more likely to have diabetes. 

#Let's check how Polyuria and Polydipsia affect diabetes. 
#Polyuria is defined as a urine output exceeding 3 L/day in adults and 2 L/m2 in children [3].
#Polydipsia is defined as excessive thirst or excess drinking [4]. 

Polyuria_plot <- diabetes %>%
  group_by(Polyuria) %>%
  ggplot(aes(x = Polyuria)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Polyuria")+
  ggtitle("Diabetes vs Polyuria")

Polydipsia_plot <- diabetes %>%
  group_by(Polydipsia) %>%
  ggplot(aes(x = Polydipsia)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Polydipsia")+
  ggtitle("Diabetes vs Polydipsia")

library("gridExtra")
grid.arrange(Polyuria_plot, Polydipsia_plot,
ncol= 2, nrow = 1)

#Very clear corelation between Polyuria and Diabetes. 
#Almost all patients with Polydipsia does suffer from Diabetes. Very strong correlation as well.

#Let's check how `sudden weight loss` and weakness affect diabetes.

sudden_weight_loss_plot <- diabetes %>%
  group_by(`sudden weight loss`) %>%
  ggplot(aes(x = `sudden weight loss`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("sudden weight loss")+
  ggtitle("Diabetes vs sudden weight loss")

weakness_plot <- diabetes %>%
  group_by(weakness) %>%
  ggplot(aes(x = weakness)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("weakness")+
  ggtitle("Diabetes vs weakness")

library("gridExtra")
grid.arrange(sudden_weight_loss_plot, weakness_plot,
             ncol= 2, nrow = 1)

#Patients with sudden weight loss do suffer from Diabetes more often. Clear correlation.
#No strong correlation between weakness and Diabetes. 

#Let's check how Polyphagia and ‘Genital thrush‘  affect diabetes.
#Polyphagia is the medical term for excessive or extreme hunger [5].
#‘Genital thrush‘is a common vagina/penis condition caused by a type of yeast called Candida [6].

Polyphagia_plot <- diabetes %>%
  group_by(Polyphagia) %>%
  ggplot(aes(x = Polyphagia)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Polyphagia")+
  ggtitle("Diabetes vs Polyphagia")

Genital_thrush_plot <- diabetes %>%
  group_by(`Genital thrush`) %>%
  ggplot(aes(x = `Genital thrush`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Genital thrush")+
  ggtitle("Diabetes vs Genital thrush")

library("gridExtra")
grid.arrange(Polyphagia_plot, Genital_thrush_plot,
ncol= 2, nrow = 1)

#Most of the patients with Polyphagia does have Diabetes. Clear correlation. 
#No correlaton between Genital thrush and Diabetes. 

#Let's check how `visual blurring` and Itching affect diabetes.

visual_blurring_plot <- diabetes %>%
  group_by(`visual blurring`) %>%
  ggplot(aes(x = `visual blurring`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("visual blurring")+
  ggtitle("Diabetes vs visual blurring")

Itching_plot <- diabetes %>%
  group_by(Itching) %>%
  ggplot(aes(x = Itching)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Itching")+
  ggtitle("Diabetes vs Itching")

library("gridExtra")
grid.arrange(visual_blurring_plot, Itching_plot,
             ncol= 2, nrow = 1)

#Patients with visual blurring does suffer from Diabetes somewhat more often. 
#No correlation between Itching and Diabetes. 

#Let's check how Irritability and `delayed healing` affect diabetes.

Irritability_plot <- diabetes %>%
  group_by(Irritability) %>%
  ggplot(aes(x = Irritability)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Irritability")+
  ggtitle("Diabetes vs Irritability")

delayed_healing_plot <- diabetes %>%
  group_by(`delayed healing`) %>%
  ggplot(aes(x = `delayed healing`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("delayed healing")+
  ggtitle("Diabetes vs delayed healing")

library("gridExtra")
grid.arrange(Irritability_plot, delayed_healing_plot,
ncol= 2, nrow = 1)

#There seems to be a strong correlation between Irritability and Diabetes. No correlation between delayed healing and Diabetes. 

#Let's check how `partial paresis` and `muscle stiffness` affect diabetes.

partial_paresis_plot <- diabetes %>%
  group_by(`partial paresis`) %>%
  ggplot(aes(x = `partial paresis`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("partial paresis")+
  ggtitle("Diabetes vs partial paresis")

muscle_stiffness_plot <- diabetes %>%
  group_by(`muscle stiffness`) %>%
  ggplot(aes(x = `muscle stiffness`)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("muscle stiffness")+
  ggtitle("Diabetes vs muscle stiffness")

library("gridExtra")
grid.arrange(partial_paresis_plot, muscle_stiffness_plot,
             ncol= 2, nrow = 1)

#Partial paresis has strong correlation with Diabetes. No significant correlation between muscle stiffness and Diabetes. 

#Let's check how Alopecia and Obesity affect diabetes.
#Alopecia is an autoimmune disorder that causes your hair to come out [7].

Alopecia_plot <- diabetes %>%
  group_by(Alopecia) %>%
  ggplot(aes(x = Alopecia)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Alopecia")+
  ggtitle("Diabetes vs Alopecia")

Obesity_plot <- diabetes %>%
  group_by(Obesity) %>%
  ggplot(aes(x = Obesity)) +
  geom_bar(aes(fill = class), position = "dodge")+
  ylab("count")+
  xlab("Obesity")+
  ggtitle("Diabetes vs Obesity")

library("gridExtra")
grid.arrange(Alopecia_plot, Obesity_plot,
ncol= 2, nrow = 1)

#No clear correlation between Alopecia and Diabetes. What is very surprising, there is also no significant correlation between obesity and Diabetes among the patients in the dataset. 

#From the analyzis above it can be concluded that Polyuria and Polydipsia have biggest impact on `class´ variable.

#Let's encode the parameters from categorical to numerical values. By doing so we'll be able to identify and plot the correlation between all parameters.

diabetes_num <- diabetes
diabetes_num <- diabetes_num %>% mutate(Gender = ifelse(Gender == "Male", 0, 1)) %>% mutate(Polyuria = ifelse(Polyuria == "No", 0, 1)) %>% mutate(Polydipsia = ifelse(Polydipsia == "No", 0, 1)) %>% mutate(`sudden weight loss` = ifelse(`sudden weight loss` == "No", 0, 1)) %>% mutate(weakness = ifelse(weakness == "No", 0, 1)) %>% mutate(Polyphagia = ifelse(Polyphagia == "No", 0, 1)) %>% mutate(`Genital thrush` = ifelse(`Genital thrush` == "No", 0, 1)) %>% mutate(`visual blurring`= ifelse(`visual blurring` == "No", 0, 1)) %>% mutate(Itching = ifelse(Itching == "No", 0, 1)) %>% mutate(Irritability = ifelse(Irritability == "No", 0, 1)) %>% mutate(`delayed healing` = ifelse(`delayed healing` == "No", 0, 1)) %>% mutate(`partial paresis` = ifelse(`partial paresis` == "No", 0, 1)) %>% mutate(`muscle stiffness` = ifelse(`muscle stiffness` == "No", 0, 1)) %>% mutate(Alopecia = ifelse(Alopecia == "No", 0, 1)) %>% mutate(Obesity = ifelse(Obesity == "No", 0, 1)) %>% mutate(class = ifelse(class == "Negative", 0, 1)) 

#Let's plot correlation between all the measurments. 

corrplot(cor(diabetes_num[, -9]), method = "circle")

# Method

#In this chapter we're going to fit two different machine learning models for the dataset: logistic regression and decision tree. 

# Model setup

#Let's start with dividing dataset into train and test sets. Test set will correspond to 20% of the dataset.  

set.seed(1234)

inTrain <- createDataPartition(y = diabetes_num$class, p = .2, list = FALSE)
training <- diabetes_num[-inTrain,]
testing <- diabetes_num[inTrain,]

nrow(training)
nrow(testing)

# Model 0: Random sampling

#Let's start with random sampling for which we assume equal probability for positive and negativ outcome of diabetes diagnosis. Probability would simply be 0.5. 

p <- 0.5


#Now we can make our first prediction using random sampling. As expected the accuracy is rather poor, only 45%. 

set.seed(1234)

rs_prediction <- 
  sample(c(1,0), length(testing$class), prob = c(1-p,p), replace = TRUE)
rs_prediction <- as.numeric(rs_prediction)
rs_cm <- confusionMatrix(table(rs_prediction, testing$class))
rs_cm


# Model 1: Logistic Regression

#Logistic regression model is used with consideration to all measurments. Logistic regression is considered an appropriate method for binary classification problems - problems with two class values. 

set.seed(1234)

lr_model <-glm(class ~ Age + Gender + Polyuria + Polydipsia + `sudden weight loss` + weakness + Polyphagia + `Genital thrush` + `visual blurring` + Itching + Irritability + `delayed healing` + `partial paresis` + `muscle stiffness` + Alopecia + Obesity, family=binomial(link='logit'),data=training)
summary(lr_model)

#Gender, Polyuria, Polydipsia, Itching and Irritability are most releveant measurments - based on their low p_values. 

#Next, Confusion Matrix is used in order to measure performance of Logistic Regression model. Confusion matrix is a summary of prediction results and an appropriate approach for classification problems. Confusion Matrix allows to study sensitivity, specificity and balanced accuracy. 

lr_prediction <- predict(lr_model,newdata=testing,type='response')
lr_prediction <- ifelse(lr_prediction > 0.5,1,0)
lr_prediction <- as.numeric(lr_prediction)
lr_cm <- confusionMatrix(table(lr_prediction, testing$class))
lr_cm

# Model 2: Decision Tree

#Decision tree is another model which can be built on categorical data. Decision tree predicts an outcome variable by partitioning the predictors.
#In the model below the complexity parameter (cp) has been adjusted in order to enlarge the tree. With relatively small sample size the lower cp does not effect the computing time but it can slightly improve the error. 

set.seed(1234)

dt_model <- rpart(class ~ Age + Gender + Polyuria + Polydipsia + `sudden weight loss` + weakness + Polyphagia + `Genital thrush` + `visual blurring` + Itching + Irritability + `delayed healing` + `partial paresis` + `muscle stiffness` + Alopecia + Obesity, data = training, control = rpart.control(cp = 0.005))

plot(dt_model, uniform=TRUE, 
  	main="Decision Tree - Diabetes")
text(dt_model, use.n=TRUE, all=TRUE, cex=.7)

#If a person has Polydipsia and Polyuria then she is most likely to have diabetes. 

#Let's use Confusion Matrix as to measure performance of our Decision Tree model. 

dt_prediction <- predict(dt_model, testing)
dt_prediction<- ifelse(dt_prediction > 0.5,1,0)
dt_prediction <- as.numeric(dt_prediction)
dt_cm <- confusionMatrix(table(dt_prediction, testing$class))
dt_cm


#Results

#This is the summary table of performances for the two studied models: logistic regression and decision tree. Performance for random aampling is added for comparison. 

results <- data.frame(Method = character(),
                      Accuracy = double(),
                      Sensitivity = double(),
                      Specificity = double())

results[1,] <- c("Random sampling",
                 format(round(rs_cm$overall["Accuracy"],2), nsmall = 2),
                 format(round(rs_cm$byClass["Sensitivity"],2), nsmall = 2),
                 format(round(rs_cm$byClass["Specificity"],2), nsmall = 2))

results[2,] <- c("Logistic regression",
                 format(round(lr_cm$overall["Accuracy"],2), nsmall = 2),
                 format(round(lr_cm$byClass["Sensitivity"],2), nsmall = 2),
                 format(round(lr_cm$byClass["Specificity"],2), nsmall = 2))

results[3,] <- c("Decision tree",
                 format(round(dt_cm$overall["Accuracy"],2), nsmall = 2),
                 format(round(dt_cm$byClass["Sensitivity"],2), nsmall = 2),
                 format(round(dt_cm$byClass["Specificity"],2), nsmall = 2))
results


# Conclusion

#Both decision tree model and logistic regression model can achieve very good accuracy for the dataset. Logistic regression achieves the best result with accuracy of 91% and sensitivity of 95%.
#The measurments importance for both models show that Polydipsia and Polyuria are particularly significant for diagnosing diabetes. 

#Following improvements for the models could be considered:
#Dataset is somehow imbalanced with diagnosis distribution of ca. 2:3. Implementing techniques which help dealing with imbalanced data could improve the performance even more.
#It is somehow doubtfull that there is no clear correlation between obesity and diabetes among patients in general. What might be true for this particular dataset might not be true (and most likely is not true) for datasets from other countries or other hospitals. Bigger dataset (sample size) would ensure higher credibility. 
