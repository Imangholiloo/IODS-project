'''
Mohammad Imangholiloo
2020.11.12
In this script we assess Student Performance Data Set from 
URL: https://archive.ics.uci.edu/ml/datasets/Student+Performance
'''

"""
Data description:
This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). 
more info: https://archive.ics.uci.edu/ml/datasets/Student+Performance
"""

## Data wrangling %%%%%%%%%%%%%%%%%%%

# setting working directory
setwd("C:/Users/Mohammad/Documents/IODS-project/data")

# read math class questionaire data into memory
student_mat=read.csv("student-mat.csv",sep=";",header=TRUE)

# Read portuguese class questionaire data into memory
student_por=read.csv("student-por.csv",sep=";",header=TRUE)

# checkdimensions of both data
dim(student_mat)
dim(student_por)
# check structure of both data
str(student_mat)
str(student_por)

# check first 5 rows of both data
head(student_mat)
head(student_por)

# check last 5 rows of both data
tail(student_mat)
tail(student_por)

#a short view about descriptive staticial feastures of the columns, e.g. mean, min, max, etc
summary(student_mat) 
summary(student_por) 

#column names are printed in consule
colnames(student_mat) 
colnames(student_por)

#to show the number of observations based on their school name
table(student_mat$school) 
table(student_por$school) 


# //////merge two datasets usig dplyr library
# load library
library(dplyr)

# make a list of common columns to be used as identifiers in both datasets when merging
join_by_columns <- c("school","sex","age","address","famsize","Pstatus","Medu",
                     "Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by inner join function of the dplyr library, using the selected identifiers
# Note: inner_join keeps only rows (observations) that arein both input datasets
students_math_por_joint <- inner_join(student_mat, student_por, 
                                      by = join_by_columns, suffix = c("_math", "_por"))

#check dimention of newly merged dataset
dim(students_math_por_joint) # 382 students

# see the new column names
colnames(students_math_por_joint)

# view the file in R
#View(students_math_por_joint)

# glimpse at the data, it is like str() but prints more data. In other words, like a transposed version of print()
glimpse(students_math_por_joint)

# check data structure
str(students_math_por_joint)




# combine the 'duplicated' answers in the joined data
# create a new data frame with only the joined columns
alc <- select(students_math_por_joint, one_of(join_by_columns))

# columns that were not used for joining the data
notjoined_columns <- colnames(student_mat)[!colnames(student_mat) %in% join_by_columns]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # print the column bane while looping to show the progress
  print(column_name)
  # select two columns from 'math_por' with the same original name
  two_columns <- select(students_math_por_joint, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)



#%%%%%%%%%%%%%%%%%%%%%%%%%%
# define a new column alc_use by combining weekday and weekend alcohol use (i.e. columns Dalc and Walc, respectively)
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# load library for plotting
library(ggplot2)

# Let's plot the alcohol use based on gender
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# create a new logical column named 'high_use'. This column will make a True/Fale column if consuption is >2
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()


##### EXTRA################
#### lets check it by table also
table(alc$high_use)
"as it shows 114 student were consumed alchol more than threshold (2) so were high use"

table(alc$high_use)/nrow(alc)*100
"It means that 29.84% of students were high use, while majority (70.15%) were low use"


table(alc$alc_use)/nrow(alc)*100
"as this shows, majoriy of them (36.65%) consumed just 1"
#Let's make a histogram and check the ditribution
hist(alc$alc_use)

#Let's make a boxplot to furhture see the ditribution
boxplot(alc$alc_use)




7
# alc is available

# access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(alc) 

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()








# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))





"Are you interested to create more plots, for example poxplot per groups?"
  
# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")


"So, as plots show, those who consumed alchol highly, their grade reduced especially for males"
# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

"As plots show, those who consumed alchol highly, their absense were higher especially in males"

dim(alc)

# Save files to your computer directory
write.csv(alc, "alc_joinet.csv") 
write.csv(alc, "students_math_por_joint.csv")



## Data analysis %%%%%%%%%%%%%%%%%%%
data_student_perf_alc = read.csv("alc_joinet.csv")

#Alternatively you can read from the URL:
data_student_perf_alc <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep=",", header=TRUE)

"""
Data description:
This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). We joint the two dataset and now are ready to go for analysis phase.
Thus, we will use this dataset to analysis the relationships between high/low alcohol consumption and other variables. Is there any relationship between students performance and alcohol? Logistic regression will be used.
For more info visit: https://archive.ics.uci.edu/ml/datasets/Student+Performance
"""

# Variables names are as following
colnames(data_student_perf_alc)






# 3 //////////////////////
#4 interesting variables for me are:
#Internet: students with high Internet access at home, will consume less alcohol 
#goout: students who go out with friend more, will consume more alcohol
#absences students who are often absent from class, consume more alcohol
#activities "students with higher extra-curricular activities will consume less alcohol" 


# 4 //////////////////////
variables <- c("internet", "goout", "absences", "activities", 
               "alc_use", "high_use")

dt_some_col =select(data_student_perf_alc, variables)
dim(dt_some_col)

#-------------
# draw a bar plot of each variable
gather(dt_some_col) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

#for (i in colnames(dt_some_col)){
#  print(i)
#  boxplot(dt_some_col$i)
  #to make a table of all variables and give proportion of each
  #table_variable = round(table(dt_some_col$i)/nrow(dt_some_col)*100, 2)
  #print the values in consule
  #print(table_variable)
  
  #print(table(dt_some_col$absences))
#  } 
#sprintf("ccccc: %f", table_variable)
table(dt_some_col$absences)/nrow(dt_some_col)*100

#hist(dt_some_col$absences)
str(dt_some_col)

#Crosstable
cross_table = xtabs(~alc_use + internet, data = dt_some_col)
cross_table


##### EXTRA 
# to get proportion of each, with 2 decimal rounded
round(prop.table(cross_table), 2)

#if distrubution is significantly differnet
chisq.test(cross_table)
"As p-value > 0.05, thus the distrubution of this variable (internet access at home) is not significantly differnet"
"Note: don't worry about the warning message, becuase the number of data is few."

" for nother variable ---------"
#Crosstable
cross_table = xtabs(~alc_use + goout, data = dt_some_col)
cross_table
# to get proportion of each, with 2 decimal rounded
round(prop.table(cross_table), 2)

#if distrubution is signoficantly differnet
chisq.test(cross_table)
"As p-value < 0.05, thus the distrubution of this variable (going out behaviour) is significantly differnet"
"Note: don't worry about the warning message, becuase the number of data is few."





" for nother variable ---------"
#Crosstable
cross_table = xtabs(~alc_use + absences, data = dt_some_col)
cross_table
# to get proportion of each, with 2 decimal rounded
round(prop.table(cross_table), 2)

#if distrubution is signoficantly differnet
chisq.test(cross_table)
"As p-value < 0.05, thus the distrubution of this variable (absence) is significantly differnet"
"Note: don't worry about the warning message, becuase the number of data is few."


" for nother variable ---------"
#Crosstable
cross_table = xtabs(~alc_use + activities, data = dt_some_col)
cross_table
# to get proportion of each, with 2 decimal rounded
round(prop.table(cross_table), 2)

#if distrubution is signoficantly differnet
chisq.test(cross_table)
"As p-value > 0.05, thus the distrubution of this variable (extra-curricular activities) is not significantly differnet"
"Note: don't worry about the warning message, becuase the number of data is few."


"------------ Variables -----------"
barplot(sort(table(dt_some_col$internet),decreasing=T))
barplot(sort(table(dt_some_col$goout),decreasing=T))
barplot(sort(table(dt_some_col$absences),decreasing=T))
barplot(sort(table(dt_some_col$activities),decreasing=T))

barplot(sort(table(dt_some_col$alc_use),decreasing=T))
barplot(sort(table(dt_some_col$high_use),decreasing=T))

boxplot(dt_some_col$goout)
boxplot(dt_some_col$absences)
boxplot(dt_some_col$alc_use)

plot(alc_use ~ goout, data=dt_some_col)
plot(alc_use ~ absences, data=dt_some_col)


# Alternatively, this code also does the same:
"table(dt_some_col$activities, dt_some_col$alc_use)"


# Create logistic regression model
# 5 //////////////////////
colnames(dt_some_col)

dt_some_col$internet <- factor(dt_some_col$internet)
dt_some_col$activities <- factor(dt_some_col$activities)


# find the model with glm()
regres_model <- glm(high_use ~ internet + goout + absences + activities, data = dt_some_col, family = "binomial")
head(dt_some_col)
# print out a summary of the model
summary(regres_model)
"#Based on model summary absence is statistically significant relationship with high alchol use, becuase the probability value Pr(>|z|) = 0.000284, thus the model is valid in >99% significance level.
# students with hight going out behaviour, consumed more alcohol. It is statistically proved becuase Pr(>|z|) = 1.34e-10, thus our hypothesis is accepted in >99% significance level.
# howevere, having Internet access seemed not to have relationship with thise who had the Internet at home. Becuase the Pr(>|z|) for this was >0.05, thus is not statistically significant (at least in 95%). Thus, our hypoethsis that we asumed those who have Internet at home wil not consume more alcohol, would be rejected.
# it is the same as our conclusion of chisq.test explained above!
#Noteably, the statistical relationship between having more extra-curriculum activities and alcohol consumption was not valid in 95% significance level, howevere in 90% becuase the P value is 0.074. So it shall be rejected and we can ignore this variable.
"
"
#Hence, in fact our model would be
#alcohol consumption = goingout*0.76811 + absence*0.06175 + (-3.47384), becuase y = ax1 + bx2 + c
"

# print out the coefficients of the model
coef(regres_model)


# compute odds ratios (OR)
OR <- coef(regres_model) %>% exp

# compute confidence intervals (CI)
CI <- confint(regres_model) %>% exp

# bind odds ratios (OR) and confidence intervals (CI) columns together
cbind(OR, CI)
"so as it shows, two variables of going out and absence has high odds ratio (same as propability in non-logistic regression models). And their confience interval is still good becuase it's very close interval, so the most of values in the variable are like that, thus with high OR value"

# 6 ///////////////////






# predict() the probability of high_use
probabilities <- predict(regres_model, type = "response")

# add the predicted probabilities to 'dt_some_col'
dt_some_col <- mutate(dt_some_col, probability = probabilities)

# use the probabilities to make a prediction of high_use
dt_some_col <- mutate(dt_some_col, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(dt_some_col, internet, goout, absences, activities, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = dt_some_col$high_use, prediction = dt_some_col$prediction)
"2x2 cross tabulation of predictions versus the actual values"
"as it shows, 247 cases of high_use alcohol consumptions were false and were predicted correctly as false, 71 of them were miscalssifed "


##### Extra
#Let's make a nice graph with confusion matrix
# insprited from https://stackoverflow.com/a/64539733
library('caret')
cm <- confusionMatrix(factor(dt_some_col$prediction), factor(dt_some_col$high_use), dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009193") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("False","True")) +
  scale_y_discrete(labels=c("True","False"))



# initialize a plot of 'high_use' versus 'probability' in 'dt_some_col'
g <- ggplot(dt_some_col, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = dt_some_col$high_use, prediction = dt_some_col$prediction) %>% prop.table %>% addmargins












# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = dt_some_col$high_use, prob = dt_some_col$probability)


##### K-fold cross-validation
#Perform leave-one-out cross-validation and print out the mean prediction error for the testing data
#It divids the observations into k number of folds, leaves-one-out for validation and uses the rest for training, for example, if k=5, uses 4 folds for training and 1 fold for validation
#load library
library(boot)
# cv.glm function from the 'boot' library computes the error and stores it in delta
cv <- cv.glm(data = dt_some_col, cost = loss_func, glmfit = regres_model, K = 2)

# average number of wrong predictions in the cross validation
cv$delta[1]

#### Extra ..........
"try different k values using a loop"
k = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25)
for (i in k){
  print(i)
  cv <- cv.glm(data = dt_some_col, cost = loss_func, glmfit = regres_model, K = i)
  # average number of wrong predictions in the cross validation
  print (cv$delta[1])
  
}
"So, the accuracy of model imprved by hypertuning uisng crossvalidation values in loop (above), becuase the, becuase the average number of wrong predictions (shown by cv$delta[1] in above) decreased a bit"








# Bounus 2
# let's try with different variables now
# I like to add two variables such as romantic (weather or not in a romatic relationship) and famrel (the quality of family relationship)
# I keep the previously significant variables
# 4 //////////////////////
colnames(data_student_perf_alc)
variables <- c("romantic", "goout", "absences", "famrel", 
               "alc_use", "high_use")
dt_some_col =select(data_student_perf_alc, variables)
dim(dt_some_col)

#-------------
# draw a bar plot of each variable
gather(dt_some_col) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

table(dt_some_col$absences)/nrow(dt_some_col)*100



barplot(sort(table(dt_some_col$romantic),decreasing=T))
barplot(sort(table(dt_some_col$goout),decreasing=T))
barplot(sort(table(dt_some_col$absences),decreasing=T))
barplot(sort(table(dt_some_col$famrel),decreasing=T))

barplot(sort(table(dt_some_col$alc_use),decreasing=T))
barplot(sort(table(dt_some_col$high_use),decreasing=T))

# find the model with glm()
regres_model <- glm(high_use ~ romantic + goout + absences + famrel, data = dt_some_col, family = "binomial")

# print out a summary of the model
summary(regres_model)
"#Based on model summary the quality of family relationship (famrel) has statistically significant relationship with high alchol use, becuase the probability value Pr(>|z|) = 0.002380, thus the model is valid in >99% significance level.
# students with hight going out behaviour, consumed more alcohol. It is statistically proved becuase Pr(>|z|) = 1.34e-10, thus our hypothesis is accepted in >99% significance level.
# Thus, this model would in fact be model be like this:
#alcohol consumption = goingout*0.79978 + absence*0.06001 + (-0.37335*romanticyes) * 0.37335  + (-2.15491), becuase y = ax1 + bx2 + cx3 + c
"
# print out the coefficients of the model
coef(regres_model)


# compute odds ratios (OR)
OR <- coef(regres_model) %>% exp

# compute confidence intervals (CI)
CI <- confint(regres_model) %>% exp

# bind odds ratios (OR) and confidence intervals (CI) columns together
cbind(OR, CI)




# predict() the probability of high_use
probabilities <- predict(regres_model, type = "response")

# add the predicted probabilities to 'dt_some_col'
dt_some_col <- mutate(dt_some_col, probability = probabilities)

# use the probabilities to make a prediction of high_use
dt_some_col <- mutate(dt_some_col, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(dt_some_col, romantic , goout , absences , famrel, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = dt_some_col$high_use, prediction = dt_some_col$prediction)
"2x2 cross tabulation of predictions versus the actual values"
"as it shows, 251 cases of high_use alcohol consumptions were false and were predicted correctly as false, 66 of them were miscalssifed "
"so the accuracy improved" 
"Moreover, the numbers in diagonal (251 and 46) were increased compared to above model, which shoes the increase of the corrected predicted values, and hence, increase in model accuracy"


# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = dt_some_col$high_use, prob = dt_some_col$probability)


##### K-fold cross-validation
#Perform leave-one-out cross-validation and print out the mean prediction error for the testing data
#It divids the observations into k number of folds, leaves-one-out for validation and uses the rest for training, for example, if k=5, uses 4 folds for training and 1 fold for validation
#load library
library(boot)
# cv.glm function from the 'boot' library computes the error and stores it in delta
cv <- cv.glm(data = dt_some_col, cost = loss_func, glmfit = regres_model, K = 2)

# average number of wrong predictions in the cross validation
cv$delta[1]

#### Extra ..........
"try different k values using a loop"
k = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25)
for (i in k){
  print(i)
  cv <- cv.glm(data = dt_some_col, cost = loss_func, glmfit = regres_model, K = i)
  # average number of wrong predictions in the cross validation
  print (cv$delta[1])
  
}
#as you can see, it did not improve significantly
#So we can see that chaning k value could not help so much

cv$delta[1]
"So, the accuracy of crossvalidation improved by adding the new variable (quality of family relation), becuas ethe average number of wrong predictions (shown by cv$delta[1] in above) decreased"
