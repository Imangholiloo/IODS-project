#Mohammad Imangholiloo
#20201105
#Exercise 2: Linear regression analysis

# *Part 1) data wrangling*
# read the data to R
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
"metadata (description of data) is here (in Finnish): https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS2-meta.txt"
#alternatively, read it from your local drive
#lrn14 <- read.table("C:/Users/Mohammad/Documents/IODS-project/data/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

#alternatively, read it from your clipboard
#lrn14 <- readClipboard()

#FYI: to keep characters as characters in R: use ___stringsAsFactors=FALSE___ also in the read file command


# check dimensions of the data
dim(lrn14)

# check structure of the data
str(lrn14)

#Write short code comments describing the output of these explorations. (1 point)


#3///////////////////////
#selecting columns

#install.packages('dplyr')
library(dplyr)
library(rmarkdown)
# Name of group of columns (in List) realted to questions under the umbrella of 'deep', 'surface' and 'strategic learning'
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30", "D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface_questions and create column 'surface' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surface <- rowMeans(surface_columns)

# select the columns related to strategic_questions and create column 'strategic' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$strategic <- rowMeans(strategic_columns)


# select only specific needed columns, made recently
keep_columns <- c("gender","Age","Attitude", "deep", "strategic", "surface", "Points")
needed_columns_2014 <- select(lrn14, one_of(keep_columns))

# check structure of the new df
str(needed_columns_2014)


# Modfiy columns names, let's make all names smallcase (not capital)
colnames(needed_columns_2014)[2] <- "age"
colnames(needed_columns_2014)[3] <- "attitude"
colnames(needed_columns_2014)[7] <- "points"

#check if worked fine
str(needed_columns_2014)

#excluding rows (observations) with point in exam >0
needed_columns_2014 <- filter(needed_columns_2014, points > 0) #alternatively can be points *** != 0 ***

#let's check descriptive staticial summary of columns, for FYI
summary(needed_columns_2014)
View(needed_columns_2014) #to view the table in R

#4///////////////////////////////////////////
#set working dir
setwd("C:/Users/Mohammad/Documents/IODS-project/data/")

#Save the analysis dataset to the ‘data’ folder
write.csv(needed_columns_2014, "learning2014.csv") 


#for the sake of being sure, read the data again!
learning2014_read_again <- read.csv("learning2014.csv")
head(learning2014_read_again)
str(learning2014_read_again)
tail(learning2014_read_again)
summary(learning2014_read_again)



# *Part 2) Data analysis*
#read the data
learning2014 <- read.csv("learning2014_MI.csv")
"Metadata can be found in https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-meta.txt"
"column attitue shall be divided to 10 to be ideantical with given column by teacher"
#alternatively by:
#learning2014 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt", header=TRUE, sep = ",")

#---Description about data----
#this dateset is gather by Kimmo Vehkalahti in Introduction to Social Statistics, fall 2014,
#funded by Teachers' Academy funding for him (2013-2015)

str(learning2014)
View(learning2014)
dim(learning2014) #this dataset contains 166 row and 8 columns
colnames(learning2014) #column names are printed in consule
summary(learning2014) #a short view about descriptive staticial feastures of the columns, e.g. mean, min, max, etc

table(learning2014$gender) # to show the number of participants by gender"
table(learning2014$age) # to show the number of participants by age"
#2/////////////////
#graphical shows interactions (i.e. correlations and distrbutions) of columns (e.g. variables) of this dataset togehter
plot(learning2014[c(-1,-2)]) #[c(-1,-2)] >>> is to remove indexcolumn and gender (strings) elements of df, source: https://statisticsglobe.com/remove-element-from-list-in-r

hist(learning2014$age, col='grey') # to check histogram of a column (e.g. age)

#************ Extra ***************
# A loop to automaticlly plot histogram of all variables
# NB: Check that variables are all numberical, otherwise in gender (male/female, this will stop)
colnames(learning2014[-2]) #[-2] is to remove an element of list, source: https://statisticsglobe.com/remove-element-from-list-in-r
for (i in colnames(learning2014[-2])){
  print(i)
  hist(learning2014[[i]], col='grey')
}


## Visualize with ggplot
#install and load it
#install.packages("ggplot2")
library(ggplot2)

# setup plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = attitude, y = points, col = gender))

# select visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title and draw the plot
p4 <- p3 + ggtitle("Student's attitude versus exam points")
p4
"Interpretation: As graph shows, there is a positive strong correlation between student's attitude and their exam points
the correlation was more stronger with Male student as the slop is higher than female. So, attitude of male student indulents the exame point more than attitue of females"


# Still interested on more visualization?
# then read this section ... :)
#draw a scatter plot matrix
pairs(learning2014[c(-1:-2)])# ___c(-1:-2)___, is to remove those column
pairs(learning2014[c(-1:-2)], col= c("black", "red"))#learning2014$gender)
"F: black, M: red"
# wishing to plot even more advanced?
#If yes, use GGally and ggplot2 libraries to create even more advanced plot matrix with ggpairs()
library(GGally)
library(ggplot2)
p <- ggpairs(learning2014[c(-1:-2)], mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p
## hope you got facinated and liked it like me :)
'The graph shows a (strong) negative correlation (r2= -324) between surface and deep learning2014'


#3/////////////////
#now that we got familiar with variables and their distribution and correction, it's time to fit a regression model
### ---- simple linear regression model -----
# Let's create a scatter plot of points versus attitude variables
qplot(attitude, points, data = learning2014) + geom_smooth(method = "lm")

# let's make a simple linear regression model between exam points and attitude variables
my_model <- lm(points ~ attitude, data = learning2014)

# print out a summary of the model
summary(my_model)
"this model shows that attitude has statistically significant relationship with the target variable, 
and can be used to predict the exam points, becuase Pr(>|t|) values are way below 0.05, 
thus, our model is statistically significant in the significance level of 95%,
in fact, this case is even valid in the significance level of >99%, as the values are <0.01"
"Our model will be in fact this:
points = 3.5255*attitude+11.6372, as linear model is y=ax+b, where a is coeffient of the variable x, and b is the intercept, 
both can be derive from model summary"
# How about multilinear regression model (with multiple variables)
# Let's select variables f interest and plo them: 
ggpairs(learning2014, lower = list(combo = wrap("facethist", bins = 20)))

# create a regression model with multiple explanatory variables
my_model2 <- lm(points ~ attitude + stra + surf, data = learning2014)

# print out a summary of the model
summary(my_model2)

"In this model now, we tried to predict the exam point using attitude, stra, and surf variables,
but as the results show. only attitude has statistically significant relation with the exam model,
thus, we shall change the other variables"

#4 /////////////////
"It seems that there is not significant relationship between exam point and stratigic learnign and surface. So we shall rmeove them"


# lets try another model
my_model3 <- lm(points ~ attitude + stra+ deep +age, data = learning2014)
summary(my_model3)
"as model summary shows, still other variables such as age, deep, stra are not significant, but stra is very close to significance level of 95%, 
as it's Pr(>|t|) value is just a bit higher than 0.05 (so, very close"

#5 /////////////////
# create a regression model with multiple explanatory variables
my_model4 <- lm(points ~ attitude + stra, data = learning2014)
summary(my_model4)
"Same conclusion as above, stra is not statistically significant (within significance level of 95%), 
so we could either remove it from equation (preffered), or keep it for the purpose of this practical, to have multi-variable regression"
# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5
par(mfrow = c(2,2)) #defines that plots go to 2*2 frame (joining plots together)
plot(my_model4, which = c(1,2,5))
"In linear regression, we assume:
1. linear relationship between variables and target variable
2. error are distributed normally
3. Errors do not correlated
4. errors have constant variance
5. the size of given error does not depend of the explanatory variables"
"So, we can still check them as following: "
"To explore the normality assumption, QQ-plot can help us. As it shows, there is a reasonable fit between the dots and the line. Thus, it was normally distrbuted"
"To analyse the assumption of constant variance of errors (#4 above), we can check residuals vs fitted plot. It shows a reasonable random spead/distribution of dots around the line. So, this assumtion was also valid. 
In other words, Residual vs Fitted plot confirm that the fitted model is appropriate becuase the variabilty of residual is not increasing with increase of fitted value"

"Residuals vs Leverage plot helps identify which observation have an unusual high impact, like outlier, 
It seems that there is no such outlier in this data"

#more info about interpreations: https://vimeo.com/199820384
######## Extra: 
"let's als check the residuales of the model using a boxplot
The closer to 0 the better the model. In other words, it shows the
symmetry and specifically the normality of the error terms in the regression model"

boxplot(my_model4$residuals,
        main = "Residuals of the model",
        xlab = "Residual values",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = F,
        notch = F)
