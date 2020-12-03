# Exercise 6: Analysis of longitudinal data
# Mohammad Imangholiloo
# 2020.12.03
#Note that some of comments were given inside quotation marks (""), because I use black background in R, and this way they look green color. 
"Longitudinal data are those that multiple observations or measurements of the same individuals exist, thus, observation of the data could be correlated as well. So far, we learned about correlated variables, but it could happen that observations itself correlate (because was measured of same individual at different times, or under different treatments. For example, this could happen if an observation is measured twice or multiple times (repeatedly). It is called longitudinal data.
In this practical, we will analyse and understand handling of this effect. Converting the data between the wide form and the long form is important to learn. Because, typically, these types of data appear in the wide form, but most of the analyses require the data to be in the long form. 
Note that main objective in the analysis of longitudinal data is to detect the change in the repeated values of the response variable and to determine the explanatory variables most associated with any change.

So, stay tuned!
We will prepare and analyse two data sets, BPRS and RATS."



"Task 1. Read and understand the data"
"Description about data
BPRS data have 40 male subjects which were randomly assigned to one of two treatment groups and each subject was rated on the brief psychiatric rating scale (BPRS) measured before treatment began (week 0) and then at weekly intervals for eight weeks. The BPRS assesses the level of 18 symptom constructs such as hostility, suspiciousness, hallucinations, and grandiosity; each of these is rated from one (not present) to seven (extremely severe). The scale is used to evaluate patients suspected of having schizophrenia."
# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

# Look at the (column) names of BPRS
names(BPRS)

# Look at the structure of BPRS
str(BPRS)

#check dimention of the data
dim(BPRSL)

# print out summaries of the variables
summary(BPRS)

# see first 10 rows of data
head(BPRS)

"We can see that there are some treatments and subjects which were seemed to be measured in different weeks (week 1-8). It was labelled repeated measurements; which is, nowadays, common to measure the response variable under a number of different experimental conditions or on a number of different occasions over time; such data are labelled repeated measures or longitudinal data. For example, person 1 were under treatment 1 and subject 1 and outputted different brief psychiatric rating scale (BPRS) values in weeks 0 to 8"

# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)

#Task 2
# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
" now we set the treatment and subjects columns as factor, so computer handles them as non-continus variables"

#Task 3: Convert the data sets to long form. Add a week variable to BPRS and a Time variable to RATS. 
# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
dim(BPRSL)
dim(BPRS)
" As you can see, we used Gather function to Gather columns into key-value pairs, so the three columns called Weeks and it contains now week numbers and bprs value is the value of measurement. So, now we 
converted the data to long form"

"Our weeks are in a bit inconvenient form as characters, so we somehow need to extract the week numbers from the character vector weeks."
# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))
dim(BPRSL)
"so now we made a new integer column of weeks number"

# Take a glimpse at the BPRSL data
glimpse(BPRSL)
" you can see that we have 360 rows and 5 columns, which is different than wide form of the data. During transformation of data from wider to long form, we re-structured the weeks to be under one column and brps values to be same. Added a new column called 'week' which is the integer number of weeks, because the column 'weeks' contains the week0 etc, which is character, which could be hard in handing numerical data analysis in next phase."

"Before wrangling of RATS dataset (which is in below) let's plot some graphical of this data"

"Now Let’s make nice descriptive graphical plots"
"To begin we should plot the BPRS values for all 40 men, differentiating between the treatment groups into which the men have been randomized. This simple graph makes a number of features of the data readily apparent."
#Load the library to R
library(ggplot2)

# Draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

"each subject (shown by lines) are each individual (men) who participated in the experiment so there were 20 men per treatment subjects thus 20 lines in each graphs
we can see how the subjects (per person) and bprs values changed (generally declined) during weeks for persons, for each treatmets."

#let's write the wrangled data to files in drive
write.csv(BPRSL, "BPRSL.csv")


###### Data analysis 
"Task 1: Implement the analyses of Chapter 8 of MABS using the RATS data. "

#As mentioned in the textbook of the course: 
#Graphical Displays of Longitudinal Data is to:
"• Show as much of the relevant raw data as possible rather than only data
summaries
• Highlight aggregate patterns of potential scientific interest
• Identify both cross-sectional and longitudinal patterns
• Try to make the identification of unusual individuals or unusual observations
simple."

# Standardise the variable bprs
"Standardising the values of a phenomenon can lead to a better and clearer tracking and viewing the data. We can do it by subtracting the relevant occasion mean from the original observation and then dividing by the corresponding visit standard deviation."
BPRSL <- BPRSL %>%
  group_by(week) %>%
  mutate(stdbprs = (bprs - mean(bprs))/sd(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")


"This graph shows that there are considerable individual differences and variability which are appeared to
decrease with time."


"apply scatterplot matrix.
applyt box plot"


#---------------
"Good things come in Summary graphs"
# Number of weeks, baseline (week 0) included
n <- BPRSL$week %>% unique() %>% length()
"so there were 9 weeks, i.e. the duration of treatment"

# Summary data with mean and standard error of bprs by treatment and week 
BPRSS <- BPRSL %>%
  group_by(treatment, week) %>%
  summarise( mean = mean(bprs), se = sd(bprs)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSS)

# Plot the mean profiles
ggplot(BPRSS, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")

"Now, you can see the mean BPRS value with standard deviation (shown in vertical bars for each week"
"We can see that treatment 1 and 2 were started with about 47 and 49 bprs, respectively and declined as time went. The decrease in bprs value of treatment 1 was more than in treatment 2, because treatment 2 did not gave lower value than about 36, while treatment 1 levelled off to below 30. "

#-----------------------
"Find the outlaw... Outlier!"
# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
BPRSL8S <- BPRSL %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL8S)

# Draw a boxplot of the mean versus treatment
ggplot(BPRSL8S, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")
"In this graph, the mean of weeks 1 to 8 per each treatment (1 and 2) was measured. We can see that mean of treatment 1 is higher than treatment 2"
" From the boxplots, we can see that in treatment 2, there is an outlier which has value over 70"
"so let's filter it out"




"------------Another visualization for summary of this dataset is boxplot, so let's try it--------------"



# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
BPRSL8S1 <- BPRSL8S %>%
  filter(mean < 60)
"Here, we used 'filter' function which subsets rows using column values, in this case mean < 60, so keeps any value below 60"

#-------------------------
#### Doing a t-test to check group differences
"Although the graph showed all indicated a lack of difference in the two treatment groups, we better check with statistical tests like t-test if the difference exist between treatments."
"we will use the data without outlier. So, lets proceed"
# Perform a two-sample t-test
t.test(mean ~ treatment, data = BPRSL8S1, var.equal = TRUE)

"Interpretation: "
"As t-value is 0.52095 (t = 0.52095), our null hypothesis (there is difference between treatments) shall be rejected. So, it confirms the lack of any evidence for a group difference withi confidence interval of above 95%, because t-value is not smaller than 0.05"


"Now, let's add a baseline to our calculations, because as book says baseline measurements of the outcome variable in a longitudinal study are often correlated with the chosen summary measure and using such measures in the analysis can often lead to substantial gains in precision when used appropriately as a covariate in an analysis of covariance"
# Add the baseline from the original data as a new variable to the summary data
BPRSL8S2 <- BPRSL8S %>%
  mutate(baseline = BPRS$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = BPRSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

"The ANOVA test shows that the baseline is statistically significant with BPRS values taken after treatment has begun, but treatments are not statistically significant (Pr = 0.8148 which is higher than 0.05 confidence interval) difference in BPRS values even after conditioning on the baseline value. Thus, same conclusion as t-test (explained above)"



" ************* let's do the same for RATS data ********************* "
#------------------------------
"Meet and Repeat: PART II"
# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

"Data description:
This dataset belong to a nutrition study carried out using three groups of rats. The groups were put on different diets, and each animal’s body weight (grams) was recorded repeatedly (approximately) weekly, except in week seven when two recordings were taken) over a 9-week period. The question of most interest is whether the growth profiles of the three groups differ.
so, let's study the question."

library(GGally)
ggpairs(RATS, lower = list(combo = wrap("facethist", bins = 20)))
" We can see that the varables are all highly (above 0.9) correlated to each other, so we need to wonder about it. Stay tuned, we will tell in a bit"

# Factor variables ID and Group
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Glimpse the data
glimpse(RATS)


#-------------------
"Linear Mixed Effects Models: Gather 'round"
# Convert data to long form
" Converting the long form is explained in above for BPRS data, so I don't repeat the explanation now"
RATSL <- RATS %>% 
  gather(key = WD, value = Weight, -ID, -Group) %>% ###Convert data to long form
  mutate(Time = as.integer(substr(WD,3,4))) ### Extract the week number

dim(RATSL)
dim(RATS)
" As you can see, we used Gather function to Gather columns into key-value pairs, so the three columns called Time and it contains now week numbers and rats value is the value of measurement. So, now we 
converted the data to long form"
"so now we made a new integer column of weeks number"



# Glimpse the data
glimpse(RATSL)
" you can see that we have 176 rows and 5 columns."

#------------------
"Plot first, ask questions later"
# Check the dimensions of the data
dim(RATSL)


# Let's draw a plot
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  theme(legend.position = "top")
"As plot shows, the weights of rats generally increased over time of experiments. The weight of group 1 rats were distinguishable lower than group 2 and 3. the increase in weight of group 2 and 3 rats were seems to be sharper than group 1 rats"
"Additionally, it seems that the repeated measures are dependent of one another"

#let's write the wrangled data to files in drive
write.csv("RATSL, RATSL.csv")

#--------------------
"Holding on to independence: The Linear model"
"Let's fit a multiple linear regression model with ignoring repeated-measures structure of the data"
# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, data = RATSL)
"we fitted a multiple linear regression model with weight as response and Time and Group as explanatory variables"
# print out a summary of the model
summary(RATS_reg)
"Interpretation"
"Becuase the P-values of all variables (time, group 2 and 3) were very much lower than 0.05, thus they are definately different from each other. Thus, they are all highly significantly different"
"Also there is high variation in the intercepts of the regression fits of the individual rat growth profiles" 
"**Notable**, because we ignored the probable within-subject dependences, 
the standard error of a within-subject covariate such as time being larger. Thus, now we should create Random Intercept Model to consider it"
#-----------------
"The Random Intercept Model"
# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)
"Note the random-effects terms distinguished by vertical bars (|)"
# Print the summary of the model
summary(RATS_ref)
"As we can see from the summary, the estimated standard error of time (0.03158) is much smaller now than previously with regression model (0.1331), which reflects the effects of considering the probable within-subject dependence applied in this Random Intercept Model"
"standard errors of each variables (Group2 and Group3) are now at least three time bigger than the previous regression model, same for intercept of the model.
More importantly, we can see that the estimated mean weight of rats (244.06890) is larger than weight of rats in Group2 (220.98864, t = 10.92) but smaller than estimated weight of rats in Group3 (262.07955, t = 12.95). Using a rule-of-thumb of 2 for t value (introduced in this link: https://web.stanford.edu/class/psych252/section/Mixed_models_tutorial.html), we can say that t is probability significant."

### Extra coding 1: Extracting all the coefficients
"Insprited from https://web.stanford.edu/class/psych252/section/Mixed_models_tutorial.html"
d_bysubj = na.omit(RATSL) %>%
  group_by(Group) %>%
  summarise(mean_weight_of_Rats = mean(Weight))
ggplot(d_bysubj, aes(x=factor(Group), y=mean_weight_of_Rats)) +
  geom_point(size=4, aes(colour = factor(Group)))
"we can see that mean weight of rats in group 1 is very lower than group 2 and 3 (maximum)"

#---------------------
## Random Intercept and Random Slope Model
"Now, let us proceed to fit the random intercept and random slope model to the rat growth data. Fitting a random intercept and random slope model allows the linear regression fits for each individual to differ in intercept but also in slope. This way it is possible to consider the individual differences in the rats' growth profiles, but also the effect of time."

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)
"The random intercept and slope model provide a better fit for these data. It shows that the growth rate slopes are considerably higher for rats in group 3 (estimated 258.92732) than for rats in group 2 (214.58736) with same standard error, but higher t-value for group 3. The bottom of results shows that group 2 and 3 correlate with 0.333, but intercept has higher correlation with group 2 and 3 (both -0.569)."
# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)
"The ANOVA test of the two models show that the latter model 'random intercept and random slope model' is significant because P value is much less than 0.05. It's significant in 99% significant interval "


#------------------
## Random Intercept and Random Slope Model with interaction
"Let's test differnet variables for the random intercept and random slope model with interaction"
# create a random intercept and random slope model
RATS_ref2 <- lmer(Weight ~ Time * Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref2)

# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)
"now, we can see that the latter model in which we multiple time and groups is more significant (in 99% significance interval because P value is smaller than 0.01, compare to previous model where we did not make the multiplication "


# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)

# Create a new column fitted to RATSL
RATSL <- RATSL %>%
  mutate(Fitted)


# draw the plot of original RATSL
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")


# draw the plot of model fitted RATSL
ggplot(RATSL, aes(x = Time, y = Fitted, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")
"We can see that the value of fitted weight, dervided from the last random intercept and random slope model look veey simlar to observations graph in earlier, thus it's a successful and nice model. Good job! "
"Thus, this graph shows that the interaction model fits well with the observed data (plotted earlier)"


# Extra coding 2: creating the standardized residual of predicted mean weights of each rat during the experiment
# creating and plot the residuals  of model
resid <- residuals(RATS_ref2)
qqnorm(resid)

resid.std <- resid/sd(resid)
plot(RATSL$ID, resid.std, ylim=c(-3, 3), ylab="Residuals of predicted weight of each rat")
abline(h=0, col= "blue")
"It seems that the model predicted the weights well, because if the predicted mean weight, shown by middle line of each box, of each rat is closer to abline (horizontal line in middle of graph) the better. The blue line is to show the place of ideal case to have residuals close to 0"


## Extra coding 3
"Since we have original value and model fitted values for BPRS, we can calculate root mean square error (RMSE) 
the statistical metric that measures how far apart our fitted values are from our original values in a regression analysis, on average."
"inspired from https://www.statology.org/how-to-calculate-rmse-in-r/"
head(RATSL, 2) # to print columns with 2 row to make it easy in here to write the formula
RMSE = sqrt(mean((RATSL$Weight - RATSL$Fitted)^2))
"the smaller the RMSE, the better a model is able to fit the data."
RMSE_percent = 100*RMSE/mean(RATSL$Weight)
"We can also calculate Mean squered error (MSE). It is another most common metrics used to measure the prediction accuracy of a model. It is also called bias."
"So, RMSE is 1.05%, which cofirms our model is predicting the weight of Rats very well. Good job!"
MSE = mean((RATSL$Weight - RATSL$Fitted)^2)
MSE
"The lower the value for MSE, the more accurately a model is able to predict values."

MSE_percent = 100*MSE/mean(RATSL$Weight)
MSE_percent 
"So, our model is 4.24% underestimating the actual weight of Rats weight"


## Extra coding 4: plotting measured and fitted values together with abline
"Now, we can plot the model fitted mean weight (in X axis) vs original observed value (in Y axis)"
plot(RATSL$Fitted, RATSL$Weight, main = "Measured and fitted mean weight of rats", 
     xlab = "Model fitted weight of rats", ylab = "Measured weight of rats")
abline(a = 0, b= 1, col = "blue")
"The blue line in graph shows the 1:1 diagonal line, for example if x = 500, y = 500. So, the ideal case will be like that. So, we compare now our predicted and original value using that line, because if the measured weight of rat is 600 grams, then ideally the predicted weight should be 600 grams too. So, this line shows that. As graph shows, the predicted weights were very closed to original values, which was also shown by the RMSE and Bias numbers. For example, if the weight of rat was 500 grams, it was models very closely to that number (with MSE of 16.32%)"

"Final notes"
"Independence was a most important assumption in the mixed models that we used here to resolve the non-independencies in our data. Otherwise, linear regression models could also do."