#Exercise 4: Clustering and classification
#Mohamamd Imangholiloo

#load the required library
library(MASS)

#load the data
data("Boston")

# Data description:
"This dataset is about housing values in suburbs of Boston.
More info can be found in: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html"

#explore the dataset
str(Boston)

#summary of the data
summary(Boston)
"the summary shows minimum, mean, median, maximun and first and thrid quantiles for each columns of the dataset"
#check dimention of the data
dim(Boston)
"as it shows, this dataset has 506 rows (observations) and 14 columns (variables)"
# See head (fist 5 rows) of the file
head(Boston)

# See tail (last 5 rows) of the file
tail(Boston)

#see column names
colnames(Boston)

# plot matrix of the variables
pairs(Boston)
" you can see pair-wise plots of each variable (column) with each other and how they were distributed"

library(ggplot2)
library(GGally)
p <- ggpairs(Boston, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p
"Still some more plots to that show correction of each column (variable) with others and the distribution"


#let's make also boxplot to visually see the distribution, min, max and quantiles and mean of each variable
boxplot(Boston,
        main = "Boxplot of all columns of Boston data",
        xlab = "variable name",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = F,
        notch = F)
#As the graph shows, the column "tax" has highest variation then black. The values in other columns have quite less variation!

#let's see the strcutre of data even better using glimpse
# read libraries
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(Boston) 

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(Boston) %>% glimpse


# draw a bar plot of each variable
gather(Boston) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

"shows barplot of each column"


#Let's investigate any possible correlctions within data 
# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(digits = 2)

# print the correlation matrix
cor_matrix

### exta coding: make publication ready interactive Tables in R
"inspired from http://www.htmlwidgets.org/showcase_datatables.html"
library('DT')
datatable(cor_matrix, options = list(pageLength = 10))

"Or alternatively set the options to be limiting by ht enumber of rows of the imput df"
datatable(cor_matrix, options = list(nrow(cor_matrix)))




# visualize the correlation matrix
library("corrplot")
corrplot(cor_matrix, method="circle", type="upper", 
         cl.pos="b", tl.pos="d", tl.cex = 0.7)

"The graph shows the correlation between variables, the larger the circle (and the darker the color), the higher the correlation is, and color shows if the correlation is positive or negative."
"for example, the correlation between nox and dis is strong and negative, ie the increase of one variable, decreases the other one.
Or another example is rad and tax, which have strong positive correlation"

"on the other hand, chas does not have good correlation with other variables"


"Task 4"
# Let's standardize the variables suing scale funcation
boston_scaled <- scale(Boston)
"As center = True (by defult), now, this scale is done now by 
cantering is done by subtracting the column means (omitting NAs) of x from their corresponding columns"


# summaries of the scaled variables
summary(boston_scaled)
"So, the variables were standardize as explained above. So, the mean of each column were converted to 0 and other values were scales based on that. So, mean of all columns is 0"

# class of the boston_scaled object
class(boston_scaled)
"as you can see, the data class is "matrix" "array", so we will change the object to dataframe"

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

summary(boston_scaled)

str(boston_scaled)
head(boston_scaled)



boxplot(boston_scaled,
        main = "Boxplot of all columns of scaled Boston data",
        xlab = "variable name",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = F,
        notch = F)
"So, now the distribution of data is shown better. Yaaay"

# summary of the scaled crime rate
summary(boston_scaled$crim)



#As instructed, we shall now use quantile vector of column crim to create a categorical variable of the crime rate in the dataset 
#So, let's start
# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins
"so you can see the quantiles of the column called crim, for example minimum is -0.419366929 and first quantile is -0.410563278"


#now, we can catergorise the continuous (float) values based on the quantiles of data (as instructed)
# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
"cut()>>> divides the range of x into intervals and codes the values in x according to which interval they fall."
#so, we labelled the crim rate to be "low", "med_low", "med_high" or "high" based on the quantile values

# look at the table of the new factor crime
table(crime)
"Now, you can see that the number of observation divided/categorised to low is 127, medium low: 126, medium high 126 and high 127 cases/observation"
"we can even plot them"
plot(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)



#############
# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows as train dataset
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set by selecting those observations (rows) that were not in the training data (using -ind)
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)


#5 --------------
# Fit a linear discriminant analysis model on the train set
lda.fit <- lda(crime ~ ., data = train)
"this uses all variables (columns) of the train dataset to predict crime (the categorised column) "

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)



# Preditc-------------------
# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)


# EXTRA coding: Create a nice graph with confusion matrix
# insprited from https://stackoverflow.com/a/64539733
library('caret')
cm <- confusionMatrix(factor(lda.pred$class), factor(correct_classes), dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009193") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c('low','med_low','med_high','high')) +
  scale_y_discrete(labels=c('high', 'med_high', 'med_low', 'low'))


#####Extra Coding 2
#We can calucate overal accuracy 
### inspritred from https://stackoverflow.com/a/24349171
all_accuracies= cm$overall
all_accuracies
all_accuracies['Accuracy']*100
all_accuracies['Kappa']*100
"The confusing matirx (graph) shows that for example 25 of class high were predicted correctly as high, but one of the misclassified to med_high"
"As the confusion matrix shows, most f high were classified to high"
#Overal accuracy shows the overall number of correctly classified labels over the incorrect ones.


#Task 7--------------
#reload the dataset and calculate the distances between the observations
# load MASS and Boston
library(MASS)
data('Boston')

# euclidean distance matrix
dist_eu <- dist(Boston)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(Boston, method = 'manhattan')

# look at the summary of the distances
summary(dist_man)
"as you can see, mean of the distance between variables is 342.899 (2.02 and 1198.26 as min and max, respectively)"



##### Kmean cluster ---------
# k-means clustering
km <-kmeans(Boston, centers = 3)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)
"these pairs of plots show if we cluster the dataset to 3 clusters, then how differentically/discriminately can the variables be from each other."
# for example, tax and rad, tax and black, or tax and lstat columns were cluster-able visually. Interestingly, these columns were shown some high correlation between each other, for example 
#for example, the correlation between tax and rad is 0.91 (see below). Therefore, I plot the correlation again in below. FYI: we can plot them in 3D to see visually better. More info: check bonus exercises.

cor(Boston$tax, Boston$rad)

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", 
         cl.pos="b", tl.pos="d", tl.cex = 0.7)



# Investigate the optimal number of clusters
set.seed(1)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line', xlab= "number of clusters", ylab="Within groups sum of squares")
"As the results show in the figure, the total within sum of squares declines sharply by increasing the number of K's, but after 5, is gets almost steady, so something like 4 or 5 number of clusters is enough"

# k-means clustering
km <-kmeans(Boston, centers = 2) #number of clusters
km

# Extra coding: run a loop to optimse number of clusters
# Here I analyse different number of clusters by changing the centers = 2); 
#and report below the Within cluster sum of squares by cluster:
#By looking at the code, I understood that within cluster sum of squares by cluster is calculated by dividing  between_SS to total_SS. So, let's write that to our loop to get that printed
#So I write the code to print this as it loops

centers_list = c(2, 3,4,5,6,7,8,9,10,15,20)
for (i in centers_list){
  print('.........')
  print(i)
  km <-kmeans(Boston, centers = i)
  #print(km)
  print(km$betweenss/km$totss*100)
}
"the above number are the number of cluster, and within cluster sum of squares by cluster"
###Althogh the accuracy improves as the number of clusters increases, it seems 4 clusters to be fine, because accuracy improved dramatically until there, then get almost steady.
### Notably, I think this depends on application and aim of each project. For example, my personal experience in forest mapping using aerial imagery, the increase of 
### number of clusters, often led to horrible maps, thus somewhere between 3-5 is good.

#Thus, as explained above, we can use 4 number of clusters to not get too complicated and doable in this exercise. So, I run this clustering again to get it saved in R object
km <-kmeans(Boston, centers = 4)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)

#As figure shows, and explained above, using some columns like tax and black, tax and dist are easily separable.




"::::::::::::::::: alternatively new ::::::::::::::::"
### Extra coding =============
# Another code to compute and plot the total within-cluster sum of square
#inspired from https://www.r-bloggers.com/2020/05/practical-guide-to-k-means-clustering/
wssplot <- function(Boston, nc=15, seed=1234){
  wss <- (nrow(Boston)-1)*sum(apply(Boston,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(Boston, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

# plotting values for each cluster starting from 1 to 9
wssplot(Boston, nc = 9)
#As the plot shows, the total within-cluster sum of square declines as number of clusters increases. However, gets steady after 5, Thus, selecting 4 or 5 number of clusters sounds optimal.
"::::::::::::::::::::::::::::::::::::::::::::::::::::"


### Extra coding: Another method to find optimal number of clusters for k-means
#inspired from: http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization#cluster-analysis-and-factoextra
library("factoextra")
my_data <- scale(Boston)
clust_vis = fviz_nbclust(my_data, kmeans, method = "wss") #method = "wss" is total within sum of square
clust_vis
"fviz_nbclust function determines and visualize the optimal number of clusters using different methods: within cluster sums of squares"



#----------Bouns 2 --------------
model_predictors <- dplyr::select(train, -crime)

# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)

# matrix multiplication
library("plotly")
library("colorspace")
#red <- hex(HLS(0, 0.5, 1))
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, 
        z = matrix_product$LD3, type= 'scatter3d', mode='markers', 
        color = train$crime, colors = c("blue", "yellow", "black", "red"))
"For some tech reason this is not showing the plot."
require(plot3D)

attach(mtcars)

scatter3D(x = matrix_product$LD1, y = matrix_product$LD2, 
          z = matrix_product$LD3, pch = 18, cex = 2, 
          theta = 15, phi = 20, #by chaning the theta and phi, change your view angle to 3d plot
          ticktype = "detailed",
          xlab = "LD1", ylab = "LD2", zlab = "LD3", clab = "", 
          colkey = list(length = 0.8, width = 0.4),            
          main = "3D scatter of clusters", col=c("blue", "yellow", "black", "red"))
"Note: by chaning the theta and phi, change your view angle to 3d plot"
"From this view, we can see that it's clear to discriminate two cluster from this view (one cluster in right side and other one in left side), however, if we were able to rotate this plot, we could rotate and see other side to discuss more about the 4 possible clusters visually. I made it possible in below extra coding:"


##### Extra coding: to make interactive 3Dplot; interactively rotate and zoom the graphs.
#inspired from http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
# Make the rgl version
library("plot3Drgl")
plotrgl()
"you should run your 3d plot then run this code to plot it in interactive zoomable way"
#So, we can see that it is very easy to make 3-4 clusters visually.

#You can even plot a histogram of all columns in 3D and zoom in it:
hist3D(z=as.matrix(test))

##### Yet another way to visualize
# plot the lda results
plot(lda.fit, dimen = 3, col = classes, pch = classes)
#So we can see that it is very easy to make 2 clusters visually. and even possible to 4 clusters.


# Extra coding: to show the clusters in graph
#Related to bouns 2: coloring
library(factoextra)
#inspired from https://www.r-bloggers.com/2020/05/practical-guide-to-k-means-clustering/
#fviz_cluster provides ggplot2-based elegant visualization of partitioning methods including kmeans
fviz_cluster(km, data = Boston, main = "Partitioning Clustering Plot")
"Since the graph is 2D, we cannot rotate it or zoom to see other views of the graph. But generally, it looks good!"

