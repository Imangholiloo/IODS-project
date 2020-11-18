#Data wrangling for the next week’s data

#Read the “Human development” file into R
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

#Read the “Gender inequality” file into R
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


#For data decription and more info please visit:
#  http://hdr.undp.org/en/content/human-development-index-hdi and http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

#strcutre of data
str(hd)

#dimension of file (row* column)
dim(hd)

#summary of file
summary(hd)

#column names
colnames(hd)

#see head and tail of the data
head(hd)
tail(hd)

#Make some visual graphs of all plot.safs(
library(ggplot2)
library(GGally)
#pairs(hd)
#p <- ggpairs(hd, mapping = aes(), cardinality_threshold=NULL, #without cardinality_threshold, it was giving error, inspired from https://stackoverflow.com/a/61525354
#             lower = list(combo = wrap("facethist", bins = 20)))
#p

#Let's do the same for gii dataset
#strcutre of data
str(gii)

#dimension of file (row* column)
dim(gii)

#summary of file
summary(gii)

#column names
colnames(gii)

#see head and tail of the data
head(gii)
tail(gii)

#Make some visual graphs of all plot.safs(
library(ggplot2)
library(GGally)
#pairs(gii)
#p <- ggpairs(gii, mapping = aes(), cardinality_threshold=NULL, 
#             lower = list(combo = wrap("facethist", bins = 20)))
#p



##### 4: rename the variables with (shorter) descriptive names
colnames(hd)
colnames(hd) <- c('HDI_Rank', 'Country', 'HDI', 'Life_Expec_Birth', 
                  'Expec_yr_Edu', 'Mean_yr_Edu', 'GNI_per_Cap', 
                  'GNI_per_Cap_Min_HDI_Rank')
colnames(hd)
#so you can see that column names were shorten.


#Let's do the same fo gii dataset
colnames(gii)
colnames(gii) <- c('GII_Rank', 'Country', 'GII', 'Mortality_R', 
                  'Adolescent_Birth_R', 'Representation_Parliament',
                  'Sec_Edu_Fem','Sec_Edu_Mal',
                  "Lab_Forc_Particip_R_Fem",
                  "Lab_Forc_Particip_R_Mal" )
colnames(gii)
#so you can see that column names were shorten.




###5.Mutate the “Gender inequality” data and create two new variables
gii <- mutate(gii, edu_R = (Sec_Edu_Fem/Sec_Edu_Mal))
head(gii)

### Extra coding: alternative to mutate function
#instead of using mutate, this is also possible
gii$edu_R <- gii$Sec_Edu_Fem/gii$Sec_Edu_Mal

library(dplyr)
gii <- mutate(gii, Lab_Forc_R = (Lab_Forc_Particip_R_Fem/Lab_Forc_Particip_R_Mal))
head(gii)



#6.Join together the two datasets using the variable Country as the identifier
"TIP: we use inner join to keep only observations in both data sets"
hd_gii_joint <- inner_join(hd, gii, 
                           by = 'Country', suffix = c("_hd", "_gi"))

head(hd_gii_joint)
dim(hd_gii_joint)

str(hd_gii_joint)


#setwd("./data")
write.csv(hd_gii_joint, "./data/human.csv")

