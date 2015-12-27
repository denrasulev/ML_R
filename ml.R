# set working directory
setwd("C:/Users/Peretz/Dropbox/Coursera/ML")

# load required packages
library(caret)

# set seed for reproducible results
set.seed(2015)

# read in our data
df <- read.csv("pml-training.csv")

# some exploration on our data
str(df)
head(df)
summary(df)
hist(as.numeric(df$classe),
     main = "Activity Type Distribution",
     xlab = "Activity Type",
     col = "green",
     ylim = c(0,6000),
     las = 1)

# how many complete cases?
sum(complete.cases(df))

# after some research on our data, we can see that in most cases data are dirty with the following things:
# missing values, i.e. "", NA values and "#DIV/0!" values. Number of such observations is significant. To
# make data clean and tidy we will remove variables where number of such observations is greater than 50%.

delete <- numeric()
margin <- nrow(df) * 0.5

for(i in 1:length(df)) {
    if ( sum(df[i] == "") || sum(is.na(df[i])) || sum(df[i] == "#DIV/0!") > margin ) {
        delete <- c(delete, i)
    }
}

# make clean data set without variables with more than 50% of missing observations
clean <- df[,-delete]
str(clean)

# how many complete cases in clean data set?
sum(complete.cases(clean))