# set working directory
setwd("C:/Users/Peretz/Dropbox/Coursera/ML")

# load required packages
library(caret)

# set seed for reproducible results
set.seed(2015)

# read in our data
trn <- read.csv("pml-training.csv")
tst <- read.csv("pml-testing.csv")

# some exploration on our data
str(trn)
head(trn)
summary(trn)
hist(as.numeric(trn$classe),
     main = "Activity Type Distribution",
     xlab = "Activity Type",
     col = "green",
     ylim = c(0,6000),
     las = 1)

# how many complete cases?
sum(complete.cases(trn))

# after some research on our data, we can see that in most cases data are dirty with the following things:
# missing values, i.e. "", NA values and "#DIV/0!" values. Number of such observations is significant. To
# make data clean and tidy we will remove variables where number of such observations is greater than 50%.

del <- numeric() # variables to be removed
mar <- nrow(trn) * 0.5 # margin to cut off variables. 0.5 = 50%

for(i in 1:length(trn)) {
    if ( sum(trn[i] == "") || sum(is.na(trn[i])) || sum(trn[i] == "#DIV/0!") > mar ) {
        del <- c(del, i)
    }
}

# make clean data set without variables with more than 50% of missing observations
trn <- trn[,-del]

# there are also some descriptive variables (first 7) which we do not need to build our predictive model,
# so we remove them also
trn <- trn[,-c(1:7)]

str(trn)

# how many complete cases in clean data set?
sum(complete.cases(trn))

# we also need to process our testing data set in the same way
sum(complete.cases(tst))
tst <- tst[,-del]
tst <- tst[,-c(1:7)]
sum(complete.cases(tst))
