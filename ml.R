# SETUP AND PREPARE

# set working directory
setwd("C:/Users/Peretz/Dropbox/Coursera/ML")

# load required packages
library(caret)
library(e1071)
library(randomForest)

# set seed for reproducible results
set.seed(2015)

# read in our data
trn <- read.csv("pml-training.csv")
tst <- read.csv("pml-testing.csv")

# MAKE EXPLORATORY ANALYSIS

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

# DATA CLEANING

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

# TRAINING AND VALIDATION

# We have a classification task here and to solve it, we'll use Random Forest method, which handles such cases very well.
# For this we need to split our training data into training and validation sets at the ratio of 75-25.

split <- createDataPartition(trn$classe, p = 0.75, list = FALSE)
training <- trn[split,]
validate <- trn[-split,]

# now we build and train our model using training set
# number of observations allow us to make 5-fold cross-validation, which is more than enough here.
model <- train( classe ~ .,
                data = training,
                method = "rf",
                trControl = trainControl(method = "cv", number = 5),
                prox = TRUE,
                allowParallel = TRUE)
print(model)

# now we use our model to predict 'classe' variable in validation data set.
prediction <- predict(model, validate[,-53])

# and check how well our prediction works
confusionMatrix(prediction, validate$classe)

# now we use our model to predict 'classe' variable in our testing data set.
answers <- predict(model, tst)

# and write all answers to text files for submission.
pml_write_files = function(x) {
    n = length(x)
    for(i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
}
pml_write_files(answers)
