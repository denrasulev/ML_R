setwd("C:/Users/Peretz/Dropbox/Coursera/ML")

library(caret)

set.seed(6543)

df <- read.csv("pml-training.csv")

str(df)
head(df)
summary(df)
hist(as.numeric(df$classe),
     main = "Activity Type Distribution",
     xlab = "Activity Type",
     col = "green",
     ylim = c(0,6000),
     las = 1)

sum(complete.cases(df))

sum(is.na(df[18]))
nrow(df)
0.1*nrow(df)

sum(df$kurtosis_picth_belt == "")
sum(is.na(df$avg_roll_belt))
sum(df$kurtosis_picth_belt == "#DIV/0!")

delete <- numeric()
for(i in 1:length(df)) {
    if(sum(is.na(df[i])) > 0.2*nrow(df)) {
        delete <- c(delete, i)
    }
}



str(delete)
clean <- df[,-delete]
str(clean)
