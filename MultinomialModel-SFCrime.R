install.packages("nnet")
library(nnet)
setwd("~/Development/SFCrimeClassificationAttempt3")
train <- read.csv("~/Development/SFCrimeClassificationAttempt3/train.csv")
test <- read.csv("~/Development/SFCrimeClassificationAttempt3/test.csv")

# New dataframes with the columns to use
train.df <- data.frame(Category = train$Category, DayOfWeek = train$DayOfWeek,
                       PdDistrict = train$PdDistrict)
test.df <- data.frame(DayOfWeek = test$DayOfWeek, PdDistrict = test$PdDistrict)

# Create a new column with the hour of the incident
train.df$Hour <- sapply(train$Dates, function(x) as.integer(strftime(x, format = "%H")))
test.df$Hour <- sapply(test$Dates, function(x) as.integer(strftime(x, format = "%H")))

# Remove the original dataframes
rm(train)
rm(test)


# Multinomial log-linear model using the day of the week and the district of the crime
# as the predictors.
multinom.model <- multinom(Category ~ DayOfWeek + PdDistrict + Hour, data = train.df, 
                 maxit = 500)
predictions <- predict(multinom.model, test.df, "probs")
submission <- format(predictions, digits=4, scientific = FALSE)
submission <- cbind(id = 0:884261, submission)
submission <- as.data.frame(submission)
write.csv(submission, file = "results.csv", row.names = FALSE)