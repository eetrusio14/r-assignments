# Load the usual libraries
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textmodels)

# Read in the email data (1 = spam, 0 = ham)
emails <- read.csv("/Users/emmatrusio/Downloads/emails.csv", stringsAsFactors = FALSE)
names(emails) <- c("label", "text") 

# Turn the text column into a corpus (quanteda needs this format)
emails_corpus <- corpus(emails)

# Tokenize and make a document-feature matrix (dfm)
emails_dfm <- dfm(tokens(emails_corpus), tolower = TRUE)

# Split the data: 75% for training, 25% for testing
split_point <- floor(0.75 * nrow(emails))
emails_dfm_train <- emails_dfm[1:split_point, ]
emails_dfm_test <- emails_dfm[(split_point + 1):nrow(emails_dfm), ]

# Also split the original data frame so we still have the labels
emails_train <- emails[1:split_point, ]
emails_test <- emails[(split_point + 1):nrow(emails), ]

# Train model using the training set and display
emails_classifier <- textmodel_nb(emails_dfm_train, emails_train$label)
print(emails_classifier)

# Make predictions on the test set
emails_pred <- predict(emails_classifier, newdata = emails_dfm_test)

# Show the confusion matrix
conf_matrix <- table(emails_pred, emails_test$label)
print(conf_matrix)
