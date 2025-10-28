
# Load necessary packages
library(tidyverse)
library(tidytext)
library(readr)

setwd("/Users/emmatrusio/Downloads")

# Load the Apples Never Fall review data
ANF_reviews <- read_csv("ANFReviews3.csv")

ANF_reviews <- ANF_reviews %>%
  rename(metacritic = stars)

# Step 1: Tokenize the text into words
ANF_tidy <- ANF_reviews %>% 
  unnest_tokens(word, text) 

# Step 2: Join with the 'bing' sentiment lexicon
ANF_sent <- ANF_tidy %>%
  inner_join(get_sentiments("bing"))

# Step 3: Count number of positive and negative words for each review
ANF_sent_cnt <- ANF_sent %>%
  count(source, metacritic, sentiment)

# Step 4: Spread the sentiment counts into separate columns for positive and negative
ANF_sent_cnt <- ANF_sent_cnt %>%
  spread(sentiment, n, fill = 0)  # Fill missing values with 0

# Step 5: Create new column for sentiment score (positive - negative)
ANF_sent_cnt <- ANF_sent_cnt %>%
  mutate(bing_sentiment = positive - negative)

print(ANF_sent_cnt)

# Step 6: Create scatterplot of sentiment score vs Metacritic score
ggplot(ANF_sent_cnt, aes(x = bing_sentiment, y = metacritic)) +
  geom_point() +
  geom_text(aes(label = source), hjust = 1.1, vjust = 0.5, size = 3) +  # Optional: add source labels
  labs(title = "Sentiment Score vs Metacritic Score",
       x = "Bing Sentiment Score (Positive - Negative)",
       y = "Metacritic Score") +
  theme_minimal()

# Step 7: Calculate correlation between sentiment score and Metacritic score
correlation <- cor(ANF_sent_cnt$bing_sentiment, ANF_sent_cnt$metacritic, use = "complete.obs")
print(paste("Correlation between sentiment score and Metacritic score:", round(correlation, 3)))
