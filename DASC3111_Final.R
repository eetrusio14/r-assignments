# Final Project: IMDB Movie Reviews Analysis
# Name: Emma Trusio
# Date: May 4, 2025

# 0. Setup: load packages and read in the CSVs
# We're using tidyverse for data wrangling, tidytext for text mining,
# wordcloud for word clouds, topicmodels for LDA, tm for text cleaning,
# e1071 for Naive Bayes, and caret for evaluation.
library(tidyverse)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(tm)
library(e1071)
library(caret)
library(textdata)   # for AFINN lexicon and NRC lexicon

# Read the positive and negative reviews from the folder
pos <- read_tsv("/Users/emmatrusio/Downloads/FinalProjectFiles/pos_reviews.csv", col_names = FALSE)
neg <- read_tsv("/Users/emmatrusio/Downloads/FinalProjectFiles/neg_reviews.csv", col_names = FALSE)
colnames(pos) <- "review"
colnames(neg) <- "review"
# Add a sentiment label and unique ID to each row
pos <- pos %>% mutate(sentiment = "positive", id = row_number())
neg <- neg %>% mutate(sentiment = "negative", id = row_number())

# Load stop words
data("stop_words")

# 1. Wordclouds of positive vs negative reviews
# Tokenize into words, remove stop words, then count frequency
pos_words <- pos %>% unnest_tokens(word, review) %>% anti_join(stop_words)
neg_words <- neg %>% unnest_tokens(word, review) %>% anti_join(stop_words)
pos_freq <- pos_words %>% count(word, sort = TRUE)
neg_freq <- neg_words %>% count(word, sort = TRUE)
# Plot the top 100 words for each
set.seed(123)
wordcloud(pos_freq$word, pos_freq$n, max.words = 100, scale = c(4, 0.5), random.order = FALSE)
wordcloud(neg_freq$word, neg_freq$n, max.words = 100, scale = c(4, 0.5), random.order = FALSE)

# 2. Scatterplot comparing positive vs negative word counts
# Combine frequencies, add 1 to avoid log(0), and filter rare words
a <- pos_freq %>% rename(pos = n)
b <- neg_freq %>% rename(neg = n)
freq_compare <- full_join(a, b, by = "word") %>%
  replace_na(list(pos = 0, neg = 0)) %>%
  mutate(pos = pos + 1, neg = neg + 1) %>%
  filter(pos + neg > 100)  # only keep words that appear more than 100 times total
# Plot on log scales to see which words lean positive or negative
ggplot(freq_compare, aes(x = pos, y = neg)) +
  geom_jitter(alpha = 0.5, size = 1) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Positive vs Negative Word Frequencies",
       x = "Positive count +1 (log scale)",
       y = "Negative count +1 (log scale)") +
  theme_minimal()

# 3. Sentiment scoring using AFINN
# AFINN gives each word a score between -5 (negative) and 5 (positive)
afinn <- get_sentiments("afinn")
# Join words with AFINN, then sum scores per review
score_df <- bind_rows(pos, neg) %>%
  unnest_tokens(word, review) %>%
  inner_join(afinn, by = "word") %>%
  group_by(sentiment, id) %>%
  summarize(sentiment_score = sum(value), .groups = 'drop')
# Get mean, median, sd for positive vs negative reviews
overall_scores <- score_df %>%
  group_by(sentiment) %>%
  summarize(
    mean_score = mean(sentiment_score),
    median_score = median(sentiment_score),
    sd_score = sd(sentiment_score)
  )
print(overall_scores)
# Plot density of sentiment scores for each group
ggplot(score_df, aes(sentiment_score, fill = sentiment)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Review Sentiment Scores",
       x = "AFINN Sentiment Score",
       y = "Density") +
  theme_minimal()

# 4. NRC sentiment for 'sadness'
# Get all words, then pick only those marked as 'sadness' in NRC
df_all <- bind_rows(pos, neg) %>% unnest_tokens(word, review)
# Pull just the sad words
sad_words <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness") %>%
  select(word)
# Count how many sad words show up in each review group
sadness_counts <- df_all %>%
  inner_join(sad_words, by = "word") %>%
  count(sentiment) %>%
  rename(sadness_count = n)
print(sadness_counts)

# 5. Topic modeling (LDA) on all reviews with k = 20. Topic modeling (LDA) on all reviews with k = 20
# Create a Document-Term Matrix (DTM)
dtm <- df_all %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  cast_dtm(id, word, n)
# Fit LDA model
lda20 <- LDA(dtm, k = 20, control = list(seed = 123))
# Get top 10 terms per topic
topics <- tidy(lda20, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)
print(top_terms)

# 6. Text classification: Naive Bayes to predict positive vs negative
# Split 75% train / 25% test by ID
train <- bind_rows(pos %>% filter(id <= 750), neg %>% filter(id <= 750))
test  <- bind_rows(pos %>% filter(id > 750), neg %>% filter(id > 750))
# Clean text function for tm
clean_corpus <- function(text) {
  VCorpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
}
# Build DTMs for train/test using same terms
train_dtm <- DocumentTermMatrix(clean_corpus(train$review)) %>% removeSparseTerms(0.99)
test_dtm  <- DocumentTermMatrix(clean_corpus(test$review), control = list(dictionary = Terms(train_dtm)))
# Convert to data frames
train_df <- as.data.frame(as.matrix(train_dtm))
train_df$sentiment <- factor(train$sentiment)
test_df  <- as.data.frame(as.matrix(test_dtm))
test_df$sentiment  <- factor(test$sentiment)
# Train Naive Bayes model and evaluate
model_nb <- naiveBayes(sentiment ~ ., data = train_df)
pred_nb  <- predict(model_nb, test_df)
cm <- confusionMatrix(pred_nb, test_df$sentiment)
print(cm)
