# Load libraries
library(tidyverse)
library(tidytext)

# Read the text file into a data frame
hist_text <- read_tsv("hist(2).txt")

# Tokenize the text into individual words
hist_words <- hist_text %>%
  unnest_tokens(word, text)

# Remove common stop words
data("stop_words")
hist_words_clean <- hist_words %>%
  anti_join(stop_words, by = "word")

# Count word frequencies by chapter
word_counts <- hist_words_clean %>%
  count(chapter, word, sort = TRUE)

# Calculate tf-idf for each word by chapter
word_tf_idf <- word_counts %>%
  bind_tf_idf(word, chapter, n)

# Get top 5 tf-idf words per chapter
hist_top_by_chap <- word_tf_idf %>%
  group_by(chapter) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(chapter)

# Print top 5 words from each chapter
print(hist_top_by_chap)

print(hist_top_by_chap, n = Inf)

