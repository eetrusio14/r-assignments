# Load Required Packages
library(tidyverse)    # for dplyr, ggplot2, readr, tidyr, etc.
library(tidytext)     # for text tokenization
library(wordcloud)    # for wordclouds
library(scales)       # for formatting axes in plots
library(text2vec)     # for cosine similarity calculations
library(purrr)        # for mapping over files

#-------------------------
# Q1: Word Frequency Tables for the Harry Potter Series (with and without stopwords)
#-------------------------

setwd("C:/Users/trusioem/Downloads/MidtermProjectFiles")

print("Q1: Word Frequency Tables for Harry Potter Series (with stopwords)")
hp_series <- read_lines("HarryPotterSeries.txt")
hp_series_df <- tibble(text = hp_series)
hp_tokens <- hp_series_df %>% unnest_tokens(word, text)
hp_freq_all <- hp_tokens %>% count(word, sort = TRUE)
top10_all <- hp_freq_all %>% slice_max(n, n = 10)
print(top10_all)

print("Q1: Word Frequency Tables for Harry Potter Series (without stopwords)")
data("stop_words")
hp_tokens_nostop <- hp_tokens %>% anti_join(stop_words, by = "word")
hp_freq_nostop <- hp_tokens_nostop %>% count(word, sort = TRUE)
top10_nostop <- hp_freq_nostop %>% slice_max(n, n = 10)
print(top10_nostop)

#-------------------------
# Q2: Stopword-Removed Word Frequency Tables for Each HP Book and Bar Graph
#-------------------------
print("Q2: Generating Stopword-Removed Top 10 Word Frequencies for Each HP Book")

# Assume HP files are named "Rowling_HP1.txt" through "Rowling_HP7.txt" in a subfolder "corpus"
hp_files <- list.files(path = "corpus", pattern = "Rowling_HP[0-9]+\\.txt", full.names = TRUE)
hp_books <- map_dfr(hp_files, ~{
  text <- read_lines(.x)
  tibble(text = text, file = basename(.x))
})
hp_books_tokens <- hp_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")
hp_books_freq <- hp_books_tokens %>%
  group_by(file, word) %>%
  summarise(n = n(), .groups = "drop")
top10_by_book <- hp_books_freq %>%
  group_by(file) %>%
  slice_max(n, n = 10) %>%
  ungroup()

# Plot the top 10 words per book using ggplot2
ggplot(top10_by_book, aes(x = reorder(word, n), y = n, fill = file)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ file, scales = "free_y") +
  coord_flip() +
  labs(title = "Q2: Top 10 Most Frequent (Stopword-Removed) Words for Each HP Book",
       x = "Word", y = "Frequency")

#-------------------------
# Q3: Wordcloud for a Single HP Book
#-------------------------
print("Q3: Creating a Wordcloud for Rowling_HP1.txt")
hp1 <- read_lines("corpus/Rowling_HP1.txt")
hp1_df <- tibble(text = hp1)
hp1_tokens <- hp1_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")
hp1_freq <- hp1_tokens %>% count(word, sort = TRUE)
wordcloud(words = hp1_freq$word, freq = hp1_freq$n, max.words = 100,
          colors = brewer.pal(8, "Dark2"), main = "Q3: Wordcloud for HP Book 1")

#-------------------------
# Q4: Stopword-Removed Word Frequency Table for a Selected YA Author (Suzanne Collins)
#-------------------------
print("Q4: Generating Stopword-Removed Word Frequency Table for Suzanne Collins' Texts")
collins_files <- list.files(path = "corpus", pattern = "Collins_HG[0-9]+\\.txt", full.names = TRUE)
collins_df <- map_dfr(collins_files, ~{
  text <- read_lines(.x)
  tibble(text = text, file = basename(.x))
})
collins_tokens <- collins_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")
collins_freq <- collins_tokens %>% count(word, sort = TRUE)
print(collins_freq %>% slice_max(n, n = 10))

#-------------------------
# Q5: Jittered Scatterplot Contrasting Word Frequencies of Suzanne Collins vs. J.K. Rowling
#-------------------------
print("Q5: Creating a Jittered Scatterplot Comparing Word Frequencies (Collins vs. Rowling)")
# Process Rowling’s texts (already tokenized above)
rowling_files <- list.files(path = "corpus", pattern = "Rowling_HP[0-9]+\\.txt", full.names = TRUE)
rowling_df <- map_dfr(rowling_files, ~{
  text <- read_lines(.x)
  tibble(text = text, file = basename(.x))
})
rowling_tokens <- rowling_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")
rowling_freq <- rowling_tokens %>% count(word, sort = TRUE) %>% mutate(author = "Rowling")
collins_freq2 <- collins_freq %>% mutate(author = "Collins")

# Combine and reshape to compare frequencies:
combined_freq <- bind_rows(rowling_freq, collins_freq2) %>%
  group_by(author, word) %>%
  summarise(freq = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = author, values_from = freq, values_fill = 0)

# Jittered scatterplot
ggplot(combined_freq, aes(x = Collins, y = Rowling)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(title = "Q5: Word Frequency Comparison: Suzanne Collins vs. J.K. Rowling",
       x = "Collins Frequency (log scale)", y = "Rowling Frequency (log scale)")

#-------------------------
# Q6: Compare TTR and Hapax Measures for J.K. Rowling and Suzanne Collins
#-------------------------
print("Q6: Calculating TTR and Hapax Measures for Rowling vs. Collins")

# Function to compute Type-Token Ratio (TTR) and number of hapaxes:
compute_stats <- function(tokens) {
  freq <- tokens %>% count(word, sort = TRUE)
  total_tokens <- sum(freq$n)
  types <- nrow(freq)
  ttr <- types / total_tokens
  hapax <- sum(freq$n == 1)
  tibble(total_tokens, types, ttr, hapax)
}
rowling_stats <- compute_stats(rowling_tokens)
collins_stats <- compute_stats(collins_tokens)
ttr_comparison <- bind_rows(Rowling = rowling_stats, Collins = collins_stats, .id = "author")
print(ttr_comparison)

#-------------------------
# Q7: Stylometric Analyses for Authorship of Galbraith and "Leopard" Excerpts
#-------------------------
print("Q7: Stylometric Analysis for Authorship of Galbraith_Cuckoo.txt and Rowling_Leopard.txt")

# Read the two additional excerpts:
galbraith <- read_lines("corpus/Galbraith_Cuckoo.txt")
leopard <- read_lines("corpus/Rowling_Leopard.txt")
galbraith_df <- tibble(text = galbraith) %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word")
leopard_df <- tibble(text = leopard) %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word")
galbraith_freq <- galbraith_df %>% count(word, sort = TRUE)
leopard_freq <- leopard_df %>% count(word, sort = TRUE)

# For a simple stylometric comparison, compute cosine similarity over a common vocabulary.
# Create a vocabulary from the combined known texts (Rowling and Collins here)
combined_known <- bind_rows(rowling_tokens, collins_tokens)
vocab <- combined_known %>% count(word) %>% filter(n >= 5) %>% pull(word)

# Function to get term-frequency vector over the vocabulary:
get_tf_vector <- function(freq_df, vocab) {
  vec <- freq_df %>% filter(word %in% vocab) %>% arrange(word)
  tf <- setNames(rep(0, length(vocab)), sort(vocab))
  tf[names(tf) %in% vec$word] <- vec$n[match(names(tf)[names(tf) %in% vec$word], vec$word)]
  return(tf)
}
tf_galbraith <- get_tf_vector(galbraith_freq, vocab)
tf_leopard <- get_tf_vector(leopard_freq, vocab)
tf_rowling <- get_tf_vector(rowling_freq, vocab)
tf_collins <- get_tf_vector(collins_freq2, vocab)

# Cosine similarity function:
cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
}

sim_galbraith_rowling <- cosine_sim(tf_galbraith, tf_rowling)
sim_galbraith_collins <- cosine_sim(tf_galbraith, tf_collins)
sim_leopard_rowling <- cosine_sim(tf_leopard, tf_rowling)
sim_leopard_collins <- cosine_sim(tf_leopard, tf_collins)

similarity_results <- tibble(
  Excerpt = c("Galbraith_Cuckoo", "Rowling_Leopard"),
  Rowling_sim = c(sim_galbraith_rowling, sim_leopard_rowling),
  Collins_sim = c(sim_galbraith_collins, sim_leopard_collins)
)
print(similarity_results)
