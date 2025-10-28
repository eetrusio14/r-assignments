# Load libraries
library(gutenbergr)
library(tidyverse)
library(tidytext)

# Download Wells novels
wells_books <- gutenberg_download(c(35, 36, 5230))  # The Time Machine, War of the Worlds, Invisible Man

# Tidy format
wells_tidy <- wells_books %>%
  unnest_tokens(word, text)

# Word counts by book
wells_word_freq <- wells_tidy %>%
  group_by(gutenberg_id) %>%
  count(word, sort = TRUE)

# Compute tf-idf
wells_tf_idf <- wells_word_freq %>%
  bind_tf_idf(word, gutenberg_id, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup() %>%
  mutate(book = case_when(
    gutenberg_id == 35 ~ "The Time Machine",
    gutenberg_id == 36 ~ "The War of the Worlds",
    gutenberg_id == 5230 ~ "The Invisible Man"
  ))

# Top 10 tf-idf words per book
wells_tf_idf_top10 <- wells_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE)

# Plot Wells tf-idf chart
wells_tf_idf_top10 %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf (Wells)") +
  facet_wrap(~book, ncol = 1, scales = "free") +
  coord_flip()

# Download Bronte novels
bronte_books <- gutenberg_download(
  c(768, 767),  # Wuthering Heights, Agnes Grey
  mirror = "http://www.gutenberg.lib.md.us"
)

# Tidy format
bronte_tidy <- bronte_books %>%
  unnest_tokens(word, text)

# Word counts by book
bronte_word_freq <- bronte_tidy %>%
  group_by(gutenberg_id) %>%
  count(word, sort = TRUE)

# Compute tf-idf
bronte_tf_idf <- bronte_word_freq %>%
  bind_tf_idf(word, gutenberg_id, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup() %>%
  mutate(book = case_when(
    gutenberg_id == 768 ~ "Wuthering Heights",
    gutenberg_id == 767 ~ "Agnes Grey"
  ))

# Top 10 tf-idf words per book
bronte_tf_idf_top10 <- bronte_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE)

# Plot Brontë tf-idf chart
bronte_tf_idf_top10 %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf (Brontë Sisters)") +
  facet_wrap(~book, ncol = 1, scales = "free") +
  coord_flip()

print(wells_tf_idf)

print(bronte_tf_idf)
