# ----- Libraries -----
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(stylo)

# ----- File Paths -----
hp_series_file <- "C:/Users/trusioem/Downloads/MidtermProjectFiles/HarryPotterSeries.txt"
# Set the corpus directory path
corpus_dir <- "C:/Users/trusioem/Downloads/MidtermProjectFiles/corpus"

# Check if the directory exists
if(file.exists(corpus_dir)){
  print("Corpus directory exists.")
} else {
  print("Corpus directory does not exist. Check the path!")
}

# List all files in the directory (recursively)
all_files <- list.files(path = corpus_dir, full.names = TRUE, recursive = TRUE)
print("All files found in corpus_dir (recursively):")
print(all_files)

# List all files with a .txt extension, case-insensitive, recursively
all_txt_files <- list.files(path = corpus_dir, pattern = "(?i)\\.txt$", full.names = TRUE, recursive = TRUE)
print("All txt files found in corpus_dir:")
print(all_txt_files)

# ----- PART 1: Harry Potter Series Word Frequency -----
hp_data <- read_csv(hp_series_file, col_names = TRUE)
if ("X1" %in% names(hp_data)) hp_data <- hp_data %>% select(-X1)
hp_words <- hp_data  # Columns: book, chapter, word
hp_freq_all <- hp_words %>% count(word, sort = TRUE)
print("Top 10 most frequent words (with stopwords):")
print(head(hp_freq_all, 10))
data("stop_words")
hp_words_clean <- hp_words %>% anti_join(stop_words, by = "word")
hp_freq_clean <- hp_words_clean %>% count(word, sort = TRUE)
print("Top 10 most frequent words (stopwords removed):")
print(head(hp_freq_clean, 10))

# ----- PART 2: Word Frequencies per Book & Bar Graph -----
hp_book_freq <- hp_words %>%
  anti_join(stop_words, by = "word") %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  top_n(10, n) %>%
  ungroup()
print("Top 10 words for each book (stopwords removed):")
print(hp_book_freq)
ggplot(hp_book_freq, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ book, scales = "free_y") +
  coord_flip() +
  labs(title = "Top 10 Words per Book", x = "Word", y = "Frequency") +
  theme_minimal()

# ----- PART 3: Wordcloud for a Selected Harry Potter Book -----
hp_book1 <- hp_words %>%
  filter(book == "Philosopher's Stone" & chapter == 1) %>%  # Adjust as needed
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)
print("Word frequencies for selected book (for wordcloud):")
print(head(hp_book1, 20))
set.seed(123)
wordcloud(words = hp_book1$word, freq = hp_book1$n, min.freq = 2, max.words = 100,
          colors = brewer.pal(8, "Dark2"))

# ----- PART 4: Word Frequency for Suzanne Collins (stopwords removed) -----
# List all files in the corpus directory with .txt extension
corpus_files <- list.files(path = corpus_dir, pattern = "\\.txt$", full.names = TRUE)
# Filter for files with "Collins" in the filename (case-insensitive)
collins_files <- corpus_files[str_detect(corpus_files, regex("Collins", ignore_case = TRUE))]
print("Collins files found:")
print(collins_files)

# Initialize an empty tibble with expected columns
collins_text <- tibble(doc_text = character(), file = character())

# Loop through each Collins file, reading its lines and binding the result
for(file in collins_files){
  temp_text <- read_lines(file)
  temp_df <- tibble(doc_text = temp_text, file = basename(file))
  collins_text <- bind_rows(collins_text, temp_df)
}

print("Collins doc_text column names:")
print(names(collins_text))  # Expected: "doc_text" and "file"

# Tokenize the text using the new column name and remove stopwords
collins_words <- collins_text %>% 
  unnest_tokens(word, doc_text) %>% 
  anti_join(stop_words, by = "word")
collins_freq <- collins_words %>% count(word, sort = TRUE)
print("Top 10 most frequent words for Suzanne Collins (stopwords removed):")
print(head(collins_freq, 10))

# ----- PART 5: Jittered Scatterplot: Rowling vs. Collins -----
# For Rowling, rename the count column to 'rowling_n'
rowling_freq <- hp_words_clean %>% count(word, sort = TRUE) %>% rename(rowling_n = n)
# For Collins, rename the count column to 'collins_n'
collins_freq2 <- collins_freq %>% rename(collins_n = n)
freq_comparison <- inner_join(rowling_freq, collins_freq2, by = "word")
ggplot(freq_comparison, aes(x = rowling_n, y = collins_n, colour = "red")) +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Word Frequency: Rowling vs. Collins", x = "Rowling (log scale)", y = "Collins (log scale)") +
  theme_minimal()

# ----- PART 6: TTR and Hapax Measures -----
calc_measures <- function(df) {
  total_words <- sum(df$n)
  unique_words <- nrow(df)
  hapax <- nrow(df %>% filter(n == 1))
  tibble(total_words = total_words, unique_words = unique_words, hapax = hapax,
         TTR = unique_words / total_words, Hapax_Ratio = hapax / total_words)
}
rowling_measures <- calc_measures(rowling_freq %>% rename(n = rowling_n))
print("J.K. Rowling Measures:")
print(rowling_measures)
collins_measures <- calc_measures(collins_freq %>% rename(n = n))
print("Suzanne Collins Measures:")
print(collins_measures)

# ----- PART 7: Additional Excerpts Stylometry (Basic Comparison) -----
galbraith_file <- file.path(corpus_dir, "Galbraith_Cuckoo.txt")
dragon_file <- file.path(corpus_dir, "Rowling_Leopard.txt")
galbraith_text <- tibble(text = read_lines(galbraith_file))
dragon_text <- tibble(text = read_lines(dragon_file))
galbraith_words <- galbraith_text %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word") %>% count(word, sort = TRUE)
dragon_words <- dragon_text %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word") %>% count(word, sort = TRUE)
print("Top 10 words in Galbraith_Cuckoo.txt (stopwords removed):")
print(head(galbraith_words, 10))
print("Top 10 words in Rowling_Leopard.txt (stopwords removed):")
print(head(dragon_words, 10))
galbraith_ttr <- nrow(galbraith_words) / sum(galbraith_words$n)
dragon_ttr <- nrow(dragon_words) / sum(dragon_words$n)
print(paste("TTR for Galbraith_Cuckoo.txt:", round(galbraith_ttr, 4)))
print(paste("TTR for Rowling_Leopard.txt:", round(dragon_ttr, 4)))

# ----- PART 8: Stylo Analysis -----
stylo(
  gui = FALSE,
  corpus.dir = corpus_dir,
  corpus.lang = "English",
  analyzed.features = "w",
  mfw.min = 50,
  mfw.max = 300,
)
