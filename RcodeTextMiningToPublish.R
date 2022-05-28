# https://www.kirenz.com/post/2019-09-16-r-text-mining/#data-import

install.packages(c("dplyr", "tidytext", "stringr", "tidytext", "tidyr", "stopwords") 

library(dplyr)
library(tidytext)
library(stopwords) 
library(tibble)
library(ggplot2)

library(gutenbergr)
library(stringr)

# check books 

doyle <- gutenberg_works(str_detect(author, "Doyle"))

# load books

books <- gutenberg_download(c(30155, 13476), meta_fields = "author")

books <- as_tibble(books) %>% 
  mutate(document = row_number()) %>% 
  select(-gutenberg_id)

# we need to both break the text into individual tokens (a process called tokenization) and transform it to a tidy data structure (i.e. each variable must have its own column, each observation must have its own row and each value must have its own cell). To do this, we use tidytext’s unnest_tokens() function. We also remove the rarest words in that step, keeping only words in our dataset that occur more than 10 times.

tidy_books <- books %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

# remove stop words 

stopword <- as_tibble(stopwords::stopwords("en")) 
stopword <- rename(stopword, word=value)
tb <- anti_join(tidy_books, stopword, by = 'word')

# term frequency 

word_count <- count(tb, word, sort = TRUE)

# term frequency by author 

author_count <-  tb %>% 
  count(author, word, sort = TRUE)

# Plot terms with a frequency greater than 100:

tb %>%
  count(author, word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(aes(fill=author)) +
  xlab(NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(fill= "Author", title="Word frequency", subtitle="n > 100")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer() 


#Plot top 20 terms by author:

tb %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, author), n,
    fill = author)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~author, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  labs(fill= "Author", 
       title="Most frequent words", 
       subtitle="Top 20 words by book",
       x= NULL, 
       y= "Word Count")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer() 

