library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(tidyr)
library(textdata)

data <- read.csv("C:\\Users\\Riya\\Desktop\\text analysis\\tweets.csv")
data <- data[,-c(1:2)]
names(data)[5] <- 'created_at'
names(data)[11] <- 'screen_name'
data <- data[, c('text','screen_name','created_at')]
col <- colnames(data)

dataAPI <- search_tweets(q = "demonetization", n = 10000,
                                lang = "en",
                                include_rts = FALSE)
dataAPI <- dataAPI[,col]

demonetization <- rbind(data,dataAPI)

demonetization$stripped_text <- gsub("http.*","",  demonetization$text)
demonetization$stripped_text <- gsub("https.*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("ed*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("00B8*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("00A0*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("00BD*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("RT*","", demonetization$stripped_text)
demonetization$stripped_text <- gsub("U","", demonetization$stripped_text)
demonetization$stripped_text <- gsub('[[:digit:]]+',"", demonetization$stripped_text)

demonetization_clean <- demonetization %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

demonetization_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

data("stop_words")
head(stop_words)

nrow(demonetization_clean)

cleaned_tweet_words <- demonetization_clean %>%
  anti_join(stop_words)

nrow(cleaned_tweet_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

demo_tweets_paired_words <- demonetization %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

demo_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

demo_tweets_separated_words <- demo_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

demo_tweets_filtered <- demo_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

demo_words_counts <- demo_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(demo_words_counts)

demo_words_counts %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the search word - demonetization",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

bingwords <- demonetization_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

print(bingwords)

bingwords %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets with #demonetization",
       y = "contribution of sentiment",
       x = NULL)+
  coord_flip() + theme_bw()
  
positive <- demonetization_clean %>%
  inner_join(get_sentiments("bing") %>% filter(sentiment=='positive')) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
negative <- demonetization_clean %>%
  inner_join(get_sentiments("bing") %>% filter(sentiment=='negative')) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

x1 <- sum(positive[3])
y1 <- sum(negative[3])

bingwords %>%
  group_by(sentiment) %>%
  top_n(50) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets with #demonetization",
       y = "contribution of sentiment",
       x = NULL)+
  coord_flip() + theme_bw()

bingwords %>%
  group_by(sentiment) %>%
  ggplot(aes(sentiment, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Tweets with demonetization",
       y = "contribution of sentiment",
       x = NULL)+
  coord_flip() + theme_bw()


