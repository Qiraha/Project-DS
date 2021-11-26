# Title     : TODO
# Objective : TODO
# Created by: Qiraha
# Created on: 17/11/2021

library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(SnowballC)
library(SentimentAnalysis)
library(RColorBrewer)
library(wordcloud)
library(sentiment)

# load data
raw_data_gephi <- read.csv(file = "data-raw/Ted_Cruz_AOC.csv")
raw_data_gephi <- raw_data_gephi %>%
  filter(twitter_type == "Tweet") %>%
  arrange(desc(Id)) %>%
  select(Id, Label) %>%
  sample_n(100)
train_data <- read.csv(file = "data-raw/train_data.csv")

# get spesific column
all_data <- data.frame(text = raw_data_gephi$Label,
                       sentiment = NA) %>%
  rbind(train_data) %>%
  mutate(id = row_number(), .before = "text")

# split data training & data testing
predict_data_all <- all_data[1:100,]
train_data <- all_data[101:250,]

# cleaning data
temp_clean <- all_data

## remove retweet entities
temp_clean$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", temp_clean$text)
## remove at people
temp_clean$text <- gsub("@\\w+", " ", temp_clean$text)
## remove hastag
temp_clean$text <- gsub("#\\w+", " ", temp_clean$text)
## remove html links
temp_clean$text <- gsub("https://t.co/\\w+", " ", temp_clean$text)
## remove emoticon
temp_clean$text <- gsub('[^\x01-\x7F]', "", temp_clean$text)
## remove dot
temp_clean$text <- gsub('[\\.\\,]', " ", temp_clean$text)
## remove puntuation
temp_clean$text <- gsub('[[:punct:]]', "", temp_clean$text)
## remove control character
temp_clean$text <- gsub('[[:cntrl:]]', " ", temp_clean$text)
## remove digit
temp_clean$text <- gsub('\\d+', "", temp_clean$text)
## remove unnecessary spaces
temp_clean$text <- gsub("[ \t]{2,}", " ", temp_clean$text)
temp_clean$text <- gsub("^\\s+|\\s+$", "", temp_clean$text)
## change to lower case
temp_clean$text <- tolower(temp_clean$text)
temp_clean[temp_clean == ""] <- NA
## remove stop words
temp_clean <- temp_clean %>%
  select(id, text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()

# split clean data training & data testing
train_data_clean <- train_data %>%
  left_join(temp_clean, by = "id") %>%
  select(id, text.y)
colnames(train_data_clean)[2] <- "text"

predict_data_clean <- predict_data_all %>%
  left_join(temp_clean, by = "id") %>%
  select(id, text.y)
colnames(predict_data_clean)[2] <- "text"

# predict all data
predict_result <- predict_data_all

for (j in seq_len(nrow(predict_data_clean))) {
  cat(sprintf("\nProses: (%d / %d)", j, nrow(predict_data_clean)))
  # Executing Process
  data_predict <- predict_data_clean[j,]
  tidy_data <- train_data_clean %>%
    rbind(data_predict)

  tf_idf <- tidy_data %>%
    unnest_tokens(word, text) %>%
    count(id, word, sort = TRUE) %>%
    bind_tf_idf(word, id, n)

  # wdi*wdj
  weight_predict <- tf_idf %>%
    filter(id == data_predict$id)

  weight_train <- data.frame(id = integer(),
                               sum = numeric())

  for (i in seq_len(nrow(train_data_clean))) {
    temp_data <- tf_idf %>%
      filter(id == train_data_clean$id[i])

    join <- weight_predict %>%
      inner_join(temp_data, by = "word") %>%
      mutate(kali = tf_idf.x * tf_idf.y)

    weight_train <- weight_train %>%
      rbind(data.frame(id = train_data_clean$id[i], sum = sum(join$kali)))
  }

  # panjang vector
  weight_square <- tf_idf
  weight_square$tf_idf <- weight_square$tf_idf^2

  vector <- data.frame(id = integer(),
                       sum = numeric(),
                       sqrt = numeric())

  for (i in seq_len(nrow(tidy_data))) {
    temp_data <- weight_square %>%
      filter(id == tidy_data$id[i])

    temp_sum <- sum(temp_data$tf_idf)
    temp_square <- sqrt(temp_sum)

    vector <- vector %>%
      rbind(data.frame(id = tidy_data$id[i],
                       sum = temp_sum,
                       sqrt = temp_square))
  }

  # cosine similarity
  vector_predict <- vector %>% filter(id == data_predict$id)

  cosine <- data.frame(id = integer(),
                       cosine = numeric())
  for (i in seq_len(nrow(train_data_clean))) {
    temp_id <- train_data_clean$id[i]
    temp_weight <- weight_train %>% filter(id == temp_id)
    temp_vector <- vector %>% filter(id == temp_id)

    temp_cosine <- temp_weight$sum / (vector_predict$sqrt * temp_vector$sqrt)

    cosine <- cosine %>%
      rbind(data.frame(id = temp_id,
                       cosine = temp_cosine))
  }

  # knn
  k <- 5

  cek <- cosine %>%
    left_join(train_data, by = "id") %>%
    select(id, cosine, sentiment) %>%
    arrange(desc(cosine)) %>%
    head(k)

  predict_sentiment <- cek %>%
    count(sentiment)
  predict_sentiment <- predict_sentiment$sentiment[which.max(predict_sentiment$n)]

  predict_result$sentiment[j] <- predict_sentiment
}
write.csv(predict_data_all, file = "data-raw/predict_data_all.csv", row.names = FALSE)
write.csv(predict_data_clean, file =  "data-raw/predict_data_clean.csv", row.names = FALSE)
write.csv(predict_result, file =  "data-raw/predict_result.csv", row.names = FALSE)
cat(sprintf("\nSelesai"))

save.image("Main_App.RData")
load('Main_App.RData')