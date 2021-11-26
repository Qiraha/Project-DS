# Title     : TODO
# Objective : TODO
# Created by: Qiraha
# Created on: 15/11/2021

library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# load data
original_data <- read.csv(file = "data-raw/data_twitter_kaggle.csv")

# get data
original_data <- original_data %>%
  filter(Sentiment == "Extremely Positive") %>%
  arrange(UserName) %>%
  head(75) %>%
  rbind(original_data %>%
          filter(Sentiment == "Extremely Negative") %>%
          arrange(UserName) %>%
          head(75) %>%
          rbind(original_data %>%
                  filter(Sentiment == "Extremely Positive") %>%
                  arrange(desc(UserName)) %>%
                  head(25) %>%
                  rbind(original_data %>%
                          filter(Sentiment == "Extremely Negative") %>%
                          arrange(desc(UserName)) %>%
                          head(25))))
csv <- original_data[1:150,]
# get spesific column
data <- data.frame(text = original_data$OriginalTweet, sentiment = original_data$Sentiment) %>%
  mutate(id = row_number(), .before = text)
data$sentiment <- ifelse(data$sentiment == "Extremely Positive", "Positive", "Negative")

# split data training & data testing
train_data <- data[1:150,]
train_data %>% select(-id) %>% write.csv("data-raw/train_data.csv", row.names = FALSE)
data_testing <- data[151:nrow(data),]

# cleaning data
temp_clean <- data

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
train_clean <- train_data %>%
  left_join(temp_clean, by = "id") %>%
  select(id, text.y)
colnames(train_clean)[2] <- "text"

test_clean <- data_testing %>%
  left_join(temp_clean, by = "id") %>%
  select(id, text.y)
colnames(test_clean)[2] <- "text"

# predict all data testing
compare_accuracy <- data.frame(k = numeric(),
                               accuracy = numeric(),
                               precision = numeric(),
                               recall = numeric(),
                               f_measure = numeric())

for (j in c(3, 5, 7, 9, 11, 15, 21, 31, 51)) {
  result_predict <- data.frame(id = integer(),
                               predict = character())
  for (i in seq_len(nrow(test_clean))) {
    cat(sprintf("\nProses k-%d: (%d / %d)", j, i, nrow(test_clean)))
    # Executing Process
    data_predict <- test_clean[i,]
    tidy_data <- train_clean %>%
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
    
    for (i in seq_len(nrow(train_clean))) {
      temp_data <- tf_idf %>%
        filter(id == train_clean$id[i])
      
      join <- weight_predict %>%
        inner_join(temp_data, by = "word") %>%
        mutate(kali = tf_idf.x * tf_idf.y)
      
      weight_train <- weight_train %>%
        rbind(data.frame(id = train_clean$id[i], sum = sum(join$kali)))
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
      temp_sqrt <- sqrt(temp_sum)
      
      vector <- vector %>%
        rbind(data.frame(id = tidy_data$id[i],
                         sum = temp_sum,
                         sqrt = temp_sqrt))
    }
    
    # cosine similarity
    vector_predict <- vector %>% filter(id == data_predict$id)
    
    cosine <- data.frame(id = integer(),
                         cosine = numeric())
    for (i in seq_len(nrow(train_clean))) {
      temp_id <- train_clean$id[i]
      temp_bobot <- weight_train %>% filter(id == temp_id)
      temp_vector <- vector %>% filter(id == temp_id)
      
      temp_cosine <- temp_bobot$sum / (vector_predict$sqrt * temp_vector$sqrt)
      
      cosine <- cosine %>%
        rbind(data.frame(id = temp_id,
                         cosine = temp_cosine))
    }
    
    # knn
    k <- j
    
    cek <- cosine %>%
      left_join(train_data, by = "id") %>%
      select(id, cosine, sentiment) %>%
      arrange(desc(cosine)) %>%
      head(k)
    
    sentiment_predict <- cek %>%
      count(sentiment)
    sentiment_predict <- sentiment_predict$sentiment[which.max(sentiment_predict$n)]
    
    result_predict <- result_predict %>%
      rbind(data.frame(id = data_predict$id,
                       predict = sentiment_predict))
  }
  result_predict <- data_testing %>%
    left_join(result_predict, by = "id")
  
  #testing accuracy
  compare_predict <- result_predict %>%
    mutate(interpretation = ifelse(sentiment == predict, 1, 0),
           TP = ifelse(sentiment == "Positive" & predict == "Positive", 1, 0),
           FP = ifelse(sentiment == "Negative" & predict == "Positive", 1, 0),
           TN = ifelse(sentiment == "Negative" & predict == "Negative", 1, 0),
           FN = ifelse(sentiment == "Positive" & predict == "Negative", 1, 0))
  
  accuracy <- (compare_predict %>%
                 filter(interpretation == 1) %>%
                 count)$n /
    (compare_predict %>% count)$n
  
  precision <- sum(compare_predict$TP) /
    (sum(compare_predict$TP) + sum(compare_predict$FP))
  
  recall <- sum(compare_predict$TP) /
    (sum(compare_predict$TP) + sum(compare_predict$FN))
  
  f_measure <- 2 * ((precision * recall) / (precision + recall))
  
  compare_accuracy <- compare_accuracy %>%
    rbind(data.frame(k = j,
                     accuracy = accuracy,
                     precision = precision,
                     recall = recall,
                     f_measure = f_measure))
}

#save.image("AppTesting.RData")
#load('AppTesting.RData')