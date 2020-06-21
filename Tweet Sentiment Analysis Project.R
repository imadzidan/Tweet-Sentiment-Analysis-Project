#loading required libraries

if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(textclean)) install.packages("textclean", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(SentimentAnalysis)) install.packages("SentimentAnalysis", repos = "http://cran.us.r-project.org")
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/sentimentr", "sfeuerriegel/SentimentAnalysis")
pacman::p_load(syuzhet, microbenchmark, RSentiment)


# 3 significant digits
options(digits = 3)



# This function is responsible for performing the initial cleaning of the tweets text
# Function require cleantext package
# function names are self explanatory but some explained.
# for full reference please visit https://github.com/trinker/textclean

cleanText  <- function(text){
  
  text = replace_url(text)
  text = replace_emoticon(text)
  text = replace_tag(text) 
  text = replace_non_ascii(text)
  # Replace contractions with both words
  text = replace_contraction(text)
  text = replace_date(text)
  text = replace_number(text)
  text = replace_html(text)
  text = replace_kern(text)

  return(text)
}

# Function to convert text sentiment to integers
# this is done for validation purposes later on
decodeSentiment<- function(sn) {
  case_when(
    sn == "neutral" ~ 0,
    sn == "negative" ~ -1,
    sn  == "positive" ~ 1
  )
}

# Function to convert integer sentiment to text sentiment
# this is used for the final output
encodeSentiment<- function(sn) {
  case_when(
    sn == 0 ~ "neutral",
    sn < 0  ~ "negative",
    sn > 0  ~ "positive"
  )
}

# Function to interpret predicted sentiments to integer form (-1,0,1) for consistency
matchSentiment<- function(sn) {
  case_when(
    sn == 0 ~ 0,
    sn < 0  ~ -1,
    sn > 0  ~ 1
  )
}


# set working Directory mainly to pickup the datafile from a known location
setEnv <- function(){
  setwd(str_c(getwd(),"/"))
  str_c("Working directory is set. Place data file in this location ",getwd(),"/")
}

# Loading the tweets data provided
load.Data <- function(dataFile){
  
  if (file.exists(dataFile))  {
    df <- read_csv(dataFile)
    return(df)
  } else {
    
    str_c("place file in this location ",getwd(),"/")
  }
  
}

# The check_text function helps as it gives you suggestions on how to clean the text
# This check_text exists in the textClean library
check.Text <- function(df){
  
  # The check_text function helps as it gives you suggestions on how to clean the text
  x <- as.factor(df$text)
  return(check_text(df$text))
  
}

# Function to clean the tweets text and drop unwanted columns.
# Function will create a new column "modeled_text" which is the cleaned version of the text. 
# The clean text will be used to get the sentiment
clean.Data <- function(df){
  df <- df %>% drop_na() %>% mutate(modeled_text = cleanText(text))
  df <- df[-c(2,3)]
  return(df)
}


#partition data to start the text analysis
partition.Data <- function(df){
  # partition dataset. 50-50. 50% for training and 50% for test
  tweets_test_index <- createDataPartition(y = df$sentiment, times = 1,  p = 0.5, list = FALSE)
  tweets_train_set <- df[-tweets_test_index,]
  tweets_test_set <- df[tweets_test_index,]
  return(list(tweets_train_set,tweets_test_set))

  }


# This function will take the text through many algorithms.
# Function will provide stats for each algorithm and pick the best one.
# sentimentr/rinker, vegnitta and syuzhet libraries are used.
train.model <- function(which_df){
  
  #Which dataset to process
  if (which_df == "train") {
    df <- train_set
  }else{
    df <- test_set
  }
  
  #Remove na if exists
  df <- df %>% drop_na()
  
  # Run 4 selected algorithms using Syuzhet package
  # Store result in a dataframe
  syuzhet <- setNames(as.data.frame(lapply(c("syuzhet","bing", "afinn", "nrc"),
                                           function(x) matchSentiment(get_sentiment(df$modeled_text, method=x)))), 
                      c("jockers","bing", "afinn", "nrc"))
  
  pred_tbl_p1 <- data.frame(
    textID = df$textID,
    modeled_text = df$modeled_text,
    syuzhet,
    #include 5th algorithm from another package vegnitte
    vegnitte= matchSentiment(replace_na(analyzeSentiment(df$modeled_text)$SentimentQDAP,0)),
    actual_sentiment = decodeSentiment(df$sentiment),
    stringsAsFactors = FALSE
  )
  
  # Run another algorithm using sentimentr
  (rinker <- with(
    df, 
    sentiment_by(
      get_sentences(modeled_text), question.weight = 0,
      averaging.function = average_weighted_mixed_sentiment,list(textID)
    )
  ) %>% filter(!is.na(textID)))
  
  # Store sentimentr output into a frame
  pred_tbl_p2 <- rinker %>% mutate(sentimentr= matchSentiment(ave_sentiment))
  pred_tbl_p2 <- pred_tbl_p2[,-c(2,3,4)]
  
  #join the the above two outputs into one data frame (Combining results)
  pred_tbl <- pred_tbl_p1 %>% inner_join(pred_tbl_p2,by="textID")
  
  return(pred_tbl)
  
}

# Function to validate predicted sentiment against the actual sentiment.
# sentimentr::validate_sentiment is used
validate.Prediction <- function(df){
  
  nrc_pred <- validate_sentiment(df$nrc, df$actual_sentiment)
  nrc_pred<- nrc_pred %>% 
    mutate(mda=attributes(nrc_pred)$mda,
           mare=attributes(nrc_pred)$mare,method="nrc")
  
  bing_pred <- validate_sentiment(df$bing, df$actual_sentiment)
  bing_pred<- bing_pred %>% 
    mutate(mda=attributes(bing_pred)$mda,
           mare=attributes(bing_pred)$mare,method="bing")
  
  afinn_pred <- validate_sentiment(df$afinn, df$actual_sentiment)
  afinn_pred <- afinn_pred %>%
    mutate(mda=attributes(afinn_pred)$mda,
           mare=attributes(afinn_pred)$mare,method="afinn")
  
  sentimentr_pred <- validate_sentiment(df$sentimentr, df$actual_sentiment)
  sentimentr_pred <- sentimentr_pred %>% 
    mutate(mda=attributes(sentimentr_pred)$mda,
           mare=attributes(sentimentr_pred)$mare,method="sentimentr")
  
  jockers_pred <- validate_sentiment(df$jockers, df$actual_sentiment)
  jockers_pred <- jockers_pred %>% 
    mutate(mda=attributes(jockers_pred)$mda,
           mare=attributes(jockers_pred)$mare,method="jockers")
  
  vegnitte_pred <- validate_sentiment(df$vegnitte, df$actual_sentiment)
  
  vegnitte_pred <- vegnitte_pred %>% 
    mutate(mda=attributes(vegnitte_pred)$mda,
           mare=attributes(vegnitte_pred)$mare,method="vegnitte")
  
  all_pred<- bind_rows(nrc_pred, bing_pred,afinn_pred,sentimentr_pred,jockers_pred)
  
  # Returning a list of two dataframes. One contains all validation
  # whilst the second contains only the best validation.
  return(list(all_pred,all_pred[which.max(all_pred$accuracy),]))
  
}

# Function to perform prediction with the best algorithm found in our case afinn
predict.Sentiment<- function(df){
  
  df <- df %>% drop_na()
  
  syuzhet <- setNames(as.data.frame(lapply(c("bing"),
                                           function(x) matchSentiment(get_sentiment(df$modeled_text, method=x)))), c("sentiment"))
  
  pred <- data.frame(
    textID = df$textID,
    modeled_text = df$modeled_text,
    syuzhet,
    stringsAsFactors = FALSE
  ) 
  pred <- within(pred, sentiment <- encodeSentiment(sentiment))
  
  return(pred)
}

#Clean output by dropping and renaming columns.
clean.output <- function(df,alg,n){
  
  df <- df[c(1,2,n)]
  
  names(df)[names(df)==alg] <- "sentiment"
  
  return(within(df, sentiment <- encodeSentiment(sentiment)))
  
}

set.seed(1974)

#----------------------training the model-------------------------------
  # Set working directory to where the datafiles are
  setEnv()
  
  # pick up the tain.csv file from the location set above
  tweet_train <- load.Data("train.csv")
  
  # The data looks like this
  head(tweet_train,5)
  
  # Function from textclean package that gives recommendation
  # on where to performs cleanups. very very helpful
  Text_cleanup_recommendation <- check.Text(tweet_train)
  
  # Sample of the cleanup recommendations
  Text_cleanup_recommendation
  
  
  # Take the loaded data through a cleaning process.
  # Text column will be prepared to use in the sentiment analysis process 
  tweet_train <- clean.Data(tweet_train)
  
  # Sample the cleaned data now looks like this
  # modeled_text is the text to source of sentiment analysis
  head(tweet_train,5)
  
  # Explore the data.
  
  # The main conclusion from this exploration is that neutral data seems to occupy most of the dataset.
  # The data is very unpredictable. More insights can be drawn but it is very time consuming.
  # In my opinion not much exploration we can do about the data itself in this context.
  # new texts are very unpredictable and can be anything.
  # Exploring the actual data helped in deciding what to clean. 
  
  
  # Show the dataset structure after cleaning
  glimpse(tweet_train)
  
  # Records count broken down by sentiment.
  # Neutral sentiment seems to have the majority of records
  tweet_train %>% group_by(sentiment) %>% summarise(n())
  
  # More information on the breakdown
  by(tweet_train, tweet_train$sentiment, summary)
  
  # Quick graph showing words count per sentiment
  sentiment_words <- tweet_train %>%
    unnest_tokens(word, modeled_text) %>% filter(!word %in% stop_words$word) %>%
    count(sentiment, word, sort = TRUE)
  
  #Tidy text to show a data split by sentiment.
  total_words <- sentiment_words %>%
    group_by(sentiment) %>% 
    summarize(total = sum(n))
  
  sentiment_words <- left_join(sentiment_words, total_words)
  
  # Ploting word counts by sentiment.
  ggplot(sentiment_words, aes(n/total, fill = sentiment)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~sentiment, ncol = 2, scales = "free_y")
  
  
  # create a partition 50% training and 50% test
  # As mentioned before, this betters the validation numbers.
  # Partition function retuns a list of two frames
  dataPartitions <- partition.Data(tweet_train)
  
  # assign the first dataframe in the list to training
  train_set <- dataPartitions[[1]]
  
  # training will run multiple algorthms and returns all results
  train_pred <- train.model("train")
  
  # Sample of the data output looks like this
  head(as_tibble(train_pred),5)
  
  
  # Now validation of the above output will take place.
  # The best result is decided on the accuracy figure.
  # mda (Mean Directional Accuracy) and mare(Mean Absolute Rescaled Error) are 
  # also important, hence their inclusion in the output
  # More explanation found on https://rdrr.io/cran/sentimentr/man/validate_sentiment.html
  validation <- validate.Prediction(train_pred)
  
  # validation process will return a list of two dataframes
  # this is the first dataframe in the list and contains all validations.
  train_pred_validations <- as_tibble(validation[[1]])[,c(1,2,3,4,5,6,7,8)]
  
  # All validation frame look this this
  train_pred_validations
  
  # this is the second dataframe in the list and contains the best validation results
  train_best_prediction <- as_tibble(validation[[2]])[,c(4,6,7,8)]
  
  # best prediction dataframe looks like this
  train_best_prediction

  # Note
  str_c("the best model when training is " ,  train_best_prediction$method , " with accuracy of " , format(round(train_best_prediction$accuracy*100, 3), nsmall = 3),"%")
  
  train_pred <- clean.output(train_pred,train_best_prediction$method,which(colnames(train_pred)==train_best_prediction$method))

  
  # Sample of the final output of the training data set
  head(as_tibble(train_pred),5)
  
  
  #----------------------testing the model-------------------------------
  
  
  # Repeating steps applied when training the model on the test data set
  
  # assign the second dataframe in the list to test
  # this will create the test dataset
  test_set <- dataPartitions[[2]]
  
  test_pred <- train.model("test")
  
  validation <- validate.Prediction(test_pred)
  
  test_pred_validations <- as_tibble(validation[[1]])[,c(1,2,3,4,5,6,7,8)]
  test_pred_validations
  
  
  test_best_prediction <- as_tibble(validation[[2]])[,c(4,6,7,8)]
  test_best_prediction
  
  # Note
  str_c("the best model when testing is " ,  test_best_prediction$method , " with accuracy of " ,format(round(test_best_prediction$accuracy*100, 3), nsmall = 3),"%")
  
  test_pred <- clean.output(test_pred,test_best_prediction$method,which(colnames(test_pred)==test_best_prediction$method)) 
  
  # Sample of the final output of the test data set
  head(as_tibble(test_pred),5)
  
  remove(dataPartitions)
  
  # This concludes the training and test process.
  # The conclusion is that when running algorithms for training dataset the best score came from algorithm bing.
  # However, afinn shows as the best score when running against test data.
  #I have decided to go for bing as the accuracy it produced was higher than the afinn.
  # Final model will run with bing algorithm.
  
  #----------------------running the model on validation data set--------
  
  # load final dataset for prediction
  tweet_validation <- load.Data("validation.csv")
  
  tweet_validation <- clean.Data(tweet_validation)
  
  # the data to be predicted looks like this
  as_tibble(tweet_validation)
  
  # Perform prediction
  prediction <- predict.Sentiment(tweet_validation)
  
  # output the final prediction data frame
  as_tibble(prediction)
  
  

