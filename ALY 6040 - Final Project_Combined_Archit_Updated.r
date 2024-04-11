
# Importing the dataset 
Tweets.data <-read.csv("/Users/architbarua/Desktop/ALY6040/Tweets.csv",
                       header=TRUE,sep=",")

# Understanding the dataset
str(Tweets.data)
summary(Tweets.data)
glimpse(Tweets.data)
headTail(Tweets.data)

# Data Pre-Processing & Data Cleaning

# Checking data is clean?
colSums(is.na(Tweets.data)) # check & Returns the number of missing values in each column
sum(is.na(Tweets.data)) # Counts missing values in entire data frame
colSums(Tweets.data==0) #Using colSums function to find the total number of Zero records in each column
plot_missing(Tweets.data, title="Missing Data Profile",geom_label_args = list("size" = 2, "label.padding" = unit(0.1, "lines")))

# Dropping tweet_id column (numeric)
drop1<-c("tweet_id" , "airline_sentiment_confidence" ,"negativereason_confidence")
Tweets.data<-Tweets.data[,!(names(Tweets.data) %in% drop1)]


# Replacing NA values with the mean of the column ( NOT REQUIRED)
Tweets.data$negativereason_confidence[is.na(Tweets.data$negativereason_confidence)]<-round (mean(Tweets.data$negativereason_confidence,na.rm=TRUE),2)

# Checking for duplicated rows and removing them
duplicated(Tweets.data)
anyDuplicated(Tweets.data)
Tweets.data<-Tweets.data[!duplicated(Tweets.data), ]

# Seperating date and time into two columns
Tweets.data<-Tweets.data %>%
  separate(tweet_created, c("date", "time"), " ")

# Hour from time
Tweets.data$hour <- hour(hms(Tweets.data$time))

# Replacing hours to Morning, Afternoon,Evening and Night
# 0-5 night
# 6-11 morning
# 12-17 afternoon
# 18-24 evening
Tweets.data <- Tweets.data %>% 
  mutate(hour = ifelse(hour %in% c(0:5), "Night",
                       ifelse(hour %in% c(6:11), "Morning",
                              ifelse(hour %in% c(12:17), "Afternoon", "Evening"))))

# Creating day column
Tweets.data$day<-Tweets.data$date
Tweets.data$day[Tweets.data$date == '2015-02-16'] <- 'Monday'
Tweets.data$day[Tweets.data$date == '2015-02-17'] <- 'Tuesday'
Tweets.data$day[Tweets.data$date == '2015-02-18'] <- 'Wednesday'
Tweets.data$day[Tweets.data$date == '2015-02-19'] <- 'Thursday'
Tweets.data$day[Tweets.data$date == '2015-02-20'] <- 'Friday'
Tweets.data$day[Tweets.data$date == '2015-02-21'] <- 'Saturday'
Tweets.data$day[Tweets.data$date == '2015-02-22'] <- 'Sunday'
Tweets.data$day[Tweets.data$date == '2015-02-23'] <- 'Monday'
Tweets.data$day[Tweets.data$date == '2015-02-24'] <- 'Tuesday'

# Relocating the columns
Tweets.data<-Tweets.data %>% relocate(day,.after = date)
Tweets.data<-Tweets.data %>% relocate(hour,.after = time)

# Changing the datatypes
Tweets.data$airline_sentiment<-as.factor(Tweets.data$airline_sentiment)
Tweets.data$airline<-as.factor(Tweets.data$airline)
Tweets.data$day<-as.factor(Tweets.data$day)
Tweets.data$hour<-as.factor(Tweets.data$hour)
str(Tweets.data)

# Descriptive Statistics for entire dataset
formattable(describe(Tweets.data), 
            caption = "Descriptive statistics summary of the Twitter US Airline Sentiment Dataset")

# Histogram Distribution

# Airline Sentiment Confidence
ggplot(Tweets.data, aes(x=airline_sentiment_confidence)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Airline Sentiment Confidence Distribution",x="Airline Sentiment Confidence", y = "Count")

# Negative Reason Confidence
ggplot(Tweets.data, aes(x=negativereason_confidence)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Negative Reason Confidence Distribution",x="Negative Reason Confidence", y = "Count")

# Retweet Count
ggplot(Tweets.data, aes(x=retweet_count)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Retweet Count Distribution",x="Retweet Count", y = "Count")

# Correlation Matrix
corr<-Tweets.data %>% select(airline_sentiment_confidence,negativereason_confidence,retweet_count)
corr_matrix <- cor(corr)
corr_matrix

ggcorrplot(corr_matrix, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3.5, 
           colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Correlation Plot of Selected Variables")

# Data Profiling Report
create_report(Tweets.data)

# Data Visualizations
# Graph 1 - Sentiment analysis by airline
df1<-Tweets.data %>% group_by(airline, airline_sentiment) %>% summarize(count=n())

In2 <- hchart(df1, 'column',
              hcaes(x = 'airline', y = 'count', group = 'airline_sentiment'),
              stacking = "normal") %>%
  hc_colors(c("#CD0000", "#EEE8CD", "#698B69")) %>%
  hc_xAxis(title = "Airline") %>%
  hc_yAxis(title = "Count") %>%
  hc_title(text = "Sentiment Analysis by Airline")
In2

# Graph 2 interactive - Tweet Count by Day and Hour
df2 <- Tweets.data %>% group_by(day,hour) %>% summarize(count=n())

In2 <- hchart(df2, 'column',
              hcaes(x = 'day', y = 'count', group = 'hour')) %>%
  hc_colors(c("#B0E2FF", "#FF8247", "#FDE725", "#00008B")) %>%
  hc_xAxis(title = "Day") %>%
  hc_yAxis(title = "Count") %>%
  hc_title(text = "Tweet Count by Day and Hour")
In2

# Graph 3 interactive - Negative Reason Counts by Airline
df3 <- Tweets.data %>%
  filter(!is.na(negativereason) & negativereason != "") %>%
  group_by(negativereason, airline) %>%
  summarize(count = n()) %>%
  filter(count > 0) %>%
  arrange(desc(count))

In3 <- hchart(df3, 'bar', hcaes(x = negativereason, y = count, group = airline)) %>%
  hc_colors(c("#1874CD", "#B22222", "#FFD700", "#00BFFF","#8B8682","#FF0000")) %>%
  hc_xAxis(title = "Negative Reasons") %>%
  hc_yAxis(title = "Count") %>%
  hc_title(text = "Negative Reason Counts by Airline")
In3

# Graph 4 - Timeline of Daily Cumulative Tweets

dailyTweetscum <- Tweets.data %>% group_by(date) %>% dplyr::summarise(count = n()) %>%
  mutate(cuml = cumsum(count))

p1<-  ggplot(data=dailyTweetscum , aes(x = date, y = cuml)) +
  geom_point(size=2.0 ,color = "skyblue")+
  theme_classic()+
  ggtitle('Daily Cumulative Tweets') +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

ggplotly(p1)

# Graph 5 - Density graph of Negative tweets

negativeTweets <- Tweets.data %>% filter(airline_sentiment=="negative")
negativeTweets <- negativeTweets %>% group_by(airline , date) %>% dplyr::summarise(count = n()) 

p2 <- ggplot(negativeTweets, aes(x = count, fill = airline)) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribution of Negative Tweets by Airline") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p2

# Graph 6 - Timeline of Daily Tweets for Each Airline
dailyTweets <- Tweets.data %>% group_by(airline,date) %>% dplyr::summarise(count = n())
dailyTweets$date <- as.Date(dailyTweets$date)

p3 <-dailyTweets %>% ggplot(aes(x = date, y = count, 
                                group = airline,
                                color = airline)) +
  theme_bw()+
  geom_line() +
  geom_point() +
  ggtitle("Daily Tweets per Airline") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  transition_reveal(date)

animate(p3, renderer = gifski_renderer())

# Graph 7 - Location Wise Tweets - Circular Bar chart
location <- Tweets.data %>% group_by(tweet_location) %>%
  dplyr::summarise(count=n()) %>% arrange(desc(count)) %>% filter(!is.na(tweet_location) & tweet_location != "") %>%  top_n(10)
location

ggplot(location, aes(tweet_location, count, fill = tweet_location)) +
  geom_col(position = "dodge") +
  coord_polar() +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  scale_fill_viridis_d() +
  ggtitle("Circular Bar chart- Top 10 Location Wise Tweets") +
  xlab("Location") +
  theme(axis.text.x = element_text(hjust = 1))

# Graph 8 - Treemap of Negative reasons 
data = Tweets.data %>% group_by(negativereason) %>% summarise(count = n())%>%  filter(!is.na(negativereason) & negativereason != "")

ggplot(data, aes(area = count, fill = negativereason, label = negativereason)) +
  geom_treemap() +
  scale_fill_brewer(name = "Negative Reasons",palette = "Set3") +
  geom_treemap_text(family = "Comic Sans MS", colour = "black", place = "centre", reflow = T) +
  theme_void() +
  ggtitle("Treemap - Negative reason") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Comic Sans MS", color = "black")) +
  guides(fill = FALSE) +
  coord_fixed(ratio = 0.75)

# Graph 9 - Pie charts with Distribution of Sentiments by Airline
sentiment_summary <- Tweets.data %>%
  group_by(airline, airline_sentiment) %>%
  summarise(n = n()) %>%
  mutate(percent = n / sum(n) * 100)
pie_charts <- ggplot(sentiment_summary, aes(x = "", y = percent, fill = airline_sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ airline, nrow = 2) +
  theme_void() +
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Sentiments by Airline") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Comic Sans MS", color = "black"),
        strip.text = element_text(size = 16, family = "Comic Sans MS", color = "black"),
        text = element_text(family = "Comic Sans MS", color = "black")) +
  scale_fill_manual(name = "Sentiment",
                    values = c("#CD0000", "#EEE8CD", "#698B69"),
                    labels = c("Negative", "Neutral","Positive")) +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_stack(vjust = 0.5))

print(pie_charts)

# Graph 10 - Wordcloud of words used in tweets
Tweets.data %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  dplyr::count(word, name = "n") %>%
  arrange(desc(n)) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Dark2")))

# Graph 11 - Top 20 words in Tweets
colour <- c(
  "#E31A1C",
  "green4",
  "#6A3D9A", 
  "#FF7F00", "gold1",
  "skyblue2","dodgerblue2", "#FB9A99",
  "palegreen2",
  "#CAB2D6", 
  "#FDBF6F", 
  "khaki2",
  "maroon",  "green1", "orchid1","deeppink1", "steelblue4",
  "darkturquoise", "yellow3",
  "brown"
)

bar <- Tweets.data %>%
  unite(text_combined, text, sep = " ") %>%
  select(text_combined) %>%
  unnest_tokens(word, text_combined) %>%
  filter(!grepl("^[0-9]+$", word)) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(color = "black", fill = colour) +
  coord_flip() +
  labs(x = "Words", y = "Frequency", title = "Top 20 Most Frequent Words in Tweets") +
  theme_bw() +
  theme(text = element_text(family = "Comic Sans MS", color = "black"))

ggplotly(bar)

# Graph 12 -  Ridge chart - Tweet Text Length by Sentiment
Tweets.data$text_length <- nchar(Tweets.data$text)
Tweets.data %>%
  ggplot(aes(x = text_length, y = airline_sentiment, fill = airline_sentiment)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01,alpha=0.5) +
  scale_fill_manual(name = "Sentiment",labels = c("Negative", "Neutral","Positive"),values = c("#CD0000", "#EEE8CD", "#698B69")) +
  labs(x = "Text Length", y = "Sentiment", title = "Tweet Text Length by Sentiment") +
  theme_minimal() +
  theme(text = element_text(family = "Comic Sans MS", color = "black"))

############################################. TEXT PRE-PROCESSING #############################################

# Convert Tweets data to UTF-8 encoding
tweets_utf8 <- iconv(Tweets.data$text, to = "UTF-8")

# Create a Corpus object from the text data
corpus_text <- Corpus(VectorSource(tweets_utf8))

# Inspect the first five documents in the Corpus object
inspect(corpus_text[1:5])

# Convert all text to lowercase
corpus_lc <- tm_map(corpus_text, tolower)

# Remove all punctuation from the text
corpus_np <- tm_map(corpus_lc, removePunctuation)

# Remove all numbers from the text
corpus_nn <- tm_map(corpus_np, removeNumbers)

# Remove common English words from the text
corpus_sw <- tm_map(corpus_nn, removeWords, stopwords('english'))

# Remove additional words such as airline-specific terms from the text
corpus_asw <- tm_map(corpus_sw, removeWords, c('flight','get','plane','flights','flightl', 
                                               'i', 'day', 'im', 'cant', 'can', 'now', 'just',
                                               'will', 'dont', 'ive', 'got', 'much'))

# Remove any extra white space from the text
corpus_ws <- tm_map(corpus_asw, stripWhitespace)

# Reduce each word to its base form
corpus_stem <- tm_map(corpus_ws, stemDocument)

# Create a TermDocumentMatrix from the pre-processed text
tdm <- TermDocumentMatrix(corpus_stem)

# Remove sparse terms from the TermDocumentMatrix
tdm <- removeSparseTerms(tdm, sparse = 0.999)

# Convert the resulting tdm to a Matrix
tdm_mat <- as.matrix(tdm)
tdm_mat[1:7, 1:7]

# Compute the frequency of each word in the Corpus object
word_freq <- rowSums(tdm_mat)

# Select words with a frequency of more than 500
freq_words <- subset(word_freq, word_freq >= 500)

# Sort the selected words in decreasing order
sorted_freq_words <- sort(freq_words, decreasing = T)

# Create a bar plot of the selected words with their frequency
barplot(sorted_freq_words, col = rainbow(38), las = 2,
        main = "Plot of words with frequency more than 500",
        xlab = "Words", ylab = "Frequency", border = "black")

# Create a DocumentTermMatrix from the pre-processed text
dtm <- DocumentTermMatrix(corpus_stem)

# Remove sparse terms from the DocumentTermMatrix
dtm_clean <- removeSparseTerms(dtm, 0.999)

# Convert the resulting matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm_clean))
colnames(dtm_df) <- make.names(colnames(dtm_df))

# Add the airline sentiment labels as a factor variable to the data frame
dtm_df$airline_sentiment <- Tweets.data$airline_sentiment

# Convert the airline sentiment labels to a factor variable
dtm_df$airline_sentiment <- as.factor(dtm_df$airline_sentiment)

# Split the data into a training set and a testing set
set.seed(222)
split <- sample(2, nrow(dtm_df), prob = c(0.8,0.2), replace = TRUE)
train_set <- dtm_df[split == 1,]
test_set <- dtm_df[split == 2,]


#Compute the baseline accuracy for the training set
train_set_baseline_acc <- prop.table(table(train_set$airline_sentiment))

#Compute the baseline accuracy for the testing set
test_set_baseline_acc <- prop.table(table(test_set$airline_sentiment))

#Display the baseline accuracies for both sets
train_set_baseline_acc
test_set_baseline_acc

#Removing Duplicacy#

#Display column names of train_set data frame
names(train_set)

#Rename any duplicate variables to ensure unique names
colnames(train_set)[duplicated(colnames(train_set))] <- paste0(colnames(train_set)[duplicated(colnames(train_set))], "_2")
names(test_set) <- names(train_set)


########################################## Decision Tree ################################################

# Fit a decision tree classifier using the training set
dt_classifier <- rpart(airline_sentiment ~ ., data = train_set, method = "class")

# Plot the decision tree
rpart.plot(dt_classifier)

# Display a summary of the decision tree
summary(dt_classifier)

# Predict the airline sentiment using the decision tree classifier and the testing set
dt_predict <- predict(dt_classifier, newdata = test_set, type = "class")

# Compute the accuracy of the prediction on the testing set
dt_accuracy <- accuracy(dt_predict, test_set$airline_sentiment)
dt_accuracy

# Evaluate model performance
conf_mat1 <- confusionMatrix(dt_predict,test_set$airline_sentiment , mode="everything")
conf_mat1

############################################## Random Forest##############################################

install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(airline_sentiment ~ ., data = train_set, ntree = 20)
print(rf_model)

#Predict on Train set
rf_predict1 <- predict(rf_model, newdata=train_set, type="class")
accuracy(rf_predict1,train_set$airline_sentiment)

# Predict on test data
rf_predict2 <- predict(rf_model, newdata=test_set , type="class")
accuracy(rf_predict2,test_set$airline_sentiment)

# Evaluate model performance
conf_mat <- confusionMatrix(rf_predict2,test_set$airline_sentiment , mode="everything")
conf_mat


#################################### KNN #########################################

# Load required libraries
library(caret)

# Split the data into a training set and a testing set
set.seed(222)
split <- sample(2, nrow(dtm_df), prob = c(0.8, 0.2), replace = TRUE)
train_set <- dtm_df[split == 1, ]
test_set <- dtm_df[split == 2, ]

# Prepare the training set and test set without the sentiment column
train_data <- train_set[, -ncol(train_set)]
test_data <- test_set[, -ncol(test_set)]

# Prepare the target variable for the training set
train_labels <- train_set$airline_sentiment

# Normalize the data
normalize <- preProcess(train_data, method = c("center", "scale"))
train_data_norm <- predict(normalize, train_data)
test_data_norm <- predict(normalize, test_data)

# Create a data frame for training data and labels
train_df <- cbind(train_data_norm, airline_sentiment = train_labels)

# Define the hyperparameter search grid
k_grid <- expand.grid(k = seq(5, 30, by = 1))
control <- trainControl(method = "cv", number = 5, search = "grid")

# Train the model using the training set with hyperparameter tuning
knn_model <- train(airline_sentiment ~ .,
                   data = train_df,
                   method = "knn",
                   trControl = control,
                   tuneGrid = k_grid,
                   preProcess = c("center", "scale"))

# Print the best hyperparameter values
print(knn_model$bestTune)

# Predict the test set labels
knn_predict_test <- predict(knn_model, newdata = test_data_norm)

# Compute the accuracy for the test set
test_accuracy <- mean(knn_predict_test == test_set$airline_sentiment)
print(test_accuracy)

# Evaluate model performance
conf_mat1 <- confusionMatrix(knn_predict_test, test_set$airline_sentiment)
conf_mat1


################################# SVM #############################################

library(e1071)

svm_model<- svm(airline_sentiment ~ .,
                
                data = train_set,
                
                type = "C-classification",
                
                kernel = "linear",
                
                scale = FALSE)



# predict on the testing set
svm_pred <- predict(svm_model, newdata = test_set)

# evaluate the performance of the model
conf_mat4 <- confusionMatrix(svm_pred, test_set$airline_sentiment)
conf_mat4
svm_f1_score <- F1_Score(test_set$airline_sentiment, svm_pred)
svm_f1_score





