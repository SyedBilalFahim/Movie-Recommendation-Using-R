
# Fetching movies, rating and tag data

library(readr)
library(caret)
library(stringr)
library(qdapTools)
library(data.table)
library(dplyr)
library(textshape)
library(lubridate)
library(ggplot2)

ratings <- read_csv("E:/Fiver Sampath/ratings.csv")
movies<- read_csv("E:/Fiver Sampath/movies.csv")
tags<-  read_csv("E:/Fiver Sampath/tags.csv")
genome_tags<- read_csv("E:/Fiver Sampath/genome-tags.csv")



head(ratings)

# Rating data has 4 columns including the target column of rating. Each movie is defined by a movie id and release of 
# each movie is defined by a time stamp

head(movies)

# Movies has 3 columns including a genre column which is identifying genre for each movie.

head(tags)

# Tag is defining a tag like epic , sci-fi etc for each movie

head(genome_tags)

# This data has the release years of each tag category.

# Data Preparation and cleaning

# The first step requires the cleaning of genres column to more respectable form 
data_clean= movies %>% cbind(mtabulate(str_split(movies$genres,"\\|"))) %>% select(-'(no genres listed)')
head(data_clean)
# Now, we will only select the movies which are rated, will join the movies data with the rating data

rated_movies<- data_clean %>% inner_join(ratings,by ="movieId")



head(rated_movies)

glimpse(movies)
str(rated_movies)

# We have a big data, for further analysis we will be using only a portion of data.
set.seed(1234567)
sample <- sample(c(TRUE, FALSE), nrow(rated_movies), replace=TRUE, prob=c(0.1,0.9))
new_data  <- rated_movies[sample, ]

#Lets keep a validation set too which will be 10% of our sampled data
set.seed(1234567)
sample2 <- sample(c(TRUE, FALSE), nrow(new_data), replace=TRUE, prob=c(0.1,0.9))
valid  <- new_data[sample2, ]
new_data<- new_data[!sample2,]

head(new_data)
# Step 2: Merging tags and genome_tags by tagid

tags_final<- merge(tags,genome_tags,by = "tag")

head(tags_final)

max(data_movie$movieId)


# The next step is to convert this time stamp into a date format
# For this we will be using lubridate r library

new_data$timestamp=as.Date(as.POSIXct(new_data$timestamp, origin="1970-01-01"))
valid$timestamp=as.Date(as.POSIXct(valid$timestamp, origin="1970-01-01"))

# We will do the same with the tags data
tags_final$timestamp=as.Date(as.POSIXct(tags_final$timestamp, origin="1970-01-01"))




# Now that we have successfully prepared our data, we can now do some eda's on this data.
# Lets start with the genre summarization
rated_movies %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  head()
ggplot(table,aes(genres,n))+geom_point()

# We can see that, there are 6 types of films in total and among these action plus adventure films are in big numbers

# Now, we will see the Year wise rating.


avg_ratings<-new_data %>% group_by(year=year(timestamp)) %>% 
  summarise(ratings=mean(rating))


ggplot(avg_ratings,aes(year,ratings))+geom_line()

# We can see a trend here which is on its peak around 2012 but overall we can say that there are very few movies 
# whose imdb rating is greater than 5 lets see the count of such movies
subset_by_rating<- subset(new_data,new_data$rating>3)

head(subset_by_rating)


# Now we will see the genres with max ratings

genres<- subset_by_rating %>% group_by(genres,rating) %>% summarise(n=n()) %>% arrange(desc(n))
genres
# We can see that drama genre has the highest number of 4 and 5 rated films throughout the years.

# Now lets get the info about the users who watched the movies and rated them
new_data %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Users and the number of ratings") +
  xlab("Number of Ratings") +
  ylab("Number of Users")


# We can see that the a very few number of users have actually rated movies.

# Nowwe will see the visualization of the movies itself

new_data %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black") +
  scale_x_log10() + 
  ggtitle("Movies Distribution") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") 

# We can see that the distribution is somewhat right skewed which is probably because many users didn;t watch all the
# Movies hence they didn't rate them.

# Data Split

# Since, we are done with the initial data exploration, we will now go forward with the modeling
# Our goal here is to predict the rating of the movies over time. For this we will be splitting our data into test 
# and train first:

head(data)
set.seed(1234567)
samplenew <- sample(c(TRUE, FALSE), nrow(new_data), replace=TRUE, prob=c(0.8,0.2))
train  <- new_data[samplenew, ]
test   <- new_data[!samplenew, ]
head(train)


# here we have splitted our data in 80 20 split. *0 % of data will be trained and the trained data will be tested on
# remaining 20%

# For the modeling purpose, we will be using the linear regression model to predict the ratings of the movies first.
# Target variable would be the rating and all other variables will be predictor variables. We will be evaluating 
# the model on the basis of rmse and mse values and rsquared values


# Lets define model evaluation functions
#Mean Absolute Error (MAE)
mae <- function(ratings_original, ratings_predicted){
  mean(abs(ratings_original -  ratings_predicted))
}

# Mean Squared Error (MSE)
MSE <- function(ratings_original,ratings_predicted){
  mean((ratings_original -  ratings_predicted)^2)
}

# Root Mean Squared Error (RMSE)
RMSE <- function(ratings_original,ratings_predicted){
  sqrt(mean((ratings_original -  ratings_predicted)^2))
}

min(new_data$rating)

train<- train %>% select(-title,-genres)
plot(rating ~., data = train)

# Model:
# We will exclude genres column


# For modeling purpose we will be using matrix regularization model to get the ratings

train <- train %>% 
  as.matrix()

library(recosystem)

set.seed(123232323)
str(train)
# the first and the most important step isto convert the train and test data to reco input format for training our model

train_data <-  with(train, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating,
                                              ))
test_data  <-  with(test,  data_memory(user_index = userId, 
                                           item_index =movieId, 
                                           rating     = rating))





# Now we will create a reco model and than will test it
r <-  recosystem::Reco()



# Now we will train our model with tuned parameters
  
tune = r$tune(train_data,                                   
                   opts = list(dim      = c(10, 20),        
                               costp_l2 = c(0.05, 0.1),         
                               costq_l2 = c(0.05, 0.1),         
                               costp_l1 = 0,                    
                               costq_l1 = 0,                    
                               lrate    = c(0.05, 0.1),         
                               nthread  = 4,                    
                               niter    = 5,                   
                               verbose  = TRUE))    


rm(data_clean,movies,rated_movies,ratings)
rm(new_data)

  model<- r$train(train_data, opts = c(tune$min,                      
                             nthread = 4))
  
  
# Calculate the predicted values  
pred<-  r$predict(test_data, out_memory())


# Lets have the results

# Rmse
RMSE(test$rating,pred)

#Mae
mae(test$rating,pred)


# mse
MSE(test$rating,pred)


# Now that we are done with initial modeling, we gonna check our model with the data and valid data we created to validate
# our data.

reco_new<-  with(new_data, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validat_reco  <-  with(valid, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))
tune2 = r$tune(reco_new,                                   
              opts = list(dim      = c(10, 20),        
                          costp_l2 = c(0.05, 0.1),         
                          costq_l2 = c(0.05, 0.1),         
                          costp_l1 = 0,                    
                          costq_l1 = 0,                    
                          lrate    = c(0.05, 0.1),         
                          nthread  = 4,                    
                          niter    = 5,                   
                          verbose  = TRUE))    


model<- r$train(reco_new, opts = c(tune2$min,niter=100,                      
                                     nthread = 4))

# Calculate the predicted values  
pred2<-  r$predict(validat_reco, out_memory())


# Lets have the results

# Rmse
RMSE(valid$rating,pred2)
#Mae
mae(valid$rating,pred2)


# mse
MSE(valid$rating,pred2)


top10<-

tibble(title = valid$title, rating = pred2) %>%
  arrange(-rating) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

library(MASS)
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)


# Linear Model
str(train)
train<- train %>%select(-title,-genres)
lm<- step(lm(rating~.,data=train),direction = "both")
summary(lm)
vif(lm)


data_x= train[c(1:21,23)]                             

var <- cor(data_x)                                     

var_inv <- ginv(var)                                  

colnames(var_inv) <- colnames(data_x)                 
rownames(var_inv) <- colnames(data_x)

corrplot(var_inv,method='number',is.corr = F)         


# testing
pre <- predict.lm(, newdata=plotting.data)