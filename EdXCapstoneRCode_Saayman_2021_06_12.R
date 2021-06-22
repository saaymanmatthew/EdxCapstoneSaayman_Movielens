#Data Science Capstone project June 2021, Matthew Saayman 

# course provided code. My code begins further down at line 63

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



#MY CODE BEGINS HERE - Matthew Saayman, June 2021, EDX Data Science Capstone
####################################################################
#load appropriate libraries 
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(lubridate)
library(tinytex)
library(knitr)
#create a test and training set with 50% of the data assigned to each set
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

#ensure the same users and movies in the test set appear in the training set
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

#explore the data, beginning with movie and user counts

topedx <- head(edx_train)
head(edx)
tail(edx)
str(edx)

#compute three new columns for both test and train set
 
#First, compute a year_of_rating column. This is the year that the rating was made

edx_test$year_of_rating <- as.numeric(str_sub(round_date(as_datetime(edx_test$timestamp, origin="1970-01-01"), unit = "year"),1,4))
edx_train$year_of_rating <- as.numeric(str_sub(round_date(as_datetime(edx_train$timestamp, origin="1970-01-01"), unit = "year"),1,4))

#Second computer a year column. This is the year that the movie was released.

edx_test$year <- as.numeric(str_sub(edx_test$title, -5,-2))
edx_train$year <- as.numeric(str_sub(edx_train$title, -5,-2))

#Finally, compute a year difference column. This is the difference between the year of the user's rating and the year
# the movie was released

edx_test$yeardif <- edx_test$year_of_rating - edx_test$year
edx_train$yeardif <- edx_train$year_of_rating - edx_train$year

#computer a "weekday column" and a weekdayorder column for ordering the data

edx_test$weekday <- weekdays(round_date(as_datetime(edx_test$timestamp, origin="1970-01-01")))
edx_train$weekday <- weekdays(round_date(as_datetime(edx_train$timestamp, origin="1970-01-01")))
edx_test$weekdayorder <- ifelse(edx_test$weekday == "Sunday", 0, 
                                ifelse(edx_test$weekday == "Monday", 1,ifelse(edx_test$weekday == "Tuesday", 2,
                                                                              ifelse(edx_test$weekday == "Wednesday", 3,
                                                                                     ifelse(edx_test$weekday == "Thursday", 4,
                                                                                            ifelse(edx_test$weekday == "Friday", 5,
                                                                                                   ifelse(edx_test$weekday == "Saturday", 6,"")))))))
                                                                              
                                                                        

 
edx_train$weekdayorder <- ifelse(edx_train$weekday == "Sunday", 0, 
                                ifelse(edx_train$weekday == "Monday", 1,ifelse(edx_train$weekday == "Tuesday", 2,
                                                                              ifelse(edx_train$weekday == "Wednesday", 3,
                                                                                     ifelse(edx_train$weekday == "Thursday", 4,
                                                                                            ifelse(edx_train$weekday == "Friday", 5,
                                                                                                   ifelse(edx_train$weekday == "Saturday", 6,"")))))))


#computer an "hour column"
edx_test$hour <- str_sub(round_date(as_datetime(edx_test$timestamp, origin="1970-01-01"), unit = "hour"), -8,-7)
edx_train$hour <- str_sub(round_date(as_datetime(edx_train$timestamp, origin="1970-01-01"), unit = "hour"), -8,-7)


#Now we can further explore the data
#Let's look if there are patterns related to these new variables

#Plotting the difference between the year of the movie release and 
#year of rating suggests there is some evidence of a time effect

edx_train %>% 
  group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Figure 1- Average movie rating in each movie-release year")

edx_train %>% 
  group_by(year_of_rating) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_of_rating, rating)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Figure 2- Average movie rating in each year of movie rating")


edx_train %>% 
  group_by(yeardif) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(yeardif, rating)) +
  geom_point() +
  geom_smooth() +
ggtitle("Figure 3- Average movie rating by year difference")

#The yeardif seems to be the most revealing chart. Not only is there a clear pattern,
#but the data also appears to more closely fit to the line

#What about examining by hour of the day? 

sample_n(edx_train, 10000)  %>% 
  group_by(hour) %>%
  summarize(count = n()) %>%
  ggplot(aes(hour,count)) +
  geom_col() +
  ggtitle("Figure 4 - Number of ratings by hour of the day, sample = 10,000")
  
#Above we can see that the busiest times for movie ratings are in the evening 
#(7 pm to 10 pm, or 19 to 22 on the chart)
#Least busy hour is 10 am

edx_train  %>%
  group_by(hour) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(hour,rating)) +
geom_point() +
  ggtitle("Figure 5 - Average rating by hour of the day")


#The above chart shows that the average rating per movie is affected by time of day

#Next, we can look at the effect of hte weekday. 

edx_train %>% 
  group_by(weekday) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = (weekday), y= count)) +
  geom_col() +
  ggtitle("Figure 6 - Number of ratings by weekday")

#The above code shows that certain days of the week are busier for movie ratings. 
#Monday and tuesday.  
#the following code shows that Saturdays have the highest average ratings

edx_train %>% 
  group_by(weekday) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(weekday, rating)) +
  geom_point()  +
  ggtitle("Figure 7 - Average ratings for each week day")

#So far the code above has explored elements related to time, including year, day of the week, and hour of the day.
#What about examining the genre column? 

#First let's take a random sample of 1000 and plot average rating by genre

sample_n(edx_train, 1000) %>%
  group_by(genres) %>%
  summarize(rating = mean(rating), n = n()) %>%
  ggplot(aes(rating,genres)) +
  geom_point()+
 
#The plot generated with the above code makes it difficult to observe a pattern for genres due to the
#numerous combinations of different genres for each movie. What about isolating each individual genre?

#Isolate genres

#What is the highest number of combinations of genres? 
#Answer will be presence of "|" (the separator) + 1

pattern <- (str_count(edx_train$genres, pattern = "[|]"))
max(pattern)
edx_train$genres[which.max(pattern)]
max(pattern) + 1 
edx_train$title[which.max(pattern)]

edx_train[which.max(pattern)]
edx_train[edx_train$title == "Host, The (Gwoemul) (2006)"]
# The Host (2006) has 8 genres

#Now let's try to find the unique values for genre and plot their occurence in the dataset
mygen <- unique(edx_train$genres)
mygen <- data.frame(mygen = mygen)
mygen <- separate(mygen, mygen, into = c("Gen1","gen2","gen3","gen4","gen5","gen6","gen7","gen8"), sep = "[|]") 
mygen <- paste(c(mygen$Gen1,mygen$gen2,mygen$gen3,mygen$gen4,mygen$gen5,mygen$gen6,mygen$gen7), sep = ",")
allgenres <- unique(mygen)

#count occurences of different genres using sapply
#The function works as follows:
#For each value in the list 'genreacounts', search for it in the column of 'genres'
#Then, provide a sum of all occurences for that value

genrecounts <- sapply(allgenres, function(x){
  a <- str_detect(edx_train$genres,x)
  sum(a)
})

as.vector(genrecounts)

#plot the count of different genres

genrecounts <- data.frame(genre = allgenres, count = as.vector(genrecounts))
genrecounts <- genrecounts %>% arrange(desc(count))
ggplot(genrecounts,aes(reorder(genre,count),count)) + geom_col() + ggtitle("Figure 8 Count of movie genres")

#The most common genres are Dramas, Comedies, Action, Thriller, Adventure, Romance,
#Science Fiction, Crime, Fantasy, Children
#what patterns can be observed looking at individual genres? 
#We can begin by looking at the top 5
 
#The code below filters according to the presence of one of the top 5 genres in the genre column,
#Then, the appearance of the top genre, Drama, results in that record recoded as Drama
#If it is not a drama, then the next value searched for is Comedy, then Action, and so on,
 
sample_n(edx_train,10000) %>% 
  group_by(userId, genres) %>% 
  filter(grepl('Drama',genres) |grepl('Comedy',genres) |grepl('Action',genres) 
         |grepl('Thriller',genres) |grepl('Adventure',genres)) %>%
  mutate(onegenre = if_else(str_detect(genres,'Drama') == TRUE,'Drama',
                            if_else(str_detect(genres,'Comedy') == TRUE,"Comedy", 
                                    if_else(str_detect(genres,'Action') == TRUE,"Action",
                                            if_else(str_detect(genres,'Thriller') == TRUE,'Thriller',
                                                    'Adventure'))))) %>%
  summarize(rating = mean(rating),onegenre) %>%
  ggplot(aes(onegenre, rating)) +
  geom_boxplot()+
  ggtitle("Figure 9 -  Ratings by Top 5 Genres")

#The obvious limitation of the above approach is that in the case where movies have multiple genres,
#they are being 'coerced' into one genre. A "Drama | Comedy" is coded here as a "Drama"
#and a "Comedy | Thrliler" coded as a Comedy.

#Nevertheless, a pattern emerges here with some movies having an overall higher median rating.
#The plot below shows the median values

edx_train %>% 
  group_by(genres) %>% 
  filter(grepl('Drama',genres)) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(genres, rating)) +
  geom_point() +
  geom_smooth()

#We can also pursue an alternative approach, looking for the word 'Action' for example in the genres column and then
#grouping accordingly. The plot below takes that approach

edx_train %>% 
  group_by(genres) %>% 
  filter(grepl('Action',genres)) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(genres, rating)) +
  geom_point() +
  geom_smooth()

edx_train %>% 
  group_by(genres) %>% 
  filter(grepl('Film-Noir',genres)) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(genres, rating)) +
  geom_point() +
  geom_smooth()


#What about separating out the Genres column to have a single genre in each new column?
#As previously shown,
#a movie can have anywhere between 1 and 8 genres. 
#Hence the code below splits out the genres column
#into 8 new columns

temp_df <- separate(edx_train, genres, into = c("Gen1","gen2","gen3","gen4","gen5","gen6","gen7","gen8"), sep = "[|]", remove = FALSE)

 
#However, the problem with the above approach is that some columns will have many 'NA' values.
#Given the complexity of parsing out the genre column ,
#we will  treat each combination of genres as a genre in itself


#finally let's quickly look at the user and movie effects seen in the previous course machine learning, 
#section 6.2
#mean of ratings per user for users who had more than 50 reviews
edx_train %>% 
  group_by(userId) %>% 
  filter(n()>=50) %>%
  summarize(avg = mean(rating)) %>% 
  ggplot(aes(avg)) + 
  geom_histogram(bins = 30, color = "black")

#mean of ratings per movie for movies that had at least 50 reviews
edx_train %>% 
  group_by(movieId) %>% 
  filter(n()>=50) %>%
  summarize(avg = mean(rating)) %>% 
  ggplot(aes(avg)) + 
  geom_histogram(bins = 30, color = "black")

#MODELLING

#We can build a bias reflecting each of the effects before
#Hour effect
#Week effect
#Yeardif effect
#genre effect
#In addition, the movie and user effects from the Machine Learning course

#Below, model we begin by adding a single effect and then adding in additional effects 
#with each subsequent model

#model 1 - Weekday only
mu <- mean(edx_train$rating) 
weekdayavgs <- edx_train %>% 
  group_by(weekday) %>% 
  summarize(b_wd = mean(rating - mu))

predicted_ratings <- edx_test %>% 
  left_join(weekdayavgs, by='weekday') %>%
  mutate(pred = mu + b_wd) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is  1.060155

#Model 2 - weekday and yeardif
yeardifavgs <- edx_train %>% 
  left_join(weekdayavgs, by='weekday') %>%
  group_by(yeardif) %>%
  summarize(b_yd = mean(rating - mu - b_wd))

predicted_ratings <- edx_test %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  mutate(pred = mu + b_yd + b_wd) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is 1.051497

#Model 3 - weekday, yeardif and hour
houravvgs <- edx_train %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  group_by(hour) %>%
  summarize(b_h = mean(rating - mu - b_wd - b_yd))

predicted_ratings <- edx_test %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  left_join(houravvgs, by='hour') %>%
  mutate(pred = mu + b_yd + b_wd + b_h) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is 1.051458

#Model 4 - weekday, yeardif, hour and genre 
genresavg <- edx_train %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_yd - b_wd - b_h))

predicted_ratings <- edx_test %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  mutate(pred = mu + b_yd + b_wd + b_h + b_g) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is  1.010949


#Model 5 - weekday, yeardif, hour, genre , movie effect
movieavgs <- edx_train %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu - b_yd - b_wd - b_h - b_g))


predicted_ratings <- edx_test %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  left_join(movieavgs, by='movieId') %>%
  mutate(pred = mu + b_yd + b_wd + b_h + b_g + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is 0.9443406

#Model 6 - weekday, yeardif, hour, genre , movie  effect and user effect
useravgs <- edx_train %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  left_join(movieavgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_yd - b_wd - b_h - b_g - b_i))


predicted_ratings <- edx_test %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  left_join(movieages, by='movieId') %>%
  left_join(useravgs, by='userId') %>%
  mutate(pred = mu + b_yd + b_wd + b_h + b_g + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, edx_test$rating)
#RMSE is  0.8700048

#Now, after training the model we can test on the edx set and on the validation set
# first we must ensure the Edx and validation sets have the same columns as in the training/test sets

#Compute a year_of_rating column. This is the year that the rating was made

edx$year_of_rating <- as.numeric(str_sub(round_date(as_datetime(edx$timestamp, origin="1970-01-01"), unit = "year"),1,4))
validation$year_of_rating <- as.numeric(str_sub(round_date(as_datetime(validation$timestamp, origin="1970-01-01"), unit = "year"),1,4))

#Computer a year column. This is the year that the movie was released.

edx$year <- as.numeric(str_sub(edx$title, -5,-2))
validation$year <- as.numeric(str_sub(validation$title, -5,-2))
#Compute a year difference column. This is the difference between the year of the user's rating and the year
# the movie was released

edx$yeardif <- edx$year_of_rating - edx$year
validation$yeardif <- validation$year_of_rating - validation$year 

#computer a "weekday column"

edx$weekday <- weekdays(round_date(as_datetime(edx$timestamp, origin="1970-01-01")))
validation$weekday <- weekdays(round_date(as_datetime(validation$timestamp, origin="1970-01-01")))
 
#computer an "hour column"
edx$hour <- str_sub(round_date(as_datetime(edx$timestamp, origin="1970-01-01"), unit = "hour"), -8,-7)
validation$hour <- str_sub(round_date(as_datetime(validation$timestamp, origin="1970-01-01"), unit = "hour"), -8,-7)

#Final_model tested on the edx  set
 
mu <- mean(edx$rating) 

weekdayavgs <- edx %>% 
  group_by(weekday) %>% 
  summarize(b_wd = mean(rating - mu))

yeardifavgs <- edx %>% 
  left_join(weekdayavgs, by='weekday') %>%
  group_by(yeardif) %>%
  summarize(b_yd = mean(rating - mu - b_wd))


houravvgs <- edx %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by='weekday') %>%
  group_by(hour) %>%
  summarize(b_h = mean(rating - mu - b_wd - b_yd))

genresavg <- edx %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_wd - b_yd - b_h))


movieavgs <- edx %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu - b_wd - b_yd - b_h - b_g))


useravgs <- edx %>% 
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by='hour') %>%
  left_join(genresavg, by='genres') %>%
  left_join(movieavgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_wd - b_yd - b_h - b_g - b_i))


predicted_ratings <- edx %>% 
  left_join(movieavgs, by='movieId') %>%
  left_join(useravgs, by='userId') %>%
  left_join(genresavg, by='genres') %>%
  left_join(yeardifavgs, by='yeardif') %>%
  left_join(weekdayavgs, by = 'weekday') %>%
  left_join(houravvgs, by = 'hour') %>%
  mutate(pred = mu + b_wd + b_yd + b_h + b_g + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, edx$rating)

#Test on the edx data set yields RMSE of  0.8567675

#Final test on the validation model
#confirm that all new columns have been computed
head(validation)

#Now test on the validation set

  mu <- mean(validation$rating) 
  
  weekdayavgs <- validation %>% 
    group_by(weekday) %>% 
    summarize(b_wd = mean(rating - mu))
  
  yeardifavgs <- validation %>% 
    left_join(weekdayavgs, by='weekday') %>%
    group_by(yeardif) %>%
    summarize(b_yd = mean(rating - mu - b_wd))
  
  
  houravvgs <- validation %>% 
    left_join(yeardifavgs, by='yeardif') %>%
    left_join(weekdayavgs, by='weekday') %>%
    group_by(hour) %>%
    summarize(b_h = mean(rating - mu - b_wd - b_yd))
  
  
  
  genresavg <- validation %>% 
    left_join(yeardifavgs, by='yeardif') %>%
    left_join(weekdayavgs, by = 'weekday') %>%
    left_join(houravvgs, by='hour') %>%
    group_by(genres) %>%
    summarize(b_g = mean(rating - mu - b_wd - b_yd - b_h))
  
  
  movieavgs <- validation %>% 
    left_join(yeardifavgs, by='yeardif') %>%
    left_join(weekdayavgs, by = 'weekday') %>%
    left_join(houravvgs, by='hour') %>%
    left_join(genresavg, by='genres') %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu - b_wd - b_yd - b_h - b_g))
  
  
  useravgs <- validation %>% 
    left_join(yeardifavgs, by='yeardif') %>%
    left_join(weekdayavgs, by = 'weekday') %>%
    left_join(houravvgs, by='hour') %>%
    left_join(genresavg, by='genres') %>%
    left_join(movieavgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_wd - b_yd - b_h - b_g - b_i))
  
  
  predicted_ratings <- validation %>% 
    left_join(movieavgs, by='movieId') %>%
    left_join(useravgs, by='userId') %>%
    left_join(genresavg, by='genres') %>%
    left_join(yeardifavgs, by='yeardif') %>%
    left_join(weekdayavgs, by = 'weekday') %>%
    left_join(houravvgs, by = 'hour') %>%
    mutate(pred = mu + b_wd + b_yd + b_h + b_g + b_i + b_u) %>%
    pull(pred)
  RMSE(predicted_ratings, validation$rating)
 
#Final Model test on validation yields RMSE of .82

 
   