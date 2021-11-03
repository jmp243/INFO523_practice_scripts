# Jung Mee Park
# jmpark@arizona.edu
# 2021-10-27
# INFO 523 - Data Mining
# HW 3

# load in training data
setwd("~/Documents/INFO 523 fall 2021/Assignments/HW3/UCI HAR Dataset/train")

data_y <- read.table(file="y_train.txt", header = FALSE)
data_x <- read.table(file="X_train.txt", header = FALSE) 
subject <- read.table(file="subject_train.txt", header = FALSE)

# load in labels
setwd("~/Documents/INFO 523 fall 2021/Assignments/HW3/UCI HAR Dataset")
features <- read.table(file="features.txt", header = FALSE) # read this data set with two columns

activity <- read.table(file="activity_labels.txt", header = FALSE)

# add labels for activities
activity_code <- c(WALKING=1, WALKING_UPSTAIRS=2, WALKING_DOWNSTAIRS=3, 
                     SITTING=4,  STANDING=5, LAYING=6)


# rename variables 
# for subjects
names(subject)[names(subject)=="V1"] <- "SubjectID"

# for data y
names(data_y)[names(data_y)=="V1"] <- "Activity_labels"

data_y$Activity_labels <- names(activity_code)[match(data_y$Activity_labels, activity_code)]

# add official column names for variables in data_x

colnames(data_x) <- features$V2 # make sure they are unique colnames
# names(data_x)

# check for uniqueness 
# !data_x %in% data_x[duplicated(data_x)]

any(duplicated(names(data_x))) # TRUE
length(which(duplicated(names(data_x))==TRUE))
# there are 84 duplicated names

# features_df <- as.data.frame(t(features)) 
# 
# features_new <- features_df[-c(1), ]

# combine the two data frames
sub_y <- cbind(subject, data_y)

# add colnames to features
# feature_data <- rbind(features_new, data_x)
# write.csv(feature_data, file = "train_features.csv")

# train_features <- read.csv(file="train_features.csv", header = TRUE) # but I manipulated the excel

# train_features <- train_features[-1] # remove the first column
# however, the first row is being read in as a variable 

# names(features) = data_x$new[match(names(features_new), data_x$old)]

### Problem Set 1
########################
# Question 1           #
########################
train <- cbind(sub_y, data_x)
# write.csv(train, file = "training_data.csv")

train <- read.csv(file="training_data.csv", header = TRUE) 

####
library(data.table)
library(Hmisc)

###

########################
# Question 2           #
########################
# 2.	Find any column with both positive and negative numbers. 
# Create a new column and name it “discrete” with only observations 
# falling in two discrete categories based on your target column: "pos" 
# (positive observations) and "neg" (negative observations). The output 
# should be saved in an object named 'train_q2'.

# turn  "tBodyAcc-mean()-Y"  into discrete

train$discrete <- ifelse(train$`tBodyAcc-mean()-Y`>0,"pos","neg")
train_q2 <- as.data.frame(train)
# train <- train[,-564] 

########################
# Question 3           #
########################
# remove the var "tBodyAcc-mean()-Y" from q2
train_q3 <- train_q2[,-4] 

########################
# Question 4           #
########################
# reorder the columns in "train_q3" 
# mean variables appear first
library(tidyverse)
library(dplyr)

# train_q4 <- train_q3 %>% 
#   arrange(contains("mean"))

train_q4 <- train_q3 %>% select(contains("mean"), everything()) 

########################
# Question 5           #
########################
# move train_q3 to show first 4 rows of every column with angle and vector
train_q3 %>% 
  select(starts_with("angle")) %>% 
  head(n=4)

########################
# Question 6           #
########################
# Use "train_q3"
# remove all the rows with at least one observation falling 
# outside quantiles 0.025 and 0.975 of any column in the data set. 
# Store the resulting object in "train_q6".
# how to find outliers in r

# if c < 0.025 | c > 0.975 outside the interval
# write a function to calculate

# define lower and upper bounds
lower_bound <- quantile(train_q3$`angle(Y,gravityMean)`, 0.025)
lower_bound

upper_bound <- quantile(train_q3$`angle(Y,gravityMean)`, 0.975)
upper_bound

outlier_ind <- which(train_q3$`angle(Y,gravityMean)`< lower_bound | train_q3$`angle(Y,gravityMean)` > upper_bound)
outlier_ind

# outlier_ind <- which(train_q3$`angle(Y,gravityMean)`< quantile(train_q3$`angle(Y,gravityMean)`, 0.025)
#                | train_q3$`angle(Y,gravityMean)` > quantile(train_q3$`angle(Y,gravityMean)`, 0.975))


# remove outliers for all columns
train_q6 <- train_q3[-outlier_ind,]

########################
# Question 7           #
########################
# call outlier values NA
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.025, .975), na.rm = T)
  x[x < (qnt[1])] <- NA
  x[x > (qnt[2])] <- NA
  return(x)}

numeric_cols<-names(train_q2)[sapply(train_q2, is.numeric)]
numeric_data<-train_q2[,names(train_q2)%in%numeric_cols]

train_q2_IQR<-as.data.frame(sapply(numeric_data,outlierTreament))

# for the discrete variable was created from `tBodyAcc-mean()-Y`
# remove NA from the column `tBodyAcc-mean()-Y` in train_q2_IQR

train_q2_IQR$discrete <- ifelse(train$`tBodyAcc-mean()-Y`>0,"pos","neg") # recreate discrete
train_q2_IQR <- cbind(data_y, train_q2_IQR) # add back activity label
train_q7 <- train_q2_IQR[!is.na(train_q2_IQR$`tBodyAcc-mean()-Y`),] 

########################
# Question 8           #
########################
#	Using "train_q3" data set. 
# calculate the average estimate of the minimum value across X, Y, and Z 
# for the "tBodyGyro-mean()-" features. Store the results in "train_q8".

train_q8 <- train_q3 %>% 
  # group_by(SubjectID) %>% 
  select(starts_with(c("tBodyGyro-mean()-"))) %>% 
  summarise_if(is.numeric, min) 
  # ungroup()

########################
# Question 9           #
########################
# create a new column in train_q3 where the min value for "tBodyGyro-mean()-" is present

# train_q3_gyro <- train_q3 %>%
#   select(starts_with(c("tBodyGyro-mean()-"))) %>% 
#   lapply(sort)
  # select_if(is.numeric, min) %>% 
  # mutate(min_value = is.logical(min))
  # mutate(min_value = case_when(.$"tBodyGyro-mean()-" = min ~ "TRUE"))
   # mutate(min_value = case_when("tBodyGyro-mean()-X" == min ~ "memory", 
   #                              "tBodyGyro-mean()-Y" == min ~ "choice",
   #                              "tBodyGyro-mean()-Z" == min ~ "justification")

# min(train_q3[,122:124 ])
# 
# train_q3$min_value <- sapply(1:nrow(train_q3),
#                              function(i) min(train_q3[i,122:124])) # not correct

min_value <- mapply(min, train_q3[,c(122:124)]) # I am given the min values
min_value

min(c(122:124)) # col 122 has the smallest min

train_q3[which.min(train_q3$"tBodyGyro-mean()-X"),] #3955 smallest min value
train_q3[which.min(train_q3$"tBodyGyro-mean()-Y"),] #1370
train_q3[which.min(train_q3$"tBodyGyro-mean()-Z"),] #1893

train_q3$row_minimum <- rowMins(as.matrix(train_q3[,c(122:124)]), value = FALSE)

train_q3_gyro$row_min <- apply(as.matrix(train_q3[,c(122:124)]),1, min)

min_gyro <- function(a){
  return = which(a == min(a))
}

# train_q3_gyro$row_min <- apply(as.matrix(train_q3[,c(122:124)]),1, min_gyro)
# 
# train_q3_gyro$row_min <- recode_factor(train_q3_gyro$row_min, "1" = "X", "2" = "Y", "3" = "Z") 

# save as q9
train_q9 <- data.frame(train_q3)
train_q9$row_min <- apply(as.matrix(train_q9[,c(122:124)]),1, min_gyro)

train_q9$row_min <- recode_factor(train_q9$row_min, "1" = "X", "2" = "Y", "3" = "Z") 


library(matrixStats)
# train_q2$"tBodyGyro-mean()-Y"== min(train_q2$"tBodyGyro-mean()-Y"), "min", "not min",
# train_q2$"tBodyGyro-mean()-Z"== min(train_q2$"tBodyGyro-mean()-Z"), "min", "not min"

# train_q2$gyro_min <- "not small"
# train_q2$gyro_min[train_q3$"tBodyGyro-mean()-X" == -0.9140496] <- "smallest"
# train_q2$gyro_min[train_q3$"tBodyGyro-mean()-Y" == -0.8519732] <- "small"
# train_q2$gyro_min[train_q3$"tBodyGyro-mean()-Z" == -0.9028598] <- "small"

# train_q2 %>%  
#   mutate(across(starts_with(c("tBodyGyro-mean()-"))) 
#            


# train_q3_gyro <- train_q3 %>%
#   select(starts_with(c("tBodyGyro-mean()-")))
# 
# train_q3_gyro$row_minimum = rowMins(as.matrix(train_q3_gyro[,c(1:3)]), value = FALSE)
# 
# 
# library(keras)

  # %>%
#   mutate(gyro_min = case_when(value = mapply(min, train_q3[,c(122:124)]) ~ "small"))

# train_q3_gyro <- train_q3 %>% 
#                  select(starts_with(c("tBodyGyro-mean()-"))) %>%
#   mutate(Gyro_min = min, 'Yes', 'No') # does not work yet

               
# train_q3_gyro <- train_q3 %>%
#   mutate(Min=if_else(.$starts_with(c("tBodyGyro-mean()-")) == min, 'Yes', 'No'))


# mutate(min_value = mapply(min, train_q3[,c(122:124)]))

# mapply(function(train_q3, z) mutate_(train_q3, z=z), train_q3, 3:4, SIMPLIFY=FALSE)

 
# library(car)
# library(purrr)
# train_q3$col2 <- rep(NA, nrow(train_q3))
# train_q3[train_q3$`tBodyGyro-mean()-X` = min, ][, "col2"] <- "min"
# train_q3[train_q3$`tBodyGyro-mean()-Y` = min, ][, "col2"] <- "min"
# train_q3[train_q3$`tBodyGyro-mean()-Z` = min, ][, "col2"] <- "min"


### Problem Set 2
########################
# Question 1           #
########################
setwd("~/Documents/INFO 523 fall 2021/Assignments/HW3/")
data <- read.csv(file="population-figures-by-country-csv_csv.csv", header = TRUE) 

# rename the second column to "code"
names(data)[names(data)=="Country_Code"] <- "code"

# create 2 new columns summarizing the percentage of world population for each country 
# at two times: 1960 and 2016 (columns '%_timeA' and '%_timeB')
# percent function
# pcentFun <- function(x) {
#   res <- x > 0
#   100 * (sum(res) / length(res))
# }
# 
# # don't count NA
# length2 <- function (x, na.rm=FALSE) {
#   if (na.rm) sum(!is.na(x))
#   else       length(x)
# }

# colSums(is.na(data))
# # apply(data, 2, pcentFun)
# data %>%  gather(code, Year_1960, Year_2016) %>% 
#   group_by(Year_1960) %>% 
#   summarise(count = n()) %>% 
#   mutate(pct1960 = count/sum(count))
library(stringr)
library(devtools)
library(tidyverse)
library(dplyr)
# percent function
# pcent_func = function(data, col) {
#   data %>%
#     # drop_na({{col}}) %>%
#     group_by({{col}}) %>%
#     summarize(count = n()) %>%
#     ungroup %>% 
#     mutate(
#       "%_{{col}}" := count / sum(count)
#     )
# }
# 
# pcent_func(data, Year_1960)

####
data1 <- as.data.frame(data)

data1$"%_Year_1960" <- data1$Year_1960/data1$Year_1960[257]
  
# data1 <- data %>% 
#   drop_na(Year_1960) %>%
#   group_by(Year_1960) %>% 
#   summarise(count = n()) %>% 
#   mutate("%_Year_1960" = count/sum(count))
# # data1 <- merge(data1,data3, all.x=TRUE) 
# 
# data3 <- data %>% 
#   drop_na(Year_2016) %>%
#   group_by(Year_2016) %>% 
#   summarize(count1 = n()) %>% 
#   mutate("%_Year_2016" = count1/sum(count1)) %>% 
#   ungroup()

# data1 <- merge(data1,data3, all.x=TRUE) 
# 
# data2 <- subset(data1, select = -c(count, count1)) # This is quite cumbersome but it sort of works
# data2 <- data2 %>% relocate(Year_1960, .before = Year_1961)
# data2 <- data2 %>% relocate(Year_2016, .after = Year_2015)
# 
data1$"%_Year_2016" <- data1$Year_2016/data1$Year_2016[257]

data1$diff <- (data1$"%_Year_1960"- data1$"%_Year_2016")

# create a new row showing the total of every numeric col

# data1$Total <- colSums( data1[ sapply(data1, is.numeric)] ) # why won't this work

# data1 <- colSums(data1[, -c(Country, code)], na.rm = TRUE)
# 
# total <- colSums(data1[,3:59])

library(janitor)
library(purrr)
library(tibble)
total <- data1 %>%
  select_if(is.numeric) %>%
  replace(is.na(.), 0) %>% 
  map_dbl(sum)

# data2 <- data1 %>%
#   select_if(is.numeric) %>%
#   replace(is.na(.), 0) %>% 
#   rbind(data1) %>%
#   mutate(Total = map_dbl(sum))
# data1_subset <- unlist(lapply(data1, is.numeric)) 
# colSums(data1[ , data1_subset]) 

# sum(data1$Year_1960, na.rm = TRUE)
# 
# mapply(sum,data0[,c(-1,-2)])
# data2 %>%
#   bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
# 
# # library(janitor)
# data2 %>%
#   adorn_totals("row")

# data2 <- as.data.frame(data1)

data0 <- data2 %>%
  replace(is.na(.), 0)
  #rowwise will make sure the sum operation will occur on each row
  # rowwise() %>%
  # #then a simple sum(..., na.rm=TRUE) is enough to result in what you need

total2 <- mapply(sum,data0[,c(-1,-2)])


data0 %>% 
  select_if(is.numeric) %>%
  # replace(is.na(.), 0) %>% 
  add_row(total = map_dbl(sum)) %>% 
  mutate(total = map_dbl(sum))
# 
# data0[nrow(data0) + 1, ] <- total
# data2 <- data2 %>% 
#   #rowwise will make sure the sum operation will occur on each row
#   rowwise() %>% 
#   #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
#   replace(is.na(.), 0) %>%
#   mutate(Total = sum(3:59))
total2 <- as.data.frame(total)

# data0[nrow(data0) + 1,] <- total 
total_new <- as.data.frame(t(total2))
Country <- c("nothing")
code <- c("nothing")

total_new$Country <- Country
total_new$code <- code

total_new <- total_new %>% relocate(code, .before = Year_1960)
total_new <- total_new %>% relocate(Country, .before = code)

data2 <- rbind(data1, total_new)
# total_new$code 
# total_new <- as.data.frame(total_new)
# data2 <- rbind(data0, total_new)
# data2 <- data2 %>% 
#   #rowwise will make sure the sum operation will occur on each row
#   # rowwise() %>% 
#   #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
#   replace(is.na(.), 0) %>%
#   mutate(Total = colSums(3:59))
# 
# numeric_cols<-names(data)[sapply(data, is.numeric)]

# df$v5 <- apply(df,1,sum)
# data$total <- apply(is.numeric(data),1,sum)

# data_num <- data[ , 3:59]  # all numeric columns

# data1 <- data_num %>%                      # Row sums
#   replace(is.na(.), 0) %>%        # Replace NA with 0
#   mutate(sum = rowSums(.))

# data1 <- data %>%    
#   select_if(is.numeric) %>% 
#   replace(is.na(.), 0) %>%        # Replace NA with 0
#   mutate(sum = rowSums(.))
# 
# data1 <- data %>%    
#   select_if(is.numeric) %>% 
#   replace(is.na(.), 0) %>%        # Replace NA with 0
#   mutate(sum = rowSums(.))

########################
# Question 2           #
########################
data3 <- as.data.frame(data2)
# reshape the data from wide to long
library(reshape2)
data_long1 <- melt(data3,                                 # Apply melt function
                   id.vars = c("Country", "code"))        # try pivot_longer later


# x <- gregexpr("[0-9]+", data_long1$variable)
# x <- unlist(regmatches(data_long1$variable, x))
# x
# library(stringr)
# names(data_long1$variable) <- sub(".*\\.", "", names(data_long1$variable))

# data_long1$variable <- as.numeric(data_long1$variable)
# keep only the oldest and most recent years with non-missing values for population sizes

# min_year <- function(a){
#   return = which(a == min(a))
# }
# 
# data$min_year <- apply(as.matrix(data[,c(3:59)]), 1, min_year)  # this is not right

# https://tonyladson.wordpress.com/2015/05/13/find-the-dates-of-the-first-and-last-non-missing-data-values/

# min(which(!is.na(data3))) # first non-missing element
# max(which(!is.na(data3))) # last non-missing element

# NonMissingStartEnd <- function(code, date.col, data){
#   start <- data3[[date.col]][min(which(!is.na(data3[[code]])))]
#   end <- data3[[date.col]][max(which(!is.na(data3[[code]])))]
#   # data.frame(code = code, start=start, end=end)
#   return(end - start)
# }
# 
# NonMissingStartEnd <- function(code, data){
#   start <- min(which(!is.na(data3[[code]])))
#   end <- max(which(!is.na(data3[[code]])))
#   # data.frame(code = code, start=start, end=end)
#   return(end - start)
# }
# NonMissingStartEnd(ABW, data3) 
# https://stackoverflow.com/questions/6808621/find-the-index-position-of-the-first-non-na-value-in-an-r-vector
NonNAindex <- which(!is.na(data3))
firstNonNA <- min(NonNAindex)

find_nonNA <- function(x){
  # return = which(a == min(a))
  # return = which(a == !is.na(a))
  NonNAindex <- which(!is.na(x))
  firstNonNA <- min(NonNAindex)
  return(firstNonNA)
}

# index <- apply(as.matrix(data3[,c(3:59)]), 1, find_nonNA) 

# subset_row <- data3 %>% slice(11) # subset to see a row with NA's
# find_nonNA(subset_row[,3:59])

data3$index2 <- apply(data3[,3:59], 1, find_nonNA) 

# find the max
find_nonNA2 <- function(x){
  # return = which(a == min(a))
  # return = which(a == !is.na(a))
  NonNAindex <- which(!is.na(x))
  lastNonNA <- max(NonNAindex)
  return(lastNonNA)
}

data3$index3 <- apply(data3[,3:59], 1, find_nonNA2) 

# (1) 'Diff_year' (difference in time between years), 
data3$Diff_year <- (data3$index3- data3$index2)

# Position(function(data3) !is.na(data3), data3)
# (start <- which.min(!is.na(data3)))

# data %>%
#   group_by(Country) %>%
#   summarise(first_non_missing = colnames[which(!is.na(colnames))[1]])

# is.na(data3) <- seq(firstNonNA, length.out=15)

# calculate (1) 'Diff_year' (difference in time between years), 
# colnames(data3)<-gsub("Year_","",colnames(data3))

# (2) 'Diff_growth' (difference in population size between years)
find_diff <- function(x){
  NonNAindex <- which(!is.na(x))
  lastNonNA <- max(NonNAindex)
  NonNAindex <- which(!is.na(x))
  firstNonNA <- min(NonNAindex)
  diff = x[lastNonNA] - x[firstNonNA]
  return(diff)
}

data3$diff_growth <- apply(data3[,3:59], 1, find_diff)

# (3) 'Rate_percent' = ((Diff_growth / Diff_year)/oldest_year). 
rate_pct <- function(x){
  NonNAindex <- which(!is.na(x))
  lastNonNA1 <- max(NonNAindex)
  NonNAindex2 <- which(!is.na(x))
  firstNonNA <- min(NonNAindex2)
  diff = x[lastNonNA1] - x[firstNonNA]
  growth = 100*(diff/(lastNonNA1-firstNonNA))/x[lastNonNA1]
  return(growth)
}
data3$Rate_percent <- apply(data3[,3:59], 1, rate_pct)

########################
# Question 3           #
########################
# Split "data3" into a continent-based list
continent <- read.csv(file="country-and-continent-codes-list-csv_csv.csv", header = TRUE, sep = ",")

sub_continent <- subset(continent, select = c(Continent_Name, Continent_Code, Three_Letter_Country_Code))

data3_cont <- merge(data3,sub_continent, by.x = "code", by.y = "Three_Letter_Country_Code")

## In a comment within the script, provide two ideas on how this new 
## list could be used to perform calculations with functions within the "apply” family.

# tapply can be used to pass categorical variables. 
# We can calculate lengths to see how many countries are from given continents. 
# tapply(data3_cont$Continent_Name, data3_cont$Country, median)

# lapply can be used to fix the name of continent abbreviation for North America from NA to something 
# else so the dataset is read in correctly. 

save.image()
savehistory(file="HW3_park")
