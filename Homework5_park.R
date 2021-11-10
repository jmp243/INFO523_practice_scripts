# Jung Mee Park
# INFO 523 
# Homework 5
# 2021-11-11

# load dataset
# setwd("~/Downloads")
# load("/Users/jungmeepark/Downloads/7tULbt5T.rdata.part")

load("/Users/jungmeepark/Downloads/dataset.RData")
str(titanic)

setwd("~/Documents/INFO 523 fall 2021/Assignments/titanic")
titanic <- read.csv("hw5_titanic.csv")
titanic <- subset(titanic, select = -X)

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(arules)
library(factoextra)
library(cluster)
library(arulesViz)
library(caret)
library(Matrix)
# 
# titanic.raw<-NULL
# for(i in 1:4){
#   titanic.raw<-cbind(titanic.raw,rep(as.character(df[,i]),df$Freq))
# }

# titanic.data<-as.data.frame(titanic.raw)
# rm(titanic.raw)
# names(titanic.data)<-names(df[1:4])
# head(titanic.data)

# section i. 
# 1. calculate percentages that were children
titanic %>% 
  group_by(Age) %>% 
  summarise(Count=n()) %>% 
  mutate(Percentage=Count/sum(Count)*100)

# 2. percent women
titanic %>% 
  group_by(Sex) %>% 
  summarise(Count=n()) %>% 
  mutate(Percentage=Count/sum(Count)*100)

# 3. percentage in first class
titanic %>% 
  group_by(Class) %>% 
  summarise(Count=n()) %>% 
  mutate(Percentage=Count/sum(Count)*100)

# 4. percent survived
titanic %>% 
  group_by(Survived) %>% 
  summarise(Count=n()) %>% 
  mutate(Percentage=Count/sum(Count)*100)

# 5. children survived
options(digits = 2)
prop.table(table(titanic$Age, titanic$Survived),1)
# apply(Titanic, c(3, 4), sum)
# 6. women survived
prop.table(table(titanic$Sex, titanic$Survived),1)
# apply(Titanic, c(2, 4), sum)
# 7. first class
prop.table(table(titanic$Class, titanic$Survived),1)

# 8. 3rd class survived
prop.table(table(titanic$Class, titanic$Survived),1)
# http://pbahr.github.io/tutorials/2016/04/25/titanic_kaggle_competition_with_r_part_1
# titanic %>% filter(Class == "3rd") %>% 
#   mutate(Freq_per =  Freq * 0.1)
# titanic_4 <- titanic %>% 
#   select(Survived, Class, Age, Sex) %>%
#   filter(!is.na(Age)) %>%
#   group_by(Class, Age, Sex) %>%
#   summarize(N = n(),
#             survivors = sum(Survived == 1),
#             perc_survived = 100 * survivors / N,
#             .groups = "drop")
# 
# library(gtsummary)
# trial2 <- titanic %>% 
#   select(Survived, Class, Sex, Age)

# apply(Titanic, c(1, 4), sum)

# section ii. 
# 9.	Create a function that subsets a given data frame by using sex, age, class and 
# survival as arguments. This function should return a new data frame of passengers 
# that satisfies a specified criteria.
subbing <- function(data=titanic, Class, Sex, Age, Survived){
  data[data$Class == Class & data$Sex == Sex & data$Age == Age & data$Survived == Survived,]
}
subbing(Class = "1st", Sex = "Male", Age = "Adult", Survived = "No") 

# alternate way
my_selector <- function(data=titanic, Class, Sex, Age, Survived){
  data[titanic$Class %in% Class & titanic$Sex %in% Sex & titanic$Age %in% Age & titanic$Survived %in% Survived, ]
}
my_selector(Class = "1st", Sex = "Male", Age = "Child", Survived = "Yes") 

# # function using base R
# my_selector <- function(data=mtcars, vs, am, gear, carb){
#   data[, vs== vs & am == am & gear == gear & carb == carb]
# }
# my_selector(vs=1, am=0, gear=1, carb=1)
# 
# # another way
# my_selector <- function(data=mtcars, vs = 0:1, am = 0:1, gear = 3:5, carb=1:8){
#   data[mtcars$vs %in% vs & mtcars$am %in% am & mtcars$gear %in% gear & mtcars$carb %in% carb, ] # with an in fix operator
# }
# 
# my_selector(vs = 0:1, gear = 4)

#10.	Using the previous function, create a function that calculates the percentages of people who lived or died:
#a.	Compare survival rates between men and boys in third-class fares.
survived_3rd_mAd <- my_selector(Class = "3rd", Sex = "Male", Age = "Adult", Survived = "Yes") # 16% of 3rd class men survived
died_3rd_mAd <- my_selector(Class = "3rd", Sex = "Male", Age = "Adult", Survived = "No") 
 
survived_3rd_mCh <- my_selector(Class = "3rd", Sex = "Male", Age = "Child", Survived = "Yes") # 27% of 3rd class boys survived
died_3rd_mCh <- my_selector(Class = "3rd", Sex = "Male", Age = "Child", Survived = "No") 

nrow(survived_3rd_mAd) # 75
nrow(died_3rd_mAd) #387

nrow(survived_3rd_mCh) # 13
nrow(died_3rd_mCh) # 35

pct_survived <- function(data=titanic, Class, Sex, Age){
  # my_selector(Class = "1st", Sex = "Male", Age = "Child", Survived = "No") 
  num <- nrow(my_selector(Class = Class, Sex = Sex, Age = Age, Survived = "Yes"))
  den <- num + nrow(my_selector(Class = Class, Sex = Sex, Age = Age, Survived = "No"))
  pct <- (num/den)*100
  return(pct)
}

pct_survived(Class = "3rd", Sex = "Male", Age = "Adult")

# pct_survived <- sum(nrow(survived_3rd_mCh))/sum((nrow(titanic$Survived=="Yes" & titanic$Age=="Child")))
pct_survived_men <- nrow(survived_3rd_mAd)/(nrow(titanic$Survived=="Yes"))
pct_survived_men
survival <- function(data=titanic, )

#b.	Compare survival rates between women and girls in first-class fares.

# Section iii.
# 11.	Calculate some association rules using apriori() command. 
# Please focus on the passengers who survived only: Use the following argument in the apriori function: 
  # appearance = list(default="lhs", rhs=("Survived=Yes"))

# subset data to survived
titanic_survive <- titanic[titanic$Survived == "Yes", ]
titanic_survive <- as.data.frame(titanic_survive)
# titanic_rules <- apriori(titanic_survive,appearance = list(default="lhs", rhs=("Survived=Yes")))
# 
# titanic_rules0 <-apriori(titanic, control=list(verbose=F),
#                # parameter = list(minlen=2,supp=0.005,conf=0.8),
#                appearance = list(default="lhs", rhs=("Survived=Yes")))
# 
# titanic_rules <- apriori(titanic_survive, parameter = list(minlen=3,supp=0.002,conf=0.2),
#                          appearance = list(default="lhs", rhs=("Survived=Yes")))

titanic_rules <- apriori(titanic, parameter = list(minlen=3,supp=0.002,conf=0.2), control = list(verbose=F),
                         appearance = list(default="lhs", rhs=("Survived=Yes")))

inspect(titanic_rules)

# inspect(titanic_rules0[1:24])

# 12.	Please visualize the results (use the arulesViz). Add captions to each of the figures you generate. Explain the meaning of the axes.
plot(titanic_rules,method="scatterplot",measure=c("support","confidence"),shading=c("lift"))

plot(titanic_rules,method="grouped",measure=c("confidence")) 

plot(titanic_rules, method = "grouped")

plot(titanic_rules, method="graph")

plot(titanic_rules, method="graph", control=list(type="items"))

# http://r-statistics.co/Association-Mining-With-R.html
# subsetRules <- which(colSums(is.subset(titanic_rules, titanic_rules)) > 1) # get subset rules in vector
# length(subsetRules)  #> 11
# rules <- rules[-subsetRules] # remove subset rules.
# 13.	Sort the list of previously created rules. Use a lift > 3.0 to find the best rules. Briefly explain the best set of rules.


rules.sorted <- sort(titanic_rules, by="lift", decreasing = TRUE)
inspect(rules.sorted)

rules.subset2 <- subset(titanic_rules, lift > 3)
inspect(rules.subset2)
# 14.	Using the function in Q10, estimate the percentage of survival for each of the best rules selected in Q13. Refer to HW4 to re-assess your previous answers (+2 points).

# Part 2
houses <- read.csv("https://raw.githubusercontent.com/mohitgupta-omg/Kaggle-California-Housing-Prices/master/Data/housing.csv")
# 1.	Drop any observations of houses on Islands.
main_houses <- houses %>% 
  filter(ocean_proximity != "ISLAND")

# 2.	Use median values per column to impute any missing observation.
library(Hmisc)
library(mice)
impute_houses <- impute(main_houses)
# 3.	Remove collinearity in the dataset (fit a PCA or keep values with r<0.7)
impute_houses$total_bedrooms <- as.numeric(impute_houses$total_bedrooms)
numeric_houses <- select_if(impute_houses, is.numeric)
myCorr <- cor(numeric_houses)
myCorr
# eigensystem analysis
eigen(cor(numeric_houses))$values

kappa(cor(numeric_houses), exact = TRUE)

# create a model
library(car)
model <- lm(median_house_value ~ ., data = numeric_houses)
vif(model)

mean(vif(model))
# try with PCA
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp#general-methods-for-principal-component-analysis

res.pca <- prcomp(numeric_houses, scale = TRUE)
# Use a PCA (and use the resulting PCs)
res.pca
fviz_eig(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# additional reading - https://www.datacamp.com/community/tutorials/pca-analysis-r

# 4.	Create a training (70% of observations) and testing datasets (30%). Please set a seed to 3. Use the training dataset only.
# https://www.listendata.com/2015/02/splitting-data-into-training-and-test.html
library(caret)

set.seed(3)
trainIndex <- createDataPartition(houses$population, p = .7,
                                  list = FALSE,
                                  times = 1)
house_train <- houses[ trainIndex,]
house_test <- houses[-trainIndex,]

# 5.	Fit the k-means and k-median algorithm (use 6 clusters).
# https://uc-r.github.io/kmeans_clustering
distance <- get_dist(house_train)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
