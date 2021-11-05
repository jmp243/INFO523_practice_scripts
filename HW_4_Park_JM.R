# Jung Mee Park
# jmpark@arizona.edu
# 2021-11-01
# INFO 523 - Data Mining
# HW 4

## Part 1

# load in titanic dataset
library(datasets)
data(Titanic)

plot(Titanic)

# transform as a data.frame
titanic <- as.data.frame(Titanic)

# 4. Plot “class” (x axis) and “freq”
library(dplyr)
library(ggplot2)

# ggplot(data, aes(x, y))
ggplot(titanic, aes(Class, Freq)) 

# 5. Display the data as a boxplot. 
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot()
 
# Can you think of a better (or similar) way to display the visual patterns between “class” and “freq”? 
# I would use a violin plot because the shape of the "violin" encapsulates the distribution of the data. 

# 6. Use the geom_jitter
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot() 

# 7. set width and alpha set to width = 0.25, alpha=0.5
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot() +
  geom_jitter(width = 0.25, alpha=0.5)

# 8. Plot “freq” by “class” using histograms. Make only a single plot. Change the colors.
library(RColorBrewer)
ggplot(titanic, aes(Freq, fill = Class)) + 
  geom_histogram(binwidth = 35) +
  scale_fill_brewer(palette = "Set3") 
  
# 9. Perform steps 4–6, and 8 using functions from base R. boxplot in base R
boxplot(Freq ~ Class, method = "jitter", pch = 19,
        col = 4, data=titanic)

# 10. Histogram in plotly.  
library(plotly)

p1 <- plot_ly(titanic, x = ~Freq, type = "histogram") 
# p1 <- p1 %>% 
#         add_histogram(x=~Class)
p1 <- p1 %>%  layout(barmode = "overlay")
p1 # this does not make much sense yet. 

# Part 2 

# load in data
setwd("~/Documents/INFO 523 fall 2021/Assignments/titanic")
train_titanic <- read.csv("train.csv")


# 1.	What was the survival frequency on Titanic? 
# Examine this question from the following variables: gender, class, age, and ticket fare. 
violin_first <- ggplot(train_titanic, aes(x = Survived, y = Age, fill = Sex)) +
  geom_violin() +
  ylab("") +
  xlab("")  
  # stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") 

violin_first <- violin_first +  labs(title = "", 
                                     subtitle = "") 
print(violin_first)

####
# looked at https://medium.com/analytics-vidhya/a-beginners-guide-to-learning-r-with-the-titanic-dataset-a630bc5495a8

library(psych)
# install.packages("Amelia")
library(Amelia)

# assign as factor variables
train_titanic$Survived <- as.factor(train_titanic$Survived)
train_titanic$Pclass <- as.factor(train_titanic$Pclass, order=TRUE, levels=c(3,2,1))
train_titanic$PassengerId <- as.factor(train_titanic$PassengerId)
train_titanic$Cabin <- as.factor(train_titanic$Cabin)
# train_titanic$Age <- as.numeric(train_titanic$Age)

# prepare missing variables 
train_titanic[train_titanic == ""] <- NA                     # Replace blank by NA

library(GGally)

ggcorr(train_titanic,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50") 

# check for missing values
missmap(train_titanic, main = "Missing values vs observed") # age is missing in some cases

# survived count by sex
gender_plot <- ggplot(train_titanic, aes(x=Survived, fill=Sex)) +
  geom_bar(position=position_dodge()) +
  geom_text(stat = 'count',
            aes(label=stat(count)),
            position = position_dodge(width = 1), vjust=-0.5) + 
            xlab("0 = Dead, 1 = Survived") 
gender_plot <- gender_plot + labs(title = "Survival count by sex", 
                                      subtitle = "training dataset") 
gender_plot

# stacked bar graph
gender_stack <- train_titanic %>% 
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar() +
  labs(x = "Mode", y = "Count") + 
  # geom_text(stat = 'count',
  #           aes(label=stat(count)),
  #           position = position_dodge(width = 1), vjust=-0.5) + 
  xlab("0 = Dead, 1 = Survived") 

gender_stack <- gender_stack + labs(title = "Survival count by sex",
                                # subtitle = "training dataset", 
                                fill = "Sex")
print(gender_stack) # overall 38% of the passengers survived.

# simple table 
table(train_titanic$Survived, train_titanic$Sex)

### visualization by passenger class
class_graph <- ggplot(train_titanic, aes(x=Survived, fill=Pclass)) + 
  geom_bar(position = position_dodge()) + 
  geom_text(stat = 'count',
            aes(label=stat(count)),
            position = position_dodge(width = 1), vjust=-0.5) + 
  xlab("0 = Dead, 1 = Survived") 


class_graph <- class_graph + labs(title = "Survival count by class",
                                    # subtitle = "training dataset", 
                                    fill = "Class")
print(class_graph)

# simple table 
table(train_titanic$Survived, train_titanic$Pclass, train_titanic$Sex)

# density by age
age_density <- ggplot(train_titanic, aes(x=Age)) +
  geom_density(fill='coral')

print(age_density)

# survival by age
train_titanic$Discretized.age <- cut(train_titanic$Age, c(0,10,20,30,40,50,60,70,80,100))
train_titanic$Discretized.age = NULL
# Plot discretized age
age_bar <- ggplot(train_titanic, aes(x = Discretized.age, fill=Survived)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic() + 
  xlab("0 = Dead, 1 = Survived") 

age_bar <- age_bar + labs(title = "Survival count by age",
                                  subtitle = "training dataset", 
                                  fill = "Survived")
print(age_bar)

# Furthermore, the mean age of the passengars. 
titanic_survive$Age[is.na(titanic_survive$Age)] <- mean(titanic_survive$Age, na.rm = TRUE)

mean(titanic_survive$Age, na.rm = TRUE) # 28.32

mean(titanic_die$Age, na.rm = TRUE) #30.6
# this may also reflect the crew breakdown

table(train_titanic$Pclass, train_titanic$Survived)

# If possible, compare groups using the appropriate statistical tests 
# (e.g. t-student vs Wilcoxon-Mann-Whitney U).
agg_titanic <-aggregate(train_titanic$Fare,
                        by=list(survived=train_titanic$Survived), FUN=mean) 
# do not aggregate if you want to do a t test
# I don't have variance and T test is based on variance. 
# when you aggregate you get a single value. 
agg_titanic

# run a t.test
t.test(, var.equal = TRUE) # this did not work. 
# go over the figures first 

# aggregate for age
tabl <- aggregate(Age~Survived,data=titanic,FUN = mean)
tabl

# 2.	What was the order of priority (i.e. gender and age) to rescue passengers of Titanic?
# women and children and then 1st, 2nd, 3rd class individuals
# train_titanic$Survived ~ train_titanic$Sex

# subset data into survivor and nonsurvivor
titanic_survive <- train_titanic[train_titanic$Survived == 1, ]
titanic_die <- train_titanic[train_titanic$Survived == 0, ]


# library(rpart)
# library(rpart.plot)
# 
# fit <- rpart(Survived ~ ., data = train_titanic, method = 'class')
# rpart.plot(fit) # this does not look good. 

# 3.	What were the characteristics of the passengers that did not survive? 
# younger men who paid lower fares did not survive
overview_pct1 <- train_titanic %>% 
  group_by(Survived) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

pct_graph1 <- 
  # drop_na(LiveChatDeployment.DeveloperName) %>% 
  ggplot(overview_pct1, aes(Survived, 
                            count, fill = Survived)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  coord_flip() +
  # theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph1 <- pct_graph1 + labs(title = "", 
                                subtitle = "training dataset",  fill = "Class")

print(pct_graph1)

# survived, class, 
# https://www.kaggle.com/hamelg/intro-to-r-part-28-logistic-regression

titanic_model <- glm(Survived ~ Sex,        # Formula
                     data= train_titanic,    # Data set
                     family="binomial")      # family="binomial" for binary logistic

summary(titanic_model)
# 4.	Do you notice any interesting patterns in survival frequencies 
# based on the place where passengers boarded? (e.g. where did most 
# passengers board? Is there any difference in survival for passengers 
# boarding from different places?)
table(train_titanic$Embarked) # most people embarked in Southampton

embark_graph <- ggplot(train_titanic, aes(x=Survived, fill=Embarked)) + 
  geom_bar(position = position_dodge()) + 
  geom_text(stat = 'count',
            aes(label=stat(count)),
            position = position_dodge(width = 1), vjust=-0.5) + 
  xlab("0 = Dead, 1 = Survived") 


embark_graph <- embark_graph + labs(title = "Survival count by port of embarkment",
                                  subtitle = "training dataset", 
                                  fill = "Port")
print(embark_graph)

