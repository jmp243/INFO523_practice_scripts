# Jung Mee Park
# jmpark@arizona.edu
# 2021-11-01
# INFO 523 - Data Mining
# HW 4

## Part 1
library(corrplot)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(psych)
library(Amelia)
library(titanic)
library(GGally)
library(datasets)
data(Titanic)

plot(Titanic)

# transform as a data.frame
titanic <- as.data.frame(Titanic)

# 4. Plot “class” (x axis) and “freq”


# ggplot(data, aes(x, y))
ggplot(titanic, aes(Class, Freq)) 

# 5. Display the data as a boxplot. 
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot()
 
# Can you think of a better (or similar) way to display the visual patterns between “class” and “freq”? 
# I would use a violin plot because the shape of the "violin" encapsulates the distribution of the data. 

# 6. Use the geom_jitter
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot() +
  geom_jitter()

# 7. set width and alpha set to width = 0.25, alpha=0.5
ggplot(titanic, aes(Class, Freq)) +
  geom_boxplot() +
  geom_jitter(width = 0.25, alpha=0.5)

# 8. Plot “freq” by “class” using histograms. Make only a single plot. Change the colors.

ggplot(titanic, aes(Freq, fill = Class)) + 
  geom_histogram(binwidth = 35) +
  scale_fill_brewer(palette = "Set3") 
  
# 9. Perform steps 4–6, and 8 using functions from base R. boxplot in base R
boxplot(Freq ~ Class, method = "jitter", pch = 19,
        col = 4, data=titanic)

# 10. Histogram in plotly.  


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

violin_first <- ggplot(train_titanic, aes(x = Survived, y = Fare, fill=Sex)) +
  geom_violin() +
  ylab("cost of fare") +
  xlab("") +
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") 

violin_first <- violin_first +  labs(title = "Violin plot of Survival by fare and gender", 
                                     subtitle = "training dataset") 
print(violin_first)

####
# looked at https://medium.com/analytics-vidhya/a-beginners-guide-to-learning-r-with-the-titanic-dataset-a630bc5495a8

# assign as factor variables
# train_titanic$Survived <- as.factor(train_titanic$Survived)
train_titanic$Survived  <- recode_factor(train_titanic$Survived , "1" = "Survived", "0" = "Died")

train_titanic$Pclass <- factor(train_titanic$Pclass, levels = c("3","2","1"), 
                                  ordered = TRUE)
train_titanic$Pclass  <- recode_factor(train_titanic$Pclass,
                                       "1" = "1st", "2" = "2nd", "3" = "3rd")
train_titanic$PassengerId <- as.factor(train_titanic$PassengerId)
train_titanic$Embarked <- recode_factor(train_titanic$Embarked,
                                        "C" = "Cherbourg", "Q" = "Queenstown", "S" = "Southampton")
train_titanic$Cabin <- as.factor(train_titanic$Cabin)
train_titanic$char_cabin <- as.character(train_titanic$Cabin)     

new_Cabin <- ifelse(train_titanic$char_cabin == "",          
                    "",                        
                    substr(train_titanic$char_cabin,1,1))    

new_Cabin <- factor(new_Cabin )                
train_titanic$Cabin <- new_Cabin
# train_titanic$Age <- as.numeric(train_titanic$Age)

# prepare missing variables 
train_titanic[train_titanic == ""] <- NA                     # Replace blank by NA

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
  xlab("outcome") 

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
  xlab("outcome") 


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
# train_titanic$Discretized.age = NULL
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
t.test(train_titanic$Age ~ train_titanic$Survived) # ttest for age
t.test(train_titanic$Fare ~ train_titanic$Survived) # ttest for fare

# go over the figures first 

# aggregate for age
tabl <- aggregate(Age~Survived,data=train_titanic,FUN = mean)
tabl

# 2.	What was the order of priority (i.e. gender and age) to rescue passengers of Titanic?
# women and children and then 1st, 2nd, 3rd class individuals
# train_titanic$Survived ~ train_titanic$Sex
violin_two <- ggplot(train_titanic, aes(x = Survived, y = Fare)) +
  geom_violin() +
  ylab("cost of fare") +
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") 

violin_two <- violin_two +  labs(title = "Violin plot of Survival by fare", 
                                 subtitle = "training dataset") 
print(violin_two)

# looking at age
violin_age <- ggplot(train_titanic, aes(x = Survived, y = Age, fill = Sex)) +
  geom_violin() +
  # stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  ylab("Age") 
violin_age <- violin_age +  labs(title = "Violin plot of Survival by age and sex", 
                                 subtitle = "training dataset") 
print(violin_age)

# subset data into survivor and nonsurvivor
# titanic_survive <- train_titanic[train_titanic$Survived == 1, ]
# titanic_die <- train_titanic[train_titanic$Survived == 0, ]


# library(rpart)
# library(rpart.plot)
# 
# fit <- rpart(Survived ~ ., data = train_titanic, method = 'class')
# rpart.plot(fit) # this does not look good. 

titanic_model <- glm(Survived ~ Sex,        # Formula
                     data= train_titanic,    # Data set
                     family="binomial")      # family="binomial" for binary logistic

summary(titanic_model)

# full model
full_titantic_model <- glm(Survived ~ Sex+Pclass+Age+SibSp+Parch+Fare,
                     data= train_titanic,       
                     family="binomial")         

summary(full_titantic_model)

# calculate predicted probabilities
predicted_probabilities <- predict(full_titantic_model,              
                                   newdata=train_titanic,      
                                   type="response")   
### Convert to 0, 1 preds
class_preds <- ifelse(predicted_probabilities >= 0.5, 1, 0)  

# Make a table of predictions vs. actual
result_table <- table(class_preds,             
                      train_titanic$Survived)  

result_table

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
  labs(x = "0 = Dead, 1 = Survived", y = "") +
  coord_flip() +
  # theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph1 <- pct_graph1 + labs(title = "Overall Survival Percentage", 
                                subtitle = "training dataset",  fill = "Class")

print(pct_graph1)

# survived, class, 
# https://www.kaggle.com/hamelg/intro-to-r-part-28-logistic-regression


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
  xlab("outcome") 


embark_graph <- embark_graph + labs(title = "Survival count by port of embarkment",
                                  subtitle = "training dataset", 
                                  fill = "Port")
print(embark_graph)

# do a percentage of embarked
theme_classic()
pct_graph2 <- train_titanic %>% 
  drop_na(Embarked) %>%
  ggplot(aes(x=Embarked, 
                            y=Survived, fill = Survived)) +
  geom_bar(stat='identity') +
  labs(x = "port of embarkment", y = "count") 
  # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph2 <- pct_graph2 + labs(title = "Survival percentage by embarkment", 
                                subtitle = "training dataset",  fill = "Class")

print(pct_graph2)

# 5.	Was it a good idea to travel in big groups with parents/children or 
# was it better to travel in small groups with siblings/spouses?

violin_sib <- ggplot(train_titanic, aes(x = Survived, y = SibSp, fill = Sex)) +
  geom_violin() +
  # stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  ylab("Sibling and Spouse") 
violin_sib <- violin_sib +  labs(title = "Violin plot of Survival", 
                                 subtitle = "training dataset") 
print(violin_sib)

table(train_titanic$Survived, train_titanic$SibSp)

# violin for 

violin_parch <- ggplot(train_titanic, aes(x = Survived, y = Parch, fill = Sex)) +
  geom_violin() +
  # stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  ylab("Parent and Child") 
violin_parch <- violin_parch +  labs(title = "Violin plot of Survival", 
                                 subtitle = "training dataset") 
print(violin_parch)

table(train_titanic$Survived, train_titanic$Parch)

### extra credi
summary(mtcars)
View(mtcars)

cars_graph <- ggplot(mtcars, aes(x=))

### extra credit 2
library(tidyverse)
library(ggtext)
library(patchwork)
df_simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

## ggplot them
source(here::here("R", "tidy_grey.R"))
theme_update(rect = element_rect(color = NA, 
                                 fill = "#FFCC00"),
             line = element_blank(),
             text = element_text(color = "white"), 
             plot.margin = margin(10, 40, 20, 40))

df_simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

df_simpsons_series <- 
  df_simpsons %>% 
  filter(season != "Movie") %>% 
  mutate(season = as.numeric(season)) %>% 
  separate(number, c("no_cont", "no_season"), sep = "–")

top <- 
  df_simpsons_series %>% 
  count(guest_star) %>% 
  top_n(6, n) %>% 
  arrange(-n) %>%
  pull(guest_star)
lev <- c(top, "other")
df_simpsons_lumped <- 
  df_simpsons_series %>% 
  count(season, guest_star) %>% 
  group_by(guest_star) %>% 
  mutate(total = n()) %>% 
  group_by(season) %>% 
  arrange(desc(n), desc(total), guest_star) %>% 
  mutate(
    ranking = row_number(),
    top = ifelse(guest_star %in% top, guest_star, "other"),
    top = factor(top, levels = lev)
  )
ranks <- 
  ggplot(df_simpsons_lumped, 
         aes(season, ranking, 
             color = top)) +
  geom_segment(data = tibble(x = 0.3, xend = 30.5, y = 1:61), 
               aes(x = x, xend = xend, y = y, yend = y),
               color = "white", linetype = "dotted") +
  geom_segment(data = tibble(x = 30.5, xend = 31, y = 2:61), 
               aes(x = x, xend = xend, y = y, yend = y),
               color = "white") +
  geom_segment(data = tibble(x = 30.5, xend = 31.5, y = c(1, seq(5, 60, by = 5))), 
               aes(x = x, xend = xend, y = y, yend = y),
               color = "white") +
  geom_point(color = "white", 
             size = 5) +
  geom_point(color = "#FFCC00", 
             size = 3) + 
  geom_line(data = filter(df_simpsons_lumped, top != "other"), 
            size = 1, 
            alpha = 1) +
  geom_point(data = filter(df_simpsons_lumped, top != "other"), 
             size = 9) + 
  geom_point(data = filter(df_simpsons_lumped, top != "other"), 
             color = "#FFCC00", 
             size = 6) + 
  geom_text(data = filter(df_simpsons_lumped, top != "other"), 
            aes(label = n),
            family = "Roboto",
            fontface = "bold", 
            size = 3) +
  annotate("label", x = 10, y = 55, 
           fill = "#FFCC00", 
           color = "white",
           family = "Roboto Mono", 
           fontface = "bold", 
           size = 4.5,
           label.padding = unit(1, "lines"),
           label = 'Ranking of guest star appearances per\nseason in the TV series "The Simpsons".\n\nThe six top guests are colored\n and visualised as their common character.\nAll others are ranked anonymously and\nindicate the total number of guests\nper season. In case of a tie guest stars\nare sorted by the number of appearances.') +
  scale_x_continuous(position = "top", 
                     limits = c(0, 31.5), 
                     breaks = 1:30,
                     expand = c(0.01, 0.01)) +
  scale_y_reverse(position = "right", 
                  limits = c(61, 1), 
                  breaks = c(1, seq(5, 60, by = 5)),
                  expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("#00947E", "#FF5180", "#460046", 
                                "#727273", "#B26C3A", "#C72626")) +
  scale_linetype_manual(values = c(rep(1, 6), 0)) +
  theme(axis.text = element_text(color = "white",
                                 family = "Roboto Mono", 
                                 face = "bold"),
        axis.title.y = element_text(hjust = 0),
        panel.border = element_rect(color = NA),
        legend.position = "none") +
  labs(x = "Season", y = "Ranking\n")

labels <-
  tibble(
    labels = c(
      "<img src='https://upload.wikimedia.org/wikipedia/en/7/76/Edna_Krabappel.png'
    +     width='100' /><br><b style='color:#00947E'>Marcia Wallace</b><br><i style='color:#00947E'>Edna Krabappel</i></b>",
      "<img src='https://upload.wikimedia.org/wikipedia/en/6/6c/Troymcclure.png'
    +     width='90' /><br><b style='color:#FF5180'>Phil Hartman</b><br><i style='color:#FF5180'>Troy McClure</i></b>",
      "<img src='https://upload.wikimedia.org/wikipedia/en/3/3e/FatTony.png'
    +     width='110' /><br><b style='color:#460046'>Joe Mantegna</b><br><i style='color:#460046'>Fat Tony</i></b>",
      "<img src='./img/orson.png'
    +     width='85' /><br><b style='color:#727273'>Maurice LaMarche</b><br><i style='color:#727273'>Several VIPs</i>",
      "<img src='https://upload.wikimedia.org/wikipedia/en/8/8a/SantasLittleHelper.png'
    +     width='100' /><br><b style='color:#B26C3A'>Frank Welker</b><br><i style='color:#B26C3A'>Santas Little Helper</i></b>",
      "<img src='https://upload.wikimedia.org/wikipedia/en/c/c8/C-bob.png'
    +     width='100' /><br><b style='color:#C72626'>Kelsey Grammer</b><br><i style='color:#C72626'>Sideshow Bob</i></b>"
    ),
    x = 1:6, 
    y = rep(1, 6)
  )
legend <- 
  ggplot(labels, aes(x, y)) +
  geom_richtext(aes(label = labels), 
                fill = NA, 
                color = NA, 
                vjust = 0) +
  annotate("text", x = 3.5, y = 1.018, 
           label = 'Guest Voices in "The Simpsons"', 
           size = 15, 
           fontface = "bold", 
           family = "Poppins") +
  scale_x_continuous(limits = c(0.6, 6.1)) +
  scale_y_continuous(limits = c(1, 1.02)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FFCC00"))

caption <- 
  ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       caption = "Visualization by Cédric Scherer  |  Source: Wikipedia  |  Image  Copyright: Matt Groening & 20th Century Fox                                                   ") +
  theme(line = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", 
                                       color = "transparent"),
        panel.border = element_rect(color = "transparent"),
        axis.text = element_blank())

legend + ranks + caption + plot_layout(ncol = 1, heights = c(0.25, 1, 0))
ggsave(here::here("plots", "2019_35", "2019_35_SimpsonsGuests.pdf"), 
       width = 16, height = 24, device = cairo_pdf)

sessionInfo()
