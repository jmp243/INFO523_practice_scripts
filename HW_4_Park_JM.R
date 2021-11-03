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

