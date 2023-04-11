library(tidyverse)

library(datasets)
library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_smooth(aes(color=Species))
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point()
the.data <- read.table( header=TRUE, sep=",", 
                        text="source,year,value
    S1,1976,56.98
    S1,1977,55.26
    S1,1978,68.83
    S1,1979,59.70
    S1,1980,57.58
    S1,1981,61.54
    S1,1982,48.65
    S1,1983,53.45
    S1,1984,45.95
    S1,1985,51.95
    S1,1986,51.85
    S1,1987,54.55
    S1,1988,51.61
    S1,1989,52.24
    S1,1990,49.28
    S1,1991,57.33
    S1,1992,51.28
    S1,1993,55.07
    S1,1994,50.88
    S2,1993,54.90
    S2,1994,51.20
    S2,1995,52.10
    S2,1996,51.40
    S3,2002,57.95
    S3,2003,47.95
    S3,2004,48.15
    S3,2005,37.80
    S3,2006,56.96
    S3,2007,48.91
    S3,2008,44.00
    S3,2009,45.35
    S3,2010,49.40
    S3,2011,51.19") 
ggplot(the.data, aes( x = year, y = value,linetype = "full data") ) +
  geom_point(aes(colour = source)) +
  # geom_smooth(aes(group = 1)) +
  stat_smooth(data=subset(the.data,source=="S1"), method = "lm", se = T) +
  stat_smooth(data=subset(the.data,source=="S2"), method = "lm", se = T) +
  stat_smooth(data=subset(the.data,source=="S3"), method = "lm", se = T) +
  stat_smooth(data = the.data, method = "lm", se = T) +
  geom_smooth(aes(color=source)) +
  # scale_colour_manual(name="Line Color",
  #                     values=c(myline1="red", myline2= "black", myline3= "purple", myline4 = "green")) +
  # scale_color_manual(name="Line Color", values = c("red", "black", "purple", "green")) +
  # scale_linetype_discrete(name = "")
  scale_colour_brewer(name = 'Trendline', palette = 'Set2') +
  scale_shape_manual(values = c(16, 17, 18))

mod1 <- lm(mpg ~ disp + am + am * disp,data=mtcars)
mtcars$am <- factor(mtcars$am,labels = c(“automatic”,“manual”))
ggplot(mtcars,aes(disp,mpg,group = am)) +
  ggplot(mtcars) +
  geom_jitter(aes(disp,mpg), colour=“blue”) +
  geom_smooth(aes(disp,mpg), method=lm, se=FALSE) +
  geom_jitter(aes(hp,mpg), colour=“green”) +
  geom_smooth(aes(hp,mpg), method=lm, se=FALSE) +
  geom_jitter(aes(qsec,mpg), colour=“red”) +
  geom_smooth(aes(qsec,mpg), method=lm, se=FALSE) +
  labs(x = “Percentage cover (%)“, y = “Number of individuals (N)“)
ggplot(mtcars,aes(disp,mpg,group = am, colour = am)) +
  geom_point() +
  # facet_wrap(mtcars$am,ncol = 2) +
  geom_smooth(method = “lm”)+
  geom()
# geom_smooth(aes(color=am)) +
geom_smooth(aes(mod1))

