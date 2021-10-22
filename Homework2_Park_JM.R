# Jung Mee Park
# jmpark@arizona.edu
# 2021-10-18
# INFO 523
# homework 2

### Part A

# a) assign value 26 to var x
x <- 26
# a) create vector y with values [2,36,1,98,124]
y <- c(2, 36, 1, 98, 124)
# a) multiply x and y
z <- x*y
# calculate the sum of all elements in z
sum(z)

# b) generate a seq from 0 to 100
seq(0,100)
# b) generate a seq from 4 to -25
seq(4, -25)

# c) generate a seq from -3 to 9 by 0.1
seq(-3,9, by = 0.1)

# d) define two vectors. t = mon through sat. 
t <- c("mon", "tue", "wed", "thu", "fri", "sat")

# d) m includes [90,80,50,20,5,20]
m <- c(90,80,50,20,5,20)

# d) concatenate both vectors column-wise into a 5 by two matrix
# each column is a vector
# combine the two vectors
# remove one element to get the right shape

study <- cbind(t,m)
study <- head(study, -1)

# e) create the following data.frame in R
Code <- c("A23","F84","R2","I322")
age <- c(21,35,829,2)
gender <- c("m","f","m","e")
height <- c(181,173,171,166)
weight <- c(69,58,75,60)

df <- cbind.data.frame(Code,age,gender,height,weight)

# f) calculate min and max value for column age
min(df$age)
max(df$age)

# f) use a logical query of under age 20 and above 80 and make those NA
df[age < 20 | age > 80, "age"] = NA

# g) calculate BMI. Round to two decimal places
options(digits=2)
df$BMI <- df$weight/df$height

# h) write a function for a single person
options(digits=2)
BMI_f <- function (weight, height){
  # calculate BMI, only to two decimals
  BMI_out <- weight/height
  return(BMI_out)
}
# 
BMI_f(47,160)

# i) use *apply functions to create a data.frame with code and bmi
new_df <- df[c("Code", "BMI")]

df_split <- split(new_df, new_df$Code)
df_split

index <- mapply (FUN = BMI_f, height = df$height, weight = df$weight) # we need values from two columns
index # this is a vector

cbind.data.frame(Code = df$Code, index) 

# From CRP 
lapply(1:N, function(x){
  yourfunction(arg1=data[x,col1], arg2=data[x,col2])
})

# lapply(df, function (weight,height){
#   # calculate BMI, only to two decimals
#   BMI_out <- weight(x)/height(y)
#   return(BMI_out)
# })
# 
# library("purrr")

# lapply(split_df, FUN = BMI_function)
# 
# apply_df <- sapply(split_df, BMI_function)

# j) use tapply to calculate the mean BMI by gender 
tapply(df$BMI, df$gender, mean)

# j) modify function in question i to apply +1% correction 
# of the actual bmi for male participants

# func <- function(x) x*1.01 # this is not quite it
# tapply(df$BMI, df$gender=="m", func) #This is applying the func for all of them.

BMI_m <- function (weight, height, gender){
  # if gender is male
  if(gender == "m"){
  # add a 1% correction by * 1.01
  BMI <- (weight/height)*1.01 
  return(BMI)
  } else {  # calculate BMI
    BMI <- (weight/height)}
  return(BMI)
} 
BMI_m(weight = df$weight[2], height = df$height[2], gender = df$gender[2])

mapply(BMI_m, weight = df$weight, height = df$height, gender = df$gender) 


# k) red wine dataset
#    a) from the website
wine_data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                      header = TRUE, sep=";") 
# head(wine_data)
summary(wine_data) # There are 12 variables. 
colMeans(wine_data) # This shows the mean value for each of the columns. 
# The data frame contains 12 numeric variables that describe red wine quality.

#    b) from local directory
setwd("~/Documents/INFO 523 fall 2021")
red_wine <- read.csv("winequality-red.csv", header = TRUE, sep= ";") #somehow this did not work

# For me, grabbing the data directly from the website was faster. 
# R was not reading the csv file  from my working directory correctly. 
# I had to try a few different times to identify the sep = ";".
# When you download direct from the website, you also eliminate errors such as your computer not being
# able to read the file or corrupting it during the download. 


                       