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
# df$BMI <- df$weight/df$height
# df$BMI <- paste(format(round(df$BMI, 2)))

df$BMI <- paste(format(round((df$weight/df$height), 2)))

# h) calculate a function that creates a bmi for each person
BMI_function <- function (weight, height){
  # calculate BMI, only to two decimals
  BMI <- paste(format(round((weight/height), 2)))
  return(BMI)
}

BMI_function(47,160)

# i) use *apply functions to create a data.frame with code and bmi

new_df <- df[c("Code", "BMI")]

df_split <- split(new_df, new_df$Code)
df_split

apply_df <- lapply (df_split, BMI_function)
strike.coefs

# lapply(split_df, FUN = BMI_function)
# 
# apply_df <- sapply(split_df, BMI_function)


# j) use tapply to calculate the mean BMI by gender

tapply(df$BMI, list(df$gender), mean)

# j) modify function in question i to apply +1% correction 
# of the actual bmi for male participants


