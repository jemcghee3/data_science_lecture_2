#Sys.setenv(LANG="en")  #if you want the system language as English 

####################################
# Data visualisation



# Using the build in data set "mtcars"
mtcars


# First Insights
str(mtcars)


# Mean of wt column of mtcars
mean(mtcars$wt)

# Median of wt column of mtcars
median(mtcars$wt)

# Mode of wt column of mtcars
y <- table(mtcars$wt)
names(y)[which(y==max(y))]

# ... or everthing in one nice command:
summary(mtcars)


# plot the data of column wt
ggplot(mtcars) + geom_point(mapping = aes(x = 1:length(wt), y = wt))

# plot the data of column wt againts column dart
ggplot(mtcars) + geom_point(mapping = aes(x = drat, y = wt))

# boxplot of column wt
boxplot(mtcars$wt)

# histogram of column wt
hist(mtcars$wt)

# Q-Q plot of column wt
qqnorm(mtcars$wt)

# Q-Q plot with assumed line of column wt
qqline(mtcars$wt)



# Correlation between the attributes
cor(mtcars)
# ... or with only two digits after the comma:
round(cor(mtcars),2)


# plotting the correlation matrice

install.packages("corrplot")
library(corrplot) 
corrplot(cor(mtcars), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)







####################################
# Data Cleaning

# installing additional packages
# ... an additional package containing flights and
install.packages("nycflights13")
# ... an additional package containing nice functions
install.packages("tidyverse")

# after the installation these packages need to be "activated"
library(nycflights13)
library(tidyverse)

# see some data
flights

# count the data
count(flights)

# filtering some data lines
filter(flights, month == 1)

# filter thoses which have "NA" in departure time
filter(flights, is.na(dep_time))

# ... count them
count(filter(flights, is.na(dep_time)))

# filter thoses which have NO "NA" in departure time
filter(flights, ! is.na(dep_time))

# ... count them
count(filter(flights, ! is.na(dep_time)))

# remember those in variable "my_flights" which have NO "NA" in departure time
my_flights <- filter(flights, ! is.na(dep_time))


# replace the missing "dept_time" (i.e. NA) with the values from the "sched_dep_time"

# first make a copy of flights
flights_with_replaced_dep_time <- flights
# then identify the NA in "dep_time". IF it is there, then replace it with 
# "sched_dep_time" else take the original value from "dep_time" 
flights_with_replaced_dep_time$dep_time <- ifelse(is.na(flights$dep_time), flights$sched_dep_time, flights$dep_time)

# ... and count them
count(filter(flights_with_replaced_dep_time, is.na(dep_time)))

# ... check it for an example:

filter(flights_with_replaced_dep_time, tailnum == "N18120")

# replace the missing "dept_time" (i.e. NA) with the a fixed value

replacement <- 1200

flights_with_replaced_dep_time$dep_time <- 
  ifelse(is.na(flights$dep_time), 
          replacement,
          flights$dep_time)


replacement <- as.integer(mean(flights$dep_time, na.rm = TRUE))

flights_with_replaced_dep_time$dep_time <- 
  ifelse(is.na(flights$dep_time), 
         replacement,
         flights$dep_time)



# Identifing outliers
# lets plot the departure delay
ggplot(flights) + geom_point(mapping = aes(x = flight, y = dep_delay))

# there are some flights which really depart earlier than scheduled; 
arrange(flights, dep_delay)

# catch them
minus_delay <- filter(flights, dep_delay <= 0)

# analyse them
boxplot(minus_delay$dep_delay)

# remove those which have more delay than -29
my_flights <- filter(flights, dep_delay > -29)



####################################
# transformation

# Deleting columns
# deleting a column, e.g. dep_delay
my_flight <- subset(flights,select=-c(dep_delay))

# deleting several columns, e.g. dep_delay and flight
my_flight <- subset(flights,select=-c(dep_delay,flight))

# Transforming dep_delay
my_flights <- filter(flights, ! is.na(dep_time))
my_flights <- filter(my_flights, dep_delay > -29)

# anaylsing departure delay: imbalanced, looks like a logarithmic distribution
hist(my_flights$dep_delay)

# converting it to a more uniform distribution ...
hist(log(my_flights$dep_delay))

# ... but it produce additional NA (negative delay causes this NA). We need to shift our values
minimum <- min(my_flights$dep_delay,na.rm = TRUE)
hist(log(my_flights$dep_delay - minimum))

# Looks better; keep it
my_flights$dep_delay <- log(my_flights$dep_delay - minimum)



# normalisation
# installing additional packages
# ... an additional package containing flights and
install.packages("nycflights13")
# ... an additional package containing nice functions
install.packages("tidyverse")

# after the installation these packages need to be "activated"
library(nycflights13)
ibrary(tidyverse)

# see some data
flights

# cathing them which don't have NA
my_flights <- filter(flights, ! is.na(dep_time))

# we would like to normalise the departure time between 0 and 1 
my_flights$dep_time <- my_flights$dep_time / 2400

# however the coding of time in integer is not continuous. E.g. 1178 would never exists
# we need a conversion function "time_conversion", which translates that into continuous 
# numbers
time_conversion <- function(x) {
h <- trunc(x/100,0)
m <- x-(h*100)
r <- m+(h*60)
return(r)
}

# apply that function to flights
my_flights$dep_time <- time_conversion(my_flights$dep_time) / (24*60)




####################################
# Transorming Variables


titanic <- data.frame(Titanic)
View(titanic)

my_titanic <- titanic

# Transforming the categorical (nominal) variable Survived
# Survied is interpreted as an ordinal
# How many (unique) values do Survived have?
f <- factor(titanic$Survived)
levels(f)

typeof(f)

as.integer(f)

my_titanic$Survived <- as.integer(f)

# Transforming the categorical (nominal) variable Sex
# We use a specialised package
#install.packages("fastDummies")
library(fastDummies)

my_titanic <- dummy_cols(my_titanic, select_columns="Sex", remove_selected_columns = TRUE)


# Transforming the ordered variable Age
# Age is qualitative but ordered. This time we would like to influence 
# how the different values are translated into number. An Adult is older 
# than a child. Therefore Chile = 1, Adult = 2
ordered(my_titanic$Age, levels= c("Child", "Adult"))

as.integer(ordered(my_titanic$Age, levels= c("Child", "Adult")))

my_titanic$Age <- as.integer(ordered(my_titanic$Age, levels= c("Child", "Adult")))

# Transforming the partly ordered variable Class
# Class is qualitative but partly ordered. While the three values (”1st”, 
# “2nd”, “3rd”) are obviously ordered, is the value “Crew” a little bit 
# seperated from that. Therefore we need to have a boolean column for “Crew” 
# but an ordered culumn for the other three values.

# Extracting the crew members:
ifelse(my_titanic$Class =="Crew",1,0)

# Add them as new columns "Class_Crew"
my_titanic$Class_Crew <- ifelse(my_titanic$Class =="Crew",1,0)

# Now handle the rest of the values in column Class as ordered
ordered(my_titanic$Class, levels= c("Crew", "3rd", "2nd","1st"))

ordered(my_titanic$Class, levels= c("Crew", "3rd", "2nd","1st"))

as.integer(ordered(my_titanic$Class, levels= c("Crew", "3rd", "2nd","1st")))

as.integer(ordered(my_titanic$Class, levels= c("Crew", "3rd", "2nd","1st")))-1

my_titanic$Class <- as.integer(ordered(my_titanic$Class, levels= c("Crew", "3rd", "2nd","1st")))-1

