## Data Preprocessing on mtcars ##

### Preliminaries
#install.packages("tidyr")
#install.packages("corrplot")
library(tidyr)
library(corrplot) 



# Load mtcars_dirty.csv 
setwd("~/FHNW/O365_G_DataScience_Admin - General/_2021/Slides/02-DataPreprocessing/_laboratory")
cgh_cars <- read.csv("cgh_cars.csv") 

################
# Analyse the data

# A.1: Show the data
cgh_cars
str(cgh_cars)
# Because we would like to apply a linear regression we need to transform
# BRAND; TYPE, and STYLE into quantivative values, i.e. numbers

### A.2
is.na(cgh_cars)
sum(is.na(cgh_cars))
colSums(is.na(cgh_cars))
# NAs in MPG and PRICE needs to be handled

### A.3
summary(cgh_cars)
# There seems to be a problem in MPG: 0, NA
# There seems to be a problem in CYL: negative values
# There seems to be a problem in WT: big variations 
# There seems to be a problem in GEAR: should be integer (numbers without decimals) 
# There seems to be a problem in PRICE: NA 
# (There seems to be a problem in DISP: scaling) 

### A.4 Correlation

corrplot(cor(mtcars), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


################
# Clean the Data

### C.1: Zeros Values in MPG (2 types of missing data in mpg - NA and 0.0)
boxplot(cgh_cars$mpg)
cgh_cars$mpg <- ifelse(cgh_cars$mpg == 0.0, NA, cgh_cars$mpg)
colSums(is.na(cgh_cars))

### C.2: NAs in MPG 

# BTW, NA values impact the calculation of mean
mean(cgh_cars$mpg) 
# ignore null values and calculate mean
mean(cgh_cars$mpg, na.rm = TRUE) 

# replace NAs in mpg column with a mean for other values
cgh_cars$mpg <- ifelse(is.na(cgh_cars$mpg), mean(cgh_cars$mpg, na.rm=TRUE), cgh_cars$mpg) 
# OR replace NAs in mpg column with a median for other values
cgh_cars$mpg = ifelse(is.na(cgh_cars$mpg), median(cgh_cars$mpg, na.rm=TRUE), cgh_cars$mpg) 

### C.3 NAs in PRICE

cgh_cars <- drop_na(cgh_cars,price)

colSums(is.na(cgh_cars))


# C.4: some values in cyl are negative

# check if there are negative values (use previous dataset to make incremental changes)
cgh_cars["cyl"] < 0                 
# find the number of negative values
colSums(cgh_cars["cyl"] < 0)        

# Please note the difference between both:
typeof(cgh_cars$cyl)
typeof(cgh_cars["cyl"])

# replace negative numbers by positive
cgh_cars$cyl = ifelse((cgh_cars$cyl < 0), cgh_cars$cyl*-1, cgh_cars$cyl) 
# verify if successfully replaced
colSums(cgh_cars["cyl"] < 0)            

### C.5 some values in wt have a missing decimals, some are in pounds instead of kilo pounds

# find numbers that have a quotient more than 1 digit (outliers with very high weight)
cgh_cars["wt"] %/% 10 > 0               
# find the number
colSums(cgh_cars["wt"] %/% 10 > 0 )      
# list row details
cgh_cars[cgh_cars$wt %/% 10 > 0, ]    

# first convert the largest value
cgh_cars[cgh_cars$wt %/% 1000 > 0, ]    
cgh_cars$wt <- ifelse((cgh_cars$wt %/% 1000 > 0), cgh_cars$wt/1000, cgh_cars$wt)
# now convert the second largest value
cgh_cars[cgh_cars$wt %/% 100 > 0, ]     
cgh_cars$wt <- ifelse((cgh_cars$wt %/% 100 > 0), cgh_cars$wt/100, cgh_cars$wt)
# finally the smallest conversion
cgh_cars[cgh_cars$wt %/% 10 > 0, ]      
cgh_cars$wt <- ifelse((cgh_cars$wt %/% 10 > 0), cgh_cars$wt/10, cgh_cars$wt)
# verify if all conversions are complete
colSums(cgh_cars["wt"] %/% 10 > 0 )            



### C.6 Noisy data in gear

# Everything below 1.5 will be considered at 1 gear
cgh_cars$gear <- ifelse((cgh_cars$gear < 1.5), 1, cgh_cars$gear)
# Everything between 1.5 and 2.499 will be considered as 2 gears
cgh_cars$gear <- ifelse((cgh_cars$gear >= 1.5) & (cgh_cars$gear < 2.5), 2, cgh_cars$gear)
# Everything between 2.5 and 3.499 will be considered as 3 gears
cgh_cars$gear <- ifelse((cgh_cars$gear >= 2.5) & (cgh_cars$gear < 3.5), 3, cgh_cars$gear)
# Everything between 3.5 and 4.499 will be considered as 4 gears
cgh_cars$gear <- ifelse((cgh_cars$gear >= 3.5) & (cgh_cars$gear < 4.5), 4, cgh_cars$gear)
# Everything between 4.5 and 5.499 will be considered as 5 gears
cgh_cars$gear <- ifelse((cgh_cars$gear >= 4.5) & (cgh_cars$gear < 5.5), 5, cgh_cars$gear)
# Everything above 5.5 will be considered as 5 gears
cgh_cars$gear <- ifelse((cgh_cars$gear >= 5.5), 6, cgh_cars$gear)

# ... or this variant
cgh_cars$gear <- round(cgh_cars$gear, digits = 0)


##############################
### Variable Transformation

### VT.1 Transforming STLYE

unique(cgh_cars$style)
ordered(cgh_cars$style, levels= c("basic", "medium","luxus"))
as.integer(ordered(cgh_cars$style, levels= c("basic", "medium","luxus")))
cgh_cars$style <- as.integer(ordered(cgh_cars$style, levels= c("basic", "medium","luxus")))
cgh_cars$style

### VT.2 Transforming TYPE

unique(cgh_cars$type)

cgh_cars[cgh_cars$type == "ship",]
cgh_cars <- cgh_cars[cgh_cars$type != "ship",]

ordered(cgh_cars$type, levels= c("hatchback","limosine","station","coupe","convertible"))
as.integer(ordered(cgh_cars$type, levels= c("hatchback","limosine","station","coupe","convertible")))
cgh_cars$type <- as.integer(ordered(cgh_cars$type, levels= c("hatchback","limosine","station","coupe","convertible")))
cgh_cars$type

### VT.3 Transforming BRAND
# install.packages("fastDummies")
library(fastDummies)

cgh_cars <- dummy_cols(cgh_cars, select_columns="brand", remove_selected_columns = TRUE)

str(cgh_cars)


###################################3
### Correlation Analysis

# install.packages("corrplot")
library(corrplot) 
corrplot(cor(cgh_cars), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

