#         week  2 - Base plots
# plotting a histogram
library(datasets)
hist(airquality$Ozone) 
# plotting a boxplot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")
#plotting a scatterplot
with(airquality, plot(Wind, Ozone))
# testing deafult parameters
par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")
# Base plots with title
library(datasets)
# Make the initial plot
with(airquality, plot(Wind, Ozone))
# Add a title
title(main = "Ozone and Wind in New York City")  
# Adding colour to plots
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
# Adding legends to plots
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))
# Adding regression lines to plot
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
# Fit a simple linear regression model
model <- lm(Ozone ~ Wind, airquality)
# Draw regression line on plot
abline(model, lwd = 2)
# Plotting multiple base plots, panel plot with two plots
par(mfrow = c(1, 2))
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")})
# Panel plot with three plots
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
     plot(Temp, Ozone, main = "Ozone and Temperature")
     mtext("Ozone and Weather in New York City", outer = TRUE)
   })
#           Week 3
# reading the data into r using readr
library(readr)
data_EPA <- read_csv("C:/Users/mawufemor/Desktop/US EPA data 2017.csv")
# rewriting the names of the columns to remove spaces
names(data_EPA) <- make.names(names(data_EPA))
# checking the number of rows
nrow(data_EPA)
# checking the number of columns
ncol(data_EPA)
# reviewing the dataset
str(data_EPA)
# looking at the top of the data
head(data_EPA)
# looking at the botton of the data
tail(data_EPA)
# looking at only a few columns of the top of data
head(data_EPA[,c(6:7, 10)])
# looking at only a few columns of the tail of data
tail(data_EPA[,c(6:7, 10)])
# looking at the time measurements were taken
table(data_EPA$X1st.NO.Max.DateTime)
# installing dplyr library
library(dplyr)
#filter(data_EPA, X1st.NO.Max.DateTime == "15:00") %>%
#  select(state.name, County.name, Date.Local,
#         Time.Local, Sample.Measurement)
#
#filter(ozone, State.Code == "36" 
#               & County.Code == "033" 
#               & Date.Local == "2014-09-30") %>%
#           select(Date.Local, Time.Local, 
#                                   Sample.Measurement) %>% 
#           as.data.frame
#Date.Local Time.Local Sample.Measurement
# reviewing uniques State names 
unique(data_EPA$State.Name)
# validating with a dat source
summary(data_EPA$Parameter.Code)
# getting more detail on the distribution
quantile(data_EPA$Parameter.Code, seq(0, 1, 0.1))
# Identiyfing each county using a combination of state name and county name
rank_EPA <- group_by(data_EPA, State.Name, County.Name) %>%
  summarize(data_EPA = mean(X1st.Max.DateTime)) %>%
  as.data.frame %>%
  arrange(desc(data_EPA))
rank_EPA
# looking at the top 10 counties in this ranking 
head(rank_EPA, 10)
# looking at the botton 10 counties in this ranking 
tail(rank_EPA, 10)
# reviewing number of observations for Alaska
#filter(data_EPA, State.Name == "California" & County.Name == "Riverside") %>%
#  mutate(month = factor(months(Date.Local), levels = month.name)) %>%
#  group_by(month) %>%
#  summarize(data_EPA = mean(Pollutant.Standard))
# converting the date variable into a Date class
# reviewing one o the counties
filter(data_EPA, state.name == "California" & County.Name == "Riverside") %>%nrow
# Randomizing the data
set.seed(10234)
N_EPA <- nrow(data_EPA)
id_EPA <- sample(N_EPA, N_EPA, replace = TRUE)
data_EPA2 <- data_EPA[id_EPA, ]
# reconstructing the rankings based on resampled data
rank_EPA2 <- group_by(data_EPA2, State.Name, County.Code) %>%
  summarize(data_EPA= mean(X1st.Max.DateTime)) %>%
  as.data.frame %>%
  arrange(desc(data_EPA))
# reviewing top 10 ranking based on resampled data
cbind(head(rank_EPA, 10),
      head(rank_EPA2, 10))
# reviewing bottom 10 ranking based on resampled data
cbind(tail(rank_EPA, 10),
      tail(rank_EPA2, 10))

#         week 4
# 1. combining all the information collected into one datarame
pirate_info <- data.frame("Name" = c("Astrid", "Lea", "Sarina","Remon","Letizia","Babice","Jonas","Wendy","Niveditha","Gioia"),
                          "sex" = c("F","F","F","M","F","F","M","F","F","F"),
                          "Age" = c(30,25,25,29,22,22,35,19,32,21),
                          "Superhero" = c("Batman","Superman","Batman","Spiderman","Batman","Antman","Batman","Superman","Maggot","Superman"),
                          "Tatoos" = c(11,15,12,5,65,3,9,13,900,0))
pirate_info
# 2. finding the median age of 10 pirates
median(pirate_info$Age)
# 3. finding the mean age of female and male pirates separately
#mean(pirate_info$Age[pirate_info$sex=="F"])
with(pirate_info,mean(pirate_info$Age[sex=="F"]))
with(pirate_info,mean(pirate_info$Age[sex=="M"]))
# 4. finding the most number of tattoos owned by a male pirate
with(pirate_info,max(pirate_info$Tatoos[sex=="M"]))
# 5. percentage of female pirates under the age of 32
with(subset(pirate_info,pirate_info$sex == "F"), mean(pirate_info$Age<32))
# 6. percentage of pirates under the age of 32
with(subset(pirate_info),mean(pirate_info$Age<32))
# 7. adding a new tattoos.per.year to the dataframe
pirate_info$tattoos.per.year <- c(12,19,100,53,24,1,2,0,9,5)
pirate_info
# 8. pirate with the most number pf tattoos per year
with(pirate_info, Name[tattoos.per.year == max(tattoos.per.year)])
# 9. names of female pirates whose favorite superhero is superman
subset(x=pirate_info, subset = sex == "F" & Superhero == "Superman", select = Name)
# 10. median number of tattoos of pirates over the age of 20 whose favorite superhero is Spiderman     
with(pirate_info, sum(Age > 20 & Superhero == "Spiderman"))
## PRACTICE 2 - week4
library(tidyverse)
typeof(letters)
typeof(1:10)
x <- list("a", "b", 1:10)
length(x)
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)
typeof(1)
typeof(1L)
1.5L
x <- sqrt(2) ^ 2
x
x -2 
c(-1, 0, 1)/0
is.finite(0)
is.infinite(Inf)
is.na(NA)
is.nan(NaN)
pryr::object_size(x)
x <- "This is a reasonably long string"
pryr::object_size(x)
y <- rep(x, 1000)
pryr::object_size(y)
NA
NA_integer_
NA_real_
NA_character_
# 1. Describe the difference between is.finite(x) and !is.infinite(x)
# Both functions accomplish the same goal. Mathematically, !is.infinite is a negation of is.finite is.finite() tests whether a value is not infinite 
# whereas !is.infinite() tests whether a values is infinite.

# 2. Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). How does it work?
# dplyr::near() calculates the tolerance level for differnce between two values.

# 3. A logical vector can take 3 possible values. How many possible values can an integer vector take? How many values can a double take? 
# Integer vector can take one value , NA. Whereas double can take four values: -Inf, Inf, NaN and NA

# 4. Brainstorm at least four functions that allow you to convert a double to an integer
# a) as.integer() - returns the integer value
# b) floor() - rounds down to the nearest integer
# c) round 0.5 up, round 0.5 down
# d) ceil ()

# 5. what functions from teh readr package allow you to turn a string into logical, integer, and double vector ?
# parse()

x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)
mean(y)
if(length(x)) {
}
typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))
sample(10)+100
runif(10) > 0.5
1:10+1:2
1:10+1:3
kl <- 1:10+1:3
rep(kl)
tibble(x= 1:4, y=1:2)
tibble(x= 1:4, y= rep(1:2, 2))
c(x=1, y=2, z=4)
set_names(1:3, c("a", "b", "c"))
x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]
x[c(1,1,5,5,5,2)]
x[c(-1, -3, -5)]
x[c(1,-1)]
x[0]
x <- c(10,3,NA,5,8,1,NA)
x[!is.na(x)]
x[x %%2 == 0]
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz","def")]
# Q1. What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?
# Mean(is.na(x)) gives us the proportion of elements in the vector that are missing values.
# Sum(!is.finite(x)) gives us the number of elements in the vector that are not finite values

# Q2. Carefully read the documentation of is.vector(). What does it actually test for? Why does is.atomic() not agree with the definition of atomic vectors above?
# is.vector() tests whether x is vector os the specified mode. is.atomic() returns TRUE is x is an atomic value, this is also TRUE for NULL values

# Q3. Compare and contrast setNames() with purrr::set_names()
#  setNames() chnages the name of the data frame or table by reference and returns the object
# puyrr::Set_names() is used to set the names of objects in a pipeline

# Q4. Create functions that take a vector as input and returns:
# 1. The last value. Should you use [ or [[? - get_last <- function(x){x[[length(x)]]}: we use [[ to retrieve justa  single value
# 2. The elements at even numbered positions. - get_even_pos <- function(x){x[c(FALSE, TRUE)]}: both elements will be recycled to the full length of x
# 3. Every element except the last value - drop_last <- x[-length(x)]: drops the last value 
# 4. Only even numbers (and no missing values).- get_even_val <- function(x){s[!is.na(x) & x %% 2 == 0]}

# Q5.Why is x[-which(x > 0)] not the same as x[x <= 0]?
# 

# Q6. What happens when you subset with a positive integer that's bigger than the length of the vector? What happens when you subset with a name that doesn't exist?
# Returns N/A for all values beyond the length of the vector. Also NA for non-existing names

x <- list(1,2,3)
x
str(x)
x_named <- list(a=1, b=2, c=3)
str(x_named)
y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1,2), list(3,4))
str(z)
x1 <- list(c(1,2), c(3,4))
x2 <- list(list(1,2), list(3,4))
x3 <- list(1, list(2, list(3)))
a <- list(a=1:3, b="a string", c=pi, d=list(-1,-5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])
a$a
a[["a"]]

# Q1. Draw the following lists
# list(a,b,list(c,d),list(e,f))
# list(list(list(list(list(list(a))))))

# Q2. What happens if you subset a tibble as if you're subsetting a list? What are the key differences between a list and a tibble?
# x_list <- list(a = c(1,2,3), b = c("d","e","f"), c = 1:5])
# x_tibble <- tibble(a = c(1,2,3), b = c("d","e","f"), c = 1:5)
# A tibble is similar to a list. Subsetting a tibble produces a tibble unless [[]] is used which produces a vector

# Augmented Vectors
# Q1. What does hms::hms(3600) return? How does it print? What primitive type is the augmented vector built on top of? What attributes does it use?
# This produces the time in hours, minutes and seconds. Its atrributes are units (seconds) and class (minutes)

# Q2. Try and make a tibble that has columns with different lengths. What happens?
 tib <- tibble(x=c("a","b","c"), y=c(1:5))
# The function throws an error because columns must be of equal size in a tibble. This is not the case in the fucntions above
 
 # Q3. Based on the definition above, is it ok to have a list as a column of a tibble?
 # It is possible in cases where the vectors all have the same column length

 
 
#             Week 5  
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("labeling")
install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)
library(labeling)
library(ggplot2)
library(tidyverse)
table1
table2
table3
# Spread across 2 tibbles 
table4a # cases
table4b # population
# compute rate per 10,000
table1 %>%
   mutate(rate = cases / population * 10000)
# compute cases per year 
table1 %>%
   count(year, wt = cases)
# visualize changes over time
ggplot(table1, aes(year, cases)) +
   geom_line(aes(group = country), colour = "grey50") +
   geom_point(aes(colour = country))
# description 
# Exercise 12.2.1
# Q1. Using prose, describe how the variables and observations are organised in each of the sample tables.
# In Table 1, year and country identiy each row as observations, the variables are represented in the cases and population columns
# In Table 2, each row indicates country, variable and year. Variables count and population are identified on the columns
# In Table 3, each row is represnted by country and year. The column rate contains variables count and population in a string format
# In Table 4, we can see two distinct tables. Table 4a contains the cases and 4b represents population counts. For both tables, each row contains 
# values for country and year
# Exercise 12.2.2
# creating separate tables for cases and populations 
table2_cases <- filter(table2, type == "cases") %>%
 rename(cases = count) %>%
 arrange(country, year)
table2_population <- filter(table2, type == "population") %>%
  rename(population = count) %>%
  arrange(country, year)
table2_cases
table2_population
# creating a new dataframe with cases and population columns. Calculating the cases per capita in a new column 
t2cases_per_capita <- tibble(
  year = table2_cases$year,
  country = table2_cases$country,
  cases = table2_cases$cases,
  population = table2_population$population
)%>%
mutate(cases_per_capita = (cases / population) * 10000) %>%
   select(country, year, cases_per_capita)
t2cases_per_capita <- t2cases_per_capita %>%
   mutate(type = "cases_per_Capita") %>%
   rename(count = cases_per_capita)
bind_rows(table2, t2cases_per_capita) %>%
   arrange(country, year, type, count)
table4c <- tibble(
   country = table4a$country, 
   `1999` = table4a[["1999"]] / table4b[["1999"]] * 10000,
   `2000` = table4a[["2000"]] / table4b[["2000"]] * 10000
 )
 table4c
# Table 2 is slight easier to work with in this case beacuse we are able to filter without too much work.
# Table 4 was already split into two distinct tables so it was realtively easy to divide cases by population, however this step needed to
# be reapeated to each row. The mutate function made is easy to do all the necessary work despite the arrangement of the tables
table2 %>%
   filter(type =="cases") %>%
   ggplot(aes(year, count)) +
   geom_line(aes(group = country), colour = "grey50") +
   geom_point(aes(colour = country)) +
   scale_x_continuous(breaks = unique(table2$year)) +
   ylab("cases")
tidy4a <- table4a %>%
   gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
   gather(`1999`, `2000`, key = "year", value = "population")
 # Joining two tables, table4a and table4b with left_join
left_join(tidy4a,tidy4b)
table2
table2 %>%
  spread(key = type, value = count)
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>%
  spread(year, return) %>%
  gather(`2015`:`2016`, key = "year", value = "return")
 
# spread() and gather() are not perfectly symmetrical because column type information is not available. Gather() discards the original column types
# by gathering all the variables and coercing them into a single type. The use of spread() on the same dataframe after the use
# of gather does not produce similar results since it does not know the original datatypes of the variables. Using the type.convert() function helps to solve this 
# problem and makes gather() and spread() symmetrical 
stocks %>%
  spread(year, return)
stocks %>%
  spread(year, return) %>%
  gather(`2015`:`2016`, key = "year", value = "return", convert = TRUE)
table4a %>%
  gather(1999, 2000, key = "year", value = "cases")
 
# This code fails because the gather() function in tidyverse views '1999' and '2000' as column numbers/variables instead off column headers.
# The solution to this problem is to put quotation marks around the values so they are recognized as character values rather than column variables which do not exist
# see code below
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
table4b %>%
  gather(`1999`,`2000`,key = "year", value = "population")
people <- tribble(
 ~name, ~key, ~value
)
glimpse(people)
spread(people, key, value)
# In this case, spreading the dataframe fails because there are no unique identifiers in this dataframe. This can be resolved by adding unique row counts for each row 
# in order to serve as a unique identifier for each possible combinations of values before spreading the dataframe. For example, Philip Woods has two different values for age
people_again <- people %>%
  group_by(name,key) %>%
  mutate(obs = row_number())
people_again
spread(people_again, key, value)
people %>%
  distinct(name, key, .keep_all = TRUE) %>%
  spread(key, value)
preg <- tribble(
 ~pregnant, ~male, ~female,
  "yes", NA, 10,
 "no", 20, 12
)
sex("female", "male")
pregnant ("yes", "no")
count
preg_tidy <- preg%>%
  gather(male, female, key = "sex", value = "count")
preg_tidy
preg_tidy2 <- preg %>%
  gather(male, female, key = "sex", value = "count", na.rm = TRUE)
preg_tidy2
preg_tidy3 <- preg_tidy2 %>%
  mutate(
    female = sex == "female",
    pregnant = pregnant == "yes"
  ) %>%
  select(female, pregnant, count)
preg_tidy3
filter(preg_tidy2, sex =="female", pregnant == "no")
filter(preg_tidy3, female, !pregnant)
# Separating and Uniting 
# separate
table3
table3 %>%
  separate(rate, into = c("cases", "population"))
 table3 %>%
   separate(rate, into = c("cases", "population"), sep ="/")
 table3 %>%
   separate(rate, into = c("cases", "population"), convert = TRUE)
 table3 %>%
   separate(year, into = c("century","year"), sep = 2)
 #unite
 table5 %>%
   unite(new, century, year)
 table5 %>%
   unite(new, century, year, sep = "")
 # what do the extra and fill arguments do in separate()? Experiment with
 # the various options for the following two toy datasets
 tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
   separate(x, c("one", "two", "three"))
 tibble(x = c("a,b,c","d,e", "f,g,i")) %>%
   separate(x, c("one","two","three"))
 # when we run the two sets of data, we get error warning. Experiemnting with fill and extra
 tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
   separate(x, c("one", "two", "three"), extra = "warn")
 tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
   separate(x, c("one", "two", "three"), extra = "merge")
 tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
   separate(x, c("one", "two", "three"), extra = "drop")
 
 tibble(x = c("a,b,c","d,e", "f,g,i")) %>%
   separate(x, c("one","two","three"), fill = "right")
 tibble(x = c("a,b,c","d,e", "f,g,i")) %>%
   separate(x, c("one","two","three"), fill = "warn")
 tibble(x = c("a,b,c","d,e", "f,g,i")) %>%
   separate(x, c("one","two","three"), fill = "left")
 # Both unite() and separate() have remove argument. waht does it do? why would you
 # set it to FALSE
 # compare and contrast separate () and extract (). why are there three variations of separation
 # (by position, by separator, and with groups), but by only one unite?
 
 # missing values
 # Explicitly, i.e. flagged with NA.
 # Implicitly, i.e. simply not present in the data.
 stocks <- tibble (
   year = c(2015,2015,2015,2015,2016,2016,2016),
   qtr = c(1, 2, 3, 4, 2, 3, 4),
   return = c(1.88,0.59,NA,0.92,0.17,2.66)
 )
 stocks %>%
   spread(year, return)
 stocks %>%
   spread(year, return) %>%
   gather(year, return, `2015`:`2016`, na.rm = TRUE)
 stocks %>%
   complete(year, qtr)
 treatment <- tribble(
   ~ person,           ~ treatment, ~response,
   "Derrick Whitmore",1,             7,
   NA,                2,             10,
   NA,                3,             9,
   "Katherine Burke", 1,             4
 )
 treatment %>%
   fill(person)
 # compare and contrast the fill() arguments to spread() and complete()
 # what does the direction argument to fill() do
 
 # case study
 who
 # gathering together all teh columns from new_sp_m014 to newrel_f65.
 # we laso use na.rm so we can focus on he values that are present 
 who1 <- who %>%
   gather(new_sp_m014:newrel_f65, key = "key", value ="cases", na.rm = TRUE)
 who1
 # counting all the columns
 who1 %>%
   count(key)
 # formatting the column names to make all the variables consistent 
 who2 <- who1 %>%
   mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
 who2
 # separating the values with separate
 who3 <- who2 %>%
   separate(key, c("new", "type","sexage"), sep = "_")
 who3
 # counting all the columns and dropping columns iso2, iso3 since they are redundant
 who3 %>%
   count(new)
 who4 <- who3 %>%
   select(-new, -iso2, -iso3)
 # separate the sexage into sex and age by splitting after the first character
 who5 <- who4 %>%
   separate(sexage, c("sex", "age"), sep =1)
 who5
 # combining all the different pieces above together to create one code
 who %>%
   gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
   mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
   separate(key, c("new", "var", "sexage")) %>%
   select(-new, -iso2, -iso3) %>%
   separate(sexage, c("sex","age"), sep = 1)
 # 1.  In this case study I set na.rm = TRUE just to make it easier to check that we had the correct values. Is this reasonable? 
 # Think about how missing values are represented in this dataset. Are there implicit missing values? What's the difference between an NA and zero?
 # NA represents missing values whereas 0 is for cases that are not present
 # 2. What happens if you neglect the mutate() step? (mutate(key = stringr::str_replace(key, "newrel", "new_rel")))
 # When we neglect the mutate() step, we are unable to identify newrel observations/values
 # 3. I claimed that iso2 and iso3 were redundant with country. Confirm this claim.
 who %>%
   count(country, iso2, iso3)
 # 4. For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.
 t_who %>%
   group_by(country) %>%
   mutate(cases_per_country = sum(cases)) %>%
   group_by(country, year, sex) %>%
   filter(cases_per_country > 1000000, year > 1995) %>%
   count(wt = caes) %>%
   ggplot(aes(year,n)) +
   geom_line(aes(color = country)) +
   facet_wrap(~sex, nrow = 2) 
 
 
 
 
 