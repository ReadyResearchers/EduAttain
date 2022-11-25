# EDUATTAIN 
# ANALYSIS SOURCE CODE
# Kyrie Doniz, CS600, Fall 2022

########################################################

# remove packages
remove.packages("RSQLite")

# install necessary packages
install.packages("RSQLite")
install.packages("vtable")
install.packages("plotly")

# load necessary libraries
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(psych)
library(vtable)
library(plotly)

# setting database path
db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# connect to database
conn <- dbConnect(drv = SQLite(), dbname = db)

# view structure
dbGetQuery(conn, "SELECT type, tbl_name  FROM sqlite_master")

# list fields in cps
dbListFields(conn,"CPS")

################################################
# QUERIES 
################################################

# query to display the first 5 rows
q <- 'SELECT * from CPS;'
result <- dbGetQuery(conn,q)

# filter header out (already there)
result = result[-1,]

# display data
head(result)

# looking at yr ranges in data
rng <- range(result$YEAR)
rng

###### 

# query to get all data from 2014 for sex + educ attain
q1 <- 'SELECT cpsidp, sex, educ, month, age, statefip FROM CPS WHERE year = 2014'
data2014 <- dbGetQuery(conn, q1)
# test to check if it is a dataframe --> need to remove
data2014
class(data2014)

# display data
head(data2014)

# view data sets
View(data2014)

###### 

# query to get all data from 2010 for sex + educ attain
q2 <- 'SELECT cpsidp, sex, educ, month, age, statefip FROM CPS WHERE year = 2010'
data2010 <- dbGetQuery(conn, q1)
# test to check if it is a dataframe --> need to remove
data2010
class(data2010)

# display data
head(data2010)

# view data sets
View(data2010)

#################################
# Filter Data / Styling Data for Visuals
#################################

# replacing educational attainment numerical codes for textual values


# 2010
data2010$EDUC[data2010$EDUC == "10"]<-"Grades 1-4"
data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
data2010$EDUC[data2010$EDUC == "20"]<-"Grades 5-6"
data2010$EDUC[data2010$EDUC == "30"]<-"Grades 7-8"
data2010$EDUC[data2010$EDUC == "40"]<-"HS, Grade 9"
data2010$EDUC[data2010$EDUC == "50"]<-"HS, Grade 10"
data2010$EDUC[data2010$EDUC == "60"]<-"HS, Grade 11"
data2010$EDUC[data2010$EDUC == "71"]<-"HS, Grade 12, no diploma"
data2010$EDUC[data2010$EDUC == "73"]<-"HS Diploma or Equiv."
data2010$EDUC[data2010$EDUC == "81"]<-"Some college, no degree"
data2010$EDUC[data2010$EDUC == "91"]<-"Occupational/Vocational Program Degree"
data2010$EDUC[data2010$EDUC == "92"]<-"Associate's Degree, Academic"

# 2014

data2014$EDUC[data2014$EDUC == "10"]<-"Grades 1-4"
data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
data2014$EDUC[data2014$EDUC == "20"]<-"Grades 5-6"
data2014$EDUC[data2014$EDUC == "30"]<-"Grades 7-8"
data2014$EDUC[data2014$EDUC == "40"]<-"HS, Grade 9"
data2014$EDUC[data2014$EDUC == "50"]<-"HS, Grade 10"
data2014$EDUC[data2014$EDUC == "60"]<-"HS, Grade 11"
data2014$EDUC[data2014$EDUC == "71"]<-"HS, Grade 12, no diploma"
data2014$EDUC[data2014$EDUC == "73"]<-"HS Diploma or Equiv."
data2014$EDUC[data2014$EDUC == "81"]<-"Some college, no degree"
data2014$EDUC[data2014$EDUC == "91"]<-"Occupational/Vocational Program Degree"
data2014$EDUC[data2014$EDUC == "92"]<-"Associate's Degree, Academic"


# replacing state fips code with state name
data2010$STATEFIP[data2010$STATEFIP == "6"]<-"CA"
data2010$STATEFIP[data2010$STATEFIP == "48"]<-"TX"
data2010$STATEFIP[data2010$STATEFIP == "36"]<-"NY"
data2010$STATEFIP[data2010$STATEFIP == "12"]<-"FL"
data2010$STATEFIP[data2010$STATEFIP == "17"]<-"IL"
data2010$STATEFIP[data2010$STATEFIP == "42"]<-"PA"
data2010$STATEFIP[data2010$STATEFIP == "39"]<-"OH"
data2010$STATEFIP[data2010$STATEFIP == "24"]<-"MD"
data2010$STATEFIP[data2010$STATEFIP == "26"]<-"MI"
data2010$STATEFIP[data2010$STATEFIP == "27"]<-"MN"
data2010$STATEFIP[data2010$STATEFIP == "8"]<-"CO"
data2010$STATEFIP[data2010$STATEFIP == "13"]<-"GA"
data2010$STATEFIP[data2010$STATEFIP == "51"]<-"VA"
data2010$STATEFIP[data2010$STATEFIP == "9"]<-"CT"
data2010$STATEFIP[data2010$STATEFIP == "37"]<-"NC"
data2010$STATEFIP[data2010$STATEFIP == "34"]<-"NJ"

#---- work on adding states below starting at 33
  
data2010$STATEFIP[data2010$STATEFIP == "6"]<-"CA"
data2010$STATEFIP[data2010$STATEFIP == "48"]<-"TX"
data2010$STATEFIP[data2010$STATEFIP == "36"]<-"NY"
data2010$STATEFIP[data2010$STATEFIP == "12"]<-"FL"
data2010$STATEFIP[data2010$STATEFIP == "17"]<-"IL"
data2010$STATEFIP[data2010$STATEFIP == "42"]<-"PA"
data2010$STATEFIP[data2010$STATEFIP == "39"]<-"OH"
data2010$STATEFIP[data2010$STATEFIP == "24"]<-"MD"
data2010$STATEFIP[data2010$STATEFIP == "26"]<-"MI"
data2010$STATEFIP[data2010$STATEFIP == "27"]<-"MN"
data2010$STATEFIP[data2010$STATEFIP == "8"]<-"CO"
data2010$STATEFIP[data2010$STATEFIP == "13"]<-"GA"
data2010$STATEFIP[data2010$STATEFIP == "51"]<-"VA"
data2010$STATEFIP[data2010$STATEFIP == "9"]<-"CT"
data2010$STATEFIP[data2010$STATEFIP == "37"]<-"NC"
data2010$STATEFIP[data2010$STATEFIP == "34"]<-"NJ"


#####
# NY
#####

# 2010
# adult male educational attainment in ny - 2010
male_2010NY <- data2010 %>% filter(STATEFIP == 36 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)

View(male_2010NY)

# adult female educational attainment in ny - 2010
female_2010NY <- data2010 %>% filter(STATEFIP == 36 & SEX == 2) %>% filter(as.numeric(AGE) >= 18)

View(female_2010NY)

# 2014

# adult male educational attainment in ny - 2014
male_2014NY <- data2014 %>% filter(STATEFIP == 36 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)

View(male_2014NY)

# adult female educational attainment in ny - 2014
female_2014NY <- data2014 %>% filter(STATEFIP == 36 & SEX == 2) %>% filter(as.numeric(AGE) >= 18)

View(female_2014NY)


################################################
# STATS
################################################

#######################################################
# What states is there data for?
#######################################################

# in 2014
state_count <- data2014 %>% count(STATEFIP, sort = TRUE)
state_count

# in 2010
state_count <- data2010 %>% count(STATEFIP, sort = TRUE)
state_count

#################################################################
# How many people received what level of educational attainment?
#################################################################

# educational attainment population count for all states surveyed
# NOTE: see if you can count by state
educ_count <- data2014 %>% count(EDUC, sort = TRUE)
educ_count

# educational attainment in NY by males in 2014
NY2014M_educ_count <- male_2014NY %>% count(EDUC, sort = TRUE)
NY2014M_educ_count

# educational attainment in NY by females in 2014
NY2014F_educ_count <- female_2014NY %>% count(EDUC, sort = TRUE)
NY2014F_educ_count

# educational attainment in NY by males in 2010
NY2010M_educ_count <- male_2010NY %>% count(EDUC, sort = TRUE)
NY2010M_educ_count

# educational attainment in NY by females in 2010
NY2010F_educ_count <- female_2010NY %>% count(EDUC, sort = TRUE)
NY2010F_educ_count

#################################################################
# What number of people were surveyed this year? 
# What gender did they identify with?
#################################################################

# gender population count
sex_count <- data2014 %>% count(SEX, sort = TRUE)
sex_count


############################################################
# Linear Regression -- TESTING
############################################################

test <- lm()


################################################
# PLOTS 
################################################


################################## SAMPLE CODE + TESTING --- DELETE ##########################################

# summary stats - works but not right
#st(male_2014NY)

# pairs panels
#pairs.panels(data2014)

# pairs panels (takes forever do not run)
# pairs.panels(result)

# summary stats
#summary(data2014)

# convert from character to factors
# res1$educ <- as.factor(res1$educ)

# educational attainment plot for gender differences - in month 3
#gender2015Plot <- ggplot(data = res1, mapping = aes(x = SEX, y = EDUC, fill = Personal.Income))

#Fem2008Plot <- ggplot(data = data_lowincome_f2008, mapping = aes(x = Educational.Attainment, y = Population.Count, fill = Personal.Income)) +
 # geom_bar(stat = "identity", position=position_dodge(), 
  #         colour="black") +
  #scale_fill_hue(name = "Personal Income") + 
  #xlab("Highest Level of Education Attained") + ylab("Number of People") + 
  #ggtitle("2008 CA Female Educational Attainment, Grouped by Income") +
  #theme(axis.text.x = element_text(angle = 90))

#ggplotly(Fem2008Plot)

#ggplot(data = data2005_Spend) + geom_point(mapping = aes(x = country, y = MPOV08_FOOD), color = "blue") +
 # geom_point(mapping=aes(x=country, y = FPOV08_FOOD), color = "red") + 
  #labs(subtitle = "2005",
   #    y = "Monthly Food Expenditure in 2011 PPP$", 
    #   x = "Country", title = "Monthly Food Expenditures")


# test plot --> working!!! but not correct :>
#plot1 <- ggplot(data = res1) + geom_point(mapping = aes(x = EDUC, y = SEX-count?), color = ) +

#bi.bars("EDUC","SEX",xlab="Education",
#       main="Education by gender",horiz=FALSE)

################################## SAMPLE CODE + TESTING --- DELETE ##########################################

#####################################################################
# What level of educational attainment was attained by each gender?
#####################################################################

# 2010
# NY Male Educational Attainment in 2010
NYMaleEdu2010 <- ggplot(NY2010M_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Males in NY (2010)") +
  theme(axis.text.x = element_text(angle = 90))

# NY Female Educational Attainment in 2010
NYFemaleEdu2010 <- ggplot(NY2010F_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Females in NY (2010)") +
  theme(axis.text.x = element_text(angle = 90))


# 2014

# NY Male Educational Attainment in 2014
NYMaleEdu2014 <- ggplot(NY2014M_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Males in NY (2014)") +
  theme(axis.text.x = element_text(angle = 90))

# NY Female Educational Attainment in 2014
NYFemaleEdu2014 <- ggplot(NY2014F_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Females in NY (2014)") +
  theme(axis.text.x = element_text(angle = 90))

# display plots
ggplotly(NYMaleEdu2010)
ggplotly(NYFemaleEdu2010)
ggplotly(NYMaleEdu2014)
ggplotly(NYFemaleEdu2014)

# closes connection to db
dbDisconnect(conn)