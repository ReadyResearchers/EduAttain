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

View(result)

# looking at yr ranges in data
rng <- range(result$YEAR)
rng


###### 

# query to get all data from 2014 for sex + educ attain
q1 <- 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014'
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
q2 <- 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010'
data2010 <- dbGetQuery(conn, q2)
# test to check if it is a dataframe --> need to remove
data2010
class(data2010)

# display data
head(data2010)

# view data sets
View(data2010)


########

# query to get all data from 2010 for sex + educ attain
q3 <- 'SELECT cpsidp, sex, educ, month, age, statefip FROM CPS WHERE year = 2012'
data2012 <- dbGetQuery(conn, q1)
# test to check if it is a dataframe --> need to remove
data2012
class(data2012)

# display data
head(data2012)

# view data sets
View(data2012)

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

# all yrs
result$EDUC[result$EDUC == "10"]<-"Grades 1-4"
result$EDUC[result$EDUC == "111"]<-"Bachelor's Degree"
result$EDUC[result$EDUC == "123"]<-"Master's Degree"
result$EDUC[result$EDUC == "124"]<-"Professional School Degree"
result$EDUC[result$EDUC == "125"]<-"Doctorate Degree"
result$EDUC[result$EDUC == "2"]<-"None/Preschool/Kindergarten"
result$EDUC[result$EDUC == "20"]<-"Grades 5-6"
result$EDUC[result$EDUC == "30"]<-"Grades 7-8"
result$EDUC[result$EDUC == "40"]<-"HS, Grade 9"
result$EDUC[result$EDUC == "50"]<-"HS, Grade 10"
result$EDUC[result$EDUC == "60"]<-"HS, Grade 11"
result$EDUC[result$EDUC == "71"]<-"HS, Grade 12, no diploma"
result$EDUC[result$EDUC == "73"]<-"HS Diploma or Equiv."
result$EDUC[result$EDUC == "81"]<-"Some college, no degree"
result$EDUC[result$EDUC == "91"]<-"Occupational/Vocational Program Degree"
result$EDUC[result$EDUC == "92"]<-"Associate's Degree, Academic"


# replacing state fips code with state name
data2010$STATEFIP[data2010$STATEFIP == "6"]<-"CA" #CALIFORNIA
data2010$STATEFIP[data2010$STATEFIP == "48"]<-"TX" #TEXAS
data2010$STATEFIP[data2010$STATEFIP == "36"]<-"NY" #NEW YORK
data2010$STATEFIP[data2010$STATEFIP == "12"]<-"FL" #FLORIDA
data2010$STATEFIP[data2010$STATEFIP == "17"]<-"IL" #ILLINOIS
data2010$STATEFIP[data2010$STATEFIP == "42"]<-"PA" #PENNSYLVANIA
data2010$STATEFIP[data2010$STATEFIP == "39"]<-"OH" #OHIO
data2010$STATEFIP[data2010$STATEFIP == "24"]<-"MD" #MARYLAND
data2010$STATEFIP[data2010$STATEFIP == "26"]<-"MI" #MICHIGAN
data2010$STATEFIP[data2010$STATEFIP == "27"]<-"MN" #MINNESOTA
data2010$STATEFIP[data2010$STATEFIP == "8"]<-"CO" #COLORADO
data2010$STATEFIP[data2010$STATEFIP == "13"]<-"GA" #GEORGIA
data2010$STATEFIP[data2010$STATEFIP == "51"]<-"VA" #VIRGINIA
data2010$STATEFIP[data2010$STATEFIP == "9"]<-"CT" #CONNECTICUT
data2010$STATEFIP[data2010$STATEFIP == "37"]<-"NC" #NORTH CAROLINA
data2010$STATEFIP[data2010$STATEFIP == "34"]<-"NJ" #NEW JERSEY
data2010$STATEFIP[data2010$STATEFIP == "33"]<-"NH" #NEW HAMPSHIRE
data2010$STATEFIP[data2010$STATEFIP == "53"]<-"WA" #WASHINGTON
data2010$STATEFIP[data2010$STATEFIP == "19"]<-"IA" #IOWA
data2010$STATEFIP[data2010$STATEFIP == "15"]<-"HI" #HAWAII
data2010$STATEFIP[data2010$STATEFIP == "55"]<-"WI" #WISCONSIN
data2010$STATEFIP[data2010$STATEFIP == "29"]<-"MO" #MISSOURI
data2010$STATEFIP[data2010$STATEFIP == "10"]<-"DE" #DELAWARE
data2010$STATEFIP[data2010$STATEFIP == "32"]<-"NV" #NEVADA
data2010$STATEFIP[data2010$STATEFIP == "18"]<-"IN" #INDIANA
data2010$STATEFIP[data2010$STATEFIP == "44"]<-"RI" #RHODE ISLAND
data2010$STATEFIP[data2010$STATEFIP == "46"]<-"SD" #SOUTH DAKOTA
data2010$STATEFIP[data2010$STATEFIP == "20"]<-"KS" #KANSAS
data2010$STATEFIP[data2010$STATEFIP == "41"]<-"OR" #OREGON
data2010$STATEFIP[data2010$STATEFIP == "49"]<-"UT" #UTAH
data2010$STATEFIP[data2010$STATEFIP == "31"]<-"NE" #NEBRASKA
data2010$STATEFIP[data2010$STATEFIP == "23"]<-"ME" #MAINE
data2010$STATEFIP[data2010$STATEFIP == "25"]<-"MA" #MASSACHUSSETTS
data2010$STATEFIP[data2010$STATEFIP == "11"]<-"DC" #DISTRICT OF COLOMBIA
data2010$STATEFIP[data2010$STATEFIP == "21"]<-"KY" #KENTUCKY
data2010$STATEFIP[data2010$STATEFIP == "4"]<-"AZ" #ARIZONA
data2010$STATEFIP[data2010$STATEFIP == "47"]<-"TN" #TENNESSEE
data2010$STATEFIP[data2010$STATEFIP == "45"]<-"SC" #SOUTH CAROLINA
data2010$STATEFIP[data2010$STATEFIP == "56"]<-"WY" #WYOMING
data2010$STATEFIP[data2010$STATEFIP == "40"]<-"OK" #OKLAHOMA
data2010$STATEFIP[data2010$STATEFIP == "16"]<-"ID" #IDAHO
data2010$STATEFIP[data2010$STATEFIP == "38"]<-"ND" #NORTH DAKOTA
data2010$STATEFIP[data2010$STATEFIP == "2"]<-"AK" #ALASKA
data2010$STATEFIP[data2010$STATEFIP == "50"]<-"VT" #VERMONT
data2010$STATEFIP[data2010$STATEFIP == "5"]<-"AR" #ARKANSAS
data2010$STATEFIP[data2010$STATEFIP == "22"]<-"LA" #LOUISIANA
data2010$STATEFIP[data2010$STATEFIP == "1"]<-"AL" #ALABAMA
data2010$STATEFIP[data2010$STATEFIP == "54"]<-"WV" #WEST VIRGINIA
data2010$STATEFIP[data2010$STATEFIP == "35"]<-"NM" #NEW MEXICO
data2010$STATEFIP[data2010$STATEFIP == "28"]<-"MS" #MISSISSIPPI
data2010$STATEFIP[data2010$STATEFIP == "30"]<-"MT" #MONTANA

data2014$STATEFIP[data2014$STATEFIP == "6"]<-"CA" #CALIFORNIA
data2014$STATEFIP[data2014$STATEFIP == "48"]<-"TX" #TEXAS
data2014$STATEFIP[data2014$STATEFIP == "36"]<-"NY" #NEW YORK
data2014$STATEFIP[data2014$STATEFIP == "12"]<-"FL" #FLORIDA
data2014$STATEFIP[data2014$STATEFIP == "17"]<-"IL" #ILLINOIS
data2014$STATEFIP[data2014$STATEFIP == "42"]<-"PA" #PENNSYLVANIA
data2014$STATEFIP[data2014$STATEFIP == "39"]<-"OH" #OHIO
data2014$STATEFIP[data2014$STATEFIP == "24"]<-"MD" #MARYLAND
data2014$STATEFIP[data2014$STATEFIP == "26"]<-"MI" #MICHIGAN
data2014$STATEFIP[data2014$STATEFIP == "27"]<-"MN" #MINNESOTA
data2014$STATEFIP[data2014$STATEFIP == "8"]<-"CO" #COLORADO
data2014$STATEFIP[data2014$STATEFIP == "13"]<-"GA" #GEORGIA
data2014$STATEFIP[data2014$STATEFIP == "51"]<-"VA" #VIRGINIA
data2014$STATEFIP[data2014$STATEFIP == "9"]<-"CT" #CONNECTICUT
data2014$STATEFIP[data2014$STATEFIP == "37"]<-"NC" #NORTH CAROLINA
data2014$STATEFIP[data2014$STATEFIP == "34"]<-"NJ" #NEW JERSEY
data2014$STATEFIP[data2014$STATEFIP == "33"]<-"NH" #NEW HAMPSHIRE
data2014$STATEFIP[data2014$STATEFIP == "53"]<-"WA" #WASHINGTON
data2014$STATEFIP[data2014$STATEFIP == "19"]<-"IA" #IOWA
data2014$STATEFIP[data2014$STATEFIP == "15"]<-"HI" #HAWAII
data2014$STATEFIP[data2014$STATEFIP == "55"]<-"WI" #WISCONSIN
data2014$STATEFIP[data2014$STATEFIP == "29"]<-"MO" #MISSOURI
data2014$STATEFIP[data2014$STATEFIP == "10"]<-"DE" #DELAWARE
data2014$STATEFIP[data2014$STATEFIP == "32"]<-"NV" #NEVADA
data2014$STATEFIP[data2014$STATEFIP == "18"]<-"IN" #INDIANA
data2014$STATEFIP[data2014$STATEFIP == "44"]<-"RI" #RHODE ISLAND
data2014$STATEFIP[data2014$STATEFIP == "46"]<-"SD" #SOUTH DAKOTA
data2014$STATEFIP[data2014$STATEFIP == "20"]<-"KS" #KANSAS
data2014$STATEFIP[data2014$STATEFIP == "41"]<-"OR" #OREGON
data2014$STATEFIP[data2014$STATEFIP == "49"]<-"UT" #UTAH
data2014$STATEFIP[data2014$STATEFIP == "31"]<-"NE" #NEBRASKA
data2014$STATEFIP[data2014$STATEFIP == "23"]<-"ME" #MAINE
data2014$STATEFIP[data2014$STATEFIP == "25"]<-"MA" #MASSACHUSSETTS
data2014$STATEFIP[data2014$STATEFIP == "11"]<-"DC" #DISTRICT OF COLOMBIA
data2014$STATEFIP[data2014$STATEFIP == "21"]<-"KY" #KENTUCKY
data2014$STATEFIP[data2014$STATEFIP == "4"]<-"AZ" #ARIZONA
data2014$STATEFIP[data2014$STATEFIP == "47"]<-"TN" #TENNESSEE
data2014$STATEFIP[data2014$STATEFIP == "45"]<-"SC" #SOUTH CAROLINA
data2014$STATEFIP[data2014$STATEFIP == "56"]<-"WY" #WYOMING
data2014$STATEFIP[data2014$STATEFIP == "40"]<-"OK" #OKLAHOMA
data2014$STATEFIP[data2014$STATEFIP == "16"]<-"ID" #IDAHO
data2014$STATEFIP[data2014$STATEFIP == "38"]<-"ND" #NORTH DAKOTA
data2014$STATEFIP[data2014$STATEFIP == "2"]<-"AK" #ALASKA
data2014$STATEFIP[data2014$STATEFIP == "50"]<-"VT" #VERMONT
data2014$STATEFIP[data2014$STATEFIP == "5"]<-"AR" #ARKANSAS
data2014$STATEFIP[data2014$STATEFIP == "22"]<-"LA" #LOUISIANA
data2014$STATEFIP[data2014$STATEFIP == "1"]<-"AL" #ALABAMA
data2014$STATEFIP[data2014$STATEFIP == "54"]<-"WV" #WEST VIRGINIA
data2014$STATEFIP[data2014$STATEFIP == "35"]<-"NM" #NEW MEXICO
data2014$STATEFIP[data2014$STATEFIP == "28"]<-"MS" #MISSISSIPPI
data2014$STATEFIP[data2014$STATEFIP == "30"]<-"MT" #MONTANA

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


#####
# TX
#####

# 2010
# adult male educational attainment in tx - 2010
male_2010TX <- data2010 %>% filter(STATEFIP == 48 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)

View(male_2010TX)

# adult female educational attainment in tx - 2010
female_2010TX <- data2010 %>% filter(STATEFIP == 48 & SEX == 2) %>% filter(as.numeric(AGE) >= 18)

View(female_2010TX)

# 2014

# adult male educational attainment in TX - 2014
male_2014TX <- data2014 %>% filter(STATEFIP == 48 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)

View(male_2014TX)

# adult female educational attainment in TX - 2014
female_2014TX <- data2014 %>% filter(STATEFIP == 48 & SEX == 2) %>% filter(as.numeric(AGE) >= 18)

View(female_2014TX)


# ALL TIME

######
###### adult female educational attainment in us across 2010-2015
######
female_US <- result %>% filter(SEX == 2) %>% filter(as.numeric(AGE) >= 18)

View(female_US)

######
###### adult male educational attainment in us across 2010-2015
######
male_US <- result %>% filter(SEX == 1) %>% filter(as.numeric(AGE) >= 18)

View(male_US)


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

# in 2012
state_count <- data2012 %>% count(STATEFIP, sort = TRUE)
state_count


#################################################################
# How many people received what level of educational attainment?
#################################################################

# educational attainment population count for all states surveyed
# NOTE: see if you can count by state
educ_count <- data2014 %>% count(EDUC, sort = TRUE)
educ_count

educ_count <- data2010 %>% count(EDUC, sort = TRUE)
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

###

# educational attainment in TX by males in 2014
TX2014M_educ_count <- male_2014TX %>% count(EDUC, sort = TRUE)
TX2014M_educ_count

# educational attainment in TX by females in 2014
TX2014F_educ_count <- female_2014TX %>% count(EDUC, sort = TRUE)
TX2014F_educ_count

# educational attainment in TX by males in 2010
TX2010M_educ_count <- male_2010TX %>% count(EDUC, sort = TRUE)
TX2010M_educ_count

# educational attainment in TX by females in 2010
TX2010F_educ_count <- female_2010TX %>% count(EDUC, sort = TRUE)
TX2010F_educ_count

###

# educational attainment in TX by males in 2010
US_M_educ_count <- male_US %>% count(EDUC, sort = TRUE)
US_M_educ_count

# educational attainment in TX by females in 2010
US_F_educ_count <- female_US %>% count(EDUC, sort = TRUE)
US_F_educ_count
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

# TX Male Educational Attainment in 2010
TXMaleEdu2010 <- ggplot(TX2010M_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Males in TX (2010)") +
  theme(axis.text.x = element_text(angle = 90))

# TX Female Educational Attainment in 2010
TXFemaleEdu2010 <- ggplot(TX2010F_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Females in TX (2010)") +
  theme(axis.text.x = element_text(angle = 90))

test <- ggplot(data2010, aes(x=EDUC, fill=SEX)) + 
  geom_bar()

ggplotly(test)

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

# TX Male Educational Attainment in 2014
TXMaleEdu2014 <- ggplot(TX2014M_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Males in TX (2014)") +
  theme(axis.text.x = element_text(angle = 90))

# TX Female Educational Attainment in 2014
TXFemaleEdu2014 <- ggplot(TX2014F_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Females in TX (2014)") +
  theme(axis.text.x = element_text(angle = 90))

# all time

# US Male Educational Attainment
USMaleEdu <- ggplot(US_M_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Males in US (2010-2015)") +
  theme(axis.text.x = element_text(angle = 90))

# US Female Educational Attainment 
USFemaleEdu <- ggplot(US_F_educ_count, aes(EDUC, n)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Count") +
  ggtitle("Educational Attainment by Adult Females in US (2010-2015)") +
  theme(axis.text.x = element_text(angle = 90))

# display plots
ggplotly(NYMaleEdu2010)
ggplotly(NYFemaleEdu2010)
ggplotly(NYMaleEdu2014)
ggplotly(NYFemaleEdu2014)
ggplotly(TXMaleEdu2010)
ggplotly(TXFemaleEdu2010)
ggplotly(TXMaleEdu2014)
ggplotly(TXFemaleEdu2014)
ggplotly(USMaleEdu)
ggplotly(USFemaleEdu)


########
# recoding categorical 2 dummy
#######

# sex
result$female <- ifelse(result$SEX == "2", 1, 0)
result$male <- ifelse(result$SEX == "1", 1, 0)

# race
result$white <- ifelse(result$RACE == "100", 1, 0)
result$black <- ifelse(result$RACE == "200", 1, 0)
result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
result$asian <- ifelse(result$RACE == "651", 1, 0)
result$islander <- ifelse(result$RACE == "652", 1, 0)
result$white_black <- ifelse(result$RACE == "801", 1, 0)
result$white_amer_indian <- ifelse(result$RACE == "802", 1, 0)
result$white_asian <- ifelse(result$RACE == "803", 1, 0)
result$white_islander <- ifelse(result$RACE == "804", 1, 0)
result$black_amer_indian <- ifelse(result$RACE == "805", 1, 0)
result$black_asian <- ifelse(result$RACE == "806", 1, 0)
result$black_islander <- ifelse(result$RACE == "807", 1, 0)
result$amer_indian_asian <- ifelse(result$RACE == "808", 1, 0)
result$asian_islander <- ifelse(result$RACE == "809", 1, 0)
result$white_black_amer_indian <- ifelse(result$RACE == "810", 1, 0)
result$white_black_asian <- ifelse(result$RACE == "811", 1, 0)
result$white_amer_indian_asian <- ifelse(result$RACE == "812", 1, 0)
result$white_asian_islander <- ifelse(result$RACE == "813", 1, 0)
result$white_black_amer_indian_asian <- ifelse(result$RACE == "814", 1, 0)
result$amer_indian_islander <- ifelse(result$RACE == "815", 1, 0)
result$white_black_islander <- ifelse(result$RACE == "816", 1, 0)
result$white_amer_indian_islander <- ifelse(result$RACE == "817", 1, 0)
result$black_amer_indian_asian <- ifelse(result$RACE == "818", 1, 0)
result$white_amer_indian_asian_islander <- ifelse(result$RACE == "819", 1, 0)
result$unspecified_2_orMore <- ifelse(result$RACE == "820", 1, 0)
result$unspecified_4_orMore <- ifelse(result$RACE == "830", 1, 0)

# 

### SAMPLE REGRESSION

lm <- lm(EDUC ~ SEX + RACE + HISPAN, data = result)
summary(lm)


# closes connection to db
dbDisconnect(conn)