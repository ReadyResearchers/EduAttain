# EDUATTAIN 
# ANALYSIS SOURCE CODE
# Kyrie Doniz, CS600, Fall 2022

########################################################

# remove packages
remove.packages("RSQLite")

# install necessary packages
install.packages("RSQLite")

# load necessary libraries
library(RSQLite)
library(dplyr)
library(ggplot2)

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
q <- 'SELECT * from CPS LIMIT 5;'
result <- dbGetQuery(conn,q)

# display data
head(result)

# query to get all data from 2015 for sex + educ attain
q1 <- 'SELECT pernum, sex, educ, month FROM CPS WHERE year = 2015'
res1 <- dbGetQuery(conn, q1)
# test to check if it is a dataframe --> need to remove
res1
class(res1)

# display data
head(res1)

# view data sets
View(res1)

# convert from character to factors
# res1$educ <- as.factor(res1$educ)

################################################
# STATS
################################################

# summary stats
summary(res1)

# pairs panels
pairs.panels(res1)

#################################################################
# How many people received what level of educational attainment?
#################################################################

# educational attainment population count 
educ_count <- res1 %>% count(EDUC, sort = TRUE)
educ_count

#################################################################
# What number of people were surveyed this year? 
# What gender did they identify with?
#################################################################

# gender population count
sex_count <- res1 %>% count(SEX, sort = TRUE)
sex_count

################################################
# PLOTS 
################################################


################################## SAMPLE CODE + TESTING --- DELETE ##########################################


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

################################## SAMPLE CODE + TESTING --- DELETE ##########################################

#####################################################################
# What level of educational attainment was attained by each gender?
#####################################################################

# test plot --> working!!! but not correct :>
plot1 <- ggplot(res1, aes(educ_count, SEX)) +
  geom_col() +
  xlab("Level of Educational Attainment") +
  ylab("Gender") +
  ggtitle("Educational Attainment by Gender in 2015")

# display plot
plot1

# closes connection to db
dbDisconnect(conn)