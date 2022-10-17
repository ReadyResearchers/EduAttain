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

# convert from character to factors
# res1$educ <- as.factor(res1$educ)

################################################
# STATS
################################################
summary(res1)

################################################
# PLOTS 
################################################

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

# closes connection to db
dbDisconnect(conn)