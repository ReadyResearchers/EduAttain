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

# display data
head(res1)
