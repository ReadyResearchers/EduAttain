# EDUATTAIN 
# ANALYSIS SOURCE CODE
# Kyrie Doniz, CS600, Fall 2022

# install necessary packages
install.packages("RSQLite")

# load necessary libraries
library(RSQLite)

# connect to database
conn <- dbConnect(RSQLite::SQLite(), "CPS.db")

# list all tables connected to db
dbListTables(conn)

# does not work rn, need to debug