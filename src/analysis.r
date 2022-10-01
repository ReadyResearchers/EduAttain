# EDUATTAIN 
# ANALYSIS SOURCE CODE
# Kyrie Doniz, CS600, Fall 2022

########################################################

rm(list = ls()) # remove variables stored in memory.

# If you want to remove all previous plots and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console


########################################################

# install necessary packages
install.packages("devtools") # install to connect to github
devtools::install_github("RcppCore/Rcpp") # download needed dependency for DBI
devtools::install_github("rstats-db/DBI") # download new version of DBI to run SQLite
install.packages("RSQLite")

# load necessary libraries
library(RSQLite)

# connect to database
conn <- dbConnect(RSQLite::SQLite(), "CPS")

# list all tables connected to db
dbListTables(conn)

# does not work rn, need to debug