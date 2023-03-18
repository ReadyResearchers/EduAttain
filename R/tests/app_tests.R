# EduAttain Dashboard App Testing

# install necessary libs
library(testthat)
library(DBI)
library(RSQLite)
library(dplyr)
#library(plotly)

# import app
# source("C:/Users/kyrie/Documents/cs600/EduAttain/R/app.R")
# 
# # Test that the data is queried correctly
# test_that("SQLite query returns expected output", {
#   
#   db <- "C:/Users/kyrie/Documents/cs600/EduAttain/R/data/CPS.db"
#   # connect to database
#   conn <- dbConnect(RSQLite::SQLite(), db)
#   
#   # execute the query in the Shiny app
#   query_result <- withProgress(message = "Running query...", value = 0, {
#     query_data()
#   })
#   
#   # verify query result matched expected output
#   expected_output <- dbGetQuery(conn, "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
#   expect_identical(query_result, expected_output)
#   
#   # disconnect from sqlite db
#   dbDisconnect(conn)
# })

# PLOTLY OBJECT TESTING

# Test that a plotly object is generated - gender
#test_that("")

# Test that a plotly object is generated - race

# Test that a plotly object is generated - hispanic


