#####################################
# EDUATTAIN - SHINY APP
#####################################

# APP/WEBPAGE INTERFACE CODE

# install needed packages
# install.packages("shiny")
# install.packages("shinydashboard")
#install.packages('rsconnect')

# install libs
library(shiny)
library(shinydashboard)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(rsconnect)

# setting database path
db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# connect to database
conn <- dbConnect(drv = SQLite(), dbname = db)

# format dash sidebar
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Home", tabName = "dash", icon = icon("house")),
    menuItem("Gender x Educational Attainment", tabName = "genxedu", icon = icon("chart-column")),
    menuItem("Race x Educational Attainment", tabName = "racxedu", icon = icon("chart-column")),
    menuItem("Income x Educational Attainment", tabName = "incxedu", icon = icon("chart-column")),
    menuItem("Regression", tabName = "reg", icon = icon("chart-line"))
  )
)

# format dash body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dash",
            h2("Data Description"),
            box(
              title = "IPUMS US CPS Data 2010-2015", status = "primary", solidHeader = TRUE,
              dataTableOutput("data_table"), width = 10)
    ),
    tabItem(tabName = "genxedu",
            h2("Educational Attainment by Gender from 2010 to 2015"),
            box(
              title = "US Educational Attainment by Gender in 2010", status = "primary", solidHeader = TRUE,
              plotlyOutput("y1_plot"), width = 10, height = 12),
            box(
              title = "US Educational Attainment by Gender in 2011", status = "primary", solidHeader = TRUE,
              plotlyOutput("y2_plot"), width = 10, height = 12),
            box(
              title = "US Educational Attainment by Gender in 2012", status = "primary", solidHeader = TRUE,
              plotlyOutput("y3_plot"), width = 10, height = 12),
            box(
              title = "US Educational Attainment by Gender in 2013", status = "primary", solidHeader = TRUE,
              plotlyOutput("y4_plot"), width = 10, height = 12),
            box(
              title = "US Educational Attainment by Gender in 2014", status = "primary", solidHeader = TRUE,
              plotlyOutput("y5_plot"), width = 10, height = 12),
            box(
              title = "US Educational Attainment by Gender in 2015", status = "primary", solidHeader = TRUE,
              plotlyOutput("y6_plot"), width = 10, height = 12)
    )
  )
)

# function for dash creation

ui <- dashboardPage(
  dashboardHeader(title = "EduAttain"),
  sidebar,
  body
)

# server
server <- function(input, output) {
  output$data_table <- renderDataTable(
    
    # query to get all data from 2010 for sex + educ attain
    data <- dbGetQuery(conn,
                           statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS')
  )
  
  
  output$y1_plot <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                           statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
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
    
    data2010$SEX[data2010$SEX == "1"]<-"Male"
    data2010$SEX[data2010$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2010 <- ggplot(data2010, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2010)
  })
  
  output$y2_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Grades 1-4"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Grades 5-6"
    data2011$EDUC[data2011$EDUC == "30"]<-"Grades 7-8"
    data2011$EDUC[data2011$EDUC == "40"]<-"HS, Grade 9"
    data2011$EDUC[data2011$EDUC == "50"]<-"HS, Grade 10"
    data2011$EDUC[data2011$EDUC == "60"]<-"HS, Grade 11"
    data2011$EDUC[data2011$EDUC == "71"]<-"HS, Grade 12, no diploma"
    data2011$EDUC[data2011$EDUC == "73"]<-"HS Diploma or Equiv."
    data2011$EDUC[data2011$EDUC == "81"]<-"Some college, no degree"
    data2011$EDUC[data2011$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2011$EDUC[data2011$EDUC == "92"]<-"Associate's Degree, Academic"
    
    data2011$SEX[data2011$SEX == "1"]<-"Male"
    data2011$SEX[data2011$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2011 <- ggplot(data2011, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2011)
  })
  
  output$y3_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "1"]<-"NIU"
    data2012$EDUC[data2012$EDUC == "10"]<-"Grades 1-4"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Grades 5-6"
    data2012$EDUC[data2012$EDUC == "30"]<-"Grades 7-8"
    data2012$EDUC[data2012$EDUC == "40"]<-"HS, Grade 9"
    data2012$EDUC[data2012$EDUC == "50"]<-"HS, Grade 10"
    data2012$EDUC[data2012$EDUC == "60"]<-"HS, Grade 11"
    data2012$EDUC[data2012$EDUC == "71"]<-"HS, Grade 12, no diploma"
    data2012$EDUC[data2012$EDUC == "73"]<-"HS Diploma or Equiv."
    data2012$EDUC[data2012$EDUC == "81"]<-"Some college, no degree"
    data2012$EDUC[data2012$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2012$EDUC[data2012$EDUC == "92"]<-"Associate's Degree, Academic"
    
    data2012$SEX[data2012$SEX == "1"]<-"Male"
    data2012$SEX[data2012$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2012 <- ggplot(data2012, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2012)
  })
  
  output$y4_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "1"]<-"NIU"
    data2013$EDUC[data2013$EDUC == "10"]<-"Grades 1-4"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Grades 5-6"
    data2013$EDUC[data2013$EDUC == "30"]<-"Grades 7-8"
    data2013$EDUC[data2013$EDUC == "40"]<-"HS, Grade 9"
    data2013$EDUC[data2013$EDUC == "50"]<-"HS, Grade 10"
    data2013$EDUC[data2013$EDUC == "60"]<-"HS, Grade 11"
    data2013$EDUC[data2013$EDUC == "71"]<-"HS, Grade 12, no diploma"
    data2013$EDUC[data2013$EDUC == "73"]<-"HS Diploma or Equiv."
    data2013$EDUC[data2013$EDUC == "81"]<-"Some college, no degree"
    data2013$EDUC[data2013$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2013$EDUC[data2013$EDUC == "92"]<-"Associate's Degree, Academic"
    
    data2013$SEX[data2013$SEX == "1"]<-"Male"
    data2013$SEX[data2013$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2013 <- ggplot(data2013, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2013)
  })
  
  output$y5_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
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
    
    data2014$SEX[data2014$SEX == "1"]<-"Male"
    data2014$SEX[data2014$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2014 <- ggplot(data2014, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2014)
  })
  
  output$y6_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Grades 1-4"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Grades 5-6"
    data2015$EDUC[data2015$EDUC == "30"]<-"Grades 7-8"
    data2015$EDUC[data2015$EDUC == "40"]<-"HS, Grade 9"
    data2015$EDUC[data2015$EDUC == "50"]<-"HS, Grade 10"
    data2015$EDUC[data2015$EDUC == "60"]<-"HS, Grade 11"
    data2015$EDUC[data2015$EDUC == "71"]<-"HS, Grade 12, no diploma"
    data2015$EDUC[data2015$EDUC == "73"]<-"HS Diploma or Equiv."
    data2015$EDUC[data2015$EDUC == "81"]<-"Some college, no degree"
    data2015$EDUC[data2015$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2015$EDUC[data2015$EDUC == "92"]<-"Associate's Degree, Academic"
    
    data2015$SEX[data2015$SEX == "1"]<-"Male"
    data2015$SEX[data2015$SEX == "2"]<-"Female"
    
    # plot
    genderxeduc2015 <- ggplot(data2015, aes(x=EDUC, fill=SEX)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(genderxeduc2015)
  })
}

# view app
shinyApp(ui, server)

#rsconnect::deployApp('C:/Users/kyrie/Documents/cs600/eduattain/app.R')

#dbDisconnect()



# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs