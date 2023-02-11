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


#### test to limit results to 18+####
# query to display the first 5 rows
q <- 'SELECT * from CPS;'
result <- dbGetQuery(conn,q)
result = result[-1,]
as.numeric(result$AGE)
res <- result %>% filter(AGE >= 18)
######################################


# format dash sidebar
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Home", tabName = "dash", icon = icon("house")),
    menuItem("Gender x Educational Attainment", tabName = "genxedu", icon = icon("chart-column")),
    menuItem("Race x Educational Attainment", tabName = "racxedu", icon = icon("chart-column")),
    menuItem("Hispanic x Educational Attainment", tabName = "hisxedu", icon = icon("chart-column")),
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
              title = "US Educational Attainment by Gender in 2010", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y1_plot"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2011", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y2_plot"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2012", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y3_plot"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2013", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y4_plot"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2014", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y5_plot"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2015", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y6_plot"), width = 10)
    ),
    tabItem(tabName = "racxedu",
            h2("Educational Attainment by Race from 2010 to 2015"),
            box(
              title = "US Educational Attainment by Gender in 2010", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y1_plot_1"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2011", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y2_plot_1"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2012", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y3_plot_1"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2013", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y4_plot_1"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2014", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y5_plot_1"), width = 10),
            box(
              title = "US Educational Attainment by Gender in 2015", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y6_plot_1"), width = 10)
    ),
    tabItem(tabName = "hisxedu",
            h2("US Hispanic/Latino Educational Attainment from 2010 to 2015"),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2010", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y1_plot_2"), width = 10),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2011", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y2_plot_2"), width = 10),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2012", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y3_plot_2"), width = 10),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2013", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y4_plot_2"), width = 10),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2014", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y5_plot_2"), width = 10),
            box(
              title = "US Educational Attainment by Hispanic Ethnicity in 2015", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("y6_plot_2"), width = 10)
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
  
###################### TABLE ###########################
  
  output$data_table <- renderDataTable(
    #res
    # query to get all data from 2010 for sex + educ attain
    df <- dbGetQuery(conn,
                          statement= "SELECT cpsidp, sex, educ, race, hispan, ftotval, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
  )
  
#########################################################
  
###################### GENDER ###########################
  # 2010 gender
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
  
  # 2011 gender
  output$y2_plot <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
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
  
  # 2012 gender
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
  
  # 2013 gender
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
  
  # 2014 gender
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
  
  # 2015 gender
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
  
#######################################################
  
  
  
###################### RACE ###########################
  # 2010 race
  output$y1_plot_1 <- renderPlotly({
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
    
    data2010$RACE[data2010$RACE == "100"]<-"White"
    data2010$RACE[data2010$RACE == "200"]<-"Black"
    data2010$RACE[data2010$RACE == "300"]<-"American Indian"
    data2010$RACE[data2010$RACE == "651"]<-"Asian"
    data2010$RACE[data2010$RACE == "652"]<-"Pacific Islander"
    data2010$RACE[data2010$RACE == "801"]<-"Other"
    data2010$RACE[data2010$RACE == "802"]<-"Other"
    data2010$RACE[data2010$RACE == "803"]<-"Other"
    data2010$RACE[data2010$RACE == "804"]<-"Other"
    data2010$RACE[data2010$RACE == "805"]<-"Other"
    data2010$RACE[data2010$RACE == "806"]<-"Other"
    data2010$RACE[data2010$RACE == "807"]<-"Other"
    data2010$RACE[data2010$RACE == "808"]<-"Other"
    data2010$RACE[data2010$RACE == "809"]<-"Other"
    data2010$RACE[data2010$RACE == "810"]<-"Other"
    data2010$RACE[data2010$RACE == "811"]<-"Other"
    data2010$RACE[data2010$RACE == "812"]<-"Other"
    data2010$RACE[data2010$RACE == "813"]<-"Other"
    data2010$RACE[data2010$RACE == "814"]<-"Other"
    data2010$RACE[data2010$RACE == "815"]<-"Other"
    data2010$RACE[data2010$RACE == "816"]<-"Other"
    data2010$RACE[data2010$RACE == "817"]<-"Other"
    data2010$RACE[data2010$RACE == "818"]<-"Other"
    data2010$RACE[data2010$RACE == "819"]<-"Other"
    data2010$RACE[data2010$RACE == "820"]<-"Other"
    data2010$RACE[data2010$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2010 <- ggplot(data2010, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2010)
  })
  
  # 2011 race
  output$y2_plot_1 <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
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
    
    data2011$RACE[data2011$RACE == "100"]<-"White"
    data2011$RACE[data2011$RACE == "200"]<-"Black"
    data2011$RACE[data2011$RACE == "300"]<-"American Indian"
    data2011$RACE[data2011$RACE == "651"]<-"Asian"
    data2011$RACE[data2011$RACE == "652"]<-"Pacific Islander"
    data2011$RACE[data2011$RACE == "801"]<-"Other"
    data2011$RACE[data2011$RACE == "802"]<-"Other"
    data2011$RACE[data2011$RACE == "803"]<-"Other"
    data2011$RACE[data2011$RACE == "804"]<-"Other"
    data2011$RACE[data2011$RACE == "805"]<-"Other"
    data2011$RACE[data2011$RACE == "806"]<-"Other"
    data2011$RACE[data2011$RACE == "807"]<-"Other"
    data2011$RACE[data2011$RACE == "808"]<-"Other"
    data2011$RACE[data2011$RACE == "809"]<-"Other"
    data2011$RACE[data2011$RACE == "810"]<-"Other"
    data2011$RACE[data2011$RACE == "811"]<-"Other"
    data2011$RACE[data2011$RACE == "812"]<-"Other"
    data2011$RACE[data2011$RACE == "813"]<-"Other"
    data2011$RACE[data2011$RACE == "814"]<-"Other"
    data2011$RACE[data2011$RACE == "815"]<-"Other"
    data2011$RACE[data2011$RACE == "816"]<-"Other"
    data2011$RACE[data2011$RACE == "817"]<-"Other"
    data2011$RACE[data2011$RACE == "818"]<-"Other"
    data2011$RACE[data2011$RACE == "819"]<-"Other"
    data2011$RACE[data2011$RACE == "820"]<-"Other"
    data2011$RACE[data2011$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2011 <- ggplot(data2011, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2011)
  })
  
  # 2012 race
  output$y3_plot_1 <- renderPlotly({
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
    
    data2012$RACE[data2012$RACE == "100"]<-"White"
    data2012$RACE[data2012$RACE == "200"]<-"Black"
    data2012$RACE[data2012$RACE == "300"]<-"American Indian"
    data2012$RACE[data2012$RACE == "651"]<-"Asian"
    data2012$RACE[data2012$RACE == "652"]<-"Pacific Islander"
    data2012$RACE[data2012$RACE == "801"]<-"Other"
    data2012$RACE[data2012$RACE == "802"]<-"Other"
    data2012$RACE[data2012$RACE == "803"]<-"Other"
    data2012$RACE[data2012$RACE == "804"]<-"Other"
    data2012$RACE[data2012$RACE == "805"]<-"Other"
    data2012$RACE[data2012$RACE == "806"]<-"Other"
    data2012$RACE[data2012$RACE == "807"]<-"Other"
    data2012$RACE[data2012$RACE == "808"]<-"Other"
    data2012$RACE[data2012$RACE == "809"]<-"Other"
    data2012$RACE[data2012$RACE == "810"]<-"Other"
    data2012$RACE[data2012$RACE == "811"]<-"Other"
    data2012$RACE[data2012$RACE == "812"]<-"Other"
    data2012$RACE[data2012$RACE == "813"]<-"Other"
    data2012$RACE[data2012$RACE == "814"]<-"Other"
    data2012$RACE[data2012$RACE == "815"]<-"Other"
    data2012$RACE[data2012$RACE == "816"]<-"Other"
    data2012$RACE[data2012$RACE == "817"]<-"Other"
    data2012$RACE[data2012$RACE == "818"]<-"Other"
    data2012$RACE[data2012$RACE == "819"]<-"Other"
    data2012$RACE[data2012$RACE == "820"]<-"Other"
    data2012$RACE[data2012$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2012 <- ggplot(data2012, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2012)
    
  })
  
  # 2013 race
  output$y4_plot_1 <- renderPlotly({
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
    
    data2013$RACE[data2013$RACE == "100"]<-"White"
    data2013$RACE[data2013$RACE == "200"]<-"Black"
    data2013$RACE[data2013$RACE == "300"]<-"American Indian"
    data2013$RACE[data2013$RACE == "651"]<-"Asian"
    data2013$RACE[data2013$RACE == "652"]<-"Pacific Islander"
    data2013$RACE[data2013$RACE == "801"]<-"Other"
    data2013$RACE[data2013$RACE == "802"]<-"Other"
    data2013$RACE[data2013$RACE == "803"]<-"Other"
    data2013$RACE[data2013$RACE == "804"]<-"Other"
    data2013$RACE[data2013$RACE == "805"]<-"Other"
    data2013$RACE[data2013$RACE == "806"]<-"Other"
    data2013$RACE[data2013$RACE == "807"]<-"Other"
    data2013$RACE[data2013$RACE == "808"]<-"Other"
    data2013$RACE[data2013$RACE == "809"]<-"Other"
    data2013$RACE[data2013$RACE == "810"]<-"Other"
    data2013$RACE[data2013$RACE == "811"]<-"Other"
    data2013$RACE[data2013$RACE == "812"]<-"Other"
    data2013$RACE[data2013$RACE == "813"]<-"Other"
    data2013$RACE[data2013$RACE == "814"]<-"Other"
    data2013$RACE[data2013$RACE == "815"]<-"Other"
    data2013$RACE[data2013$RACE == "816"]<-"Other"
    data2013$RACE[data2013$RACE == "817"]<-"Other"
    data2013$RACE[data2013$RACE == "818"]<-"Other"
    data2013$RACE[data2013$RACE == "819"]<-"Other"
    data2013$RACE[data2013$RACE == "820"]<-"Other"
    data2013$RACE[data2013$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2013 <- ggplot(data2013, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2013)
    
  })
  
  # 2014 race
  output$y5_plot_1 <- renderPlotly({
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
    
    data2014$RACE[data2014$RACE == "100"]<-"White"
    data2014$RACE[data2014$RACE == "200"]<-"Black"
    data2014$RACE[data2014$RACE == "300"]<-"American Indian"
    data2014$RACE[data2014$RACE == "651"]<-"Asian"
    data2014$RACE[data2014$RACE == "652"]<-"Pacific Islander"
    data2014$RACE[data2014$RACE == "801"]<-"Other"
    data2014$RACE[data2014$RACE == "802"]<-"Other"
    data2014$RACE[data2014$RACE == "803"]<-"Other"
    data2014$RACE[data2014$RACE == "804"]<-"Other"
    data2014$RACE[data2014$RACE == "805"]<-"Other"
    data2014$RACE[data2014$RACE == "806"]<-"Other"
    data2014$RACE[data2014$RACE == "807"]<-"Other"
    data2014$RACE[data2014$RACE == "808"]<-"Other"
    data2014$RACE[data2014$RACE == "809"]<-"Other"
    data2014$RACE[data2014$RACE == "810"]<-"Other"
    data2014$RACE[data2014$RACE == "811"]<-"Other"
    data2014$RACE[data2014$RACE == "812"]<-"Other"
    data2014$RACE[data2014$RACE == "813"]<-"Other"
    data2014$RACE[data2014$RACE == "814"]<-"Other"
    data2014$RACE[data2014$RACE == "815"]<-"Other"
    data2014$RACE[data2014$RACE == "816"]<-"Other"
    data2014$RACE[data2014$RACE == "817"]<-"Other"
    data2014$RACE[data2014$RACE == "818"]<-"Other"
    data2014$RACE[data2014$RACE == "819"]<-"Other"
    data2014$RACE[data2014$RACE == "820"]<-"Other"
    data2014$RACE[data2014$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2014 <- ggplot(data2014, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2014)
  })
  
  # 2015 race
  output$y6_plot_1 <- renderPlotly({
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
    
    data2015$RACE[data2015$RACE == "100"]<-"White"
    data2015$RACE[data2015$RACE == "200"]<-"Black"
    data2015$RACE[data2015$RACE == "300"]<-"American Indian"
    data2015$RACE[data2015$RACE == "651"]<-"Asian"
    data2015$RACE[data2015$RACE == "652"]<-"Pacific Islander"
    data2015$RACE[data2015$RACE == "801"]<-"Other"
    data2015$RACE[data2015$RACE == "802"]<-"Other"
    data2015$RACE[data2015$RACE == "803"]<-"Other"
    data2015$RACE[data2015$RACE == "804"]<-"Other"
    data2015$RACE[data2015$RACE == "805"]<-"Other"
    data2015$RACE[data2015$RACE == "806"]<-"Other"
    data2015$RACE[data2015$RACE == "807"]<-"Other"
    data2015$RACE[data2015$RACE == "808"]<-"Other"
    data2015$RACE[data2015$RACE == "809"]<-"Other"
    data2015$RACE[data2015$RACE == "810"]<-"Other"
    data2015$RACE[data2015$RACE == "811"]<-"Other"
    data2015$RACE[data2015$RACE == "812"]<-"Other"
    data2015$RACE[data2015$RACE == "813"]<-"Other"
    data2015$RACE[data2015$RACE == "814"]<-"Other"
    data2015$RACE[data2015$RACE == "815"]<-"Other"
    data2015$RACE[data2015$RACE == "816"]<-"Other"
    data2015$RACE[data2015$RACE == "817"]<-"Other"
    data2015$RACE[data2015$RACE == "818"]<-"Other"
    data2015$RACE[data2015$RACE == "819"]<-"Other"
    data2015$RACE[data2015$RACE == "820"]<-"Other"
    data2015$RACE[data2015$RACE == "830"]<-"Other"
    
    # plot
    racexeduc2015 <- ggplot(data2015, aes(x=EDUC, fill=RACE)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(racexeduc2015)
  })
  
  ###################### HISPANIC ######################
  # 2010 Hispanic
  output$y1_plot_2 <- renderPlotly({
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
    
    data2010$HISPAN[data2010$HISPAN == "0"]<-"Not Hispanic"
    data2010$HISPAN[data2010$HISPAN == "000"]<-"Not Hispanic"
    data2010$HISPAN[data2010$HISPAN == "100"]<-"Mexican"
    data2010$HISPAN[data2010$HISPAN == "200"]<-"Puerto Rican"
    data2010$HISPAN[data2010$HISPAN == "300"]<-"Cuban"
    data2010$HISPAN[data2010$HISPAN == "400"]<-"Dominican"
    data2010$HISPAN[data2010$HISPAN == "500"]<-"Salvadoran"
    data2010$HISPAN[data2010$HISPAN == "600"]<-"Other"
    data2010$HISPAN[data2010$HISPAN == "610"]<-"Other"
    data2010$HISPAN[data2010$HISPAN == "611"]<-"Other"
    data2010$HISPAN[data2010$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2010 <- ggplot(data2010, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2010)
  })
  
  # 2011
  output$y2_plot_2 <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
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
    
    data2011$HISPAN[data2011$HISPAN == "0"]<-"Not Hispanic"
    data2011$HISPAN[data2011$HISPAN == "000"]<-"Not Hispanic"
    data2011$HISPAN[data2011$HISPAN == "100"]<-"Mexican"
    data2011$HISPAN[data2011$HISPAN == "200"]<-"Puerto Rican"
    data2011$HISPAN[data2011$HISPAN == "300"]<-"Cuban"
    data2011$HISPAN[data2011$HISPAN == "400"]<-"Dominican"
    data2011$HISPAN[data2011$HISPAN == "500"]<-"Salvadoran"
    data2011$HISPAN[data2011$HISPAN == "600"]<-"Other"
    data2011$HISPAN[data2011$HISPAN == "610"]<-"Other"
    data2011$HISPAN[data2011$HISPAN == "611"]<-"Other"
    data2011$HISPAN[data2011$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2011 <- ggplot(data2011, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2011)
  })
  
  # 2012
  output$y3_plot_2 <- renderPlotly({
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
    
    data2012$HISPAN[data2012$HISPAN == "0"]<-"Not Hispanic"
    data2012$HISPAN[data2012$HISPAN == "000"]<-"Not Hispanic"
    data2012$HISPAN[data2012$HISPAN == "100"]<-"Mexican"
    data2012$HISPAN[data2012$HISPAN == "200"]<-"Puerto Rican"
    data2012$HISPAN[data2012$HISPAN == "300"]<-"Cuban"
    data2012$HISPAN[data2012$HISPAN == "400"]<-"Dominican"
    data2012$HISPAN[data2012$HISPAN == "500"]<-"Salvadoran"
    data2012$HISPAN[data2012$HISPAN == "600"]<-"Other"
    data2012$HISPAN[data2012$HISPAN == "610"]<-"Other"
    data2012$HISPAN[data2012$HISPAN == "611"]<-"Other"
    data2012$HISPAN[data2012$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2012 <- ggplot(data2012, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2012)
    
  })
  
  # 2013
  output$y4_plot_2 <- renderPlotly({
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
    
    data2013$HISPAN[data2013$HISPAN == "0"]<-"Not Hispanic"
    data2013$HISPAN[data2013$HISPAN == "000"]<-"Not Hispanic"
    data2013$HISPAN[data2013$HISPAN == "100"]<-"Mexican"
    data2013$HISPAN[data2013$HISPAN == "200"]<-"Puerto Rican"
    data2013$HISPAN[data2013$HISPAN == "300"]<-"Cuban"
    data2013$HISPAN[data2013$HISPAN == "400"]<-"Dominican"
    data2013$HISPAN[data2013$HISPAN == "500"]<-"Salvadoran"
    data2013$HISPAN[data2013$HISPAN == "600"]<-"Other"
    data2013$HISPAN[data2013$HISPAN == "610"]<-"Other"
    data2013$HISPAN[data2013$HISPAN == "611"]<-"Other"
    data2013$HISPAN[data2013$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2013 <- ggplot(data2013, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2013)
    
  })
  
  # 2014
  output$y5_plot_2 <- renderPlotly({
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
    
    data2014$HISPAN[data2014$HISPAN == "0"]<-"Not Hispanic"
    data2014$HISPAN[data2014$HISPAN == "000"]<-"Not Hispanic"
    data2014$HISPAN[data2014$HISPAN == "100"]<-"Mexican"
    data2014$HISPAN[data2014$HISPAN == "200"]<-"Puerto Rican"
    data2014$HISPAN[data2014$HISPAN == "300"]<-"Cuban"
    data2014$HISPAN[data2014$HISPAN == "400"]<-"Dominican"
    data2014$HISPAN[data2014$HISPAN == "500"]<-"Salvadoran"
    data2014$HISPAN[data2014$HISPAN == "600"]<-"Other"
    data2014$HISPAN[data2014$HISPAN == "610"]<-"Other"
    data2014$HISPAN[data2014$HISPAN == "611"]<-"Other"
    data2014$HISPAN[data2014$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2014 <- ggplot(data2014, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2014)
  })
  
  # 2015
  output$y6_plot_2 <- renderPlotly({
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
    
    data2015$HISPAN[data2015$HISPAN == "0"]<-"Not Hispanic"
    data2015$HISPAN[data2015$HISPAN == "000"]<-"Not Hispanic"
    data2015$HISPAN[data2015$HISPAN == "100"]<-"Mexican"
    data2015$HISPAN[data2015$HISPAN == "200"]<-"Puerto Rican"
    data2015$HISPAN[data2015$HISPAN == "300"]<-"Cuban"
    data2015$HISPAN[data2015$HISPAN == "400"]<-"Dominican"
    data2015$HISPAN[data2015$HISPAN == "500"]<-"Salvadoran"
    data2015$HISPAN[data2015$HISPAN == "600"]<-"Other"
    data2015$HISPAN[data2015$HISPAN == "610"]<-"Other"
    data2015$HISPAN[data2015$HISPAN == "611"]<-"Other"
    data2015$HISPAN[data2015$HISPAN == "612"]<-"Other"
    
    # plot
    hispxeduc2015 <- ggplot(data2015, aes(x=EDUC, fill=HISPAN)) + 
      geom_bar(position = "dodge", stat = "count") + 
      xlab("Level of Educational Attainment") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(hispxeduc2015)
  })

#######################################################

}

# view app
shinyApp(ui, server)

#rsconnect::deployApp('C:/Users/kyrie/Documents/cs600/eduattain/app.R')

#dbDisconnect()



# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs