#####################################
# EDUATTAIN - SHINY APP
#####################################

# APP/WEBPAGE INTERFACE CODE

# install needed packages
# install.packages("shiny")
# install.packages("shinydashboard")

# install libs
library(shiny)
library(shinydashboard)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)

# setting database path
db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# connect to database
conn <- dbConnect(drv = SQLite(), dbname = db)

# format dash sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Charts", icon = icon("bar-chart-o")),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2")
  )
)

# format dash body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content"),
            box(
              title = "2010 Data - Test", status = "primary", solidHeader = TRUE,
              dataTableOutput("test_table"), width = 10)
    ),
    tabItem(tabName = "subitem1",
            h2("Plots"),
            box(
              title = "2010 Data - Test", status = "primary", solidHeader = TRUE,
              plotlyOutput("test_plot"), width = 10, height = 12)
    ),
    tabItem(tabName = "subitem2",
            h2("Dashboard tab content")
    )
  )
)

# function for dash creation
ui <- dashboardPage(
  dashboardHeader(title = "EduAttain"),
  sidebar,
  body
  #dashboardBody(
   # box(dataTableOutput("test_table"), width = 10)
  #)
)

# server
server <- function(input, output) {
  output$test_table <- renderDataTable(
    
    # query to get all data from 2010 for sex + educ attain
    data2010 <- dbGetQuery(conn,
                           statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010')
     
    # adult male educational attainment in ny - 2010
    #male_2010NY <- data2010 %>% filter(STATEFIP == 36 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)
    
    # educational attainment in NY by males in 2010
    #NY2010M_educ_count <- male_2010NY %>% count(EDUC, sort = TRUE)
  )
  output$test_plot <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    data2010 <- dbGetQuery(conn,
                           statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010')
    
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
    
    # adult male educational attainment in ny - 2010
    male_2010NY <- data2010 %>% filter(STATEFIP == 36 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)
    
    # educational attainment in NY by males in 2010
    NY2010M_educ_count <- male_2010NY %>% count(EDUC, sort = TRUE)
    
    
    # 2010
    # NY Male Educational Attainment in 2010
    g <- ggplot(NY2010M_educ_count, aes(EDUC, n)) +
      geom_col() +
      xlab("Level of Educational Attainment") +
      ylab("Count") +
      ggtitle("Educational Attainment by Adult Males in NY (2010)") +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(g)
  })
}

# view app
shinyApp(ui, server)


# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs