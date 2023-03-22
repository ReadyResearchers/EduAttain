#####################################
# EDUATTAIN - SHINY APP
#####################################

# APP/WEBPAGE INTERFACE CODE

# install needed packages
# install.packages("shiny")
#install.packages("shinydashboard")
#install.packages('rsconnect')
#install.packages("RSQLite")
#install.packages("plotly")
#install.packages("testthat")


# install libs

library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(plotly)
library(rsconnect)
library(testthat)

# setting database path
#db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# setting database path -- via USB
db <- "data/CPS.db"

# connect to database
conn <- dbConnect(RSQLite::SQLite(), dbname = db)

# format dash sidebar
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Home", tabName = "dash", icon = icon("house")),
    menuItem("Gender x Educational Attainment", tabName = "genxedu", icon = icon("chart-column")),
    menuItem("Race x Educational Attainment", tabName = "racxedu", icon = icon("chart-column")),
    menuItem("Hispanic x Educational Attainment", tabName = "hisxedu", icon = icon("chart-column")),
    menuItem("Regression", tabName = "reg", icon = icon("chart-line"))
  )
)

# format dash body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dash",
            h2("Dataset & Description"),
            box(
              title = "IPUMS US CPS Data 2010-2015", status = "primary", solidHeader = TRUE, collapsible = FALSE,
              dataTableOutput("data_table"), width = 6),
            
            
            fluidRow(
              column(width = 6,
                     box(title = NULL, status = "primary", solidHeader = FALSE,
                         imageOutput("logo"), width = NULL, height = 450),
                     box(title = "Project Description", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                         htmlOutput("description"), width = NULL),
                     box(
                       title = "Data Key", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       htmlOutput("key"), width = NULL),
                     fluidRow(
                       valueBox(18, "years old, the minimum age in the sample", icon = icon("list")),
                       valueBox(45, "years old, the median age in the sample", icon = icon("list")),
                       valueBox(85, "years old, the maximum age in the sample", icon = icon("list"))
                     ),
                     fluidRow(
                       valueBox(753244, "individuals (18+) captured in the sample", icon = icon("list")),
                       box(
                         title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
                         htmlOutput("name"), width = 8)
                     )
              )
            ),
    ),
    tabItem(tabName = "genxedu",
            h2("Educational Attainment by Gender from 2010 to 2015"),
            box(
              title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
              htmlOutput("gen_intro"), width = 12),
            fluidRow(
              column(width = 12,
                     tabBox(
                       title = "US Educational Attainment by Gender in 2010",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2010pie")),
                       tabPanel("Female", plotlyOutput("f2010pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_10"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2011",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2011pie")),
                       tabPanel("Female", plotlyOutput("f2011pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_11"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2012",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2012pie")),
                       tabPanel("Female", plotlyOutput("f2012pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_12"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2013",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2013pie")),
                       tabPanel("Female", plotlyOutput("f2013pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_13"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2014",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2014pie")),
                       tabPanel("Female", plotlyOutput("f2014pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_14"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2015",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2015pie")),
                       tabPanel("Female", plotlyOutput("f2015pie")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_15"))
                     )
              )
            )
    ),
    tabItem(tabName = "racxedu",
            h2("Educational Attainment by Race from 2010 to 2015"),
            box(
              title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
              htmlOutput("race_intro"), width = 12),
            fluidRow(
              column(width = 12,
                     tabBox(
                       title = "US Educational Attainment by Race in 2010",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2010pie")),
                       tabPanel("Black", plotlyOutput("b2010pie")),
                       tabPanel("American Indian", plotlyOutput("ai2010pie")),
                       tabPanel("Asian", plotlyOutput("a2010pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2010pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2010pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_10"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2011",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2011pie")),
                       tabPanel("Black", plotlyOutput("b2011pie")),
                       tabPanel("American Indian", plotlyOutput("ai2011pie")),
                       tabPanel("Asian", plotlyOutput("a2011pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2011pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2011pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_11"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2012",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2012pie")),
                       tabPanel("Black", plotlyOutput("b2012pie")),
                       tabPanel("American Indian", plotlyOutput("ai2012pie")),
                       tabPanel("Asian", plotlyOutput("a2012pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2012pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2012pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_12"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2013",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2013pie")),
                       tabPanel("Black", plotlyOutput("b2013pie")),
                       tabPanel("American Indian", plotlyOutput("ai2013pie")),
                       tabPanel("Asian", plotlyOutput("a2013pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2013pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2013pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_13"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2014",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2014pie")),
                       tabPanel("Black", plotlyOutput("b2014pie")),
                       tabPanel("American Indian", plotlyOutput("ai2014pie")),
                       tabPanel("Asian", plotlyOutput("a2014pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2014pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2014pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_14"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2015",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2015pie")),
                       tabPanel("Black", plotlyOutput("b2015pie")),
                       tabPanel("American Indian", plotlyOutput("ai2015pie")),
                       tabPanel("Asian", plotlyOutput("a2015pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2015pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2015pie")),
                       tabPanel("Comparisons", htmlOutput("race_compare_15"))
                     )
              )
            )
    ),
    tabItem(tabName = "hisxedu",
            h2("US Hispanic/Latino Educational Attainment from 2010 to 2015"),
            box(
              title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
              htmlOutput("hispan_intro"), width = 12),
            fluidRow(
              column(width = 12,
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2010",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2010pie")),
                       tabPanel("Mexican", plotlyOutput("mx2010pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2010pie")),
                       tabPanel("Cuban", plotlyOutput("c2010pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2010pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_10"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2011",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2011pie")),
                       tabPanel("Mexican", plotlyOutput("mx2011pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2011pie")),
                       tabPanel("Cuban", plotlyOutput("c2011pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2011pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_11"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2012",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2012pie")),
                       tabPanel("Mexican", plotlyOutput("mx2012pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2012pie")),
                       tabPanel("Cuban", plotlyOutput("c2012pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2012pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_12"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2013",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2013pie")),
                       tabPanel("Mexican", plotlyOutput("mx2013pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2013pie")),
                       tabPanel("Cuban", plotlyOutput("c2013pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2013pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_13"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2014",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2014pie")),
                       tabPanel("Mexican", plotlyOutput("mx2014pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2014pie")),
                       tabPanel("Cuban", plotlyOutput("c2014pie")),
                       tabPanel("Dominican", plotlyOutput("d2014pie")),
                       tabPanel("Salvadorian", plotlyOutput("s2014pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2014pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_14"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2015",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2015pie")),
                       tabPanel("Mexican", plotlyOutput("mx2015pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2015pie")),
                       tabPanel("Cuban", plotlyOutput("c2015pie")),
                       tabPanel("Dominican", plotlyOutput("d2015pie")),
                       tabPanel("Salvadorian", plotlyOutput("s2015pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2015pie")),
                       tabPanel("Comparisons", htmlOutput("hispan_compare_15"))
                     )
              )
            )
    ),
    tabItem(tabName = "reg",
            h2("Statistical Analysis"),
            box(
              title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
              htmlOutput("stat_intro"), width = 12),
            fluidRow(
              column(width = 12,
                     tabBox(
                         title = "Summary", width = NULL,
                         tabPanel("Binary Logistic Regression", verbatimTextOutput("binary"), htmlOutput("reg_interpret"), verbatimTextOutput("cm")), tabPanel("Odds Ratio", verbatimTextOutput("binary_odds"), htmlOutput("odds_interpret")),
                         tabPanel("2nd Binary Logistic Regression", verbatimTextOutput("binary_2"), htmlOutput("reg_interpret_2"), verbatimTextOutput("cm_2")), tabPanel("2nd Odds Ratio", verbatimTextOutput("binary_odds_2"), htmlOutput("odds_interpret_2"))
                     )
                  )
            )
    )
  )
)

# function for dash creation

ui <- dashboardPage(
  dashboardHeader(title = "EduAttain", titleWidth = 250),
  sidebar,
  body
)

# server
server <- function(input, output) {
  
###################### TABLE ###########################
  
  output$logo <- renderImage({
    list(src = "www/eduattain.jpg",
         width = "100%")
  }, deleteFile = F)
  
  output$data_table <- renderDataTable(
    #res
    # query to get all data from 2010 for sex + educ attain
    df <- dbGetQuery(conn,
                          statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
  )
  
  output$key <- renderUI({
    HTML(paste("<b>EDUC</b> - Educational Attainment  <br>  <b>SEX</b> - Gender  <br>  <b>RACE</b> - Race  <br>  <b>HISPAN</b> - Hispanic Ethnicity  <br>  <b>CPSDIP</b> - IPUMS CPS Individual ID <br>
               <br> The numerical assignments used within the variables in the dataset are listed on IPUMS to use when identifying each category present in the data."))
  })
  
  output$description <- renderUI({
    HTML(paste("<b>EduAttain</b>, leverages data from <a href='https://cps.ipums.org/cps/index.shtml'>IPUMS</a>, to assess how an individual's <em>race, gender, or Hispanic ethnicity</em> influence the level of education attained. <br>
               <br> This project is divided into two main sections: <em>Descriptive Statistics</em> and <em>Statistical Analysis</em>. <br><br> For the descriptive statistics, pie charts based on population percentages will depict how
               educational attainment varies by race, gender, and Hispanic ethnicity over each survey year. For the statistical analysis, the statistical relationship between educational attainment and each of the explanatory variables will be tested using a binary logistic regression. 
               <br> <br>The source code for this project is stored in a <a href='https://github.com/ReadyResearchers/EduAttain'>GitHub Repository</a> that can be accessed for review of the code, adhering to fair use practices."))
  })
  
  output$name <- renderUI({
    HTML(paste("Developed by <b>Kyrie Doniz</b>, under the advisement of <b>Dr. Janyl Jumadinova</b> and <b>Dr. Timothy Bianco</b>, in partial fulfillment of the <a href = 'https://sites.allegheny.edu/academics/senior-project/'>Senior Thesis Project</a> for the Computer Science and Business and Economics Departments at <a href = 'https://allegheny.edu'>Allegheny College</a>."))
  })
#########################################################
  
###################### GENDER ###########################
  
  output$gen_intro <- renderUI({
    HTML("The comparisons in this section were computed by observing plots and recording their values on a spreadsheet. These were used to compare population percentages to see differences in educational attainment between identity groups. The levels of education were combined to create two distinct categories: <b>High School Diploma or Greater</b> and <b>High School or Less</b> <br><br>
         <small>*Any miscalculations may be a result of manual entry</small><br><br> These computations can be observed <a href='https://docs.google.com/spreadsheets/d/1KuRkeLsDDIG6rsqbB9p_4u6002yk-FJL7efE9rQhBdQ/edit?usp=sharing'>here</a>.")
  })
  
  output$m2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2010$SEX[data2010$SEX == "1"]<-"Male"
    data2010$SEX[data2010$SEX == "2"]<-"Female"
    
    df <- data2010 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
    
  })

  output$f2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2010$SEX[data2010$SEX == "1"]<-"Male"
    data2010$SEX[data2010$SEX == "2"]<-"Female"
    
    df <- data2010 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
    
  })
  
  output$gen_compare_10 <- renderUI({
    HTML("<b>In 2010:</b><br>
               <ul><li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li></ul>")
    
  })
  
  output$m2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2011$SEX[data2011$SEX == "1"]<-"Male"
    data2011$SEX[data2011$SEX == "2"]<-"Female"
    
    df <- data2011 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$f2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2011$SEX[data2011$SEX == "1"]<-"Male"
    data2011$SEX[data2011$SEX == "2"]<-"Female"
    
    df <- data2011 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$gen_compare_11 <- renderUI({
    HTML("<b>In 2011:</b><br><ul>
               <li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li></ul>")
  })
  
  output$m2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2012$SEX[data2012$SEX == "1"]<-"Male"
    data2012$SEX[data2012$SEX == "2"]<-"Female"
    
    df <- data2012 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$f2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2012$SEX[data2012$SEX == "1"]<-"Male"
    data2012$SEX[data2012$SEX == "2"]<-"Female"
    
    df <- data2012 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$gen_compare_12 <- renderUI({
    HTML("<b>In 2012:</b><br>
               <ul>
               <li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li>
               </ul>")
  })

  output$m2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2013$SEX[data2013$SEX == "1"]<-"Male"
    data2013$SEX[data2013$SEX == "2"]<-"Female"
    
    df <- data2013 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$f2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2013$SEX[data2013$SEX == "1"]<-"Male"
    data2013$SEX[data2013$SEX == "2"]<-"Female"
    
    df <- data2013 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$gen_compare_13 <- renderUI({
    HTML("<b>In 2013:</b><br>
               <ul>
               <li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li>
               </ul>")
  })
  
  output$m2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2014$SEX[data2014$SEX == "1"]<-"Male"
    data2014$SEX[data2014$SEX == "2"]<-"Female"
    
    df <- data2014 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$f2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2014$SEX[data2014$SEX == "1"]<-"Male"
    data2014$SEX[data2014$SEX == "2"]<-"Female"
    
    df <- data2014 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$gen_compare_14 <- renderUI({
    HTML("<b>In 2014:</b><br>
               <ul>
               <li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li>
               </ul>")
  })
  
  output$m2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
    data2015$SEX[data2015$SEX == "1"]<-"Male"
    data2015$SEX[data2015$SEX == "2"]<-"Female"
    
    df <- data2015 %>% 
      filter(SEX =="Male") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$f2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
    
    data2015$SEX[data2015$SEX == "1"]<-"Male"
    data2015$SEX[data2015$SEX == "2"]<-"Female"
    
    df <- data2015 %>% 
      filter(SEX =="Female") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>% 
      ungroup() %>% 
      mutate(perc = `n` / sum(`n`)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$gen_compare_15 <- renderUI({
    HTML("<b>In 2015:</b><br>
               <ul>
               <li>The <b>Female</b> population had a slightly higher proportion of individuals who had an educational attainment of a high school diploma or higher.</li>
               </ul>")
  })
  
#######################################################
  
  
  
###################### RACE ###########################
  
  output$race_intro <- renderUI({
    HTML("The comparisons in this section were computed by observing plots and recording their values on a spreadsheet. These were used to compare population percentages to see differences  in educational attainment between identity groups. The levels of education were combined to create two distinct categories: <b>High School Diploma or Greater</b> and <b>High School or Less</b> <br><br>
         <small>*Any miscalculations may be a result of manual entry, The mixed race population in this sample accounts for every possible combination of these races, as well as, any unspecified mixed race entries</small><br><br> These computations can be observed 
         <a href='https://docs.google.com/spreadsheets/d/1e4gtS3aIDYTBXB1uGxbzMUKj9pPhlBCmbXSftVrWlxI/edit?usp=sharing'>here</a>.")
  })
  
  ##2010 RACE

  output$w2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2010 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$race_compare_10 <- renderUI({
    HTML("<b>In 2010:</b><br><br>
          <strong>Conclusions</strong><br>
          <ul>
          <li>The <b>White</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>Black, American Indian, Pacific Islander, and Mixed Race</b> populations, while having a lower proportion compared to the <b>Asian</b> population.</li>
          
          <li>The <b>Black</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian</b> population, while having a lower proportion compared to the <b>White, Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Black</b> populations, while having a lower proportion compared to the <b>White, Asian, and Mixed Race</b> population.</li>
          
          <li>The <b>Asian</b> population had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
         </ul>")
  })

  ## 2011 RACE

  output$w2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"

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


    df <- data2011 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$race_compare_11 <- renderUI({
    HTML("<b>In 2011:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>White</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>Black and American Indian</b> populations, while having a lower proportion compared to the <b>Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>Black</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian</b> population, while having a lower proportion compared to the <b>White, Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>White, American Indian, Mixed Race, and Black</b> populations, while having a lower proportion compared to the <b>Asian</b> population.</li>
          
          <li>The <b>Asian</b> population had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
          
               </ul>")
  })

  
  ##2012 RACE

  output$w2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))



    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))



    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2012 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))



    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$race_compare_12 <- renderUI({
    HTML("<b>In 2012:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>White</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>Black, Pacific Islander, and American Indian</b> populations, while having a lower proportion compared to the <b>Asian and Mixed Race</b> population.</li>
          
          <li>The <b>Black</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian</b> population, while having a lower proportion compared to the <b>White, Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Black</b> populations, while having a lower proportion compared to the <b>White, Mixed Race, and Asian</b> population.</li>
          
          <li>The <b>Asian</b> population had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
          
               </ul>")
  })

  ##2013 RACE

  output$w2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2013 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2013 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))



    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2013 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
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

    df <- data2013 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2013 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2013 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  
  output$race_compare_13 <- renderUI({
    HTML("<b>In 2013:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>White</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>Black and American Indian</b> populations, while having a lower proportion compared to the <b>Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>Black</b> population had an equivalent proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian</b> population, while having a lower proportion compared to the <b>White, Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>, excluding the <b>Black</b> population.</li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Black</b> populations, while having a lower proportion compared to the <b>White, Mixed Race, and Asian</b> population.</li>
          
          <li>The <b>Asian</b> population had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
               
               </ul>")
  })
  
  
  ##2014 RACE

  output$w2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    

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

    df <- data2014 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    

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

    df <- data2014 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    

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

    df <- data2014 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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

    df <- data2014 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    

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

    df <- data2014 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    

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

    df <- data2014 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$race_compare_14 <- renderUI({
    HTML("<b>In 2014:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>White</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>Black, Pacific Islander, and American Indian</b> populations, while having a lower proportion compared to the <b>Asian and Mixed Race</b> population.</li>
          
          <li>The <b>Black</b> population had an higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian</b> population, while having a lower proportion compared to the <b>White, Asian, Pacific Islander, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b></li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Black</b> populations, while having a lower proportion compared to the <b>White, Mixed Race, and Asian</b> population.</li>
          
          <li>The <b>Asian</b> population had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>.</li>
               
               </ul>")
  })
  
  
  ## 2015 RACE

  output$w2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="White") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$b2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="Black") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$ai2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="American Indian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))

    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$a2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="Asian") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$pi2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="Pacific Islander") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })

  output$o2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"

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

    df <- data2015 %>%
      filter(RACE =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))


    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  
  output$race_compare_15 <- renderUI({
    HTML("<b>In 2015:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>White</b> population had a highest proportion of individuals who had an educational attainment of high school diploma or greater compared to  
          <b>all other racial groups</b>.</li>
          
          <li>The <b>Black</b> population had an higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Pacific Islander</b> population, while having a lower proportion compared to the <b>White, Asian, and Mixed Race</b> population.</li>
          
          <li>The <b>American Indian</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b></li>
          
          <li>The <b>Pacific Islander</b> population had a higher proportion of individuals who had an educational attainment of high school diploma or greater compared to the 
          <b>American Indian and Black</b> populations, while having a lower proportion compared to the <b>White, Mixed Race, and Asian</b> population.</li>
          
          <li>The <b>Asian</b> population had the second highest proportion of individuals who had an educational attainment of high school diploma or greater compared to 
          <b>all other racial groups</b>, with the exception of when compared to the <b>White</b> population.</li>
               
               </ul>")
  })
  
  
  ###################### HISPANIC ######################
  
  output$hispan_intro <- renderUI({
    HTML("The comparisons in this section were computed by observing plots and recording their values on a spreadsheet. These were used to compare population percentages to see differences  in educational attainment between identity groups. The levels of education were combined to create two distinct categories: <b>High School Diploma or Greater</b> and <b>High School or Less</b><br><br>
         <small>*Any miscalculations may be a result of manual entry, The <em>Other Hispanic</em> population in this sample accounts for entries left unspecified or marked as Central or South American. Data for Salvadorian and Dominican Hispanic origin only available in survey years 2014 and 2015.</small><br><br> These computations can be observed 
         <a href='https://docs.google.com/spreadsheets/d/1UmisBE7AXtq3nleSEl2d4KGAKI4VKf8qxxjICS3SKYc/edit?usp=sharing'>here</a>.")
  })
  
  # 2010 HISPAN
  output$nh2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2010 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2010 %>%
      filter(HISPAN=="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2010 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2010 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$oh2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "111"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "123"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "124"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "125"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "2"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "20"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "30"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "40"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "50"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "60"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "71"]<-"Some High School or Less"
    data2010$EDUC[data2010$EDUC == "73"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "81"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "91"]<-"High School Diploma or Greater"
    data2010$EDUC[data2010$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2010 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  
  output$hispan_compare_10 <- renderUI({
    HTML("<b>In 2010:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Other Hispanic</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>

               </ul>")
  })
  
  ##2011 HISPAN
  
  output$nh2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2011 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2011 %>%
      filter(HISPAN =="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2011 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2011 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
 
  output$oh2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "111"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "123"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "124"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "125"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "2"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "20"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "30"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "40"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "50"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "60"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "71"]<-"Some High School or Less"
    data2011$EDUC[data2011$EDUC == "73"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "81"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "91"]<-"High School Diploma or Greater"
    data2011$EDUC[data2011$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2011 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$hispan_compare_11 <- renderUI({
    HTML("<b>In 2011:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
                <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Other Hispanic</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>

               </ul>")
  })
  
  ##2012 HISPAN
  
  output$nh2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2012 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2012 %>%
      filter(HISPAN =="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2012 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2012 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$oh2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "111"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "123"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "124"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "125"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "2"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "20"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "30"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "40"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "50"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "60"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "71"]<-"Some High School or Less"
    data2012$EDUC[data2012$EDUC == "73"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "81"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "91"]<-"High School Diploma or Greater"
    data2012$EDUC[data2012$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2012 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$hispan_compare_12 <- renderUI({
    HTML("<b>In 2012:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
                <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Other Hispanic</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>

               </ul>")
  })
  
  ## 2013 HISPAN
  
  output$nh2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2013 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2013 %>%
      filter(HISPAN =="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2013 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2013 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
 
  output$oh2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "111"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "123"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "124"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "125"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "2"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "20"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "30"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "40"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "50"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "60"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "71"]<-"Some High School or Less"
    data2013$EDUC[data2013$EDUC == "73"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "81"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "91"]<-"High School Diploma or Greater"
    data2013$EDUC[data2013$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2013 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$hispan_compare_13 <- renderUI({
    HTML("<b>In 2013:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had the lowest proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Other Hispanic</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>

               </ul>")
  })
  
  ## 2014 HISPAN
  
  output$nh2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$d2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Dominican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$s2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Salvadoran") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$oh2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "111"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "123"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "124"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "125"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "2"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "20"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "30"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "40"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "50"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "60"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "71"]<-"Some High School or Less"
    data2014$EDUC[data2014$EDUC == "73"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "81"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "91"]<-"High School Diploma or Greater"
    data2014$EDUC[data2014$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2014 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$hispan_compare_14 <- renderUI({
    HTML("<b>In 2014:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
                <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had a lower proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample, excluding the <b>Salvadorian</b> population.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican, Other Hispanic, Dominican, and Salvadorian</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>
               
               <li>The <b>other Hispanic</b> population had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic, Puerto Rican, and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican, Dominican, and Salvadorian</b> populations.</li>
               
               <li>The <b>Dominican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic, Puerto Rican, Other Hispanic, and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Salvadorian</b> populations.</li>

               </ul>")
  })
  
  ## 2015 HISPAN
  
  output$nh2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Not Hispanic") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$mx2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Mexican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$pr2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Puerto Rican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$c2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Cuban") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$d2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Dominican") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$s2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Salvadoran") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$oh2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "111"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "123"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "124"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "125"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "2"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "20"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "30"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "40"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "50"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "60"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "71"]<-"Some High School or Less"
    data2015$EDUC[data2015$EDUC == "73"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "81"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "91"]<-"High School Diploma or Greater"
    data2015$EDUC[data2015$EDUC == "92"]<-"High School Diploma or Greater"
    
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
    
    df <- data2015 %>%
      filter(HISPAN =="Other") %>%
      group_by(EDUC) %>% # Variable to be transformed
      count() %>%
      ungroup() %>%
      mutate(perc = `n` / sum(`n`)) %>%
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
    
    
    plot_ly(data=df,values=~n,labels=~factor(EDUC),
            textposition="outside",textinfo = 'label+percent',
            outsidetextfont = list(color = 'red'),
            marker=list(colors=c("grey", 'blue', 'yellow'),
                        line=list(color="white",width=2)),type="pie") %>%
      layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
  })
  
  output$hispan_compare_15 <- renderUI({
    HTML("<b>In 2015:</b><br><br>
               <strong>Conclusions</strong><br>
               <ul>
               
               <li>The <b>non-Hispanic</b> population in this sample had the highest proportion of individuals who had an educational attainment of high school diploma or greater compared to <b>all other Hispanic ethnic groups</b>.</li>
               
               <li>The <b>Mexican</b> population had a lower proportion of individuals who had an educational attainment of high school diploma or greater, compared to <b>all other Hispanic populations</b> in this sample, excluding the <b>Salvadorian</b> population.</li>
               
               <li>The <b>Puerto Rican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican, Other Hispanic, Dominican, and Salvadorian</b> populations.</li>
               
               <li>The <b>Cuban</b> population in this sample had a higher proportion of individuals with an educational attainment of high school diploma or greater compared to <b>all other Hispanic populations, excluding the Non Hispanic population</b>. </li>
               
               <li>The <b>other Hispanic</b> population had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic, Puerto Rican, and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican, Dominican, and Salvadorian</b> populations.</li>
               
               <li>The <b>Dominican</b> population in this sample had a lower proportion of individuals with a high school diploma or greater than the <b>Non Hispanic, Puerto Rican, Other Hispanic, and Cuban</b> populations, but a higher proportion than the 
               <b>Mexican and Salvadorian</b> populations.</li>

               </ul>")
  })
  
  ##################################################
  
  output$stat_intro <- renderUI({
    HTML("For this project, a <b>binary logistic regression</b> model was employed to study the relationship between <em>race, gender, and Hispanic ethnicity</em> as they relate to educational attainment. To achieve these results, the educational attainment variable was recoded from its original <em>ordinal</em> form into a <em>binary</em> variable, separating the levels of educational
         attainment into two categories, <b>High School Diploma or Greater and Some High School or Lesser</b>. This should be kept in mind in the interpretation of the generated regression and subsequent odds ratio. Most of the interpretation from this model will come from the results of the odds ratio, which provide insight into the likelihood of a given identity group to attain an
         education equivalent to or greater than a high school diploma. Additionally, a second regression and odds ratio provide insight into how racial and ethnic groups compare in the likelihood of getting a High School Degree or Greater.<br><br><small>These regressions were constructed using a randomly selected sample of 100,000 observations from the source data, as to improve processing time.</small>")
  })
  
  # regression model output
  output$binary <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "0"]<-"000"
    #other hispan filtering
    result$HISPAN[result$HISPAN == "600"]<-"650"
    result$HISPAN[result$HISPAN == "610"]<-"650"
    result$HISPAN[result$HISPAN == "611"]<-"650"
    result$HISPAN[result$HISPAN == "612"]<-"650"
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$mex <- ifelse(result$HISPAN == "100", 1, 0)
    result$pr <- ifelse(result$HISPAN == "200", 1, 0)
    result$cuban <- ifelse(result$HISPAN == "300", 1, 0)
    result$dom <- ifelse(result$HISPAN == "400", 1, 0)
    result$salv <- ifelse(result$HISPAN == "500", 1, 0)
    result$otherhispan <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + mex + pr + cuban + dom + salv + otherhispan, family = binomial, data = sample_result)
    
    
    ## view a summary of the model
    summary(m)
    
  })
  
  output$reg_interpret <- renderUI({
    HTML("In this regression, all the predictor variables (<em>those included within the regression equation</em>) that are accompanied by a * or . are <b>statistically significant</b>. In this model, all of the explanatory variables are statistically significant, meaning they have an impact on the educational attainment variable. <br><br>
         Given the confusion matrix that was computed for the model, the <b>accuracy</b> of the results of the regression is as follows:")
  })
  
  output$cm <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "0"]<-"000" # non hispanic
    #other hispan filtering
    result$HISPAN[result$HISPAN == "600"]<-"650" # other hispanic
    result$HISPAN[result$HISPAN == "610"]<-"650" # central/south american
    result$HISPAN[result$HISPAN == "611"]<-"650" # central american, excluding salvadorian
    result$HISPAN[result$HISPAN == "612"]<-"650" # south american 
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$mex <- ifelse(result$HISPAN == "100", 1, 0)
    result$pr <- ifelse(result$HISPAN == "200", 1, 0)
    result$cuban <- ifelse(result$HISPAN == "300", 1, 0)
    result$dom <- ifelse(result$HISPAN == "400", 1, 0)
    result$salv <- ifelse(result$HISPAN == "500", 1, 0)
    result$otherhispan <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    # binary logistic regression model
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + mex + pr + cuban + dom + salv + otherhispan, family = binomial, data = sample_result)
    
    # obtain predictions
    predict <- predict(m, type="response")
    predict_class <- ifelse(predict > 0.5, "Yes", "No")
    
    # confusion matrix
    cm <- table(predict_class, sample_result$EDUC)
    
    TP <- cm[1,1]
    TN <- cm[2,2]
    
    accuracy <- (TP+TN)/sum(cm)
    cat("Binary Logit Model 1 Accuracy: ", round(accuracy,4))
  })
  
  output$binary_odds <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "0"]<-"000" # non hispanic
    #other hispan filtering
    result$HISPAN[result$HISPAN == "600"]<-"650" # other hispanic
    result$HISPAN[result$HISPAN == "610"]<-"650" # central/south american
    result$HISPAN[result$HISPAN == "611"]<-"650" # central american, excluding salvadorian
    result$HISPAN[result$HISPAN == "612"]<-"650" # south american 
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$mex <- ifelse(result$HISPAN == "100", 1, 0)
    result$pr <- ifelse(result$HISPAN == "200", 1, 0)
    result$cuban <- ifelse(result$HISPAN == "300", 1, 0)
    result$dom <- ifelse(result$HISPAN == "400", 1, 0)
    result$salv <- ifelse(result$HISPAN == "500", 1, 0)
    result$otherhispan <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    # binary logistic regression model
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + mex + pr + cuban + dom + salv + otherhispan, family = binomial, data = sample_result)
    
    ## view odds ratio
    exp(coef(m))
    
  })
  
  output$odds_interpret <- renderUI({
    HTML("According to the results from the <em>binary logistic regression</em> and <em>odds ratio</em>:
         <ul>
         <li>Compared to the male population, the <b>female</b> population in this sample had higher odds of having an educational 
         attainment equivalent to a high school diploma or greater</li>
         
         <li>Compared to the White population, <b>all other racial groups</b> have a lower odds of having an educational 
         attainment equivalent to a high school diploma or greater.</li>
         
         <li>Compared to the non-Hispanic population, <b>all other Hispanic ethnic groups</b> have a lower odds of having an educational 
         attainment equivalent to a high school diploma or greater.</li>
         </ul>")
  })
  
  # regression model output
  output$binary_2 <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "000"]<-"0" # non hispanic
    #hispan filtering
    result$HISPAN[result$HISPAN == "100"]<-"650" #mexican
    result$HISPAN[result$HISPAN == "200"]<-"650"#puerto rican
    result$HISPAN[result$HISPAN == "300"]<-"650"#cuban
    result$HISPAN[result$HISPAN == "400"]<-"650"#dominican
    result$HISPAN[result$HISPAN == "500"]<-"650"#salvadorian
    result$HISPAN[result$HISPAN == "600"]<-"650" # other hispanic
    result$HISPAN[result$HISPAN == "610"]<-"650" # central/south american
    result$HISPAN[result$HISPAN == "611"]<-"650" # central american, excluding salvadorian
    result$HISPAN[result$HISPAN == "612"]<-"650" # south american 
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$hispanic <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    # binary logistic regression model
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + hispanic, family = binomial, data = sample_result)
    
    summary(m)
  })
  
  output$reg_interpret_2 <- renderUI({
    HTML("In this regression, all the predictor variables (<em>those included within the regression equation</em>) that are accompanied by a * or . are <b>statistically significant</b>. In this second model, all of the explanatory variables are statistically significant, meaning they have an impact on the educational attainment variable. <br><br>
         Given the confusion matrix that was computed for the model, the <b>accuracy</b> of the results of the regression is as follows:")
  })
  
  output$cm_2 <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "000"]<-"0" # non hispanic
    #hispan filtering
    result$HISPAN[result$HISPAN == "100"]<-"650" #mexican
    result$HISPAN[result$HISPAN == "200"]<-"650"#puerto rican
    result$HISPAN[result$HISPAN == "300"]<-"650"#cuban
    result$HISPAN[result$HISPAN == "400"]<-"650"#dominican
    result$HISPAN[result$HISPAN == "500"]<-"650"#salvadorian
    result$HISPAN[result$HISPAN == "600"]<-"650" # other hispanic
    result$HISPAN[result$HISPAN == "610"]<-"650" # central/south american
    result$HISPAN[result$HISPAN == "611"]<-"650" # central american, excluding salvadorian
    result$HISPAN[result$HISPAN == "612"]<-"650" # south american 
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$hispanic <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    # binary logistic regression model
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + hispanic, family = binomial, data = sample_result)
    
    # obtain predictions
    predict <- predict(m, type="response")
    predict_class <- ifelse(predict > 0.5, "Yes", "No")
    
    # confusion matrix
    cm <- table(predict_class, sample_result$EDUC)
    
    TP <- cm[1,1]
    TN <- cm[2,2]
    
    accuracy <- (TP+TN)/sum(cm)
    cat("Accuracy: ", round(accuracy,4))
    
  })
  
  output$binary_odds_2 <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    ##coding for all yrs - binary reg
    result$EDUC[result$EDUC == "10"]<-"Some High School or Less" # Some High School or Less = 0
    result$EDUC[result$EDUC == "111"]<-"High School Diploma or Greater" # High School Diploma or Greater = 1
    result$EDUC[result$EDUC == "123"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "124"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "125"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "2"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "20"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "30"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "40"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "50"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "60"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "71"]<-"Some High School or Less"
    result$EDUC[result$EDUC == "73"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "81"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "91"]<-"High School Diploma or Greater"
    result$EDUC[result$EDUC == "92"]<-"High School Diploma or Greater"
    
    # filtering
    result$RACE[result$RACE == "801"]<-"999" #white black
    result$RACE[result$RACE == "802"]<-"999" #white american indian
    result$RACE[result$RACE == "803"]<-"999" #white asian
    result$RACE[result$RACE == "804"]<-"999" #white pacific islander
    result$RACE[result$RACE == "805"]<-"999" #black american indian
    result$RACE[result$RACE == "806"]<-"999" #black asian
    result$RACE[result$RACE == "807"]<-"999" #black pacific islander
    result$RACE[result$RACE == "808"]<-"999" #american indian asian
    result$RACE[result$RACE == "809"]<-"999" #asian pacific islander
    result$RACE[result$RACE == "810"]<-"999" #white black american indian
    result$RACE[result$RACE == "811"]<-"999" #white black asian
    result$RACE[result$RACE == "812"]<-"999" #white american indian asian
    result$RACE[result$RACE == "813"]<-"999" #white asian pacific islander
    result$RACE[result$RACE == "814"]<-"999" #white black american indian asian
    result$RACE[result$RACE == "815"]<-"999" #american indian
    result$RACE[result$RACE == "816"]<-"999" #white black pacific islander
    result$RACE[result$RACE == "817"]<-"999" #white american indian pacific islander
    result$RACE[result$RACE == "818"]<-"999" #black american indian asian
    result$RACE[result$RACE == "819"]<-"999" #white american indian asian pacific islander
    result$RACE[result$RACE == "820"]<-"999" #mixed race, 2-3, unspecified
    result$RACE[result$RACE == "830"]<-"999" #mixed race, 4-5, unspecified
    
    
    # hispanic filtering
    result$HISPAN[result$HISPAN == "000"]<-"0" # non hispanic
    #hispan filtering
    result$HISPAN[result$HISPAN == "100"]<-"650" #mexican
    result$HISPAN[result$HISPAN == "200"]<-"650"#puerto rican
    result$HISPAN[result$HISPAN == "300"]<-"650"#cuban
    result$HISPAN[result$HISPAN == "400"]<-"650"#dominican
    result$HISPAN[result$HISPAN == "500"]<-"650"#salvadorian
    result$HISPAN[result$HISPAN == "600"]<-"650" # other hispanic
    result$HISPAN[result$HISPAN == "610"]<-"650" # central/south american
    result$HISPAN[result$HISPAN == "611"]<-"650" # central american, excluding salvadorian
    result$HISPAN[result$HISPAN == "612"]<-"650" # south american 
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    #result$SEX <- replace(result$SEX == "1", 0) #male base case is == 1
    
    # race
    #result$RACE <- replace(result$RACE == "100", 0) # white base case is == 100
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    #result$HISPAN <-- replace(result$HISPAN == "000", 0) # non hispanic base case == 000
    result$hispanic <- ifelse(result$HISPAN == "650", 1, 0)
    result$EDUC <- ifelse(result$EDUC == "High School Diploma or Greater", 1, 0)
    
    ## remove na values
    result <- na.omit(result)
    
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
    
    # binary logistic regression model
    m <- glm(EDUC~female + black + amer_indian + asian + islander + mixed_race + hispanic, family = binomial, data = sample_result)
    
    ## view odds ratio
    exp(coef(m))
    
  })
  output$odds_interpret_2 <- renderUI({
    HTML("According to the results from the <em>binary logistic regression</em> and <em>odds ratio</em>:
         <ul>
         <li>Compared to the male population, the <b>female</b> population in this sample had higher odds of having an educational 
         attainment equivalent to a high school diploma or greater</li>
         
         <li>Compared to the White, non-Hispanic population, <b>all other racial and ethnic groups</b> have a lower odds of having an educational 
         attainment equivalent to a high school diploma or greater, albeit at varying levels. Following the attainment of the White population is that of the Mixed Race,
         Pacific Islander, Asian, Black, American Indian, and Hispanic populations, in order.</li>
         </ul>")
  })
}

onStop(function() {
  dbDisconnect(conn)
})

# view app
shinyApp(ui, server)

#rsconnect::deployApp('C:/Users/kyrie/Documents/cs600/eduattain/app.R')

#dbDisconnect()



# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs