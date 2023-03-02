#####################################
# EDUATTAIN - SHINY APP
#####################################

# APP/WEBPAGE INTERFACE CODE

# install needed packages
# install.packages("shiny")
#install.packages("shinydashboard")
#install.packages('rsconnect')
#install.packages("RSQLite")
#install.packages("vtable")
#install.packages("plotly")
#install.packages("foreign")
#install.packages("MASS")
#install.packages("Hmsic")
#install.packages("reshape2")

# install libs

require(foreign)
require(MASS)

library(shiny)
library(shinydashboard)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(rsconnect)

# setting database path
#db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# setting database path -- via USB
db <- "C:/Users/kyrie/Documents/cs600/CPS.db"

# connect to database
conn <- dbConnect(drv = SQLite(), dbname = db)


#### test to limit results to 18+####
# query to display the first 5 rows
#q <- 'SELECT * from CPS;'
#result <- dbGetQuery(conn,q)
#result = result[-1,]
#as.numeric(result$AGE)
#res <- result %>% filter(AGE >= 18)
######################################


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
                         imageOutput("logo"), width = NULL, height = 350),
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
                       tabPanel("Interpretation", htmlOutput("gen_interpret_11")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_11"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2012",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2012pie")),
                       tabPanel("Female", plotlyOutput("f2012pie")),
                       tabPanel("Interpretation", htmlOutput("gen_interpret_12")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_12"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2013",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2013pie")),
                       tabPanel("Female", plotlyOutput("f2013pie")),
                       tabPanel("Interpretation", htmlOutput("gen_interpret_13")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_13"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2014",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2014pie")),
                       tabPanel("Female", plotlyOutput("f2014pie")),
                       tabPanel("Interpretation", htmlOutput("gen_interpret_14")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_14"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Gender in 2015",
                       height = "500px", width = NULL,
                       tabPanel("Male", plotlyOutput("m2015pie")),
                       tabPanel("Female", plotlyOutput("f2015pie")),
                       tabPanel("Interpretation", htmlOutput("gen_interpret_15")),
                       tabPanel("Comparisons", htmlOutput("gen_compare_15"))
                     )
              )
            )
    ),
    tabItem(tabName = "racxedu",
            h2("Educational Attainment by Race from 2010 to 2015"),
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
                       tabPanel("Mixed Race", plotlyOutput("o2010pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2011",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2011pie")),
                       tabPanel("Black", plotlyOutput("b2011pie")),
                       tabPanel("American Indian", plotlyOutput("ai2011pie")),
                       tabPanel("Asian", plotlyOutput("a2011pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2011pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2011pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2012",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2012pie")),
                       tabPanel("Black", plotlyOutput("b2012pie")),
                       tabPanel("American Indian", plotlyOutput("ai2012pie")),
                       tabPanel("Asian", plotlyOutput("a2012pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2012pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2012pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2013",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2013pie")),
                       tabPanel("Black", plotlyOutput("b2013pie")),
                       tabPanel("American Indian", plotlyOutput("ai2013pie")),
                       tabPanel("Asian", plotlyOutput("a2013pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2013pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2013pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2014",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2014pie")),
                       tabPanel("Black", plotlyOutput("b2014pie")),
                       tabPanel("American Indian", plotlyOutput("ai2014pie")),
                       tabPanel("Asian", plotlyOutput("a2014pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2014pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2014pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Race in 2015",
                       height = "500px", width = NULL,
                       tabPanel("White", plotlyOutput("w2015pie")),
                       tabPanel("Black", plotlyOutput("b2015pie")),
                       tabPanel("American Indian", plotlyOutput("ai2015pie")),
                       tabPanel("Asian", plotlyOutput("a2015pie")),
                       tabPanel("Pacific Islander", plotlyOutput("pi2015pie")),
                       tabPanel("Mixed Race", plotlyOutput("o2015pie"))
                     )
              )
            )
    ),
    tabItem(tabName = "hisxedu",
            h2("US Hispanic/Latino Educational Attainment from 2010 to 2015"),
            fluidRow(
              column(width = 12,
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2010",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2010pie")),
                       tabPanel("Mexican", plotlyOutput("mx2010pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2010pie")),
                       tabPanel("Cuban", plotlyOutput("c2010pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2010pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2011",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2011pie")),
                       tabPanel("Mexican", plotlyOutput("mx2011pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2011pie")),
                       tabPanel("Cuban", plotlyOutput("c2011pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2011pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2012",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2012pie")),
                       tabPanel("Mexican", plotlyOutput("mx2012pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2012pie")),
                       tabPanel("Cuban", plotlyOutput("c2012pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2012pie"))
                     ),
                     tabBox(
                       title = "US Educational Attainment by Hispanic Origin in 2013",
                       height = "500px", width = NULL,
                       tabPanel("Not Hispanic", plotlyOutput("nh2013pie")),
                       tabPanel("Mexican", plotlyOutput("mx2013pie")),
                       tabPanel("Puerto Rican", plotlyOutput("pr2013pie")),
                       tabPanel("Cuban", plotlyOutput("c2013pie")),
                       tabPanel("Other Hispanic", plotlyOutput("oh2013pie"))
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
                       tabPanel("Other Hispanic", plotlyOutput("oh2014pie"))
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
                       tabPanel("Other Hispanic", plotlyOutput("oh2015pie"))
                     )
              )
            )
    ),
    tabItem(tabName = "reg",
            h2("Ordinal Logistic Regression Results"), 
            box(
              title = "Model Summary", status = "primary", solidHeader = TRUE, collapsible = FALSE, verbatimTextOutput("summary"))
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
               <br> This project is divided into two main sections: <em>Descriptive Statistics</em> and <em>Statistical Analysis</em>. <br> For the descriptive statistics, barplots based on counts will depict how
               educational attainment varies by race, gender, and Hispanic ethnicity over each survey year. For the statistical analysis, the statistical relationship between educational attainment and each of the explanatory variables will be tested using an ordinal logistic regression 
               <br> <br>The source code for this project is stored in a <a href='https://github.com/ReadyResearchers/EduAttain'>GitHub Repository</a> that can be accessed for review of the code, adhering to fair use practices."))
  })
  
  output$name <- renderUI({
    HTML(paste("Developed by <b>Kyrie Doniz</b>, under the advisement of <b>Dr. Janyl Jumadinova</b> and <b>Dr. Timothy Bianco</b>, in partial fulfillment of the <a href = 'https://sites.allegheny.edu/academics/senior-project/'>Senior Thesis Project</a> for the Computer Science and Business and Economics Departments at <a href = 'https://allegheny.edu'>Allegheny College</a>."))
  })
#########################################################
  
###################### GENDER ###########################
  
  output$m2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "73"]<-"HS Diploma or Equiv."
    data2010$EDUC[data2010$EDUC == "81"]<-"Some college, no degree"
    data2010$EDUC[data2010$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2010$EDUC[data2010$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "73"]<-"HS Diploma or Equiv."
    data2010$EDUC[data2010$EDUC == "81"]<-"Some college, no degree"
    data2010$EDUC[data2010$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2010$EDUC[data2010$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
               <li><b>1.57%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>0.8%</b> more <em>men</em> had <b>bachelors' degrees</b></li>
               <li><b>0.87%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.72%</b> more <em>women</em> had <b>doctorate degrees</b></li>")
    
  })
  
  output$m2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "73"]<-"HS Diploma or Equiv."
    data2011$EDUC[data2011$EDUC == "81"]<-"Some college, no degree"
    data2011$EDUC[data2011$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2011$EDUC[data2011$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "73"]<-"HS Diploma or Equiv."
    data2011$EDUC[data2011$EDUC == "81"]<-"Some college, no degree"
    data2011$EDUC[data2011$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2011$EDUC[data2011$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    HTML("<b>In 2011:</b><br>
               <li><b>1.54%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>0.7%</b> more <em>men</em> had <b>bachelors' degrees</b></li>
               <li><b>0.84%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.68%</b> more <em>women</em> had <b>doctorate degrees</b></li>")
  })
  
  output$gen_interpret_11 <- renderUI({
    HTML("<b>From 2010 to 2011</b><br>
               <small>Women experienced</small><br>
               <li>a <b>0.28%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.20%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.14%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.31%</b> increase in the amount of <b>masters' degree</b> holders</li><br>
               <small>Men experienced</small><br>
               <li>a <b>0.31%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.30%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.10%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.34%</b> increase in the amount of <b>masters' degree</b> holders</li>")
  })
  
  output$m2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "73"]<-"HS Diploma or Equiv."
    data2012$EDUC[data2012$EDUC == "81"]<-"Some college, no degree"
    data2012$EDUC[data2012$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2012$EDUC[data2012$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "73"]<-"HS Diploma or Equiv."
    data2012$EDUC[data2012$EDUC == "81"]<-"Some college, no degree"
    data2012$EDUC[data2012$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2012$EDUC[data2012$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
               <li><b>1.48%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>0.9%</b> more <em>men</em> had <b>bachelors' degrees</b></li>
               <li><b>0.94%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.82%</b> more <em>women</em> had <b>doctorate degrees</b></li>
               </ul>")
  })
  
  output$gen_interpret_12 <- renderUI({
    HTML("<b>From 2011 to 2012</b><br>
               <small>Women experienced</small><br>
               <ul>
               <li>a <b>0.20%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.50%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.04%</b> decrease in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.23%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>
               <small>Men experienced</small><br>
               <ul>
               <li>a <b>0.26%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.30%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.10%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.13%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>")
  })

  output$m2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "73"]<-"HS Diploma or Equiv."
    data2013$EDUC[data2013$EDUC == "81"]<-"Some college, no degree"
    data2013$EDUC[data2013$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2013$EDUC[data2013$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "73"]<-"HS Diploma or Equiv."
    data2013$EDUC[data2013$EDUC == "81"]<-"Some college, no degree"
    data2013$EDUC[data2013$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2013$EDUC[data2013$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
               <li><b>1.59%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>0.80%</b> more <em>men</em> had <b>bachelors' degrees</b></li>
               <li><b>1.08%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.79%</b> more <em>women</em> had <b>doctorate degrees</b></li>
               </ul>")
  })
  
  output$gen_interpret_13 <- renderUI({
    HTML("<b>From 2012 to 2013</b><br>
               <small>Women experienced</small><br>
               <ul>
               <li>a <b>0.21%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.10%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.08%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.41%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>
               <small>Men experienced</small><br>
               <ul>
               <li>a <b>0.10%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.20%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.05%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.27%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>")
  })
  
  output$m2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "73"]<-"HS Diploma or Equiv."
    data2014$EDUC[data2014$EDUC == "81"]<-"Some college, no degree"
    data2014$EDUC[data2014$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2014$EDUC[data2014$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "73"]<-"HS Diploma or Equiv."
    data2014$EDUC[data2014$EDUC == "81"]<-"Some college, no degree"
    data2014$EDUC[data2014$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2014$EDUC[data2014$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
               <li><b>1.89%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>1.10%</b> more <em>men</em> had <b>bachelors' degrees</b></li>
               <li><b>1.26%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.81%</b> more <em>women</em> had <b>doctorate degrees</b></li>
               </ul>")
  })
  
  output$gen_interpret_14 <- renderUI({
    HTML("<b>From 2013 to 2014</b><br>
               <small>Women experienced</small><br>
               <ul>
               <li>a <b>0.12%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>0.30%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.11%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.22%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>
               <small>Men experienced</small><br>
               <ul>
               <li>a <b>0.18%</b> decrease in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>no change</b> in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.13%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>0.04%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>")
  })
  
  output$m2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "73"]<-"HS Diploma or Equiv."
    data2015$EDUC[data2015$EDUC == "81"]<-"Some college, no degree"
    data2015$EDUC[data2015$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2015$EDUC[data2015$EDUC == "92"]<-"Associate's Degree, Academic"
    
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "73"]<-"HS Diploma or Equiv."
    data2015$EDUC[data2015$EDUC == "81"]<-"Some college, no degree"
    data2015$EDUC[data2015$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    data2015$EDUC[data2015$EDUC == "92"]<-"Associate's Degree, Academic"
    
    
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
               <li><b>1.93%</b> more <em>men</em> had <b>associates' degrees</b></li>
               <li><b>0.10%</b> more <em>women</em> had <b>bachelors' degrees</b></li>
               <li><b>2.55%</b> more <em>men</em> had <b>masters' degrees</b></li>
               <li><b>0.98%</b> more <em>women</em> had <b>doctorate degrees</b></li>
               </ul>")
  })
  
  output$gen_interpret_15 <- renderUI({
    HTML("<b>From 2014 to 2015</b><br>
               <small>Women experienced</small><br>
               <ul>
               <li>a <b>0.21%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>2.40%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.22%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>3.04%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>
               <small>Men experienced</small><br>
               <ul>
               <li>a <b>0.17%</b> increase in the amount of <b>associates' degree</b> holders</li>
               <li>a <b>3.60%</b> increase in the amount of <b>bachelors' degree</b> holders</li>
               <li>a <b>0.39%</b> increase in the amount of <b>doctorate degree</b> holders</li>
               <li>a <b>1.75%</b> increase in the amount of <b>masters' degree</b> holders</li>
               </ul>")
  })
  
#######################################################
  
  
  
###################### RACE ###########################
  
  ##2010 RACE

  output$w2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')

    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")

    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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

  ## 2011 RACE

  output$w2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')

    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")

    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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

  ##2012 RACE

  output$w2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')

    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")

    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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

  ##2013 RACE

  output$w2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')

    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")

    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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

  ##2014 RACE

  output$w2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')

    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")

    # 2014 filters

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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

  ## 2015 RACE

  output$w2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')

    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")

    # 2015 filters

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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

    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
  
  ###################### HISPANIC ######################
  
  # 2010 HISPAN
  output$nh2010pie <- renderPlotly({
    # query to get all data from 2010 for sex + educ attain
    d2010 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010 AND age >= 18')
    
    # filter NIU for educ
    data2010 <- d2010 %>% filter(EDUC != "1")
    
    # 2010 filters
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
    data2010$EDUC[data2010$EDUC == "10"]<-"Elementary School"
    data2010$EDUC[data2010$EDUC == "111"]<-"Bachelor's Degree"
    data2010$EDUC[data2010$EDUC == "123"]<-"Master's Degree"
    data2010$EDUC[data2010$EDUC == "124"]<-"Professional School Degree"
    data2010$EDUC[data2010$EDUC == "125"]<-"Doctorate Degree"
    data2010$EDUC[data2010$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2010$EDUC[data2010$EDUC == "20"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "30"]<-"Middle School"
    data2010$EDUC[data2010$EDUC == "40"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "50"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "60"]<-"High School, no diploma"
    data2010$EDUC[data2010$EDUC == "71"]<-"High School, no diploma"
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
  
  ##2011 HISPAN
  
  output$nh2011pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2011 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2011 AND age >= 18 ORDER BY cpsidp')
    
    # filter NIU for educ
    data2011 <- d2011 %>% filter(EDUC != "1")
    
    # 2011 filters
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
    data2011$EDUC[data2011$EDUC == "10"]<-"Elementary School"
    data2011$EDUC[data2011$EDUC == "111"]<-"Bachelor's Degree"
    data2011$EDUC[data2011$EDUC == "123"]<-"Master's Degree"
    data2011$EDUC[data2011$EDUC == "124"]<-"Professional School Degree"
    data2011$EDUC[data2011$EDUC == "125"]<-"Doctorate Degree"
    data2011$EDUC[data2011$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2011$EDUC[data2011$EDUC == "20"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "30"]<-"Middle School"
    data2011$EDUC[data2011$EDUC == "40"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "50"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "60"]<-"High School, no diploma"
    data2011$EDUC[data2011$EDUC == "71"]<-"High School, no diploma"
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
  
  ##2012 HISPAN
  
  output$nh2012pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2012 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2012 AND age >= 18')
    
    # filter NIU for educ
    data2012 <- d2012 %>% filter(EDUC != "1")
    
    # 2012 filters
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
    data2012$EDUC[data2012$EDUC == "10"]<-"Elementary School"
    data2012$EDUC[data2012$EDUC == "111"]<-"Bachelor's Degree"
    data2012$EDUC[data2012$EDUC == "123"]<-"Master's Degree"
    data2012$EDUC[data2012$EDUC == "124"]<-"Professional School Degree"
    data2012$EDUC[data2012$EDUC == "125"]<-"Doctorate Degree"
    data2012$EDUC[data2012$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2012$EDUC[data2012$EDUC == "20"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "30"]<-"Middle School"
    data2012$EDUC[data2012$EDUC == "40"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "50"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "60"]<-"High School, no diploma"
    data2012$EDUC[data2012$EDUC == "71"]<-"High School, no diploma"
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
  
  ## 2013 HISPAN
  
  output$nh2013pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2013 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2013 AND age >= 18')
    
    # filter NIU for educ
    data2013 <- d2013 %>% filter(EDUC != "1")
    
    # 2013 filters
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
    data2013$EDUC[data2013$EDUC == "10"]<-"Elementary School"
    data2013$EDUC[data2013$EDUC == "111"]<-"Bachelor's Degree"
    data2013$EDUC[data2013$EDUC == "123"]<-"Master's Degree"
    data2013$EDUC[data2013$EDUC == "124"]<-"Professional School Degree"
    data2013$EDUC[data2013$EDUC == "125"]<-"Doctorate Degree"
    data2013$EDUC[data2013$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2013$EDUC[data2013$EDUC == "20"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "30"]<-"Middle School"
    data2013$EDUC[data2013$EDUC == "40"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "50"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "60"]<-"High School, no diploma"
    data2013$EDUC[data2013$EDUC == "71"]<-"High School, no diploma"
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
  
  ## 2014 HISPAN
  
  output$nh2014pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2014 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2014 AND age >= 18')
    
    # filter NIU for educ
    data2014 <- d2014 %>% filter(EDUC != "1")
    
    # 2014 filters
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
    
    data2014$EDUC[data2014$EDUC == "10"]<-"Elementary School"
    data2014$EDUC[data2014$EDUC == "111"]<-"Bachelor's Degree"
    data2014$EDUC[data2014$EDUC == "123"]<-"Master's Degree"
    data2014$EDUC[data2014$EDUC == "124"]<-"Professional School Degree"
    data2014$EDUC[data2014$EDUC == "125"]<-"Doctorate Degree"
    data2014$EDUC[data2014$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2014$EDUC[data2014$EDUC == "20"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "30"]<-"Middle School"
    data2014$EDUC[data2014$EDUC == "40"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "50"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "60"]<-"High School, no diploma"
    data2014$EDUC[data2014$EDUC == "71"]<-"High School, no diploma"
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
  
  ## 2015 HISPAN
  
  output$nh2015pie <- renderPlotly({
    # query to get all data from 2011 for sex + educ attain
    d2015 <- dbGetQuery(conn,
                        statement= 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2015 AND age >= 18')
    
    # filter NIU for educ
    data2015 <- d2015 %>% filter(EDUC != "1")
    
    # 2015 filters
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
    
    data2015$EDUC[data2015$EDUC == "10"]<-"Elementary School"
    data2015$EDUC[data2015$EDUC == "111"]<-"Bachelor's Degree"
    data2015$EDUC[data2015$EDUC == "123"]<-"Master's Degree"
    data2015$EDUC[data2015$EDUC == "124"]<-"Professional School Degree"
    data2015$EDUC[data2015$EDUC == "125"]<-"Doctorate Degree"
    data2015$EDUC[data2015$EDUC == "2"]<-"None/Preschool/Kindergarten"
    data2015$EDUC[data2015$EDUC == "20"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "30"]<-"Middle School"
    data2015$EDUC[data2015$EDUC == "40"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "50"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "60"]<-"High School, no diploma"
    data2015$EDUC[data2015$EDUC == "71"]<-"High School, no diploma"
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
  
  output$summary <- renderPrint({
    # query to display the first 5 rows
    result <- dbGetQuery(conn,
                         statement= "SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
    
    
    # all yrs
    result$EDUC[result$EDUC == "10"]<-"Grades 1-4"
    result$EDUC[result$EDUC == "111"]<-"Bachelor's Degree"
    result$EDUC[result$EDUC == "123"]<-"Master's Degree"
    result$EDUC[result$EDUC == "124"]<-"Professional School Degree"
    result$EDUC[result$EDUC == "125"]<-"Doctorate Degree"
    result$EDUC[result$EDUC == "2"]<-"None/Preschool/Kindergarten"
    result$EDUC[result$EDUC == "20"]<-"Grades 5-6"
    result$EDUC[result$EDUC == "30"]<-"Grades 7-8"
    result$EDUC[result$EDUC == "40"]<-"HS, Grade 9"
    result$EDUC[result$EDUC == "50"]<-"HS, Grade 10"
    result$EDUC[result$EDUC == "60"]<-"HS, Grade 11"
    result$EDUC[result$EDUC == "71"]<-"HS, Grade 12, no diploma"
    result$EDUC[result$EDUC == "73"]<-"HS Diploma or Equiv."
    result$EDUC[result$EDUC == "81"]<-"Some college, no degree"
    result$EDUC[result$EDUC == "91"]<-"Occupational/Vocational Program Degree"
    result$EDUC[result$EDUC == "92"]<-"Associate's Degree, Academic"
    
    ##### converting educ levels to factor
    result$EDUC <- factor(result$EDUC, levels=c("None/Preschool/Kindergarten","Grades 1-4","Grades 5-6","Grades 7-8","HS, Grade 9", "HS, Grade 10", "HS, Grade 11", "HS, Grade 12, no diploma", "HS Diploma or Equiv.", "Some college, no degree","Occupational/Vocational Program Degree", "Associate's Degree, Academic", "Bachelor's Degree","Master's Degree", "Professional School Degree", "Doctorate Degree"))
    
    
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
    
    #dummy variable recoding
    
    # sex
    result$female <- ifelse(result$SEX == "2", 1, 0)
    
    # race
    result$black <- ifelse(result$RACE == "200", 1, 0)
    result$amer_indian <- ifelse(result$RACE == "300", 1, 0)
    result$asian <- ifelse(result$RACE == "651", 1, 0)
    result$islander <- ifelse(result$RACE == "652", 1, 0)
    result$mixed_race <- ifelse(result$RACE == "999", 1, 0)
    
    # hispanic
    result$mex <- ifelse(result$HISPAN == "100", 1, 0)
    result$pr <- ifelse(result$HISPAN == "200", 1, 0)
    result$cuban <- ifelse(result$HISPAN == "300", 1, 0)
    result$dom <- ifelse(result$HISPAN == "400", 1, 0)
    result$salv <- ifelse(result$HISPAN == "500", 1, 0)
    result$otherhispan <- ifelse(result$HISPAN == "600", 1, 0)
    result$centralamer <- ifelse(result$HISPAN == "611", 1, 0)
    result$southamer <- ifelse(result$HISPAN == "612", 1, 0)
    
    
    ## fit ordered logit model and store results 'm'
    model <- polr(EDUC ~ female + black + amer_indian + asian + islander + mixed_race + mex + pr + cuban + dom + salv + otherhispan + centralamer + southamer, data = result, Hess=TRUE, method = c("logistic"))
    
    # summary
    summary(model)
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