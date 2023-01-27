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


# function for dash creation
ui <- dashboardPage(
  dashboardHeader(title = "EduAttain"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("test_plot"))
  )
)

# server
server <- function(input, output) {
  output$test_plot <- renderPlot({
    # setting database path
    db <- "C:/Users/kyrie/Documents/cs600/CPS.db"
    
    # connect to database
    conn <- dbConnect(drv = SQLite(), dbname = db)
    
    # query to get all data from 2010 for sex + educ attain
    q2 <- 'SELECT cpsidp, sex, educ, race, hispan, ftotval, inctot, month, age, statefip FROM CPS WHERE year = 2010'
    data2010 <- dbGetQuery(conn, q2)
    
    # adult male educational attainment in ny - 2010
    male_2010NY <- data2010 %>% filter(STATEFIP == 36 & SEX == 1) %>% filter(as.numeric(AGE) >= 18)
    
    # educational attainment in NY by males in 2010
    NY2010M_educ_count <- male_2010NY %>% count(EDUC, sort = TRUE)
    
    # NY Male Educational Attainment in 2010
    ggplot(NY2010M_educ_count, aes(x = EDUC, y = n)) +
      geom_col() +
      xlab("Level of Educational Attainment") +
      ylab("Count") +
      ggtitle("Educational Attainment by Adult Males in NY (2010)") +
      theme(axis.text.x = element_text(angle = 90))
  })
  # closes connection to db
  dbDisconnect(conn)
}

# view app
shinyApp(ui, server)


# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs