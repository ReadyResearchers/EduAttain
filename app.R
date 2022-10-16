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

# function for dash creation
ui <- dashboardPage(
  dashboardHeader(title = "EduAttain"),
  dashboardSidebar(),
  dashboardBody()
)

# server
server <- function(input, output) { }

# view app
shinyApp(ui, server)

# NOTE: sample implementation of a basic dash
# works -- but needs to be fleshed out with sections + stats/graphs