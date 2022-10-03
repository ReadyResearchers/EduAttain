#####################################
# EDUATTAIN - SHINY APP
#####################################

# APP/WEBPAGE INTERFACE CODE

# install needed packages
install.packages("shinydashboard")

# install libs
library(shiny)
library(shinydashboard)

# function for dash creation
ui <- dashPage(
  dashHeader(),
  dashSidebar(),
  dashBody()
)

# server
server <- function(input, output) {}

# view app
shinyApp(ui, server)