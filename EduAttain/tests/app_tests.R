# EduAttain Dashboard App Testing

context("Testing EduAttain")

# QUERY

# Test that the data is queried correctly
test_that("SQLite query returns expected output", {

    # install necessary libs
    library(testthat)
    library(DBI)
    library(RSQLite)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinytest)
    
    # setting database path -- via USB
    db <- "data/CPS.db"
    
    # connect to database
    conn <- dbConnect(RSQLite::SQLite(), dbname = db)

    # query
    result <- dbGetQuery(conn,"SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")

    # check if result is data frame
    expect_true(is.data.frame(result))

    # disconnect from sqlite db
    dbDisconnect(conn)
})

# RANDOM SAMPLE

# Test that random sample is correctly generated
test_that("Random sample is correctly generated from original data", {
    
    # install necessary libs
    library(testthat)
    library(DBI)
    library(RSQLite)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinytest)
    
    # setting database path -- via USB
    db <- "data/CPS.db"
    
    # connect to database
    conn <- dbConnect(RSQLite::SQLite(), dbname = db)
    
    # query
    result <- dbGetQuery(conn,"SELECT cpsidp, sex, educ, race, hispan, age FROM CPS WHERE age >= 18 AND cpsidp !='CPSIDP'")
        
    ## remove na values
    result <- na.omit(result)
        
    ## sample data
    sample_result <- result[sample(nrow(result), 100000), ]
        
    # check if result is data frame
    expect_equal(dim(sample_result), c(100000, ncol(result)))
        
    # disconnect from sqlite db
    dbDisconnect(conn)
})
    

# PLOTLY OBJECT TESTING

# Test that a plotly object is generated
test_that("Test that a plotly object is correctly generated from original data", {
    
    # install necessary libs
    library(testthat)
    library(DBI)
    library(RSQLite)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinytest)
    library(dplyr)
    
    # setting database path -- via USB
    db <- "data/CPS.db"
    
    # connect to database
    conn <- dbConnect(RSQLite::SQLite(), dbname = db)
    
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
    
    p <- plot_ly(data=df,values=~n,labels=~factor(EDUC),
                    textposition="outside",textinfo = 'label+percent',
                    outsidetextfont = list(color = 'red'),
                    marker=list(colors=c("grey", 'blue', 'yellow'),
                                line=list(color="white",width=2)),type="pie") %>%
        layout(legend=list(title=list(text='<b> Level of Educational Attainment </b>')))
    
    # test plotly object is generated
    expect_visible(p)
    
    # disconnect from sqlite db
    dbDisconnect(conn)
})
