![eduattain header](files/eduattain.jpg)

# EduAttain: A Statistical Analysis of the Impact of Different Demographic Indicators on Educational Attainment

## Abstract

In recent years, trends in educational attainment have reflected simultaneous movements towards closing and widening disparities between different identity groups. Studying educational attainment, specifically revolved around studying differences or disparities in educational attainment, is important because of the implications it has for future work opportunities, financial security, and resource access. This study will look to answer the question of what specific demographic factors and to what degree they are determinants of educational attainment. The demographic factors that this study will focus on include sex, race/ethnicity, and income. The main goal of this study is to address the lack of research in comparing educational attainment trends between different identity groups, with special interest in studying differences in race/ethnicity, and income. Leveraging data from IPUMS, and using R and SQLite, trends in educational attainment across different identity groups will be studied in order to answer this question and display results on a data dashboard, in order to draw comparisons.

## Goal & Progress

*Main Goal:* create a data dashboard displaying statistics and visualizations of trends of educational attainment across different socioeconomic indicators for comparison

*Completed Work*:

- [X] Database
- [-] R visualizations
- [-] R statistics
- [*] R regression
- [*] R Shiny Web Platform

(X = completed, - = in progress, * = spring sem)

## Install & Run EduAttain

For local use, download the same data extract from [IPUMS](https://cps.ipums.org/cps/) and clone this repository. The data extract used in this project selected sample years 2010-2021 and the following variables: STATEFIP, AGE, SEX, RACE, HISPAN, EDUC, FTOTVAL, and INCTOT (alongside other automatically-selected variables). Once the data extract is downloaded, navigate to the SQLite file that contains commands to build a database from the data extract (**located in files/database.sql**) and modify the commands as necessary to run on your machine.

Once the database is successfully created (validate using query), the database is linked to the R code used to make visualization, generate statistics, and host both of these on a web platform. Currently, the web platform for EduAttain has not been fully developed, so in order to view the current results of running EduAttain, you will need a working version of RStudio to manually run each command in the code file located under **src/analysis.r**. 

The final iteration of this tool will hopefully only require a handful (or fewer) commands to increase ease of use and functionality.