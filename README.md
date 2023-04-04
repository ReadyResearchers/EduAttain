# EduAttain: A Statistical Analysis of the Impact of Different Demographic Indicators on Educational Attainment <!-- markdown-line-length-ignore -->

![MIT License badge](https://img.shields.io/github/license/ReadyResearchers/EduAttain) ![SQLite badge](https://img.shields.io/badge/SQLite-07405E?style=for-the-badge&logo=sqlite&logoColor=white) ![RStudio Badge](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white) ![Build Status](https://img.shields.io/github/actions/workflow/status/ReadyResearchers/EduAttain/main.yml) <!-- markdown-line-length-ignore -->

![eduattain header](files/eduattain.jpg)

## Table of Contents

1. [Abstract](#abstract)
2. [Technical Details](#technical-details)
3. [Installation](#installation)
4. [Structure](#structure)
5. [Usage](#usage)
6. [Results](#results)

## Abstract

Over the last fifty years, trends in educational attainment have reflected simultaneous movements towards closing and widening disparities between different identity groups. Studying educational attainment, specifically revolved around studying differences or disparities in educational attainment, is important because of the implications for future work opportunities, financial security, and resource access. This study identifies and investigates the demographic factors as determinants of educational attainment, namely sex, race, and Hispanic ethnicity. The main goal of this study is to address the lack of research in comparing educational attainment trends between different identity groups and to present research that studies these factors statistical relationship to education. Leveraging data from IPUMS, and using R and SQLite, trends in educational attainment across different identity groups will be studied through the use of pie charts to display results and draw comparisons that will be displayed on a web-based dashboard. The statistical relationship between these factors and educational attainment will be studied using a binary logistic regression, to determine what populations had a higher odds of having a high school diploma or greater. The findings of this project affirm much of the research done in the realm of gender, racial, and ethnic inequities in education, in that White, Non Hispanic, and Female populations have the highest rates of educational attainment, compared to all other populations. <!-- markdown-line-length-ignore -->

## Technical Details

[include details about tools and packages]

## Installation

This project does not require a prior install before usage and can be accessed using the [web-based platform](https://donizk.shinyapps.io/EduAttain/), deployed on [shinyapps.io](https://www.shinyapps.io/).<!-- markdown-line-length-ignore -->

For those who would like to install the project files for use on a local machine, the following steps will need to be taken to support the usage of the application:<!-- markdown-line-length-ignore -->

1. Install the source files from this GitHub Repository by clicking the **Code** button and selecting the **Download Zip** option.<!-- markdown-line-length-ignore -->
2. Ensure you have a working installation of R and R Studio.
3. Extract the files from the zipped file on your local machine.

## Structure

The structure of the files contributing to the development of EduAttain are as follows:

- `R` folder: contains the main code file for this project, `app.R`, and the corresponding `description` configuration file. This folder also contains the following folders, all of which contain pertinent information for the project:<!-- markdown-line-length-ignore -->
        - `data` folder: contains the data source for this project, `CPS.db`.
        - `tests` folder: contains the test file for this project, `app_tests.R`.
        - `www` folder: contains the picture files used in the project
- `files` folder: contains the `database.sql` code file used to create the SQLite database housing the data for this project, as well as, pictures used in the README.<!-- markdown-line-length-ignore -->

## Usage

Read along to learn more about the [Local Usage](local-usage), [Web Usage](web-usage), and [Expected Output](expected-output) of EduAttain. <!-- markdown-line-length-ignore -->

### Local Usage

To use EduAttain locally using the source files mentioned in the [Installation](#installation) section, the following steps must be taken:<!-- markdown-line-length-ignore -->

1. Traverse the installed, extracted directory within your local machine's file directory ( which should be named *EduAttain*) for the **R/app.R** file.<!-- markdown-line-length-ignore -->
2. Open the **R/app.R** file in RStudio.
3. Run the application using the IDE's run button or by typing in the following command to the RStudio console: `runApp('path/to/file/EduAttain/R')`<!-- markdown-line-length-ignore -->
4. If the application is running successfully, a separate browser or RStudio application window should open, displaying an application similar to that documented in [Expected Output](#expected-output)<!-- markdown-line-length-ignore -->

### Web Usage

For general access and use of EduAttain, without having to go through the process of local installation, the [dashboard application](https://donizk.shinyapps.io/EduAttain/) is hosted on shinyapps.io.<!-- markdown-line-length-ignore -->

### Expected Output

EduAttain is a comprehensive data analysis dashboard studying educational attainment as it relates to gender, race, and Hispanic Ethnicity, sectioned off into two main sections: *Descriptive Statistics* and *Statistical Analysis*.<!-- markdown-line-length-ignore -->

The structure of the dashboard is as follows:

- **Data & Description**: This page of the dashboard presents a table of the raw data set extracted from IPUMS, summary statistics relating to the age and amount of individuals captured in the data, project description, and a data key for the data table.
![Data & Description](files/home.jpg)<!-- markdown-line-length-ignore -->

- **Gender x Educational Attainment**: This page of the dashboard presents pie charts that were generated for each survey year representing the proportion of each gender's population above or below a High School education, and interpretations of the plots that compare female and male educational attainment.<!-- markdown-line-length-ignore -->
![Gender x Educational Attainment](files/genxedu.jpg)

- **Race x Educational Attainment**: This page of the dashboard presents pie charts that were generated for each survey year representing the proportion of each racial population above or below a High School education, and interpretations of the plots that compare White, Black, Asian, American Indian, Pacific Islander, and Mixed Race educational attainment.<!-- markdown-line-length-ignore -->
![Race x Educational Attainment](files/racexedu.jpg)

- **Hispanic x Educational Attainment**: This page of the dashboard presents pie charts that were generated for each survey year representing the proportion of each Hispanic population above or below a High School education, and interpretations of the plots that compare Non Hispanic, Mexican, Puerto Rican, Cuban, Other Hispanic, Dominican, and Salvadorian educational attainment.<!-- markdown-line-length-ignore -->
![Hispanic x Educational Attainment](files/hispanxedu.jpg)

- **Regression**: This page of the dashboard presents the results of the binary logistic regression and corresponding odds ratio, along with interpretations from these figures.<!-- markdown-line-length-ignore -->
![Statistical Analysis](files/stats.jpg)

The Gender x Educational Attainment, Race x Educational Attainment, and Hispanic x Educational Attainment fall under the Descriptive Statistics section, where plotly pie charts based on population percentages depict how educational attainment varies by race, gender, and Hispanic ethnicity over each survey year.<!-- markdown-line-length-ignore -->

The Regression section of the dashboard falls within the Statistical Analysis section, where the statistical relationship between educational attainment and each of the explanatory variables was tested using a binary logistic regression.<!-- markdown-line-length-ignore -->

## Results

[write about results of plots and regression]