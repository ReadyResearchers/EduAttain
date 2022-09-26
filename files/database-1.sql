/* CPS SQLITE DATABASE CODE -- MULTI*/
/* EduAttain */

/* DATABASE - CPS DATA */

/* 
VARIABLES KEY:

YEAR 
SERIAL - household serial #
MONTH
STATEFIP - state fips code
PERNUM - numbered count of individual in HH
WTFINL - final basic weight -- for stats
CPSIDP - unique individual identifier
AGE
SEX
RACE
BPL - birthplace
HISPAN - hispanic
EDUC - educational attainment
EDDIPGED - HS or GED
FTOTVAL - total family income
INCTOT - total personal income
/*

/* Remove previous table if already in database
Table 1: Household Information */
DROP TABLE Household;
CREATE TABLE Household (
CPSIDP VARCHAR NOT NULL PRIMARY KEY,
YEAR VARCHAR NOT NULL,
MONTH VARCHAR NOT NULL,
PERNUM VARCHAR NOT NULL,
STATEFIP VARCHAR NOT NULL,
WTFINL VARCHAR NOT NULL,
ASECWT VARCHAR NOT NULL
);

/* Remove previous table if already in database
Table 2: Characteristics */
DROP TABLE Characteristics;
CREATE TABLE Characteristics (
CPSIDP VARCHAR NOT NULL PRIMARY KEY,
AGE VARCHAR NOT NULL,
SEX VARCHAR NOT NULL,
RACE VARCHAR NOT NULL,
BPL VARCHAR NOT NULL,
HISPAN VARCHAR NOT NULL
);

/* Remove previous table if already in database
Table 3: Education */
DROP TABLE Education;
CREATE TABLE Education (
CPSIDP VARCHAR NOT NULL PRIMARY KEY,
EDUC VARCHAR NOT NULL,
EDDIPGED VARCHAR NOT NULL
);

/* Remove previous table if already in database
Table 4: Income */
DROP TABLE Income;
CREATE TABLE Income (
CPSIDP VARCHAR NOT NULL PRIMARY KEY,
FTOTVAL VARCHAR NOT NULL,
INCTOT VARCHAR NOT NULL
);