/* CPS SQLITE DATABASE CODE - SINGLE*/

/* EduAttain */

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

/* 
CREATE DATABASE FROM CMD
sqlite3 CPS.db
*/

/* Remove previous table if already in database */
DROP TABLE IF EXISTS CPS;
CREATE TABLE CPS (
CPSIDP VARCHAR NOT NULL, 
YEAR VARCHAR NOT NULL,
SERIAL VARCHAR NOT NULL,
MONTH VARCHAR NOT NULL,
STATEFIP VARCHAR NOT NULL,
PERNUM VARCHAR NOT NULL,
WTFINL VARCHAR NOT NULL,
ASECWT VARCHAR NOT NULL,
AGE VARCHAR NOT NULL,
SEX VARCHAR NOT NULL,
RACE VARCHAR NOT NULL,
BPL VARCHAR NOT NULL,
HISPAN VARCHAR NOT NULL,
EDUC VARCHAR NOT NULL,
FTOTVAL VARCHAR NOT NULL,
INCTOT VARCHAR NOT NULL,
PRIMARY KEY (`YEAR`, `SERIAL`, `MONTH`, `PERNUM`)
);

/* NOT NULL == BLANK COLUMN -- investigate whether or not it is*/
/* file import commands - csv */

/* assign import mode */
.mode csv

/* import csv file in */
.import C:\\Users\\kyrie\\OneDrive\\Desktop\\data\\cps_00002.csv CPS

/* view tables */
.tables

/* view structure of table */
.schema CPS

/* view all contents of table */
/* SELECT * from CPS*/
/* BUT DON'T ACTUALLY RUN THIS BC IT TAKES FOREVER :D*/

/* view first 5 rows of table */
SELECT * from CPS LIMIT 5;
