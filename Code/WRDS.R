#WRDS R API

#Clear enviornment
rm(list = ls())

#Packages
library(RPostgres)
library(data.table)
library(dplyr)

#WRDS Connection
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  user = "malik36",
                  password = "W0ff0rd!",
                  sslmode='require')

###########
#Meta Data#
###########
#Query libraries
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#Query datasets in specific library
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='taqm_2019'
                   order by table_name")
data <- dbFetch(res, n=-1) %>% data.table()
dbClearResult(res)
data

#Get monthly trade data
ctm <- data[table_name %like% '^ctm*']


#Look at specific dataset in library
res <- dbSendQuery(wrds, "select *
                   from information_schema.columns
                   where table_schema='taqm_2019'
                   and table_name='ctm_20190903'
                   order by column_name")
data <- dbFetch(res, n=10) %>% data.table()
dbClearResult(res)
data

#Rows
#Look at specific dataset in library
res <- dbSendQuery(wrds, "select * 
                   from taqm_2019.ctm_20190903 
                   where sym_root = 'SPY' and tr_corr = '00'
                   ")
data <- dbFetch(res, n=1000) %>% data.table()
dbClearResult(res)
data
