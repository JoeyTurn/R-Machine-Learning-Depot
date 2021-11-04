#misc info + useful things

#getting data from web
#install.packages("RCurl") #curl = client for url
library(RCurl) #masked %&% from Matrix
webpage <- getURL(url = "https://www.packtpub.com/")
str(webpage)
#install.packages("rjson")
library(rjson)
r_object <- fromJSON(json_string) #converting from JSON to R
json_string <- toJSON(r_object) #converting from R to JSON
#xlsx package can read/write between excel spreadsheets

#working with bioinfomatics starting -> bioconductor project
#working with social media data starting -> network and sna packages (statnet)

#check out CRAN

#faster data frames -> data.table package
#large data frames -> ff package
#massive matrices -> bigmemory package (also bigalgebra, biganalytics, bigtabulate)

#parallel computing -> foreach package
#multicore processing -> multicore package (with mcapply() function, multicore version of lapply())
#multicore on multiple machines -> snow (simple networking of workstations) package
#parallel cloud computing -> MapReduce... and Hadoop

#gpu computing -> gputools package

#building bigger regression & random forests: biglm & bigrf packages
