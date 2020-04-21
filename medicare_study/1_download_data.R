#################################################################
##                     1. Downloading Data                     ##
#################################################################

##---------------------------------------------------------------
##                        Preliminaries                         -
##---------------------------------------------------------------

# Libraries

library(tidyverse)
library(foreign)

# Data directory

data_dir <- "medicare_study/data/"

##----------------------------------------------------------------
##                    Download raw CPS files                     -
##----------------------------------------------------------------

### Make vectors to loop through

months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

files_04<-paste0(months[5:12],"04pub")
files_05<-paste0(months,"05pub")
files_06<-paste0(months,"06pub")

all_files<-c(files_04,files_05,files_06)
all_files_zip<-paste0(all_files,".zip")

### For loop

for (i in seq_along(all_files_zip)){
  
  # Create temp file
  temp <- tempfile()

  # Download files
  download.file(paste0("http://data.nber.org/cps-basic/",all_files_zip[i]),temp)
  
  # Unzip files
  unzip(zipfile = temp,
        exdir = paste0(data_dir,"cps_files"))
  
  unlink(temp)
  
}

##----------------------------------------------------------------
##            Fix file type and create empty dta files           -
##----------------------------------------------------------------

#!!! NOTE: file.functions require you to setwd() !!!#

setwd_o <- getwd() # To restore setwd() after file.functions

## Make file types consistent

setwd(paste0(data_dir,"cps_files"))

raw_files_cps <- list.files()
raw_files_dat <- str_replace(raw_files_cps, ".cps", ".dat")

file.rename(raw_files_cps,raw_files_dat)

# Restore setwd()

setwd(setwd_o)

## Create Empty DTA files

setwd(paste0(data_dir,"dta_files"))

for (i in seq_along(all_files)){
  file.create(paste0(all_files[i],".dta"))
}

# Restore setwd()

setwd(setwd_o)


##----------------------------------------------------------------
##  Download do files, dictionaries, and file vectors for loop   -
##----------------------------------------------------------------

### Download .do files

# First do file (May 04 to July 05)

download.file(url="http://data.nber.org/data/progs/cps-basic/cpsbmay04.do",
              destfile=paste0(data_dir,"do_files/cpbsmay04.do"))

# Second do file  (Aug 05 to Dec 06)

download.file(url="http://data.nber.org/data/progs/cps-basic/cpsbaug05.do",
              destfile=paste0(data_dir,"do_files/cpbsaug05.do"))


### Download dictionaires

# First dictionary (May 04 to July 05)

download.file(url="http://data.nber.org/data/progs/cps-basic/cpsbmay04.dct",
              destfile=paste0(data_dir,"dct_files/cpbsmay04.dct"))

# Second dictionary (Aug 05 to Dec 06)

download.file(url="http://data.nber.org/data/progs/cps-basic/cpsbaug05.dct",
              destfile=paste0(data_dir,"dct_files/cpbsaug05.dct"))



