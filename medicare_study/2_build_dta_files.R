#################################################################
##                    2. Building DTA files                    ##
#################################################################



##---------------------------------------------------------------
##                        Preliminaries                         -
##---------------------------------------------------------------

# Libraries

library(RStata)
library(magrittr)

# Set Stata version
options("RStata.StataVersion" = 13)

# Choose stata extension
RStata::chooseStataBin() 

# Data directory
data_dir <- "medicare_study/data/"
wd_o <- paste0(getwd(),"/")

##---------------------------------------------------------------
##                        Build DTA files                       -
##---------------------------------------------------------------

## Month vector to loop through

months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

files_04<-paste0(months[5:12],"04pub")
files_05<-paste0(months,"05pub")
files_06<-paste0(months,"06pub")

all_files<-c(files_04,files_05,files_06)


### This for loop imports the relevant do file, replaces the monthname and saves the dta file with the correct attributes

# First dictionary

for (i in seq_along(all_files[1:15])){
  may_do<-"medicare_study/data/do_files/cpbsmay04.do"
  do_file <- file(may_do, open = "r")
  do_file_lines <- readLines(do_file)
  do_file_rep<- gsub(x=do_file_lines,"/homes/data/cps-basic/cpsb0405.dat",
                     paste0(wd_o,data_dir,"cps_files/",all_files[i],".dat")) %>%
                gsub(x=.,"./cpsbmay04.dta",
                    paste0(wd_o,data_dir,"dta_files/",all_files[i],".dta")) %>%
                gsub(x=.,"./cpsbmay04.dct",
                    paste0(wd_o,data_dir,"dct_files/cpbsmay04.dct")) %>%
                gsub(x=.,"cpsbmay04",all_files[i]) 
  write.table(do_file_rep,file=paste0(data_dir,"do_files/do_file_rep.do"), row.names = F, quote=F, col.names = F)
  RStata::stata(src=paste0(data_dir,"do_files/do_file_rep.do"))
}

# Second dictionary

for (i in seq_along(all_files[16:32])){
  aug_do<-"medicare_study/data/do_files/cpbsaug05.do"
  do_file <- file(aug_do, open = "r")
  do_file_lines <- readLines(do_file)
  do_file_rep<- gsub(x=do_file_lines,"/homes/data/cps-basic/cpsb0508.dat",
                     paste0(wd_o,data_dir,"cps_files/",all_files[16:32][i],".dat")) %>%
    gsub(x=.,"./cpsbaug05.dta",
         paste0(wd_o,data_dir,"dta_files/",all_files[16:32][i],".dta")) %>%
    gsub(x=.,"./cpsbaug05.dct",
         paste0(wd_o,data_dir,"dct_files/cpbsaug05.dct")) %>%
    gsub(x=.,"cpsbaug05",all_files[16:32][i]) 
  write.table(do_file_rep,file=paste0(data_dir,"do_files/do_file_rep.do"), row.names = F, quote=F, col.names = F)
  RStata::stata(src=paste0(data_dir,"do_files/do_file_rep.do"))
}



