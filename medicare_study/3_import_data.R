##################################################################
##                        3. Import Data                        ##
##################################################################


## ---------------------------------------------------------------
##                        Preliminaries                         -
## ---------------------------------------------------------------

# Libraries

library(foreign)
library(data.table)
library(magrittr)
library(haven)

# Set data directory

data_dir <- "medicare_study/data/"

# Not in operator

`%notin%` <- Negate(`%in%`)


## ---------------------------------------------------------------
##                          Import data                         -
## ---------------------------------------------------------------


# Function to filter datasets

pull_cps<-function(dta){
  read_stata(dta) %>% 
  as.data.table() %>% 
  .[, .(hrhhid,
        pesex,
        hrmonth,
        hryear4,
        pemlr,
        hurespli,
        prhrusl,
        peage,
        hufaminc,
        peeduca,
        peio1cow,
        peio2cow,
        hrmis,
        perrp,
        hrnumhou,
        ptdtrace,
        gestfips,
        pemaritl,
        prcitshp,
        hrhhid2,
        hrlonglk,
        gereg)]  %>% 
        setnames(., old = (1:22),
                 new = c("house_id",
                         "sex",
                         "month",
                         "year",
                          "employed",
                         "line",
                         "hours",
                         "age",
                         "income",
                         "education",
                         "class",
                         "class2",
                         "sample_month",
                         "ref_pers",
                         "num_res",
                         "race",
                         "state",
                         "marital",
                         "citizen",
                         "house_id2",
                         "longitude",
                         "region"))
}


month_files <- paste0(data_dir,"dta_files/", list.files(paste0(data_dir,"dta_files")))

# Create merged data.table

cps_data <-  month_files %>% map(pull_cps)

cps_data <- cps_data %>% bind_rows()

saveRDS(cps_data, file="medicare_study/output/cps_merged/cps_data.RDS")

