##################################################################
##                     4. Matching CPS Data                     ##
##################################################################


##---------------------------------------------------------------
##                        Preliminaries                         -
##---------------------------------------------------------------

# Libraries

library(tidyverse)
library(janitor)

# Import data

cps_data <- readRDS("medicare_study/output/cps_merged/cps_data.RDS")


##----------------------------------------------------------------
##                      Matching Functions                       -
##----------------------------------------------------------------

# household_cps() selects households where the number of residents stays the same, and
# drops those where it changes. It also filters households with >=4 months of observations

household_cps <- function(cps_obs = cps_obs_group){
  cps_obs_group %>%
    filter(age >= 16, class > 0, education > 0, sex > 0, race > 0) %>%
    group_by(house_id, sample_month) %>%
    count() %>%
    ungroup() %>%
    chop(c(sample_month, n)) %>%
    mutate(n_unique = map(n, unique)) %>%
    mutate(length_unique = map_dbl(n_unique, length)) %>%
    mutate(length_mis = map_dbl(sample_month, length)) %>%
    filter(length_unique == 1, length_mis >= 4) %>%
    select(house_id)
}

# match_cps() matches individuals over time and gives them a unique id. It can do this
# 'naively', or by default filter out any obvious matching errors. 

match_cps<-function(cps_obs = cps_obs_group, household=household_list, naive = F, na_index=F){
  if(naive==T & na_index==F){
    cps_obs %>%
      filter(house_id %in% household$house_id) %>%
      group_by(house_id,month,year) %>%
      arrange(house_id, month, year, sample_month, sex, race, age, education, class) %>% # Add consistency in rownumbers
      mutate(unique_id = row_number(),
             unique_id = paste(house_id,unique_id, sep="_")) %>% # Create unique id
      ungroup() %>% 
      distinct() # removes any duplicates
  }
  else if(naive==F & na_index==T){
    cps_obs %>%
      filter(house_id %in% household$house_id) %>%
      group_by(house_id,month,year) %>%
      arrange(house_id, month, year, sample_month, sex, race, age, education, class) %>% # Add consistency in rownumbers
      mutate(unique_id = row_number(),
             unique_id = paste(house_id,month_i[1],year_i[1],unique_id, sep="_")) %>% # Create unique id
      ungroup() %>% 
      group_by(unique_id) %>% 
      mutate(min_age = min(age),
             max_age = max(age),
             min_sex = min(sex),
             max_sex = max(sex),
             min_race = min(race),
             max_race =max(race),
             min_ed = min(education),
             max_ed = max(education)) %>% 
      filter(min_race==max_race, 
             min_sex==max_sex,
             max_age-min_age<=2,
             (max_ed - min_ed)<=2,
             (max_ed - min_ed)>=0) %>% # removes obviously erroneous matches
      ungroup() %>% 
      select_at(vars(!starts_with("min"), !starts_with("max"))) %>% 
      distinct() # removes any duplicates
  }
  
  else {
    cps_obs %>%
      filter(house_id %in% household$house_id) %>%
      group_by(house_id,month,year) %>%
      arrange(house_id, month, year, sample_month, sex, race, age, education, class) %>% # Add consistency in rownumbers
      mutate(unique_id = row_number(),
             unique_id = paste(house_id,df_i[1,1],df_i[1,2],unique_id, sep="_")) %>% # Create unique id
      ungroup() %>% 
      group_by(unique_id) %>% 
      mutate(min_age = min(age),
             max_age = max(age),
             min_sex = min(sex),
             max_sex = max(sex),
             min_race = min(race),
             max_race =max(race),
             min_ed = min(education),
             max_ed = max(education)) %>% 
      filter(min_race==max_race, 
             min_sex==max_sex,
             max_age-min_age<=2,
             (max_ed - min_ed)<=2,
             (max_ed - min_ed)>=0) %>% # removes obviously erroneous matches
      ungroup() %>% 
      select_at(vars(!starts_with("min"), !starts_with("max"))) %>% 
      distinct() # removes any duplicates
    
  }
}


##----------------------------------------------------------------
##                      Observation Groups                       -
##----------------------------------------------------------------
    
## We have four sets of observation groups:
  # 1) Normal Set (up to 8 observations): MIS = 1 between 2004-05 and 2005-09
  # 2) Half Set (up to 4 observations): MIS = 1 between 2006-01 and 2006-09
  # 3) Truncated-at-end Set
  #   a. MIS = 1 at 2005-10 (up to 7 obs)
  #   b. MIS = 1 at 2005-11 (up to 6 obs)
  #   c. MIS = 1 at 2005-12 (up to 5 obs)
  # 4) Truncated-at-start Set
  #   a. MIS = 2 at 2004-05 (up to 7 obs)
  #   b. MIS = 3 at 2004-05 (up to 6 obs)
  #   c. MIS = 4 at 2004-05 (up to 5 obs)
  #   d. MIS = 5 at 2004-05 (up to 4 obs)

##--------------
##  Normal Set  
##--------------
    
## Create set of date patterns for the normal set
    
months_set<-tibble(months_1=c(5,6,7,8),year_1=c(2004),mis_1=c(1:4),
                   months_2=c(5,6,7,8),year_2=c(2005),mis_2=c(5:8))
  
months_set_list<-list()

months_set_list[[1]] <- months_set

for (i in 2:17){
  months_set_list[[i]]  <-  months_set_list[[i-1]] %>% 
    mutate(year_1 = case_when(months_1<12 ~ year_1,months_1==12 ~ year_1+1),
           months_1 = case_when(months_1<12 ~ months_1+1,months_1==12 ~ 1),
           year_2 = case_when(months_2<12 ~ year_2,months_2==12 ~ year_2+1),
           months_2 = case_when(months_2<12 ~ months_2+1,months_2==12 ~ 1))
}

months_set_list

## Match observations in the normal set

# Looping over the list of data patterns and applying matching function

match_obs_list_ns <- list()


for (i in 1:17) {
  # Reassignment for clearer reading
  
  df_i <- months_set_list[[i]]

  # Filter data with no missing values (>0), over 16 years old, with correct date pattern

  cps_obs_group <- cps_data %>%
    filter(
      (month == df_i[[1, 1]] & year == df_i[[1, 2]] & sample_month == df_i[[1, 3]]) |
        (month == df_i[[2, 1]] & year == df_i[[2, 2]] & sample_month == df_i[[2, 3]]) |
        (month == df_i[[3, 1]] & year == df_i[[3, 2]] & sample_month == df_i[[3, 3]]) |
        (month == df_i[[4, 1]] & year == df_i[[4, 2]] & sample_month == df_i[[4, 3]]) |
        (month == df_i[[1, 4]] & year == df_i[[1, 5]] & sample_month == df_i[[1, 6]]) |
        (month == df_i[[2, 4]] & year == df_i[[2, 5]] & sample_month == df_i[[2, 6]]) |
        (month == df_i[[3, 4]] & year == df_i[[3, 5]] & sample_month == df_i[[3, 6]]) |
        (month == df_i[[4, 4]] & year == df_i[[4, 5]] & sample_month == df_i[[4, 6]])
    ) %>%
    filter(class > 0, education > 0, class > 0, sex > 0, age >= 16)

  # Create vector of household ids with no changes in hhold size & at least four months of observations
  household_list <- household_cps()

  # Match individuals

  match_obs_list_ns[[i]] <- match_cps(household = household_list)
}

# Naive Match rate (w/o lines 247-262): 627,552
# Remove erroneous matches: 550,921 observations - 87.8% match rate

normal_set_matches <- match_obs_list_ns %>% bind_rows()

##------------
##  Half Set  
##------------

## Create set of date patterns for the normal set

months_halfset<-tibble(months=c(1,2,3,4),year=c(2006),mis=c(1:4))

months_halfset_list<-list()

months_halfset_list[[1]] <- months_halfset

for (i in 2:9){
  months_halfset_list[[i]]  <-  months_halfset_list[[i-1]] %>% 
    mutate(months = months+1)
}

## Match observations in the half set

# Empty list

match_obs_list_hs <- list()

# Loop 

for (i in 1:9){
  
  df_i <- months_halfset_list[[i]]
  
  # Filter data with no missing values (>0), over 16 years old, with correct date pattern
  
  cps_obs_group <- cps_data %>%
    filter(
      (month == df_i[[1, 1]] & year == df_i[[1, 2]] & sample_month == df_i[[1, 3]]) |
        (month == df_i[[2, 1]] & year == df_i[[2, 2]] & sample_month == df_i[[2, 3]]) |
        (month == df_i[[3, 1]] & year == df_i[[3, 2]] & sample_month == df_i[[3, 3]]) |
        (month == df_i[[4, 1]] & year == df_i[[4, 2]] & sample_month == df_i[[4, 3]])
    ) %>% 
    filter(class>0, education>0, class>0, sex>0, age>=16)
  
  # Create vector of household ids with no changes in hhold size & at least four months of observations
  
 household_list <- household_cps()
  
  # Match each group 
  
  match_obs_list_hs[[i]] <- match_cps(household=household_list)
  
}

half_set_matches<-match_obs_list_hs %>% bind_rows()

# Naive matches: 206,842
# Remove erroneous matches: 199,094, match rate: 96.3%


##------------------------
##  Truncated-at-end Set  
##------------------------

## Create set of date patterns for the truncated-at-end set

months_truncend_set<-tibble(months_1=c(10,11,12,1),year_1=c(2005,2005,2005,2006),mis_1=c(1:4),
                   months_2=c(10,11,12,NA),year_2=c(2006,2006,2006,NA),mis_2=c(5:7,NA))

months_truncend_set_list<-list()

months_truncend_set_list[[1]] <- months_truncend_set

for (i in 2:3){
  months_truncend_set_list[[i]]  <-  months_truncend_set_list[[i-1]] %>% 
    mutate(year_1 = case_when(months_1<12 ~ year_1,months_1==12 ~ year_1+1),
           months_1 = case_when(months_1<12 ~ months_1+1,months_1==12 ~ 1),
           year_2 = case_when(months_2<12 ~ year_2,months_2==12 ~ NA_real_),
           months_2 = case_when(months_2<12 ~ months_2+1,months_2==12 ~ NA_real_))
}

## Match observations in the truncated-at-end set

# Empty list

match_obs_list_tre <- list()

# Loop 

for (i in 1:3){
  
  df_i <- months_truncend_set_list[[i]]
  
  # Filter data with no missing values (>0), over 16 years old, with correct date pattern
  
  cps_obs_group <- cps_data %>%
    filter(
      (month == df_i[[1, 1]] & year == df_i[[1, 2]] & sample_month == df_i[[1, 3]]) |
        (month == df_i[[2, 1]] & year == df_i[[2, 2]] & sample_month == df_i[[2, 3]]) |
        (month == df_i[[3, 1]] & year == df_i[[3, 2]] & sample_month == df_i[[3, 3]]) |
        (month == df_i[[4, 1]] & year == df_i[[4, 2]] & sample_month == df_i[[4, 3]]) |
        (month == df_i[[1, 4]] & year == df_i[[1, 5]] & sample_month == df_i[[1, 6]]) |
        (month == df_i[[2, 4]] & year == df_i[[2, 5]] & sample_month == df_i[[2, 6]]) |
        (month == df_i[[3, 4]] & year == df_i[[3, 5]] & sample_month == df_i[[3, 6]]) |
        (month == df_i[[4, 4]] & year == df_i[[4, 5]] & sample_month == df_i[[4, 6]])
    ) %>% 
    filter(class>0, education>0, class>0, sex>0, age>=16)
  
  # Create vector of household ids with no changes in hhold size/
  # at least four months of observations
  
  # Create vector of household ids with no changes in hhold size & at least four months of observations
  household_list <- household_cps()
  
  # Match individuals
  
  match_obs_list_tre[[i]] <- match_cps(household = household_list)
}

truncend_set_matches <- match_obs_list_tre %>% bind_rows()

# Naive matches: 85,378
# Remove erroneous matches: 76,383,  match rate: 89.5%

##--------------------------
##  Truncated-at-start Set  
##--------------------------

## Create set of date patterns for the truncated-at-end set

# It was faster to do this by hand than write a filter for each combination

months_truncstart_set_a<-tibble(months_1=c(NA,5:7),year_1=c(NA,2004,2004,2004),mis_1=c(NA,2:4),
                            months_2=c(4:7),year_2=c(2005),mis_2=c(5:8))

months_truncstart_set_b<-tibble(months_1=c(NA,5,6,NA),year_1=c(NA,2004,2004,NA),mis_1=c(NA,3:4,NA),
                                months_2=c(3:6),year_2=c(2005),mis_2=c(5:8))

months_truncstart_set_c<-tibble(months_1=c(NA,5,NA,NA),year_1=c(NA,2004,NA,NA),mis_1=c(NA,4,NA,NA),
                                months_2=c(2:5),year_2=c(2005),mis_2=c(5:8))

months_truncstart_set_d<-tibble(months_1=c(5:8),year_1=c(2004),mis_1=c(5:8),
                                months_2=c(NA),year_2=c(NA),mis_2=c(NA))

months_truncstart_set_list <- list()

months_truncstart_set_list[[1]]<-months_truncstart_set_a
months_truncstart_set_list[[2]]<-months_truncstart_set_b
months_truncstart_set_list[[3]]<-months_truncstart_set_c
months_truncstart_set_list[[4]]<-months_truncstart_set_d

## Match observations in the truncated-at-start set

# Empty list

match_obs_list_trs <- list()

# Loop 

for (i in 1:4){
  
  df_i <- months_truncstart_set_list[[i]]
  
  # Filter data with no missing values (>0), over 16 years old, with correct date pattern
  
  cps_obs_group <- cps_data %>%
    filter(
      (month == df_i[[1, 1]] & year == df_i[[1, 2]] & sample_month == df_i[[1, 3]]) |
        (month == df_i[[2, 1]] & year == df_i[[2, 2]] & sample_month == df_i[[2, 3]]) |
        (month == df_i[[3, 1]] & year == df_i[[3, 2]] & sample_month == df_i[[3, 3]]) |
        (month == df_i[[4, 1]] & year == df_i[[4, 2]] & sample_month == df_i[[4, 3]]) |
        (month == df_i[[1, 4]] & year == df_i[[1, 5]] & sample_month == df_i[[1, 6]]) |
        (month == df_i[[2, 4]] & year == df_i[[2, 5]] & sample_month == df_i[[2, 6]]) |
        (month == df_i[[3, 4]] & year == df_i[[3, 5]] & sample_month == df_i[[3, 6]]) |
        (month == df_i[[4, 4]] & year == df_i[[4, 5]] & sample_month == df_i[[4, 6]])
    ) %>% 
    filter(class>0, education>0, class>0, sex>0, age>=16)
  
  # Create vector of household ids with no changes in hhold size/
  
  household_list <- household_cps()
  
  # For consistency in unique id names
  
  month_i<-df_i$months_1 %>% 
    coalesce() %>% 
    na.omit() %>% 
    first()
  
  year_i <- df_i$year_1 %>% 
    coalesce() %>% 
    na.omit() %>% 
    first()
  
  
  
  # Match each group 
  
  match_obs_list_trs[[i]] <- match_cps(household=household_list, na_index=T)
}

truncstart_set_matches <- match_obs_list_trs %>% bind_rows()

# Naive matches: 108,180
# Remove erroneous matches: 99,010,  match rate: 95.5%

##---------------------------------------------------------------
##                      Join Observations                       -
##---------------------------------------------------------------

cps_matched <- rbind(truncend_set_matches,truncstart_set_matches,normal_set_matches,half_set_matches)

saveRDS(cps_matched, file="medicare_study/output/cps_matched/cps_matched.RDS")

  

