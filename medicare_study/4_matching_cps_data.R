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

# match_cps() matches individuals over time and gives them a unique id. It can do this
# 'naively', or by default filter out any obvious matching errors. 

match_cps<-function(cps_obs = cps_obs_group, naive = F, na_index=F){
  cps_obs %>%
    group_by(house_id,month,year) %>%
    arrange(house_id, month, year, sample_month, sex, race, age, education, class) %>% 
    {if(naive==T){ mutate(.,unique_id = paste(house_id,line,num_res, sep="_")) %>% ungroup()} else if(naive==F)
    {if(na_index==T){mutate(.,unique_id = row_number(),
                            unique_id = paste(house_id,line,month_i[1],year_i[1],unique_id, sep="_"))}
      else {mutate(.,unique_id = row_number(),
                   unique_id = paste(house_id,line,df_i[1,1],df_i[1,2],unique_id, sep="_"))}} %>% 
        ungroup(.,) %>% 
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
        select_at(vars(-starts_with("min"), -starts_with("max")))} %>% 
    distinct()
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

# Looping over the list of data patterns and applying matching function to naive and filtered

match_obs_list_ns <- list()
match_obs_list_ns_n <- list()

# Filtered

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

  # Match individuals

  match_obs_list_ns[[i]] <- match_cps()
}

# Naive

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
  
  # Match individuals
  
  match_obs_list_ns_n[[i]] <- match_cps(naive = T)
}

normal_set_matches <- match_obs_list_ns %>% bind_rows()
normal_set_matches_n <- match_obs_list_ns_n %>% bind_rows()

# Naive matches: 1,204,366
# Filtered matches: 915,745 - 24% removed


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
match_obs_list_hs_n <- list()

# Loop 

# Filtered

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
  
  # Match each group 
  
  match_obs_list_hs[[i]] <- match_cps()
  
}

# Naive

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
  
  # Match each group 
  
  match_obs_list_hs_n[[i]] <- match_cps(naive=T)
  
}


half_set_matches<-match_obs_list_hs %>% bind_rows()
half_set_matches_n<-match_obs_list_hs_n %>% bind_rows()

# Naive matches: 319,830
# Remove erroneous matches: 289,474, ~ 10% removed



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
match_obs_list_tre_n <- list()

# Filtered

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
  
  # Match individuals
  
  match_obs_list_tre[[i]] <- match_cps()
}

# Naive

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
  
  # Match individuals
  
  match_obs_list_tre_n[[i]] <- match_cps(naive=T)
}


truncend_set_matches <- match_obs_list_tre %>% bind_rows()
truncend_set_matches_n <- match_obs_list_tre_n %>% bind_rows()

# Naive matches: 157,067
# Remove erroneous matches: 124,424 ~30% removed


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
match_obs_list_trs_n <- list()

# Filtered

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
  
  match_obs_list_trs[[i]] <- match_cps(na_index=T)
}

# Naive

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
  
  match_obs_list_trs_n[[i]] <- match_cps(na_index=T, naive=T)
}

truncstart_set_matches <- match_obs_list_trs %>% bind_rows()
truncstart_set_matches_n <- match_obs_list_trs_n %>% bind_rows()

match_cps(naive=T, na_index = T)

# Naive matches: 194,621
# Remove erroneous matches: 160,055,  ~ 18% removed



##---------------------------------------------------------------
##                      Join Observations                       -
##---------------------------------------------------------------

# Join observations

cps_matched <- rbind(truncend_set_matches,truncstart_set_matches,normal_set_matches,half_set_matches)

cps_matched_naive <- rbind(truncend_set_matches_n,truncstart_set_matches_n,normal_set_matches_n,half_set_matches_n)

# Save data file

saveRDS(cps_matched, file="medicare_study/output/cps_matched/cps_matched.RDS")

saveRDS(cps_matched_naive, file="medicare_study/output/cps_matched/cps_matched_naive.RDS")
