#################################################################
##                        5. Estimation                        ##
#################################################################

##---------------------------------------------------------------
##                        Preliminaries                         -
##---------------------------------------------------------------

# Libraries

library(tidyverse)
library(broom)
library(xtable)
library(lubridate)
library(lmtest)
library(sandwich)
library(car)

# Load Data

cps_matched <- readRDS("medicare_study/output/cps_matched/cps_matched.RDS")
cps_matched_naive <- readRDS("medicare_study/output/cps_matched/cps_matched_naive.RDS")

##---------------------------------------------------------------
##                          Functions                           -
##---------------------------------------------------------------

##------------------
##  check_age_turn  
##------------------

# Used to check vectors of ages of individuals in turn_matches()

check_age_turn<-function(vec, turn){
  (turn-1) %in% vec & turn %in% vec
}

##------------------
##  turn_matches()  
##------------------

# Selects only unique ids where individuals are age 64 and 65 in a single 4 period window

turn_matches<-function(matches, turnage=65){
  matches %>%
    group_by(unique_id) %>%
    mutate(
      check_turn = age == turnage,
      check_turn = sum(check_turn)
    ) %>%
    filter(check_turn > 0) %>%
    ungroup() %>%
    mutate(sample_count = case_when(
      sample_month %in% c(1:4) ~ 1,
      sample_month %in% c(5:8) ~ 2
    )) %>% # Separate 4-month windows
    group_by(unique_id, sample_count) %>%
    select(unique_id, sample_count, age) %>%
    chop(c(age)) %>% # All ages as a vector
    mutate(age_check = map2_lgl(age, turnage, check_age_turn)) %>% # Check that both 64 and 65 appear in a window
    filter(age_check == TRUE) %>%
    ungroup() %>%
    select(unique_id) %>%
    distinct() # Removes any duplicates
}


##--------------------
##  encode_matches()  
##--------------------

# Filters for employed men 
# Creates a dummy variable (turn_point) with three values: After turning 65, turning 65, and before turning 65
# Creates dummy variable for self employment

encode_matches<-function(matches, turnlist, turnage=65){
  matches %>% 
    filter(employed %in% c(1:2)) %>%  # Remove unemployed
    filter(sex==1) %>% # Only men
    mutate(sample_count = case_when(sample_month %in% c(1:4) ~ 1,
                                    sample_month %in% c(5:8) ~ 2)) %>% # Only looks in four month windwos
    filter(unique_id %in% turnlist$unique_id) %>% # Selects only individuals who go from 64 to 65
    group_by(unique_id, sample_count) %>% 
    mutate(turn = case_when(age==turnage & dplyr::lag(age,1,order_by=sample_month)==(turnage-1) ~ T, T~F)) %>% 
    mutate(turn_point = case_when(turn == F & (dplyr::lead(turn,1,order_by=sample_month)==T | dplyr::lead(turn,2,order_by=sample_month)==T) ~ "before",
                                  (turn == F & (dplyr::lag(turn,1,order_by=sample_month)==T | dplyr::lag(turn,2,order_by=sample_month)==T)) ~ "after",
                                  turn==T ~ "turn",
                                  T ~ NA_character_)) %>% # Dummy variable for turning 65
    ungroup() %>% 
    mutate(self_employed = class %in% c(5,6)) # Self employed dummy variable
}



##----------------------------------------------------------------
##                          Estimating                           -
##----------------------------------------------------------------


##-----------------
##  Filtered Data  
##-----------------

cps_turn65_df<-turn_matches(cps_matched, turnage=65) %>% 
  encode_matches(turnlist=., matches=cps_matched, turnage=65) %>% 
  mutate(date = lubridate::ymd(paste(year,month,"01",sep="-")))

### Main regression

reg_allc<-  lm(self_employed~relevel(factor(turn_point), ref=2) + factor(state)+
                 factor(marital)+ factor(citizen)+factor(education)+factor(date), data=cps_turn65_df)

reg_allc %>% tidy()
### Robust Standard Errors

allc_robust<-coeftest(reg_allc, vcov = vcovHC(reg_allc, type = "HC0"))

## Note: result is actually *more* significant w/ robust standard errors
# Classical SEs too large because values further from the mean hold more information/have lower variance

### Multicollinearity

vif(reg_allc) # Categorical adjusted GVIF values suggest no multicollinearity

# Result is not very robust to dropping covariates
# e.g. replacing date with month and year as separate variables, or replacing state with census region

##--------------
##  Naive Data  
##--------------

cps_turn65_df_n<-turn_matches(cps_matched_naive, turnage=65) %>% 
  encode_matches(turnlist=., matches=cps_matched_naive, turnage=65) %>% 
  mutate(date = lubridate::ymd(paste(year,month,"01",sep="-")))

### Main regression

reg_allc_n<-lm(self_employed~relevel(factor(turn_point), ref=2) + factor(state) +
               factor(marital) + factor(citizen)+factor(education)+factor(date), data=cps_turn65_df_n)

reg_simple_n<-lm(self_employed~relevel(factor(turn_point), ref=2), data=cps_turn65_df_n)

reg_allc_n %>% tidy()
coeftest(reg_allc_n, vcov = vcovHC(reg_allc_n, type = "HC0")) %>% tidy()

##-----------------------
##  Presentation tables  
##-----------------------

# All the results from the different studies

all_results<- readRDS("medicare_study/figs/all_results.RDS")

all_results %>% 
  arrange(model) %>% 
  ggplot(aes(model,est))+
  geom_point()+
  geom_segment(aes(yend=lower, y=upper, xend=model, x=model))+
  geom_hline(aes(yintercept=0),linetype="dashed")

##----------------------------------------------------------------
##                    Is turning 65 different?                   -
##----------------------------------------------------------------

# Two other ways of looking at self employment and medicare eligibility in more context

# Does the relationship between age and self employment break down after 65?

cps_matched %>% 
  filter(employed %in% c(1,2)) %>% 
  group_by(age,year) %>% 
  count(class) %>% 
  mutate(total = sum(n)) %>% 
  mutate(share = n/total) %>% 
  filter(class %in% c(5,6)) %>% 
  mutate(share = sum(share)) %>% 
  select(-class,total,-n) %>%
  distinct() %>% 
  mutate(medicare = age>=65) %>% 
  ggplot(aes(age, share))+
  geom_point(aes(colour=medicare), alpha=0.5)+
  geom_vline(aes(xintercept=65))+
  geom_smooth(method="lm",aes(colour=medicare), show.legend = F)+
  scale_colour_manual(labels=c("Ineligible","Eligible"), name="Medicare\nEligibility", values=c("red","blue"))+
  labs(x="Age", "Share in Self Employment", title="The relationship between self employment and age")


##-----------------------------------------------
##  Comparing regression results for other ages  
##-----------------------------------------------

# Is the change after turning 65 different from the change after other ages?

turn_function_list <-list()

all_ages <- (25:70)

for (i in seq_along(all_ages)){
  
  turn_match_vector<-turn_matches(cps_matched, turnage=(all_ages)[i])
  
  code_match_tibble <-encode_matches(matches=cps_matched, turnlist=turn_match_vector, turnage=(all_ages)[i])
  
  turn_function_list[[i]]<-code_match_tibble %>% 
    mutate(date = ymd(paste(year,month,"01",sep="-"))) %>% 
    lm(self_employed~relevel(factor(turn_point), ref=2) +
         factor(state) + factor(marital) + factor(citizen) + factor(date), data=.) %>%
    tidy() %>% 
    mutate(age = (all_ages)[i],
           individuals = nrow(turn_match_vector),
           obs = nrow(code_match_tibble)) %>% 
    slice(2) # Only take turning point estimate
}

turn_function_list %>% bind_rows() %>%  
  filter(age<71) %>% 
  mutate(upperci = estimate + 1.96*std.error,
         lowerci = estimate - 1.96*std.error) %>% 
  mutate(main=age==65) %>% 
  mutate(sig = as.factor(case_when(lowerci > 0 ~ "yes", T~"no")))%>% 
  ggplot(aes(age, estimate))+
  geom_segment(aes(xend = age,x=age,y=upperci,yend=lowerci))+
  geom_point(aes(colour=factor(sig)),show.legend=F)+
  geom_hline(aes(yintercept=0))+
  labs(x="Age", y="Effect size")+
  geom_hline(aes(yintercept=0.0702, colour=sig), linetype="dashed", show.legend=F)+
  theme_minimal()

#####