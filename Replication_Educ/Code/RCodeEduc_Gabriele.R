setwd(######################)
library(ipumsr)
library(data.table)
library(ggplot2)
library(jtools)
library(dplyr)
library(dotwhisker)
library(broom)

ddi_3 <- read_ipums_ddi("usa_00003.xml")
df <- read_ipums_micro(ddi_3)
View(df)

########################################################################
#CLEANING

df<-df[!(df$SEX==1),] #Keeping only females
df<-df[!(df$ELDCH==99),] #Keeping only variables with info on child in household
df$AGEFC <- df$AGE-df$ELDCH #Variables for age of first child
df<-df[!(df$AGEFC<15),] #Dropping extreme low age of first child
df<-df[!(df$AGEFC>60),] #Dropping extreme high age of first child


########################################################################
#INPUT_OF_COMPULSORY_AGE_FROM_ARTICLE

df$COMPULSORY <- 0
setDT(df)[df$STATEICP==41, COMPULSORY:= 16]
setDT(df)[df$STATEICP==61, COMPULSORY:= 16]
setDT(df)[df$STATEICP==42, COMPULSORY:= 16]
setDT(df)[df$STATEICP==71, COMPULSORY:= 16]
setDT(df)[df$STATEICP==62, COMPULSORY:= 16]
setDT(df)[df$STATEICP==01, COMPULSORY:= 16]
setDT(df)[df$STATEICP==11, COMPULSORY:= 16]
setDT(df)[df$STATEICP==98, COMPULSORY:= 16]
setDT(df)[df$STATEICP==43, COMPULSORY:= 16]
setDT(df)[df$STATEICP==44 & df$YEAR<1960, COMPULSORY:= 14]
setDT(df)[df$STATEICP==44 & df$YEAR>1950, COMPULSORY:= 16]
setDT(df)[df$STATEICP==63 & df$YEAR>1944, COMPULSORY:= 16]
setDT(df)[df$STATEICP==63 & df$YEAR<1944, COMPULSORY:= 18]
setDT(df)[df$STATEICP==21, COMPULSORY:= 16]
setDT(df)[df$STATEICP==22, COMPULSORY:= 16]
setDT(df)[df$STATEICP==31, COMPULSORY:= 16]
setDT(df)[df$STATEICP==32, COMPULSORY:= 16]
setDT(df)[df$STATEICP==51, COMPULSORY:= 16]
setDT(df)[df$STATEICP==45 & df$YEAR>1954, COMPULSORY:= 16]
setDT(df)[df$STATEICP==45 & df$YEAR<1954, COMPULSORY:= 14]
setDT(df)[df$STATEICP==02 & df$YEAR>1954, COMPULSORY:= 16]
setDT(df)[df$STATEICP==45 & df$YEAR==1950, COMPULSORY:= 14]
setDT(df)[df$STATEICP==45 & df$YEAR<1950, COMPULSORY:= 17]
setDT(df)[df$STATEICP==52, COMPULSORY:= 16]
setDT(df)[df$STATEICP==03, COMPULSORY:= 16]
setDT(df)[df$STATEICP==23, COMPULSORY:= 16]
setDT(df)[df$STATEICP==33, COMPULSORY:= 16]
setDT(df)[df$STATEICP==46 & df$YEAR==1940, COMPULSORY:= 17]
setDT(df)[df$STATEICP==46 & df$YEAR>1940, COMPULSORY:= 16]
setDT(df)[df$STATEICP==46 & df$YEAR>1960, COMPULSORY:= 16]
setDT(df)[df$STATEICP==34, COMPULSORY:= 16]
setDT(df)[df$STATEICP==34 & df$YEAR==1950, COMPULSORY:= 14]
setDT(df)[df$STATEICP==64, COMPULSORY:= 16]
setDT(df)[df$STATEICP==35, COMPULSORY:= 16]
setDT(df)[df$STATEICP==65, COMPULSORY:= 18]
setDT(df)[df$STATEICP==65 & df$YEAR>1960, COMPULSORY:= 17]
setDT(df)[df$STATEICP==04, COMPULSORY:= 16]
setDT(df)[df$STATEICP==12, COMPULSORY:= 16]
setDT(df)[df$STATEICP==66, COMPULSORY:= 16]
setDT(df)[df$STATEICP==66 & df$YEAR>1950, COMPULSORY:= 17]
setDT(df)[df$STATEICP==13, COMPULSORY:= 16]
setDT(df)[df$STATEICP==47, COMPULSORY:= 14]
setDT(df)[df$STATEICP==47 & df$YEAR>1950, COMPULSORY:= 16]
setDT(df)[df$STATEICP==36, COMPULSORY:= 17]
setDT(df)[df$STATEICP==36 & df$YEAR>1960, COMPULSORY:= 16]
setDT(df)[df$STATEICP==24, COMPULSORY:= 18]
setDT(df)[df$STATEICP==53, COMPULSORY:= 18]
setDT(df)[df$STATEICP==72, COMPULSORY:= 18]
setDT(df)[df$STATEICP==72 & df$YEAR==1950, COMPULSORY:= 18]
setDT(df)[df$STATEICP==14, COMPULSORY:= 17]
setDT(df)[df$STATEICP==14 & df$YEAR==1940, COMPULSORY:= 16]
setDT(df)[df$STATEICP==14 & df$YEAR==1950, COMPULSORY:= 18]
setDT(df)[df$STATEICP==05, COMPULSORY:= 16]
setDT(df)[df$STATEICP==48, COMPULSORY:= 16]
setDT(df)[df$STATEICP==48 & df$YEAR==1940, COMPULSORY:= 14]
setDT(df)[df$STATEICP==37, COMPULSORY:= 17]
setDT(df)[df$STATEICP==37 & df$YEAR>1960, COMPULSORY:= 16]
setDT(df)[df$STATEICP==54, COMPULSORY:= 16]
setDT(df)[df$STATEICP==54 & df$YEAR==1980, COMPULSORY:= 17]
setDT(df)[df$STATEICP==49 & df$YEAR == 1940, COMPULSORY:= 14]
setDT(df)[df$STATEICP==49 & df$YEAR == 1940, COMPULSORY:= 17]
setDT(df)[df$STATEICP==67, COMPULSORY:= 18]
setDT(df)[df$STATEICP==06, COMPULSORY:= 16]
setDT(df)[df$STATEICP==40, COMPULSORY:= 15]
setDT(df)[df$STATEICP==40 & df$YEAR > 1950, COMPULSORY:= 16]
setDT(df)[df$STATEICP==40 & df$YEAR > 1970, COMPULSORY:= 17]
setDT(df)[df$STATEICP==73, COMPULSORY:= 16]
setDT(df)[df$STATEICP==56, COMPULSORY:= 16]
setDT(df)[df$STATEICP==25, COMPULSORY:= 16]
setDT(df)[df$STATEICP==25 & df$YEAR==1980, COMPULSORY:= 18]
setDT(df)[df$STATEICP==68, COMPULSORY:= 17]
setDT(df)[df$STATEICP==68 & df$YEAR==1960, COMPULSORY:= 16]

df<-df[!(df$COMPULSORY==0),] #Dropping individuals with no observation for state compulsory age

########################################################################
#CREATING OTHER VARIABLES NEEDED FOR ANALYSIS

df$D_COMP <- 0 #Creating a dummy for compulsory schooling>16
setDT(df)[df$COMPULSORY>15, D_COMP:= 1]

df$D_AGEFC <- 0 #Creating a dummy for first age before 18 y.o.
setDT(df)[df$AGEFC<19, D_AGEFC:= 1]

df$D_FARM = df$FARM-1 #Creating dummy=1 for living in farm area

df$D_HISP = 0 #Creating dummy=1 for hispanic
setDT(df)[df$HISPAN>0 & df$HISPAN<9, D_HISP:= 1]

df$D_BLACK = 0 #Creating dummy=1 for black
setDT(df)[df$RACE==2, D_BLACK:= 1]

df$D_NATIVE = 0 #Creating dummy=1 for native american
setDT(df)[df$RACE==3, D_NATIVE:= 1]

df$D_ASIAN = 0 #Creating dummy=1 for asian
setDT(df)[df$RACE>3 & df$RACE<8, D_ASIAN:= 1]

df$AGESQ = (df$AGE)^2 #Creating variable for marginal returns to age

########################################################################
#ANALYSIS

m1 <- lm(df$D_AGEFC ~ df$D_COMP) #1st model: LPM, only age as control
summary(m1)

m2 <- lm(df$D_AGEFC ~ df$D_COMP+df$D_FARM+df$POVERTY+df$D_ASIAN+df$D_BLACK+df$D_NATIVE+df$D_HISP+df$AGE) #2nd model: LPM, with controls
summary(m2)

m3 <- lm(df$D_AGEFC ~ df$D_COMP + df$AGE) #3rd model: LPM, only age as control
summary(m3)

m4 <- lm(df$D_AGEFC ~ df$D_COMP + df$AGE + df$POVERTY) #4rth model: LPM, only age and poverty as control
summary(m4)

m5 <- lm(df$D_AGEFC ~ df$D_COMP+df$D_FARM+df$D_ASIAN+df$D_BLACK+df$D_NATIVE+df$D_HISP+df$AGE) #5th model: LPM, with all controls but poverty
summary(m5)

