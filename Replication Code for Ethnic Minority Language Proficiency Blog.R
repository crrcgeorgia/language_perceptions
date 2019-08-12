###Georgian Language Proficiency and Government Perceptions

library(haven)
library(stringr)
library(tidyverse)
library(ggeffects)
library(survey)
library(MASS)

download.file("https://caucasusbarometer.org/downloads/NDI_2019_April_22.04.19_Public.dta", "NDI_2019_April_22.04.19_Public.dta", quiet = FALSE, mode = "wb")

NDI19 <- read_dta("NDI_2019_April_22.04.19_Public.dta")
names(NDI19)

NDI19svy_A8 <- svydesign(id=~PSU, strata=~SUBSTRATUM, 
                         weights=~WTIND,  
                         data=NDI19)

###Perceptions of government for the respondent
##RATEGOV4
#Sum
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$RATEGOV4)
table(NDI19svy_A8$variables$RATEGOV4)
#Cleaning
NDI19svy_A8$variables$RATEGOV4<-NDI19svy_A8$variables$RATEGOV4
NDI19svy_A8$variables$RATEGOV4[NDI19svy_A8$variables$RATEGOV4==-3]<-NA
NDI19svy_A8$variables$RATEGOV4[NDI19svy_A8$variables$RATEGOV4==-2]<-NA
NDI19svy_A8$variables$RATEGOV4[NDI19svy_A8$variables$RATEGOV4==-1]<-NA
table(NDI19svy_A8$variables$RATEGOV4)

###Characteristics of the respondent
##KNOWGEO
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$KNOWGEO)
table(NDI19svy_A8$variables$KNOWGEO)
#cleaning
NDI19svy_A8$variables$KNOWGEO<-NDI19svy_A8$variables$KNOWGEO
NDI19svy_A8$variables$KNOWGEO[NDI19svy_A8$variables$KNOWGEO==-7]<-4
NDI19svy_A8$variables$KNOWGEO[NDI19svy_A8$variables$KNOWGEO==-2]<-NA
NDI19svy_A8$variables$KNOWGEO[NDI19svy_A8$variables$KNOWGEO==-1]<-NA
table(NDI19svy_A8$variables$KNOWGEO)
#Geo knowledge recoding.

#AGEGROUP
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$AGEGROUP)
table(NDI19svy_A8$variables$AGEGROUP)


###Maybe don't recode to start with 0?
#Cleaning
NDI19svy_A8$variables$AGEGROUP<-NDI19svy_A8$variables$AGEGROUP
NDI19svy_A8$variables$AGEGROUP[NDI19svy_A8$variables$AGEGROUP==1]<-0
NDI19svy_A8$variables$AGEGROUP[NDI19svy_A8$variables$AGEGROUP==2]<-1
NDI19svy_A8$variables$AGEGROUP[NDI19svy_A8$variables$AGEGROUP==3]<-2
str(NDI19svy_A8$variables$AGEGROUP)


#####OWN SERIES
##OWNWASH
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNWASH)
table(NDI19svy_A8$variables$OWNWASH)
#cleaning
NDI19svy_A8$variables$OWNWASH<-NDI19svy_A8$variables$OWNWASH
NDI19svy_A8$variables$OWNWASH[NDI19svy_A8$variables$OWNWASH==-3]<-NA
NDI19svy_A8$variables$OWNWASH[NDI19svy_A8$variables$OWNWASH==-2]<-NA
NDI19svy_A8$variables$OWNWASH[NDI19svy_A8$variables$OWNWASH==-1]<-NA
table(NDI19svy_A8$variables$OWNWASH)
#OWNFRDG
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNFRDG)
table(NDI19svy_A8$variables$OWNFRDG)
#cleaning
NDI19svy_A8$variables$OWNFRDG<-NDI19svy_A8$variables$OWNFRDG
NDI19svy_A8$variables$OWNFRDG[NDI19svy_A8$variables$OWNFRDG==-3]<-NA
NDI19svy_A8$variables$OWNFRDG[NDI19svy_A8$variables$OWNFRDG==-2]<-NA
NDI19svy_A8$variables$OWNFRDG[NDI19svy_A8$variables$OWNFRDG==-1]<-NA
table(NDI19svy_A8$variables$OWNFRDG)
#OWNSPHN
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNSPHN)
table(NDI19svy_A8$variables$OWNSPHN)
#cleaning
NDI19svy_A8$variables$OWNSPHN<-NDI19svy_A8$variables$OWNSPHN
NDI19svy_A8$variables$OWNSPHN[NDI19svy_A8$variables$OWNSPHN==-3]<-NA
NDI19svy_A8$variables$OWNSPHN[NDI19svy_A8$variables$OWNSPHN==-2]<-NA
NDI19svy_A8$variables$OWNSPHN[NDI19svy_A8$variables$OWNSPHN==-1]<-NA
table(NDI19svy_A8$variables$OWNSPHN)
#OWNCOTV
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNCOTV)
table(NDI19svy_A8$variables$OWNCOTV)
#cleaning
NDI19svy_A8$variables$OWNCOTV<-NDI19svy_A8$variables$OWNCOTV
NDI19svy_A8$variables$OWNCOTV[NDI19svy_A8$variables$OWNCOTV==-3]<-NA
NDI19svy_A8$variables$OWNCOTV[NDI19svy_A8$variables$OWNCOTV==-2]<-NA
NDI19svy_A8$variables$OWNCOTV[NDI19svy_A8$variables$OWNCOTV==-1]<-NA
table(NDI19svy_A8$variables$OWNCOTV)
#OWNTBLT
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNTBLT)
table(NDI19svy_A8$variables$OWNTBLT)
#cleaning
NDI19svy_A8$variables$OWNTBLT<-NDI19svy_A8$variables$OWNTBLT
NDI19svy_A8$variables$OWNTBLT[NDI19svy_A8$variables$OWNTBLT==-3]<-NA
NDI19svy_A8$variables$OWNTBLT[NDI19svy_A8$variables$OWNTBLT==-2]<-NA
NDI19svy_A8$variables$OWNTBLT[NDI19svy_A8$variables$OWNTBLT==-1]<-NA
table(NDI19svy_A8$variables$OWNTBLT)
#OWNCARS
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNCARS)
table(NDI19svy_A8$variables$OWNCARS)
#cleaning
NDI19svy_A8$variables$OWNCARS<-NDI19svy_A8$variables$OWNCARS
NDI19svy_A8$variables$OWNCARS[NDI19svy_A8$variables$OWNCARS==-3]<-NA
NDI19svy_A8$variables$OWNCARS[NDI19svy_A8$variables$OWNCARS==-2]<-NA
NDI19svy_A8$variables$OWNCARS[NDI19svy_A8$variables$OWNCARS==-1]<-NA
table(NDI19svy_A8$variables$OWNCARS)
#OWNAIRC
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNAIRC)
table(NDI19svy_A8$variables$OWNAIRC)
#cleaning
NDI19svy_A8$variables$OWNAIRC<-NDI19svy_A8$variables$OWNAIRC
NDI19svy_A8$variables$OWNAIRC[NDI19svy_A8$variables$OWNAIRC==-3]<-NA
NDI19svy_A8$variables$OWNAIRC[NDI19svy_A8$variables$OWNAIRC==-2]<-NA
NDI19svy_A8$variables$OWNAIRC[NDI19svy_A8$variables$OWNAIRC==-1]<-NA
table(NDI19svy_A8$variables$OWNAIRC)
#OWNCOMP
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNCOMP)
table(NDI19svy_A8$variables$OWNCOMP)
#cleaning
NDI19svy_A8$variables$OWNCOMP<-NDI19svy_A8$variables$OWNCOMP
NDI19svy_A8$variables$OWNCOMP[NDI19svy_A8$variables$OWNCOMP==-3]<-NA
NDI19svy_A8$variables$OWNCOMP[NDI19svy_A8$variables$OWNCOMP==-2]<-NA
NDI19svy_A8$variables$OWNCOMP[NDI19svy_A8$variables$OWNCOMP==-1]<-NA
table(NDI19svy_A8$variables$OWNCOMP)
#OWNHWT
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNHWT)
table(NDI19svy_A8$variables$OWNHWT)
#cleaning
NDI19svy_A8$variables$OWNHWT<-NDI19svy_A8$variables$OWNHWT
NDI19svy_A8$variables$OWNHWT[NDI19svy_A8$variables$OWNHWT==-3]<-NA
NDI19svy_A8$variables$OWNHWT[NDI19svy_A8$variables$OWNHWT==-2]<-NA
NDI19svy_A8$variables$OWNHWT[NDI19svy_A8$variables$OWNHWT==-1]<-NA
table(NDI19svy_A8$variables$OWNHWT)
#OWNCHTG
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$OWNCHTG)
table(NDI19svy_A8$variables$OWNCHTG)
#cleaning
NDI19svy_A8$variables$OWNCHTG<-NDI19svy_A8$variables$OWNCHTG
NDI19svy_A8$variables$OWNCHTG[NDI19svy_A8$variables$OWNCHTG==-3]<-NA
NDI19svy_A8$variables$OWNCHTG[NDI19svy_A8$variables$OWNCHTG==-2]<-NA
NDI19svy_A8$variables$OWNCHTG[NDI19svy_A8$variables$OWNCHTG==-1]<-NA
table(NDI19svy_A8$variables$OWNCHTG)
###Index
NDI19svy_A8$variables$OWN <- (NDI19svy_A8$variables$OWNWASH+
                                NDI19svy_A8$variables$OWNCHTG+ 
                                NDI19svy_A8$variables$OWNHWT+ 
                                NDI19svy_A8$variables$OWNCOMP+
                                NDI19svy_A8$variables$OWNAIRC+ 
                                NDI19svy_A8$variables$OWNCARS+ 
                                NDI19svy_A8$variables$OWNTBLT+ 
                                NDI19svy_A8$variables$OWNCOTV+ 
                                NDI19svy_A8$variables$OWNSPHN+ 
                                NDI19svy_A8$variables$OWNFRDG)
NDI19svy_A8$variables$OWN<-NDI19svy_A8$variables$OWN
hist(NDI19svy_A8$variables$OWN)

#Ethnicity
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$ETHNIC)
table(NDI19svy_A8$variables$ETHNIC)
NDI19svy_A8$variables$ETHNIC<-NDI19svy_A8$variables$ETHNIC
NDI19svy_A8$variables$ETHNIC[NDI19svy_A8$variables$ETHNIC==-3]<-NA
NDI19svy_A8$variables$ETHNIC[NDI19svy_A8$variables$ETHNIC==-1]<-NA
table(NDI19svy_A8$variables$ETHNIC)
#Recoding for ETHNIC_r
##ETHNIC for just Georgian and minority Areminian and Azeri
NDI19svy_A8$variables$ETHNIC[NDI19svy_A8$variables$ETHNIC>=4]<-NA
NDI19svy_A8$variables$ETHNIC[NDI19svy_A8$variables$ETHNIC==3]<-0
table(NDI19svy_A8$variables$ETHNIC)
NDI19svy_A8$variables$ETHNIC<-as.factor(NDI19svy_A8$variables$ETHNIC)


##RESPEDU
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$RESPEDU)
table(NDI19svy_A8$variables$RESPEDU)
#cleaning
NDI19svy_A8$variables$RESPEDU<-NDI19svy_A8$variables$RESPEDU
NDI19svy_A8$variables$RESPEDU[NDI19svy_A8$variables$RESPEDU==-3]<-NA
NDI19svy_A8$variables$RESPEDU[NDI19svy_A8$variables$RESPEDU==-2]<-NA
NDI19svy_A8$variables$RESPEDU[NDI19svy_A8$variables$RESPEDU==-1]<-NA
table(NDI19svy_A8$variables$RESPEDU)
#Recoding resp edu
NDI19svy_A8$variables$RESPEDU_r<-NDI19svy_A8$variables$RESPEDU
NDI19svy_A8$variables$RESPEDU_r[NDI19svy_A8$variables$RESPEDU_r==2]<-1
NDI19svy_A8$variables$RESPEDU_r[NDI19svy_A8$variables$RESPEDU_r==3]<-1
NDI19svy_A8$variables$RESPEDU_r[NDI19svy_A8$variables$RESPEDU_r==4]<-2
NDI19svy_A8$variables$RESPEDU_r[NDI19svy_A8$variables$RESPEDU_r==5]<-3
NDI19svy_A8$variables$RESPEDU_r[NDI19svy_A8$variables$RESPEDU_r==6]<-3
table(NDI19svy_A8$variables$RESPEDU_r)

##RESPSEX
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$RESPSEX)
table(NDI19svy_A8$variables$RESPSEX)

##HAVEJOB
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$HAVEJOB)
table(NDI19svy_A8$variables$HAVEJOB)
#cleaning
NDI19svy_A8$variables$HAVEJOB<-NDI19svy_A8$variables$HAVEJOB
NDI19svy_A8$variables$HAVEJOB[NDI19svy_A8$variables$HAVEJOB==-3]<-NA
NDI19svy_A8$variables$HAVEJOB[NDI19svy_A8$variables$HAVEJOB==-2]<-NA
NDI19svy_A8$variables$HAVEJOB[NDI19svy_A8$variables$HAVEJOB==-1]<-NA
table(NDI19svy_A8$variables$HAVEJOB)

##HHSIZE
#Sum 
names(NDI19svy_A8)
summary(NDI19svy_A8$variables$HHSIZE)
table(NDI19svy_A8$variables$HHSIZE)
#Recoding HHSIZE
#Explanation - the number of individuals with households larger than eight individuals were small, and therefore blocked into a (8) category.
NDI19svy_A8$variables$HHSIZE[NDI19svy_A8$variables$HHSIZE>=6]<-6
table(NDI19svy_A8$variables$HHSIZE)

#Factors and Numeric
#Numeric
NDI19svy_A8$variables$KNOWGEO_n<-as.numeric(NDI19svy_A8$variables$KNOWGEO )
NDI19svy_A8$variables$OWN_n<-as.numeric(NDI19svy_A8$variables$OWN)
NDI19svy_A8$variables$HHSIZE_n <-as.numeric(NDI19svy_A8$variables$HHSIZE)
#Numeric rating of Gov
NDI19svy_A8$variables$RATEGOV4_n<-as.numeric(NDI19svy_A8$variables$RATEGOV4)
#Factor
NDI19svy_A8$variables$RESPEDU_r_f <-as.factor(NDI19svy_A8$variables$RESPEDU_r )
NDI19svy_A8$variables$AGEGROUP_f<-as.factor(NDI19svy_A8$variables$AGEGROUP )
NDI19svy_A8$variables$RESPSEX_f<-as.factor(NDI19svy_A8$variables$RESPSEX)
NDI19svy_A8$variables$HAVEJOB_f<-as.factor(NDI19svy_A8$variables$HAVEJOB)


### Universe: only ethnic minority population, plus ordered logit

NDI192 <- NDI19svy_A8$variables %>%
  filter(ETHNIC == 1 | ETHNIC == 2)%>%
  mutate(rategov = factor(RATEGOV4_n, levels = 1:4, ordered = T, labels = c("Very badly", "Badly", "Well", "Very well")),
         knowgeo = factor(KNOWGEO, levels = 1:4, ordered = T, labels =c("No Knowledge", "Basic", "Intermediate", "Advance")),
         ethnic = factor(ETHNIC, levels = 1:2, ordered = F, labels=c("Armenian", "Azerbaijani")))    

gov_1 = polr(rategov ~ 
               ethnic*
               knowgeo +
               OWN_n + 
               RESPEDU_r_f + 
               AGEGROUP_f + 
               RESPSEX_f + 
               HHSIZE_n + 
               (SETTYPE) +
               HAVEJOB_f, data=NDI192)
summary(gov_1)

plot(ggpredict(gov_1, terms = c("knowgeo", "ethnic")))
