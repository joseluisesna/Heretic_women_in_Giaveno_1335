################################################################################
## INQUISITION IN GIAVENO (1335)
## (2) Castellario: Gender study (Person-day data exploration)
## R script by Davor Salihovic (Uni of Antwerp) & Jose Luis Estevez (Uni of Helsinki)
## Date: Apr 24th, 2025
################################################################################

# R PACKAGES REQUIRED ----
library(data.table);library(tidyr);library(dplyr) # for data wrangling
library(ggplot2);library(ggpubr);library(ggstatsplot);library(scales) # for visualization
library(igraph) # for network analyses
library(survival) # for event-history analyses 
library(broom) # for extracting tidied survfit data
library(bshazard) # for smoothed hazard probabilities
library(modelsummary)

# DATA LOADING 
rm(list=ls())
load('data/data2.RData')

source('0_Castellario_gender_theme.R')
theme_set(theme_paper)

################################################################################

# SAMPLE SELECTION FOR EHA ----

# All individuals ever reported as heretics and not reported dead
reported <- filter(nodes,denreceived > 0 & dead == 0) # N = 223

# EXIT ----
# Event of interest: summons
# We have the dates of summons (public calls and inferred values)
summons <- keyevents %>% 
  arrange(time) %>% # arranged by date
  filter(type == 'Summons' & !duplicated(source)) %>% # first summons
  select(source,time)
names(summons) <- c('id','summons.date')
# Merge the data on summons with that of all people reported
reported <- left_join(reported,summons,by='id')

# Who are those among the ones reported who experienced the event (summons)?
reported <- mutate(reported, summons = ifelse(!is.na(summons.date),1,0))
# How many?
count(reported,summons) # 83 individuals

# Exit from the sample occurs at summons ...
reported <- reported %>%
  mutate(exit = ifelse(summons == 1,summons.date,
                       max(keyevents$time))) %>% # or the last day of the trial (23rd Feb)
  mutate(exit = as.Date.numeric(exit,origin='1970-01-01')) # turn into date format

# ENTRY ----
# Let's start simply with the first day of the trial as starting point
min(keyevents$time)
reported <- mutate(reported, entry = min(keyevents$time))
# Number of observations:  days elapsed between first day and exit point
reported <- mutate(reported, obs = as.numeric(difftime(exit,entry,unit='days'))+1)

# Long format (person-day format)
max <- as.numeric(max(keyevents$time) - min(keyevents$time)+1) # max number of cuts

datalong <- survSplit(data=select(reported,id,label,sex,entry,obs,summons),
                      cut=1:max,
                      start='outset',end='obs',event='summons')
datalong <- as.data.table(datalong)

#############

# Hazard and survival probabilities
fit1 <- survfit(Surv(outset,obs,summons)~sex,data=datalong,
                conf.type='log',conf.int=.95,type="kaplan-meier",error='greenwood')
summary(fit1)
(hazarddata <- as.data.table(tidy(fit1)))
hazarddata <- mutate(hazarddata,sex = factor(ifelse(strata == 'sex=f','Woman','Man')))

# let's keep this for later (the other plot)
zeroes <- hazarddata[time == 1]
zeroes[,time := 0]

# Let's express time with real dates
hazarddata[,time := min(keyevents$time) + time - 1]

# Let's get smoothed hazard probabilities per sex
# The knots are the number of days observed divided by 4
knots <- ceiling(datalong[,max(obs)]/4)
hzp_f <- bshazard(Surv(outset, obs, summons) ~ 1, 
                  data = datalong[sex == "f"], nk =knots)
(hzp_f <- as.data.table(summary(hzp_f)$HazardEstimates))

hzp_m <- bshazard(Surv(outset, obs, summons) ~ 1, 
                  data = datalong[sex == "m"], nk =knots)
(hzp_m <- as.data.table(summary(hzp_m)$HazardEstimates))

# Put all together
(hzp <- rbind(hzp_f,hzp_m))
hazarddata <- cbind(hazarddata,hzp[,-'time'])

# Visualizations
p1 <- ggplot(data=hazarddata,aes(time,estimate,color=sex,fill=sex,group=sex,shape=sex,linetype=sex)) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=1/3,linetype='dashed') +
  geom_line() + geom_point(size=2) +
  scale_color_manual(values = c("gray0","gray70")) +
  scale_fill_manual(values = c("gray0","gray70")) +
  scale_linetype_manual(values=c(1,2)) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  labs(x='',y='Survival probability',color='',fill='',group='',shape='',linetype='') 

p2 <- ggplot(data=hazarddata,aes(time,n.event/n.risk,color=sex)) +
  geom_line() +
  geom_line(aes(time,hazard),linewidth=1.5)+
  scale_color_manual(values = c("gray0","gray70")) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  labs(x='',y='Hazard probability',color='',linetype='') 

################################################################################

# FROM MENTION TO SUMMONS ----

# Now, let's use a different entry point (first mention)
reported <- select(reported, -c(entry,obs)) # remove old entry points and obs

# Let's extract this information from the accusation data
denunciations <- data.table(as_edgelist(graph))
names(denunciations) <- c('source','target')
denunciations <- mutate(denunciations,time = as.Date(igraph::E(graph)$time))

# Remember there are denunciations directed at deceased individuals
denunciations <- filter(denunciations, target %in% reported$id) # N = 727

# Any denunciation whose time is missing?
filter(denunciations,is.na(time)) # this likely comes from P0051 second - not first - deposition
datemissing <- keyevents %>% 
  filter(source == 'P0051' & type == 'Testimonies') %>% 
  summarize(max(time))
denunciations[is.na(time)]$time <- datemissing # add missing date

# Let's find out the first time each person was reported
firstment <- denunciations %>%
  arrange(time) %>% # order by date
  filter(!duplicated(target)) %>% # first appearance as target of somebody's denunciation
  select(-source) # we don't need who reported
names(firstment) <- c('id','entry')
# Add this info to the other dataset
reported <- inner_join(reported,firstment,by='id')
# Now we have the sample, and the new entry and exit points
select(reported,id,entry,exit,summons)

# Let's just add the number of observations per person once again
reported <- mutate(reported, obs = as.numeric(difftime(exit,entry,unit='days'))+1)
reported[obs <= 0] # some people were summoned the day they were mentioned (4 cases)
reported[obs > 0] # 219 individuals eventually observed
reported[obs > 0,sum(summons)] # with 79 summon calls
# Remove those 4 cases when summons and denunciations happened the same day
reported <- filter(reported,!(summons == 1 & obs == 0))

# Long format
datalong <- survSplit(data=select(reported,id,label,sex,entry,obs,summons),
                      cut=1:max,
                      start='outset',end='obs',event='summons')
datalong <- as.data.table(datalong)

# Day in the trial
datalong[,dayintrial := entry + outset]

#############

# Hazard and survival probabilities
fit2 <- survfit(Surv(outset,obs,summons)~sex,data=datalong,
                conf.type='log',conf.int=.95,type="kaplan-meier",error='greenwood')
summary(fit2)
(hazarddata <- as.data.table(tidy(fit2)))
hazarddata[,sex := factor(ifelse(strata == 'sex=f','Woman','Man'))]
# Add the zeroes to the plots
hazarddata <- rbind(hazarddata,zeroes)

# Let's get smoothed hazard probabilities per sex
# The knots are the number of days observed divided by 4
hzp_f <- bshazard(Surv(outset, obs, summons) ~ 1, 
                  data = datalong[sex == "f"], nk =knots)
(hzp_f <- as.data.table(summary(hzp_f)$HazardEstimates))

hzp_m <- bshazard(Surv(outset, obs, summons) ~ 1, 
                  data = datalong[sex == "m"], nk =knots)
(hzp_m <- as.data.table(summary(hzp_m)$HazardEstimates))

# Put all together
(hzp <- rbind(hzp_f,hzp_m))
hzp[,sex := c(rep('Woman',35),rep('Man',35))]
hzp[,time := time - 0.5] # minus half

# Visualizations
p3 <- ggplot(data=hazarddata,aes(time,estimate,color=sex,fill=sex,group=sex,shape=sex,linetype=sex)) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=1/3,linetype='dashed') +
  geom_line() + geom_point(size=2) +
  scale_color_manual(values = c("gray0","gray70")) +
  scale_fill_manual(values = c("gray0","gray70")) +
  scale_linetype_manual(values=c(1,2)) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  labs(x='',y='',color='',fill='',group='',shape='',linetype='')

p4 <- ggplot() +
  geom_line(data=hazarddata,aes(time,n.event/n.risk,color=sex)) +
  geom_line(data=hzp,aes(time,hazard,color=sex),linewidth=1.5) +
  scale_color_manual(values = c("gray0","gray70")) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  labs(x='',y='',color='',linetype='') 

################################################################################

# NUMBER OF CALLABLE SUBJECTS

# Rename as man and woman
datalong <- mutate(datalong, sex = factor(sex,levels=c('m','f'),labels=c('Man','Woman')))

p5 <- ggplot(data=datalong) +
  geom_bar(aes(x=dayintrial,fill=sex),position='dodge',color='black',alpha=.75) +
  scale_fill_manual(values = c("gray0","gray70")) +
  labs(x='Date',y='Callable suspects',fill='')

p6 <- ggplot(data=datalong) +
  geom_bar(aes(x=outset,fill=sex),position='dodge',color='black',alpha=.75) +
  scale_fill_manual(values = c("gray0","gray70")) +
  labs(x='Time (days since first accusation)',y='',fill='')

################################################################################

# Visualization
tiff(filename="Fig6.tiff",
     width=25, height=25,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)
ggarrange(ggarrange(p1,p3,nrow=1,labels=c('A','B'),common.legend = TRUE),
          ggarrange(p2,p4,nrow=1,labels=c('C','D'),common.legend = TRUE),
          ggarrange(p5,p6,nrow=1,labels=c('E','F'),common.legend = TRUE),
          nrow = 3,common.legend = FALSE)
dev.off()

################################################################################

# DATA TRANSFORMATIONS ----

# COVARIATES

# 0) Days passed since first accused (our measure of time)
datalong <- mutate(datalong, obs.std = scale(obs,center=TRUE,scale=TRUE)) # standardized

# 1) Day of the first accusation (when the person enter the sample)
datalong <- mutate(datalong, accused = as.numeric(entry - min(dayintrial))+1) 
datalong <- mutate(datalong, accused.std = scale(accused,center=TRUE,scale=TRUE)) # standardized

# 2) Number of denunciations received (cumulative), up to the day
datalong <- mutate(datalong,denunciations = 0)
for(i in 1:nrow(datalong)){
  j <- datalong[i]$id
  t <- datalong[i]$dayintrial
  datalong[i]$denunciations <- nrow(denunciations[target == j & time <= t]) # up to the day 
}
datalong[,range(denunciations)]
datalong[,dens := scale(denunciations,center=TRUE,scale=TRUE)] # standardized

# 3) Number of family members denounced, up to the day before
kinties <- as.data.table(castellario$kinties)

# Let's find  all kin members for every person in the sample
sample <- data.table(id = datalong[,unique(id)])
family <- list()

for(i in 1:nrow(sample)){
  j <- sample[i]$id
  x1 <- kinties[V1 == j,V2]
  x2 <- kinties[V2 == j,V1]
  family[[i]] <- c(x1,x2)
}
names(family) <- sample$id
head(family,6) # see

datalong <- mutate(datalong,kin.denounced = 0)
for(i in 1:nrow(datalong)){
  j <- datalong[i]$id
  t <- datalong[i]$dayintrial
  fam <- family[[j]] # family members of focal subject
  datalong[i]$kin.denounced <- denunciations[target %in% fam & time <= t, # cumulative denunciations at family members
                                             length(unique(target))] # count the unique number of family involved
}
datalong[,range(kin.denounced)]
datalong[,kin.den := scale(kin.denounced,center=TRUE,scale=TRUE)] # standardized

# 4) Other aggravating circumstances: acquaintance with Martin, Francis, or host of congregations
circums <- as.data.table(castellario$circumstances)
datalong <- mutate(datalong, martinus = 0, franciscus = 0, host = 0)

for(i in 1:nrow(datalong)){
  j <- datalong[i]$id
  t <- datalong[i]$dayintrial
  mart <- nrow(circums[id == j & time <= t & type == 'Acquaintance with Martinus']) # up to the day before
  fran <- nrow(circums[id == j & time <= t & type == 'Acquaintance with Franciscus']) # up to the day before
  host <- nrow(circums[id == j & time <= t & type == 'Host of congregation']) # up to the day before
  datalong[i]$martinus <- 1*(mart > 0) # if association with Martin 1, otherwise 0
  datalong[i]$franciscus <- 1*(fran > 0) # if association with Francis 1, otherwise 0
  datalong[i]$host <- 1*(host > 0) # if host of congregations 1, otherwise 0
}

# Turn into factors
datalong[,summons := as.factor(summons)]
datalong[,martinus := as.factor(martinus)]
datalong[,franciscus := as.factor(franciscus)]
datalong[,host := as.factor(host)]

# 5) Day in the trial (we can nest on this dimension too, in addition to person)
datalong <- mutate(datalong, t = as.numeric(dayintrial - min(dayintrial))) 

################################################################################

# DESCRIPTIVES OF THE TIME-VARYING DATASET ----

# Data summary
datasummary_skim(datalong[sex == 'Woman',.(obs,summons,accused,denunciations,kin.denounced,martinus,franciscus,host)])
datasummary_skim(datalong[sex == 'Man',.(obs,summons,accused,denunciations,kin.denounced,martinus,franciscus,host)])
datasummary_skim(datalong[,.(obs,summons,accused,denunciations,kin.denounced,martinus,franciscus,host)])

# Further descriptives (at summons)
datalong[summons == 1,mean(accused),by=sex]
datalong[summons == 1,mean(denunciations),by=sex]
datalong[summons == 1,mean(kin.denounced),by=sex]
datalong[summons == 1,mean(dayintrial),by=sex]

datalong[summons == 1,table(martinus,sex)]
datalong[summons == 1,prop.table(table(martinus,sex),2)]
datalong[summons == 1,table(franciscus,sex)]
datalong[summons == 1,prop.table(table(franciscus,sex),2)]
datalong[summons == 1,table(host,sex)]
datalong[summons == 1,prop.table(table(host,sex),2)]

# BIVARIARE (pair-wise correlations)
p1 <- ggcorrmat(
  data = as.data.frame(sapply(select(datalong,
                                     sex,summons,denunciations,kin.denounced,
                                     martinus,franciscus,host,
                                     accused,obs),
                              as.numeric)),
  type='parametric',
  matrix.type = "lower",
  colors = c("grey0","white","grey0"),
  ggtheme = theme_paper
)

# Labels for axis
lbs <- c('Gender (woman)','Summoned',
         'Number of accusations received',"Number of kin members accused",
         'Acquaintance with Martin','Acquaintance with Francis','Host of congregation(s)',
         'Date of accusation','Time (days since first accusation)')

# Visualization
tiff(filename="Fig7.tiff",
     width=18,height=18,units="cm", 
     compression="lzw",bg="white",res=1000
)
p1 + theme(legend.position = c(0.15,0.8),
           axis.text.y = element_text(size = 10),
           axis.text.x = element_text(size = 10)) +
  scale_x_discrete(labels = lbs[-1]) +
  scale_y_discrete(labels = lbs[-length(lbs)])
dev.off()

################################################################################

# Let's use better names for the variables
datalong[,day := t]
datalong[,time0 := outset]
datalong[,time1 := obs]
datalong[,date := dayintrial]
datalong[,woman := ifelse(sex == 'Woman',1,0)]
datalong[,day_accused := accused]
datalong[,denunc := denunciations]
datalong[,kin_denunc := kin.denounced]

data <- datalong %>% 
  arrange(id) %>%
  select(id,label,sex,woman,time0,time1,summons,date,day,
         denunc,kin_denunc,martinus,franciscus,host,day_accused)

# Data to model
write.table(data,file='data/data3.csv',sep=',',row.names = FALSE)

################################################################################