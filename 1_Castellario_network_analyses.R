################################################################################
## INQUISITION IN GIAVENO (1335)
## (1) Castellario: Gender study (Network analyses)
## R script by Davor Salihovic (Uni of Antwerp) & Jose Luis Estevez (Uni of Helsinki)
## Date: Apr 24th, 2025
################################################################################

# R PACKAGES REQUIRED ----
library(data.table);library(tidyr);library(dplyr);library(scales) # for data wrangling
library(ggplot2);library(ggpubr);library(ggstatsplot) # for visualizations
library(igraph);library(netseg) # for network analyses
library(MatchIt);library(cobalt) # for matching
library(pracma) # to calculate overlapping coefficient of two distributions
library(WRS2);library(survey) # to perform statistical test

# DATA LOADING ----
rm(list=ls())
load('data/data.RData')

source('0_Castellario_gender_theme.R')
theme_set(theme_paper)

################################################################################

# SUMMONSES AND TESTIMONIES (DATES) ----

# Summons in public calls
psummons <- castellario$keyevents %>%
  filter(type == 'summons') %>% # keep only summons
  select(id,time) %>% # person ID and date
  arrange(time) %>%
  as.data.table() # turn into a data.table format
# How many persons were summons?
length(unique(psummons$id)) # 30 (one person was summoned thrice)

# Deposition dates
depositions <- as.data.table(castellario$edges)

depositions <- depositions %>% 
  select(source,time,summoned) %>% # omit info about the targets
  filter(!duplicated(.)) %>% # keep only unique cases
  filter(!is.na(time)) %>%
  arrange(time)

# How many among the 110 people deposed were summoned?
depositions %>%
  filter(!duplicated(source)) %>% # unique individuals (some were deposed a second time)
  count(summoned) # 85 were, 25 were not

# We know they were summoned, but not exactly when...
# Let's see difference in time from summons to deposition for those the date we know
summoned <- depositions %>%
  filter(!duplicated(source) & summoned == 1) # let's keep the 85 individuals called

sum(psummons$id %in% summoned$source) # 25 of the 30 names in public summons appeared before the inquisitor
names(psummons) <- c('source','time.summons')
summoned <- left_join(summoned,psummons,by='source')

summoned %>%
  filter(!is.na(time.summons)) %>% # For the 25 whose exacts dates we know...
  mutate(difftime = as.Date(time) - as.Date(time.summons)) %>% # difference in time
  count(difftime) # 14 appear the same day, 9 the next day (on average, 0.64 days)

# We can infer that, for those summoned but the date is unknown, the summons happened...
npsummons <- summoned %>%
  filter(is.na(time.summons)) %>%
  select(source,time) %>%
  mutate(time = as.Date(time - 24*60*60)) # the day before the deposition

# Summonses and deposition dates
# All summonses dates
names(npsummons) <- names(psummons) <- c('source','time')
summons <- rbind(npsummons,psummons)
summons[,type := 'Summons']

# Depositions dates
depositions <- mutate(depositions, time = as.Date(time))
depositions <- select(depositions, source,time)
depositions[,type := 'Testimonies']

# Put everything together
keyevents <- rbind(summons,depositions)
rm(psummons);rm(npsummons);rm(summons);rm(depositions) # remove unnecessary objects

# Let's add gender
nodes <- as.data.table(castellario$nodes)
keyevents <- merge(keyevents, select(nodes, id, sex),
                   by.x='source', by.y='id', all.x=TRUE)
keyevents <- mutate(keyevents,
                    sex = factor(sex,levels=c('m','f'),labels=c('Man','Woman')))

# Visualization
tiff(filename="Fig1.tiff",
     width=16, height=12,units="cm", 
     compression="lzw",bg="white",res=1000
)
ggplot(data=keyevents,aes(x=time,fill=sex,group=sex)) +
  geom_bar(color='black',position='stack',alpha=0.5) +
  facet_grid(type~.) +
  scale_fill_manual(values = c("gray0","gray70")) + 
  xlab("Date") + ylab("Count") + labs(fill='') +
  theme(legend.position=c(0.1,0.95))
dev.off()

################################################################################

# DENUNCIATION NETWORK ----

denunciations <- as.data.table(castellario$edges)
# Turn into igraph object
graph <- graph_from_data_frame(denunciations)
# Remove Waldensian masters and collective entities from the graph
toremove <- which(!(V(graph)$name %in% nodes[waldensian_master == 0]$id))
graph <- delete_vertices(graph,toremove)
# Remove loops and redundant mentions
toremove <- c(which(which_loop(graph)),which(which_multiple(graph)))
graph <- delete_edges(graph,toremove)
is_simple(graph)

# Only nodes involved in the network
nodes <- filter(nodes, id %in% V(graph)$name)
nodes <- nodes[match(V(graph)$name,nodes$id)] # same order of appearance
# node-level info
count(nodes, ocupation_known = !is.na(occupation_type)) # only know job of 19
count(nodes, place_known = !is.na(origin_or_residence)) # and place of 120
# Add gender to the graph
V(graph)$sex <- nodes$sex

# Visualization
tiff(filename="Fig2.tiff",
     width=20, height=20,units="cm", 
     compression="lzw",bg="white",res=1000
)
plot(graph,
     vertex.label='',
     vertex.size=3.5,
     edge.color='grey60',vertex.frame.color='black',
     edge.width=1.5,
     edge.arrow.size=.5,edge.arrow.width=.75,
     vertex.color=ifelse(V(graph)$sex == 'm','grey15','grey70'),
     layout=layout_with_fr(graph))
legend("bottomleft", legend = c('Man','Woman'),
       pch=21, pt.bg=c('grey15','grey70'))
dev.off()

# How many individuals were eventually incriminated
sum(degree(graph,mode='in') > 0) # 238
sum(degree(graph,mode='out') > 0) # 92 deponents denounced somebody else

# Gender composition
count(nodes,sex) # 156 men, 111 women
# Among those who were interrogated...
testifiers <- keyevents[type == 'Testimonies',unique(source)]
nodes %>%
  filter(id %in% testifiers) %>%
  count(sex) # 76 men, 34 women

################################################################################

# GENDER DIFFERENCES IN DENUNCIATIONS REPORTED ----

nodes <- mutate(nodes, denunciations = degree(graph,mode='out')) # denuncations reported
nodes <- mutate(nodes, denreceived = degree(graph,mode='in')) # denunciations received

# How many reported at least one person?
nodes %>%
  filter(id %in% testifiers) %>% # those who deposed
  group_by(sex) %>%
  count(denouncers = denunciations > 0) # 27 women and 65 men

# Average and median denunciations reported
nodes %>% 
  filter(id %in% testifiers) %>% # only among those who testify
  group_by(sex) %>%
  summarize(mean = mean(denunciations), median = median(denunciations))

# Let's subject these differences to a test
# Welch t-test
t.test(denunciations ~ sex, data=nodes[id %in% testifiers]) 
# Non-parametric versions (similar variances and normality assumptions not fulfilled)
wilcox.test(nodes[id %in% testifiers & sex == 'm']$denunciations,
            nodes[id %in% testifiers & sex == 'f']$denunciations)
# Robust estimate
yuen(denunciations ~ sex, data=nodes[id %in% testifiers],tr=.1) # highest and lowest 10% trimmed

# HOMOPHILY LEVELS IN DENUNCIATIONS ----

mixingm(graph,"sex",full=TRUE)
round(assort(graph,"sex"),3)
orwg(graph,"sex") # Odds ratio
ei(graph,"sex")
gamix(graph,"sex")
# By gender
coleman(graph,"sex")
smi(graph,"sex") # Segregation matrix index

# Let's measure the homophily of each deponent by calculating their EI and Yule's Q values
# Let's create a matrix first
mtx <- as_adjacency_matrix(graph)
mtx <- as.matrix(mtx,nrow=vcount(graph),ncol=vcount(graph),
                 dimnames=list(V(graph)$name,V(graph)$name))

# Let's reduce to the testifiers only
mtx <- mtx[rownames(mtx) %in% testifiers,]
testifiers <- nodes[id %in% testifiers]
# let's use the same order of appearance
testifiers <- testifiers[match(rownames(mtx)[1:110],testifiers$id)]

# Now, let's calculate the number of men and women reported by each testifier
women <- nodes[sex == 'f']$id
men <- nodes[sex == 'm']$id
testifiers[,menreported := rowSums(mtx[,colnames(mtx) %in% men])] # men reported
testifiers[,womenreported := rowSums(mtx[,colnames(mtx) %in% women])] # women reported

# We need to add those who were not accused (from the pool of 238 people denounced)
(densample <- nodes[denreceived > 0,table(sex)]) # 136 men and 102 women
testifiers[,meninntw := densample[2]]
testifiers[,womeninntw := densample[1]]
# Don't forget to substract ego
testifiers[sex == 'm']$meninntw <- densample[2]-1
testifiers[sex == 'f']$womeninntw <- densample[1]-1

# Finally, let's obtain the difference to find out those non-reported
testifiers[,menunreported := meninntw - menreported]
testifiers[,womenunreported := womeninntw - womenreported]

# With all this, we can calculate the EI and Yule's Q indexes
testifiers[,a := ifelse(sex == 'm',menreported,womenreported)]
testifiers[,b := ifelse(sex == 'm',womenreported,menreported)]
testifiers[,c := ifelse(sex == 'm',menunreported,womenunreported)]
testifiers[,d := ifelse(sex == 'm',womenunreported,menunreported)]
# The EI index is: (E-I)/(E+I), thus (b-a)/(b+a)
testifiers[,EI := ifelse(b+a != 0,(b-a)/(b+a),NA)]
testifiers[,yulesQ := ifelse(a*d+b*c !=0,(a*d - b*c)/(a*d + b*c),NA)]

# Significant differences?
t.test(EI ~ sex,data=testifiers)
wilcox.test(testifiers[sex == 'm']$EI,testifiers[sex == 'f']$EI)
yuen(EI ~ sex,data=testifiers,tr=.1)

t.test(yulesQ ~ sex,data=testifiers)
wilcox.test(testifiers[sex == 'm']$yulesQ,testifiers[sex == 'f']$yulesQ)
yuen(yulesQ ~ sex,data=testifiers,tr=.1)

# Let's visualize all this
# Let's put the data in long format
forplot <- tidyr::gather(testifiers,key='var',value='value',
                            denunciations,EI,yulesQ)
# Relabel
forplot <- forplot %>%
  mutate(sex = factor(sex,level=c('m','f'),labels=c('Man','Woman')))
# As data table
forplot <- as.data.table(forplot)

# Denunciations
p1 <- ggbetweenstats(data=forplot[var == 'denunciations'],x=sex,y=value,type='nonparametric') 
# Reduce sub-title
p1$labels$subtitle <- expression(list(italic("W")["Mann-Whitney"] == "1434.50", italic(p) == "0.36"))
# Customize
p1 <- p1 + scale_color_manual(values=c('grey0','grey50'),labels=c('Man','Woman')) +
  theme_paper + theme(legend.position = 'none') +
  geom_text(aes(x=sex,y=value,label=label),data=forplot[value > 40,],nudge_y=2,family='serif') +
  labs(x='',y='Accusations') 

# EI index
p2 <- ggbetweenstats(data=forplot[var == 'EI'],x=sex,y=value,type='nonparametric') 
# Reduce sub-title
p2$labels$subtitle <- expression(list(italic("W")["Mann-Whitney"] == "194.50", italic(p) < "0.001"))
# Customization
p2 <- p2 + scale_color_manual(values=c('grey0','grey50'),labels=c('Man','Woman')) +
  geom_hline(yintercept = -1,linetype='dashed',alpha=.2) +
  annotate("text",label='Perfect homophily',x=1.5,y=-1.1,family='serif',hjust=0.5,vjust=0.5) +
  geom_hline(yintercept = 1,linetype='dashed',alpha=.2) +
  annotate("text",label='Perfect heterophily',x=1.5,y=1.1,family='serif',hjust=0.5,vjust=0.5) +
  theme_paper + theme(legend.position = 'none',axis.text.x = element_blank()) +
  labs(x='',y='EI index') 

# Yule's Q
p3 <- ggbetweenstats(data=forplot[var == 'yulesQ'],x=sex,y=value,type='nonparametric')
# Reduce sub-title
p3$labels$subtitle <- expression(list(italic("W")["Mann-Whitney"] == "1467.50", italic(p) < "0.001"))
# Customization
p3 <- p3 + scale_color_manual(values=c('grey0','grey50'),labels=c('Man','Woman')) +
  geom_hline(yintercept = -1,linetype='dashed',alpha=.2) +
  annotate("text",label='Perfect heterophily',x=1.5,y=-1.1,family='serif',hjust=0.5,vjust=0.5) +
  geom_hline(yintercept = 1,linetype='dashed',alpha=.2) +
  annotate("text",label='Perfect homophily',x=1.5,y=1.1,family='serif',hjust=0.5,vjust=0.5) +
  theme_paper + theme(legend.position = 'none') +
  labs(x='',y="Yule's Q") 

# Visualization
tiff(filename="Fig3.tiff",
     width=18, height=18,units="cm", 
     compression="lzw",bg="white",res=1000
)
ggarrange(p1,ggarrange(p2,p3,labels=LETTERS[2:3],nrow=2),
          labels=c('A','',''),ncol=2)
dev.off()

################################################################################

# PAIRING MALE AND FEMALE TESTIFIERS ----

# Let's find for every testifier the date of the deposition, and whether they were already summoned and tortured
extrainfo <- denunciations %>%
  arrange(time) %>%
  filter(!duplicated(source)) %>%
  select(source,time,summoned)
testifiers <- merge(testifiers,extrainfo,by.x='id',by.y='source',all.x=TRUE)
# Info about torture
tortured <- castellario$keyevents[castellario$keyevents$type =='torture',]$id
testifiers <- mutate(testifiers,tortured = as.numeric(id %in% tortured))

# Coarsened exact matching
testifiers <- mutate(testifiers, treatment = as.numeric(sex == 'f')) # women as treatment grop
# Time in the trial
testifiers <- testifiers %>%
  mutate(time = as.Date(time), # date format
         day = as.numeric(difftime(time,min(time),unit='days')+1)) # from day 1 instead of 0

# Probit model
m.out <- matchit(treatment ~ day + summoned + tortured, 
                 data = testifiers,method='cem',
                 cutpoints = c(5,10,15,20,25,30)) # cut points every 5 days
summary(m.out)

# Visualization
mtc1 <- love.plot(m.out,stats='m',abs = FALSE,
                  drop.distance = FALSE, thresholds = c(m = .1),
                  shapes = c("circle filled", "circle"), 
                  colors = c("grey30", "grey0"),
                  sample.names = c("All", "Matched"),
                  position = "top") +
  scale_y_discrete(labels=c('Tortured','Summoned','Date of testimony'))

mtc2 <- bal.plot(m.out,var.name = 'day',
                 which="both",position='top') +  
  scale_fill_manual(values=c('grey0','grey70'),labels=c('Man','Woman')) +
  labs(title='',x='Date of testimony',fill='')

mtc3 <- bal.plot(m.out,var.name = 'summoned',
         which="both",position='top') + 
  scale_x_discrete(labels=c('No','Yes')) +
  scale_fill_manual(values=c('grey0','grey70'),labels=c('Man','Woman')) +
  labs(title='',x='Summoned',fill='')

mtc4 <- bal.plot(m.out,var.name = 'tortured',
                 which="both",position='top') + 
  scale_x_discrete(labels=c('No','Yes')) +
  scale_fill_manual(values=c('grey0','grey70'),labels=c('Man','Woman')) +
  labs(title='',x='Tortured',fill='')

# Visualize
tiff(filename="Fig5.tiff",
     width=25, height=15,units="cm", 
     compression="lzw",bg="white",res=1000
)
ggarrange(mtc1,ggarrange(mtc2,mtc3,mtc4,labels=LETTERS[2:4],ncol=3),
          labels=c('A','','',''),nrow=2)
dev.off()

# Extract the weights
testifiers$weights <- m.out$weights

# Perform the weighted t-test (denunciations reported by gender)
design <- survey::svydesign(ids = ~1, weights = ~weights, data = testifiers)
result <- survey::svyttest(denunciations ~ sex, design)
result # again, no evidence of differences across genders in number of individuals reported

################################################################################

# SIMULATION EXERCISE ----

# How many combinations?
testifiers[,table(sex)]
testifiers[,table(sex)][1] -> nf
testifiers[,table(sex)][2] -> nm
# n choose r: C(n,r) = n! / (r! * (n-r)!)
factorial(nm) / (factorial(nf) * factorial(nm - nf)) # this is a huge number, we better simulate
# If we consider only those matched (48)
nm <- 48
factorial(nm) / (factorial(nf) * factorial(nm - nf)) # still a huge number

# Let's get the IDs of the testifiers by gender
wmn <- testifiers[sex == 'f',id]
men <- testifiers[sex == 'm',id]
men2 <- testifiers[sex == 'm' & weights != 0,id] # the subset of men matched

# Let's now get the matrix of denunciations
mtx <- inc_mtx <- as.matrix(as_adjacency_matrix(graph))

##############

# Let's start by seeing the effect of excluding the 34 women
mtx[wmn,] <- mtx[wmn,]*0 # where the outgoing ties of these women have been severed

# Let's calculate now the geodesic distances
gmtx <- matrix(distances(graph_from_adjacency_matrix(mtx),mode='out'),
               nrow=267,ncol=267,
               dimnames=list(colnames(mtx),colnames(mtx)))

# These are the individuals whose reachability is assess (the 76 male deponents)
toeval <- testifiers[id %in% men]$id
refpoint <- rowSums(!(is.infinite(gmtx[toeval,])))
mean(refpoint)
sd(refpoint)/sqrt(length(refpoint))

##############

# With men

# Make copies of the denunciation network
ntws <- list()
for(i in 1:10000){
  ntws[[i]] <- inc_mtx
}

# Let's obtains combinations of the 76/48 (matched) male deponents of size 34
comb <- list()
set.seed(0708) # set seed for replication
for(i in 1:10000){
  if(i <= 5000){
    comb[[i]] <- sample(x=men,size=34,replace=FALSE) # all male deponents (men)
  }else{
    comb[[i]] <- sample(x=men2,size=34,replace=FALSE) # only the 48 with a matched among women
  }
}

for(i in 1:10000){
  # Let's remove the outgoing ties of the selected nodes
  ntws[[i]][comb[[i]],] <- ntws[[i]][comb[[i]],]*0
  # And calculate the geodesic distances
  ntws[[i]] <- matrix(distances(graph_from_adjacency_matrix(ntws[[i]]),mode='out'),
                      nrow=267,ncol=267,
                      dimnames=list(colnames(mtx),colnames(mtx)))
}

# Individuals to evaluate
spareind <- list()
for(i in 1:10000){
  # the active deponents
  x <- testifiers[,id][!(testifiers[,id] %in% comb[[i]])]
  # Number of spare individuals
  spareind[[i]] <- rowSums(!(is.infinite(ntws[[i]][x,])))
}

# Counter-factual results
counterfacts <- data.table(matrix(data=unlist(spareind),nrow=10000,ncol=76,byrow=TRUE))

##############

# Obtain the Kernel density estimates
kde1 <- density(refpoint,bw=5) # bin width of size 5
kde2 <- density(unlist(counterfacts[1:5000]),bw=5)
kde3 <- density(unlist(counterfacts[5001:10000]),bw=5)

# Visual comparison
# Step 1: Create a common grid of x values
min_x <- min(kde1$x, kde2$x, kde3$x)  # Find the minimum x value
max_x <- max(kde1$x, kde2$x, kde3$x)  # Find the maximum x value
common_x <- seq(min_x, max_x, length.out = 1000)  # Create a grid of 1000 points

# Step 2: Interpolate the KDEs onto the common grid using approx()
kde1_interp <- approx(kde1$x, kde1$y, xout = common_x)$y  # Interpolated y-values for kde1
kde2_interp <- approx(kde2$x, kde2$y, xout = common_x)$y  # Interpolated y-values for kde2
kde3_interp <- approx(kde3$x, kde3$y, xout = common_x)$y  # Interpolated y-values for kde3

# Step 3: Calculate overlapping coefficients
# Calculate the point-wise minimum of the densities
min_kde12 <- pmin(kde1_interp, kde2_interp)
min_kde13 <- pmin(kde1_interp, kde3_interp)
# Integrate to compute the overlap (OVL) using the trapezoidal rule
ovl1 <- trapz(common_x[!is.na(min_kde12)], min_kde12[!is.na(min_kde12)])
ovl2 <- trapz(common_x[!is.na(min_kde13)], min_kde13[!is.na(min_kde13)])

# Kolmogorov-Smirnov Test (K-S Test)
# The K-S test will give you a p-value that indicates whether the two samples are drawn from the same distribution. 
# A low p-value suggests that the distributions are significantly different
ks.test(refpoint,unlist(counterfacts[1:5000]),simulate.p.value = TRUE,B=5000)
ks.test(refpoint,unlist(counterfacts[5001:10000]),simulate.p.value = TRUE,B=5000)

# Step 4: visualization
labels <- c("The 34 women excluded","34 of the 76 men excluded","34 of the 48 matched men excluded")
forplot <- data.table(x = rep(common_x,times=3),
                      type = factor(rep(1:3,each=length(common_x)),labels=labels),
                      y = c(kde1_interp,kde2_interp,kde3_interp))

median(refpoint)
median(unlist(counterfacts[1:5000]))

p1 <- ggplot(data=forplot[x >= 0 & type != "34 of the 48 matched men excluded"],
             aes(x=x,y=y,fill=type)) + # don't plot below zero
  geom_ribbon(aes(ymin = 0, ymax = y),color='grey25',linewidth=0.5,alpha=1/2) +
  geom_vline(xintercept = median(refpoint),linewidth=0.5,linetype='dashed',color='grey0') +
  geom_vline(xintercept = median(unlist(counterfacts[1:5000])),linewidth=0.5,linetype='dashed',color='grey70') +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values=c('grey0','grey70')) +
  annotate("text", x = 0, y = 0.038, family = 'serif', hjust = 0,
           label = paste('OVL = ',round(ovl1,4)*100,'%')) +
  annotate("text", x = 0, y = 0.033, family = 'serif', hjust = 0,
           label = expression(list(italic("D")["Kolmogorov-Smirnov"] == "0.697", italic(p) < "0.001, based on 5,000 simulations"))) + 
  annotate("label", x = median(refpoint), y = 0.015, size=3,
           label = expression(list(hat(mu)['median'] == "173"))) +
  annotate("label", x = median(unlist(counterfacts[1:5000])), y = 0.015,,size=3,
           label = expression(list(hat(mu)['median'] == "113"))) +
  labs(x="",y='Density',color='',fill='') +
  theme(legend.position=c(0.02,1.1),legend.justification = c(0, 1))

median(unlist(counterfacts[5001:10000]))

p2 <- ggplot(data=forplot[x >= 0 & type != "34 of the 76 men excluded"],
             aes(x=x,y=y,fill=type)) + # don't plot below zero
  geom_ribbon(aes(ymin = 0, ymax = y),color='grey25',linewidth=0.5,alpha=1/2) +
  geom_vline(xintercept = median(refpoint),linewidth=0.5,linetype='dashed',color='grey0') +
  geom_vline(xintercept = median(unlist(counterfacts[5001:10000])),linewidth=0.5,linetype='dashed',color='grey70') +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values=c('grey0','grey70')) +
  annotate("text", x = 0, y = 0.038, family = 'serif', hjust = 0,
           label = paste('OVL = ',round(ovl2,4)*100,'%')) +
  annotate("text", x = 0, y = 0.033, family = 'serif', hjust = 0,
           label = expression(list(italic("D")["Kolmogorov-Smirnov"] == "0.697", italic(p) < "0.001, based on 5,000 simulations"))) + 
  annotate("label", x = median(refpoint), y = 0.015, size=3,
           label = expression(list(hat(mu)['median'] == "173"))) +
  annotate("label", x = median(unlist(counterfacts[5001:10000])), y = 0.015,size=3,
           label = expression(list(hat(mu)['median'] == "144"))) +
  labs(x="Reachable individuals by testifier",y='Density',color='',fill='') +
  theme(legend.position=c(0.02,1.1),legend.justification = c(0, 1))

tiff(filename="Fig4.tiff",
     width=18, height=16,units="cm", 
     compression="lzw",bg="white",res=1000
)
ggarrange(p1,p2,labels=LETTERS[1:2],ncol=1)
dev.off()

################################################################################

rm(list=setdiff(ls(), c("castellario","nodes","graph","testifiers","keyevents")))

# Save image
save.image('data/data2.RData')

################################################################################