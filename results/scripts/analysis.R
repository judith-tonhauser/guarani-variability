# Guarani projection variability: analyses and plots 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# install packages
library(tidyverse)
library(viridis) #for colors that are suitable for color blind and bw printing
library(forcats)
library(languageR)
library(lme4)

theme_set(theme_bw())

# read in the data
d = read.csv("../data/results.csv")
nrow(d) #1200 = 30 participants x 40 items
names(d)
head(d)

summary(d)

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) 
d %>% 
  dplyr::select(gender, participant) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 19 female, 11 male
# age: 12 below 35, 18 above 34

# exclude responses to item 22 (vya-2) because ambiguous between 'is not hungry' and 'is not stingy'
d = droplevels(subset(d,d$itemNr != "22"))
nrow(d) #1170 = 30 participants x 39 items

# exclude participant #23 because they only responded '5'
d = droplevels(subset(d,d$participant != "23"))
nrow(d) #1131 = 29 participants x 39 items

# rows without a response?
d[is.na(d$response),] #2 from participant 4 (items vya1 and controlB1)

# drop rows without response
d = droplevels(subset(d,!is.na(d$response)))

## fix class of objects 
d$participant <- as.factor(d$participant)
str(d$participant)

str(d$response)
str(d$list)

# responses to controls ----

controls = droplevels(subset(d,d$expression == "control"))
nrow(controls)
table(controls$response)

# mean response to controls
agr = aggregate(response ~ expression, data=controls,FUN="mean")
agr #2.294798

# mean response by control item 
agr = aggregate(response ~ ID, data=controls, FUN="mean")
agr$CILow = aggregate(response ~ ID, data=controls, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ ID, data=controls, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
agr
# mean responses between 1.93 and 2.52

# responses by control item
ggplot(controls, aes(x=response))+
  geom_histogram(position="dodge") + 
  facet_wrap(~ID) + 
  xlab("Responses to main clause controls by item")

# participants' responses to polar question controls
c = aggregate(response ~ participant, data=controls, FUN="mean")
c$CILow = aggregate(response ~ participant, data=controls, FUN="ci.low")$response
c$CIHigh = aggregate(response ~ participant, data=controls, FUN="ci.high")$response
c$YMin = c$response - c$CILow
c$YMax = c$response + c$CIHigh
c

controls$participant2 <- factor(controls$participant,levels=c$participant[order(c$response)])

ggplot(controls, aes(x=participant2, y=response)) + 
  geom_point(position = position_jitter(w = 0, h = 0.1),alpha = 3/8) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=3, shape = 17, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Response")+
  xlab("Participant")
ggsave(f="../graphs/boxplot-controls-by-participant.pdf",height=3,width=8)

# save cleaned data (cd)
write.csv(d,file="../data/cd.csv")
length(unique(d$participant)) #29

### plot percentage of categorical responses indicating projection (Figure 1 of NLLT paper) ----

# Guarani items
# NRRC 100%
# possNP 100%
# nte <100%
# haimete <100%
# cos 100%
# kuaa 100%
# kuaa-uka 16/16 100%
# juhu 0/16 0%, 11/16 69%, 13/16 81%
# topa 1/16 6%
# hecha 16/16 100%
# hecha-uka 16/16 100%
# hecha-kuaa 10/16 62%
# vya 15/16 94%
# gueroti 12/12 100%
# nembyasy 13/16 81%
# e 8/16 50%
# mombeu 9/16 56%, 12/16 75%, 16/16 100%
# hendu 11/16 69%
# moa 2/16 13%
# haaro 1/16 6%
# mc 0%

# create the data frame

item <- c("MC","juhu_1 \'discover\'","ha\'aro \'hope\'","topa \'discover\'",
           "mo\'a \'think\'","\'e \'say\'",
           "nte \'only\'","haimete \'almost\'","mombe\'u_1 \'confess\'",
           "hechakuaa \'realize\'","hendu \'hear\'",
           "juhu_2 \'discover\'", "mombe\'u_2 \'confess\'",
           "juhu_3 \'discover\'",
           "nembyasy \'regret\'","vy\'a \'be happy\'",
           "hechauka \'show\'","mombe\'u_3 \'confess\'",
           "CoS","kuaa \'know\'", "kuaauka \'point out\'",
           "hecha \'see\'","gueroti \'be ashamed\'","possNP","NRRC")

item <- factor(item, ordered = TRUE, levels = c("MC","juhu_1 \'discover\'","ha\'aro \'hope\'","topa \'discover\'",
                                           "mo\'a \'think\'","\'e \'say\'",
                                           "nte \'only\'","haimete \'almost\'","mombe\'u_1 \'confess\'",
                                           "hechakuaa \'realize\'","hendu \'hear\'",
                                           "juhu_2 \'discover\'", "mombe\'u_2 \'confess\'",
                                           "juhu_3 \'discover\'",
                                           "nembyasy \'regret\'","vy\'a \'be happy\'",
                                           "hechauka \'show\'","mombe\'u_3 \'confess\'",
                                           "CoS","kuaa \'know\'", "kuaauka \'point out\'",
                                           "hecha \'see\'","gueroti \'be ashamed\'","possNP","NRRC"))
item

percent = c(0,0,6,6,13,50,50,50,56,62,69,69,75,81,81,94,100,100,100,100,100,100,100,100,100)

prop = c('none','0/16','1/16','1/16','2/16','8/16','some','some','9/16','10/16','11/16',
         '11/16', '12/16','13/16','13/16','15/16','16/16','16/16','all','all', '16/16','16/16','12/12','all','all')

df <- data.frame(item,percent)
df
str(df)

levels(df$item)

# define colors for the sets of examples
cols = data.frame(V=levels(df$item))
str(df$item)
str(cols$V)

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("NRRC"), "CI", 
         ifelse(cols$V %in% c("mombe'u_1 'confess'","mombe'u_2 'confess'","mombe'u_3 'confess'", "'e 'say'", "hendu 'hear'", "hechauka 'show'"), "PNF", 
                ifelse(cols$V %in% c("MC","mo\'a \'think\'","ha\'aro \'hope\'"),"MC","PS"))))

cols
levels(cols$V)

# use viridis package to define 5 colors 
pal <- viridisLite::magma(5)
pal
# "#000004FF" "#51127CFF" "#B63679FF" "#FB8861FF" "#FCFDBFFF"
# black         purple     magenta     orange      pale yellow
barplot(c(1,1,1,1,1), col=pal)
# use black (MC, non-proj), purple (proj non-factive), magenta (ps) and orange (NRRC)


# cols$Colors =  ifelse(cols$VeridicalityGroup == "PNF", "tomato1", 
#                       ifelse(cols$VeridicalityGroup == "CI","dodgerblue",
#                              ifelse(cols$VeridicalityGroup == "MC","black","darkorchid")))


cols$Colors =  ifelse(cols$VeridicalityGroup == "PNF", "#51127CFF", 
                      ifelse(cols$VeridicalityGroup == "CI","#FB8861FF",
                             ifelse(cols$VeridicalityGroup == "MC","#000004FF","#B63679FF")))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(df$item)),]$V, ordered = TRUE)
levels(cols$V)

df$VeridicalityGroup = as.factor(
  ifelse(df$item %in% c("NRRC"), "CI", 
         ifelse(df$item  %in% c("mombe'u_1 'confess'","mombe'u_2 'confess'","mombe'u_3 'confess'", "'e 'say'", "hendu 'hear'", "hechauka 'show'"), "PNF", 
                ifelse(df$item  %in% c("MC","mo\'a \'think\'","ha\'aro \'hope\'"),"MC","PS"))))

levels(df$VeridicalityGroup)

ggplot(df, aes(x=item, y=percent, fill = VeridicalityGroup)) +
  geom_bar(stat="identity",position=position_dodge(.9)) +
  geom_text(aes(label=prop), vjust=-0.2, size=3.5)+
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("#FB8861FF","#000004FF","#51127CFF","#B63679FF")) +
  #scale_fill_manual(values=c("dodgerblue","black","tomato1","darkorchid")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=11), axis.text.x = element_text(size = 11, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("% responses \n indicating projection") +
  xlab("Set of examples") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) 
ggsave(f="../graphs/projection-categorical.pdf",height=4,width=9.5)

#### plots of experiment data (section 2.3 of NLLT paper) ----
cd = read.csv("../data/cd.csv")

nrow(cd) #1129 = 29 participants x 39 items - 2 items without response

# rename expressions
levels(cd$expression) <- list("MC"="control","CoS"="cos","haimete 'almost'"="haimete","hecha 'see'"="hecha","hechakuaa 'realize'"="hechakuaa","hechauka 'show'"="hechauka","\'e 'say'"="hei","hendu 'hear'"="hendu","juhu 'discover'"="juhu","kuaa 'know'"="kuaa","kuaauka 'point out'"="kuaauka","mo'a 'think'"="moa","mombe'u 'confess'"="mombeu","NRRC"="NRRC","nte 'only'"="nte","possNP"="possNP","topa 'discover'"="topa","vy'a 'happy'"="vya")
table(cd$expression)

### basic facts about remaining data
# how many participants in each list?
table(cd$list)
# AB  BA 
# 546 583

# age/gender of participants?
table(cd$gender,cd$age)

# Figure 2: by-expression projectivity
str(cd$response)
cd$response <- as.numeric(cd$response)
table(cd$expression)

means = cd %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
means
levels(means$expression)

# define colors for the predicates
cols = data.frame(V=levels(means$expression))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("NRRC"), "CI", 
         ifelse(cols$V %in% c("mombe'u 'confess'", "'e 'say'", "hendu 'hear'", "hechauka 'show'"), "PNF", 
                       ifelse(cols$V %in% c("MC","mo'a 'think'"),"MC","PS"))))

cols
levels(cols$V)

# cols$Colors =  ifelse(cols$VeridicalityGroup == "PNF", "tomato1", 
#                              ifelse(cols$VeridicalityGroup == "CI","dodgerblue",
#                                     ifelse(cols$VeridicalityGroup == "MC","black","darkorchid")))

cols$Colors =  ifelse(cols$VeridicalityGroup == "PNF", "#51127CFF", 
                      ifelse(cols$VeridicalityGroup == "CI","#FB8861FF",
                             ifelse(cols$VeridicalityGroup == "MC","#000004FF","#B63679FF")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(means$expression)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = as.factor(
  ifelse(means$expression %in% c("NRRC"), "CI", 
         ifelse(means$expression  %in% c("mombe'u 'confess'", "'e 'say'", "hendu 'hear'", "hechauka 'show'"), "PNF", 
                ifelse(means$expression  %in% c("MC","mo'a 'think'"),"MC","PS"))))

subjmeans = cd %>%
  group_by(expression,participant) %>%
  summarize(Mean = mean(response)) 
subjmeans$expression <- factor(subjmeans$expression, levels = unique(levels(means$expression)))
levels(subjmeans$expression)

ggplot(means, aes(x=expression, y=Mean,fill=VeridicalityGroup)) +
  geom_jitter(width=.1,shape=16,fill="gray60",data=subjmeans, alpha=.3, color="gray50") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=3.5,color="black") +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("#FB8861FF","#000004FF","#51127CFF","#B63679FF")) +
  #scale_fill_manual(values=c("dodgerblue","black","tomato1","darkorchid")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  #theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Expression") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave(f="../graphs/projection-means-by-expression.pdf",height=3.5,width=7)


# Figure 3: by-participant projection variability (without controls)
table(cd$expression)
target <- droplevels(subset(cd, cd$expression != "MC"))
nrow(target) #956 
table(target$expression)

# create column with class of content
target$VeridicalityGroup = as.factor(
  ifelse(target$expression %in% c("NRRC"), "CI", 
         ifelse(target$expression %in% c("mombe'u\n 'confess'", "'e 'say'", "hendu\n 'hear'"), "PNF", 
                ifelse(target$expression %in% c("mo'a 'think'"),"MC","PS"))))
table(target$expression,target$VeridicalityGroup)

mean_response = aggregate(response~participant, data=target, FUN="mean")

# mean rating by class and participant
means = aggregate(response~participant+VeridicalityGroup, data=target, FUN="mean")
means$YMin = means$response - aggregate(response~participant+VeridicalityGroup, data=target, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~participant+VeridicalityGroup, data=target, FUN="ci.high")$response
means

means_ps = aggregate(response~participant, data=target[target$VeridicalityGroup == "PS",], FUN="mean")
means_ps

means$participant <- factor(means$participant, levels=means[order(means_ps$response), "participant"])

# define colors for the individual ratings
cols = data.frame(V=levels(target$expression))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("NRRC"), "CI", 
         ifelse(cols$V %in% c("mombe'u\n 'confess'", "'e 'say'", "hendu\n 'hear'"), "PNF", 
                ifelse(cols$V %in% c("control","mo'a 'think'"),"MC","PS"))))

cols
levels(cols$V)

# means$Colors =  as.factor(ifelse(means$VeridicalityGroup == "PNF", "tomato1", 
#                       ifelse(means$VeridicalityGroup == "CI","dodgerblue",
#                              ifelse(means$VeridicalityGroup == "MC","black","darkorchid"))))

means$Colors =  as.factor(ifelse(means$VeridicalityGroup == "PNF", "#51127CFF", 
                                 ifelse(means$VeridicalityGroup == "CI","#FB8861FF",
                                        ifelse(means$VeridicalityGroup == "MC","#000004FF","#B63679FF"))))

head(means)
levels(means$Colors)

levels(means$VeridicalityGroup)

ggplot(means, aes(x=participant, y=response,fill=VeridicalityGroup)) +
  geom_point(aes(shape=VeridicalityGroup),stroke=.5,size=3.5,color="black",position=position_dodge(width=0.5)) +
  scale_shape_manual(values=c(21, 24, 21, 21)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax, color=factor(VeridicalityGroup)), width=0.3,linetype = "dotted",
                position=position_dodge(width=0.5)) +
  scale_fill_manual(values=c("#FB8861FF","#000004FF","#51127CFF","#B63679FF")) +
  #scale_fill_manual(values=c("dodgerblue","black","tomato1","darkorchid")) +
  scale_color_manual(values=c("#FB8861FF","#000004FF","#51127CFF","#B63679FF")) +
  #scale_color_manual(values=c("dodgerblue","black","tomato1","darkorchid")) +
  theme(legend.position = "none") +
  ylab("Mean certainty ratings") +
  xlab("Participant")
ggsave(f="../graphs/projection-by-participant.pdf",height=3,width=8)

#### plots to compare PG and English ----

# create data frames

# projection means of English expressions (from Tonhauser et al 2018)

# Exp1a
# 1             MC 0.04853968 
# 2           NRRC 0.96376190 
# 4        annoyed 0.95957143 
# 5       discover 0.85566667 
# 6           know 0.92338095 
# 7           only 0.76042857 
# 8         possNP 0.93866667 
# 9           stop 0.86504762 

# Exp1b
# 3    confess 0.68697872 
# 4     reveal 0.77842553 
# 7   find_out 0.88374468 
# 8        see 0.89059574 
# 9     amused 0.91029787 
# 10   realize 0.91331915
# 5   discover 0.85340426 
# 13   annoyed 0.92289362 

# Guarani experiment means
# 1 MC            2.29 
# 2 CoS           3.53 
# 4 hecha 'see'   3.38 
# 5 hechakuaa '~  4.05 
# 6 hechauka 's~  3.81 
# 9 juhu 'disco~  3.59 
# 10 kuaa 'know'   3.66
# 13 mombe'u 'co~  3.91
# 14 NRRC          4.38
# 15 nte 'only'    3.47
# 16 possNP        4.47
# 17 topa 'disco~  3.95
# 18 vy'a 'happy'  3.75 

# create data frame for items explored in Guarani experiment (section 2.3)
item <- c("NRRCs","possNP","=nte/only","CoS/stop","kuaa_e/know","juhu_e/discover",
                "topa/find out","hecha/see","hechakuaa_e/realize","vya_e/amused",
                "hechauka/reveal","mombeu_e/confess")

G_mean <- c(4.38,4.47,3.47,3.53,3.66,3.59,3.95,3.38,4.05,3.75,3.81,3.91)

E_mean <- c(.96,.94,.76,.87,.92,.86,.88,.89,.91,.91,.78,.69)

df_exp <- data.frame(item,G_mean,E_mean)
df_exp #12 items to compare

# create data frame for items explored in Guarani pilot (section 2.2)
item <- c("kuaa_p/know","juhu_p/discover","hechakuaa_p/realize","vya_p/amused","nembyasy/annoyed","mombeu_p/confess")

G_mean <- c(3.6,3.4,4.4,5,4.8,4)

E_mean <- c(.92,.86,.91,.91,.96,.69)

df_pilot <- data.frame(item,G_mean,E_mean)
df_pilot #6 items to compare

# bind the two data frames
df <- rbind(df_exp,df_pilot)
df # 18 items to compare

# calculate Spearman rank correlations

# remove from data frame the 5 duplicate verbs from pilot
df_exp <- droplevels(subset(df,df$item != "kuaa_p/know" & 
                              df$item != "juhu_p/discover" & 
                              df$item != "hechakuaa_p/realize" & 
                              df$item != "vya_p/amused" & 
                              df$item != "mombeu_p/confess"))
rownames(df_exp) <- NULL
df_exp #13 items to compare

cor.test(df_exp$G_mean,df_exp$E_mean,method="spearman")
length(unique(df_exp$item)) #13
# r = .584
# p = .036

# remove from data frame the 5 duplicate verbs from experiment
df_p <- droplevels(subset(df,df$item != "kuaa_e/know" & 
                              df$item != "juhu_e/discover" & 
                              df$item != "hechakuaa_e/realize" & 
                              df$item != "vya_e/amused" & 
                              df$item != "mombeu_e/confess"))
rownames(df_p) <- NULL
df_p # 13 items to compare

cor.test(df_p$G_mean,df_p$E_mean,method="spearman")
length(unique(df_p$item)) #13
# r = .556
# p = .048

# for reviewer, consider only "presuppositions" 
# correlation based on experiment items 
df_ps <- droplevels(subset(df,df$item != "NRRCs" 
                           & df$item != "mombeu_e/confess"
                           & df$item != "mombeu_p/confess"
                           & df$item != "kuaa_p/know" & 
                             df$item != "juhu_p/discover" & 
                             df$item != "hechakuaa_p/realize" & 
                             df$item != "vya_p/amused" & 
                             df$item != "mombeu_p/confess"))
rownames(df_ps) <- NULL
df_ps #11 items to compare

cor.test(df_ps$G_mean,df_ps$E_mean,method="spearman")
length(unique(df_ps$item)) #11
# r = .62
# p = .04

# correlation based on pilot items 
df_ps2 <- droplevels(subset(df,df$item != "NRRCs" 
                           & df$item != "mombeu_e/confess"
                           & df$item != "mombeu_p/confess"
                           & df$item != "kuaa_e/know" & 
                             df$item != "juhu_e/discover" & 
                             df$item != "hechakuaa_e/realize" & 
                             df$item != "vya_e/amused" & 
                             df$item != "mombeu_e/confess"))
rownames(df_ps2) <- NULL
df_ps2 #11 items to compare

cor.test(df_ps2$G_mean,df_ps2$E_mean,method="spearman")
length(unique(df_ps2$item)) #11
# r = .63
# p = .036

# plots

# order items by Guarani projection mean
levels(df$item) #alphabetical
df$item <- factor(df$item, levels = df[order(df$G_mean),]$item, ordered = TRUE)
levels(df$item) #by G_mean

df$VeridicalityGroup = as.factor(
  ifelse(df$item %in% c("NRRCs"), "CI", 
         ifelse(df$item %in% c("hechauka/reveal","mombeu_e/confess","mombeu_p/confess"), "PNF", 
                ifelse(df$item %in% c("MC","mo\'a \'think\'","ha\'aro \'hope\'"),"MC","PS"))))

# df$Color =  ifelse(df$VeridicalityGroup == "PNF", "tomato1", 
#                    ifelse(df$VeridicalityGroup == "CI","dodgerblue",
#                           ifelse(df$VeridicalityGroup == "MC","black","darkorchid")))


df$Color =  ifelse(df$VeridicalityGroup == "PNF", "#51127CFF", 
                   ifelse(df$VeridicalityGroup == "CI","#FB8861FF",
                          ifelse(df$VeridicalityGroup == "MC","#000004FF","#B63679FF")))

df
  
library(ggrepel)

# all data (for paper)
ggplot(df, aes(x=G_mean, y=E_mean,label=item,fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  #scale_fill_manual(values=c("dodgerblue","tomato1","darkorchid")) +
  scale_fill_manual(values=c("#FB8861FF","#51127CFF","#B63679FF")) +
  geom_text_repel(aes(label=item),hjust=0, vjust=0,nudge_y = 0.02,color=df$Color) +
  #geom_abline(intercept=-0.25,slope=.25,color="gray70",linetype="dashed") +
  # color=cols$Colors)) +
  xlim(3,5) +
  ylim(.66,1) +
  coord_equal(ratio = 5.3) +
  #theme(aspect.ratio=1) +
  ylab("Projection mean of  English expression/content pair") +
  xlab("Projection mean of Guarani expression/content pair") +
  guides(fill=FALSE) 
ggsave("../graphs/comparison.pdf",height=5,width=5)


# only experiment data (for XPRAG 2019 poster)

df_exp <- droplevels(subset(df,df$item != "kuaa_p/know" & 
                              df$item != "juhu_p/discover" & 
                              df$item != "hechakuaa_p/realize" & 
                              df$item != "vya_p/amused" & 
                              df$item != "mombeu_p/confess"))
rownames(df_exp) <- NULL
df_exp

ggplot(df_exp, aes(x=G_mean, y=E_mean,label=item,fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_fill_manual(values=c("dodgerblue","tomato1","darkorchid")) +
  geom_text_repel(aes(label=item),hjust=0, vjust=0,nudge_y = 0.02,color=df_exp$Color) +
  #geom_abline(intercept=-0.25,slope=.25,color="gray70",linetype="dashed") +
  # color=cols$Colors)) +
  xlim(3,5) +
  ylim(.68,1) +
  coord_equal(ratio = 5.3) +
  #theme(aspect.ratio=1) +
  ylab("Projection mean of  English expression/content pair") +
  xlab("Projection mean of Guarani expression/content pair") +
  guides(fill=FALSE) 
ggsave("../graphs/comparison-exp.pdf",height=5,width=5)

# models ----

# load clean data with main clause controls
cd = read.csv("../data/cd.csv")
nrow(cd) #1129

library(ordinal)
library(lsmeans)
library(multcomp)

# all contents (i.e., with main clauses) 
names(cd)
table(cd$expression)

cd$response <- as.factor(cd$response)

cd$expression <- relevel(cd$expression, ref = "control")

# model: expression as fixed effect, participant and item as random effect
model <- clmm(response ~ expression + (1|participant) + (1|itemNr), data=cd)
summary(model)
model.1 <- clmm(response ~ expression*list + (1|participant) + (1|itemNr), data=cd)
summary(model.1) 
anova(model,model.1) # model.1 not significantly better than model

model.2 <- clmm(response ~ expression + (1+expression|participant) + (1|itemNr), data=cd)
summary(model.2) #does not converge

# compare models with and without random effects, to test their significance
model.nop <- clmm(response ~ expression + (1|itemNr), data=cd)
summary(model.nop)
anova(model,model.nop)
# no.par            AIC  logLik  LR.stat  df Pr(>Chisq)    
# model.nop     22 2775.1 -1365.5                          
# model         23 2701.7 -1327.8  75.377  1  < 2.2e-16 ***

model.noi <- clmm(response ~ expression + (1|participant), data=cd)
summary(model.noi)
anova(model,model.noi)
# no.par    AIC  logLik LR.stat df Pr(>Chisq)
# model.noi     22 2699.7 -1327.8                      
# model         23 2701.7 -1327.8   7e-04  1     0.9794

# pairwise comparison
pc = lsmeans(model, pairwise ~ expression, adjust="tukey")
pc

# significant differences
# controls to everything but mo'a 'think'
# mo'a to CoS, hecha-kuaa, hecha-uka, hendu, juhu, kuaa, kuaa-uka, confess (marginal: almost, only, happy)
# NRRC to CoS, almost, hecha, say, mo'a, only (marginal: juhu)
# possNP to CoS, almost, hecha, say, juhu, only (marginal: kuaa)

# $contrasts
# contrast                                         estimate        SE df z.ratio p.value
# control - CoS                                -1.550477358 0.2809466 NA  -5.519  <.0001
# control - haimete\n 'almost'                 -1.529092967 0.2881837 NA  -5.306  <.0001
# control - hecha\n 'see'                      -1.356298681 0.2919815 NA  -4.645  0.0005
# control - hechakuaa\n 'realize'              -2.268646022 0.3033719 NA  -7.478  <.0001
# control - hechauka\n 'show'                  -2.025964183 0.3050150 NA  -6.642  <.0001
# control - 'e\n 'say'                         -1.371799668 0.2878575 NA  -4.766  0.0003
# control - hendu\n 'hear                      -2.277192506 0.3110231 NA  -7.322  <.0001
# control - juhu\n 'discover'                  -1.607342210 0.2882969 NA  -5.575  <.0001
# control - kuaa\n 'know'                      -1.736190203 0.2907959 NA  -5.970  <.0001
# control - kuaauka\n 'point out               -2.589821692 0.3248857 NA  -7.971  <.0001
# control - mo'a\n 'think'                     -0.335094342 0.2794244 NA  -1.199  0.9994
# control - mombe'u\n 'confess'                -2.283927822 0.3256445 NA  -7.014  <.0001
# control - NRRC                               -2.894464389 0.3351325 NA  -8.637  <.0001
# control - nte\n 'only'                       -1.493073798 0.2909095 NA  -5.132  <.0001
# control - possNP                             -3.046323540 0.3406516 NA  -8.943  <.0001
# control - topa\n 'discover'                  -2.110149440 0.2962835 NA  -7.122  <.0001
# control - vy'a\n 'happy'                     -1.836626318 0.3940357 NA  -4.661  0.0004
# CoS - haimete\n 'almost'                      0.021384390 0.3460890 NA   0.062  1.0000
# CoS - hecha\n 'see'                           0.194178677 0.3500405 NA   0.555  1.0000
# CoS - hechakuaa\n 'realize'                  -0.718168665 0.3566654 NA  -2.014  0.8632
# CoS - hechauka\n 'show'                      -0.475486825 0.3587996 NA  -1.325  0.9978
# CoS - 'e\n 'say'                              0.178677689 0.3464016 NA   0.516  1.0000
# CoS - hendu\n 'hear                          -0.726715149 0.3639684 NA  -1.997  0.8714
# CoS - juhu\n 'discover'                      -0.056864853 0.3458030 NA  -0.164  1.0000
# CoS - kuaa\n 'know'                          -0.185712846 0.3472575 NA  -0.535  1.0000
# CoS - kuaauka\n 'point out                   -1.039344334 0.3749258 NA  -2.772  0.3308
# CoS - mo'a\n 'think'                          1.215383016 0.3425450 NA   3.548  0.0412
# CoS - mombe'u\n 'confess'                    -0.733450464 0.3765881 NA  -1.948  0.8934
# CoS - NRRC                                   -1.343987032 0.3832302 NA  -3.507  0.0471
# CoS - nte\n 'only'                            0.057403559 0.3488027 NA   0.165  1.0000
# CoS - possNP                                 -1.495846182 0.3875604 NA  -3.860  0.0137
# CoS - topa\n 'discover'                      -0.559672082 0.3508884 NA  -1.595  0.9824
# CoS - vy'a\n 'happy'                         -0.286148961 0.4374332 NA  -0.654  1.0000
# haimete\n 'almost' - hecha\n 'see'            0.172794286 0.3561843 NA   0.485  1.0000
# haimete\n 'almost' - hechakuaa\n 'realize'   -0.739553055 0.3626219 NA  -2.039  0.8501
# haimete\n 'almost' - hechauka\n 'show'       -0.496871215 0.3648608 NA  -1.362  0.9969
# haimete\n 'almost' - 'e\n 'say'               0.157293299 0.3523803 NA   0.446  1.0000
# haimete\n 'almost' - hendu\n 'hear           -0.748099539 0.3698615 NA  -2.023  0.8587
# haimete\n 'almost' - juhu\n 'discover'       -0.078249243 0.3520846 NA  -0.222  1.0000
# haimete\n 'almost' - kuaa\n 'know'           -0.207097236 0.3532339 NA  -0.586  1.0000
# haimete\n 'almost' - kuaauka\n 'point out    -1.060728724 0.3808641 NA  -2.785  0.3223
# haimete\n 'almost' - mo'a\n 'think'           1.193998626 0.3485329 NA   3.426  0.0611
# haimete\n 'almost' - mombe'u\n 'confess'     -0.754834855 0.3821753 NA  -1.975  0.8814
# haimete\n 'almost' - NRRC                    -1.365371422 0.3891519 NA  -3.509  0.0469
# haimete\n 'almost' - nte\n 'only'             0.036019169 0.3548775 NA   0.101  1.0000
# haimete\n 'almost' - possNP                  -1.517230572 0.3933341 NA  -3.857  0.0138
# haimete\n 'almost' - topa\n 'discover'       -0.581056472 0.3570615 NA  -1.627  0.9784
# haimete\n 'almost' - vy'a\n 'happy'          -0.307533351 0.4420623 NA  -0.696  1.0000
# hecha\n 'see' - hechakuaa\n 'realize'        -0.912347341 0.3667512 NA  -2.488  0.5398
# hecha\n 'see' - hechauka\n 'show'            -0.669665502 0.3687936 NA  -1.816  0.9400
# hecha\n 'see' - 'e\n 'say'                   -0.015500987 0.3564191 NA  -0.043  1.0000
# hecha\n 'see' - hendu\n 'hear                -0.920893825 0.3736190 NA  -2.465  0.5576
# hecha\n 'see' - juhu\n 'discover'            -0.251043529 0.3559397 NA  -0.705  1.0000
# hecha\n 'see' - kuaa\n 'know'                -0.379891522 0.3576092 NA  -1.062  0.9999
# hecha\n 'see' - kuaauka\n 'point out         -1.233523011 0.3846445 NA  -3.207  0.1167
# hecha\n 'see' - mo'a\n 'think'                1.021204339 0.3520325 NA   2.901  0.2518
# hecha\n 'see' - mombe'u\n 'confess'          -0.927629141 0.3858463 NA  -2.404  0.6047
# hecha\n 'see' - NRRC                         -1.538165709 0.3927996 NA  -3.916  0.0110
# hecha\n 'see' - nte\n 'only'                 -0.136775117 0.3586098 NA  -0.381  1.0000
# hecha\n 'see' - possNP                       -1.690024859 0.3973500 NA  -4.253  0.0028
# hecha\n 'see' - topa\n 'discover'            -0.753850759 0.3611358 NA  -2.087  0.8240
# hecha\n 'see' - vy'a\n 'happy'               -0.480327637 0.4456448 NA  -1.078  0.9998
# hechakuaa\n 'realize' - hechauka\n 'show'     0.242681840 0.3742571 NA   0.648  1.0000
# hechakuaa\n 'realize' - 'e\n 'say'            0.896846354 0.3629775 NA   2.471  0.5529
# hechakuaa\n 'realize' - hendu\n 'hear        -0.008546484 0.3789802 NA  -0.023  1.0000
# hechakuaa\n 'realize' - juhu\n 'discover'     0.661303812 0.3622882 NA   1.825  0.9372
# hechakuaa\n 'realize' - kuaa\n 'know'         0.532455819 0.3632219 NA   1.466  0.9928
# hechakuaa\n 'realize' - kuaauka\n 'point out -0.321175669 0.3893535 NA  -0.825  1.0000
# hechakuaa\n 'realize' - mo'a\n 'think'        1.933551681 0.3607751 NA   5.359  <.0001
# hechakuaa\n 'realize' - mombe'u\n 'confess'  -0.015281800 0.3910513 NA  -0.039  1.0000
# hechakuaa\n 'realize' - NRRC                 -0.625818367 0.3970749 NA  -1.576  0.9844
# hechakuaa\n 'realize' - nte\n 'only'          0.775572224 0.3651697 NA   2.124  0.8026
# hechakuaa\n 'realize' - possNP               -0.777677517 0.4011814 NA  -1.938  0.8973
# hechakuaa\n 'realize' - topa\n 'discover'     0.158496583 0.3663880 NA   0.433  1.0000
# hechakuaa\n 'realize' - vy'a\n 'happy'        0.432019704 0.4499805 NA   0.960  1.0000
# hechauka\n 'show' - 'e\n 'say'                0.654164515 0.3649948 NA   1.792  0.9464
# hechauka\n 'show' - hendu\n 'hear            -0.251228324 0.3814595 NA  -0.659  1.0000
# hechauka\n 'show' - juhu\n 'discover'         0.418621972 0.3645356 NA   1.148  0.9996
# hechauka\n 'show' - kuaa\n 'know'             0.289773979 0.3656845 NA   0.792  1.0000
# hechauka\n 'show' - kuaauka\n 'point out     -0.563857509 0.3919764 NA  -1.438  0.9942
# hechauka\n 'show' - mo'a\n 'think'            1.690869841 0.3621466 NA   4.669  0.0004
# hechauka\n 'show' - mombe'u\n 'confess'      -0.257963639 0.3933056 NA  -0.656  1.0000
# hechauka\n 'show' - NRRC                     -0.868500207 0.3996462 NA  -2.173  0.7717
# hechauka\n 'show' - nte\n 'only'              0.532890385 0.3674893 NA   1.450  0.9937
# hechauka\n 'show' - possNP                   -1.020359357 0.4038049 NA  -2.527  0.5094
# hechauka\n 'show' - topa\n 'discover'        -0.084185257 0.3688396 NA  -0.228  1.0000
# hechauka\n 'show' - vy'a\n 'happy'            0.189337864 0.4520274 NA   0.419  1.0000
# 'e\n 'say' - hendu\n 'hear                   -0.905392838 0.3701542 NA  -2.446  0.5723
# 'e\n 'say' - juhu\n 'discover'               -0.235542542 0.3521943 NA  -0.669  1.0000
# 'e\n 'say' - kuaa\n 'know'                   -0.364390535 0.3537065 NA  -1.030  0.9999
# 'e\n 'say' - kuaauka\n 'point out            -1.218022024 0.3811868 NA  -3.195  0.1205
# 'e\n 'say' - mo'a\n 'think'                   1.036705327 0.3482570 NA   2.977  0.2113
# 'e\n 'say' - mombe'u\n 'confess'             -0.912128154 0.3824401 NA  -2.385  0.6195
# 'e\n 'say' - NRRC                            -1.522664721 0.3894361 NA  -3.910  0.0113
# 'e\n 'say' - nte\n 'only'                    -0.121274130 0.3550671 NA  -0.342  1.0000
# 'e\n 'say' - possNP                          -1.674523872 0.3939411 NA  -4.251  0.0028
# 'e\n 'say' - topa\n 'discover'               -0.738349772 0.3574204 NA  -2.066  0.8361
# 'e\n 'say' - vy'a\n 'happy'                  -0.464826650 0.4423072 NA  -1.051  0.9999
# hendu\n 'hear - juhu\n 'discover'             0.669850296 0.3696943 NA   1.812  0.9411
# hendu\n 'hear - kuaa\n 'know'                 0.541002303 0.3707246 NA   1.459  0.9932
# hendu\n 'hear - kuaauka\n 'point out         -0.312629185 0.3964801 NA  -0.789  1.0000
# hendu\n 'hear - mo'a\n 'think'                1.942098165 0.3674004 NA   5.286  <.0001
# hendu\n 'hear - mombe'u\n 'confess'          -0.006735316 0.3978179 NA  -0.017  1.0000
# hendu\n 'hear - NRRC                         -0.617271883 0.4040608 NA  -1.528  0.9888
# hendu\n 'hear - nte\n 'only'                  0.784118708 0.3724263 NA   2.105  0.8136
# hendu\n 'hear - possNP                       -0.769131033 0.4083682 NA  -1.883  0.9184
# hendu\n 'hear - topa\n 'discover'             0.167043067 0.3739793 NA   0.447  1.0000
# hendu\n 'hear - vy'a\n 'happy'                0.440566188 0.4559930 NA   0.966  1.0000
# juhu\n 'discover' - kuaa\n 'know'            -0.128847993 0.3530564 NA  -0.365  1.0000
# juhu\n 'discover' - kuaauka\n 'point out     -0.982479482 0.3804857 NA  -2.582  0.4669
# juhu\n 'discover' - mo'a\n 'think'            1.272247869 0.3484600 NA   3.651  0.0290
# juhu\n 'discover' - mombe'u\n 'confess'      -0.676585612 0.3820419 NA  -1.771  0.9518
# juhu\n 'discover' - NRRC                     -1.287122179 0.3887340 NA  -3.311  0.0866
# juhu\n 'discover' - nte\n 'only'              0.114268412 0.3547736 NA   0.322  1.0000
# juhu\n 'discover' - possNP                   -1.438981329 0.3930478 NA  -3.661  0.0280
# juhu\n 'discover' - topa\n 'discover'        -0.502807229 0.3566423 NA  -1.410  0.9954
# juhu\n 'discover' - vy'a\n 'happy'           -0.229284108 0.4420469 NA  -0.519  1.0000
# kuaa\n 'know' - kuaauka\n 'point out         -0.853631489 0.3815627 NA  -2.237  0.7286
# kuaa\n 'know' - mo'a\n 'think'                1.401095862 0.3504285 NA   3.998  0.0080
# kuaa\n 'know' - mombe'u\n 'confess'          -0.547737619 0.3830442 NA  -1.430  0.9946
# kuaa\n 'know' - NRRC                         -1.158274186 0.3897172 NA  -2.972  0.2137
# kuaa\n 'know' - nte\n 'only'                  0.243116405 0.3561138 NA   0.683  1.0000
# kuaa\n 'know' - possNP                       -1.310133336 0.3937538 NA  -3.327  0.0825
# kuaa\n 'know' - topa\n 'discover'            -0.373959236 0.3577729 NA  -1.045  0.9999
# kuaa\n 'know' - vy'a\n 'happy'               -0.100436115 0.4428688 NA  -0.227  1.0000
#  kuaauka\n 'point out - mo'a\n 'think'         2.254727350 0.3790854 NA   5.948  <.0001
#  kuaauka\n 'point out - mombe'u\n 'confess'    0.305893870 0.4080370 NA   0.750  1.0000
#  kuaauka\n 'point out - NRRC                  -0.304642698 0.4137880 NA  -0.736  1.0000
#  kuaauka\n 'point out - nte\n 'only'           1.096747894 0.3833844 NA   2.861  0.2751
#  kuaauka\n 'point out - possNP                -0.456501848 0.4177360 NA  -1.093  0.9998
#  kuaauka\n 'point out - topa\n 'discover'      0.479672252 0.3844280 NA   1.248  0.9989
#  kuaauka\n 'point out - vy'a\n 'happy'         0.753195373 0.4648973 NA   1.620  0.9794
#  mo'a\n 'think' - mombe'u\n 'confess'         -1.948833480 0.3798121 NA  -5.131  
#  mo'a\n 'think' - NRRC                        -2.559370048 0.3875299 NA  -6.604  <.0001<.0001
#  mo'a\n 'think' - nte\n 'only'                -1.157979457 0.3509754 NA  -3.299  0.0896
#  mo'a\n 'think' - possNP                      -2.711229198 0.3922542 NA  -6.912  <.0001
#  mo'a\n 'think' - topa\n 'discover'           -1.775055098 0.3547486 NA  -5.004  0.0001
#  mo'a\n 'think' - vy'a\n 'happy'              -1.501531977 0.4398122 NA  -3.414  0.0634
#  mombe'u\n 'confess' - NRRC                   -0.610536567 0.4153314 NA  -1.470  0.9926
#  mombe'u\n 'confess' - nte\n 'only'            0.790854024 0.3846165 NA   2.056  0.8413
#  mombe'u\n 'confess' - possNP                 -0.762395718 0.4194356 NA  -1.818  0.9394
#  mombe'u\n 'confess' - topa\n 'discover'       0.173778382 0.3861715 NA   0.450  1.0000
#  mombe'u\n 'confess' - vy'a\n 'happy'          0.447301504 0.4661282 NA   0.960  1.0000
#  NRRC - nte\n 'only'                           1.401390591 0.3914790 NA   3.580  0.0370
#  NRRC - possNP                                -0.151859150 0.4246792 NA  -0.358  1.0000
#  NRRC - topa\n 'discover'                      0.784314950 0.3923638 NA   1.999  0.8703
#  NRRC - vy'a\n 'happy'                         1.057838071 0.4716786 NA   2.243  0.7247
#  nte\n 'only' - possNP                        -1.553249742 0.3958394 NA  -3.924  0.0107
#  nte\n 'only' - topa\n 'discover'             -0.617075642 0.3598411 NA  -1.715  0.9642
#  nte\n 'only' - vy'a\n 'happy'                -0.343552520 0.4444872 NA  -0.773  1.0000
#  possNP - topa\n 'discover'                    0.936174100 0.3965610 NA   2.361  0.6381
#  possNP - vy'a\n 'happy'                       1.209697221 0.4752312 NA   2.545  0.4950
#  topa\n 'discover' - vy'a\n 'happy'            0.273523121 0.4457154 NA   0.614  1.0000
# 
# P value adjustment: tukey method for comparing a family of 18 estimates 



######## order effects ----
agr = aggregate(response ~ list, data=cd, FUN="mean")
agr$CILow = aggregate(response ~ list, data=cd, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ list, data=cd, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
dodge = position_dodge(.9)
agr
# AB 3.55, BA 3.41

# were participants' responses different on their first list than on their second list?
# participants on list AB: 1...40 i.e., 1...20 (first) and then 21...40 (second)
# participants on list BA: 40...1, i.e. 40...21 (first) and then 20...1 (second)
names(cd)
# add column "order" that encodes for each item 
# whether it was first or second for that participant
cd$order <- ifelse(cd$list == "AB" & cd$itemNr < 21, "first", 
                   ifelse(cd$list == "BA" & cd$itemNr > 20, "first", "second"))
table(cd$order)
table(cd$order,cd$itemNr)
str(cd$order)
cd$order <- as.factor(cd$order)
str(cd$response)
cd$response <- as.numeric(cd$response)

cd.target$order <- ifelse(cd.target$list == "AB" & cd.target$itemNr < 21, "first", 
                   ifelse(cd.target$list == "BA" & cd.target$itemNr > 20, "first", "second"))
table(cd.target$order)
table(cd.target$order,cd.target$itemNr)
str(cd.target$order)
cd.target$order <- as.factor(cd.target$order)
str(cd.target$response)
cd.target$response <- as.numeric(cd.target$response)

agr = aggregate(response ~ order, data=cd, FUN="mean")
agr$CILow = aggregate(response ~ order, data=cd, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ order, data=cd, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=order,y=response)) +
  geom_bar(stat="identity",position=dodge) +
  #geom_bar(aes(fill = gender), position = dodge, stat = "identity") +
  #geom_bar(stat="identity",fill="white",color="black",position=dodge) +
  #geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) + 
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  xlab("Order") +
  ylab("Mean certainty rating") +
  ggsave(f="graphs/mean-response-by-order.pdf",height=3,width=4)

# did participants' responses to a particular expression change from the first time
# they heard the expression to the second time they heard it?
agr = aggregate(response ~ expression + order, data=cd, FUN="mean")
agr$CILow = aggregate(response ~ expression + order, data=cd, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ expression + order, data=cd, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=order,y=response)) +
  geom_bar(stat="identity",position=dodge) +
  #geom_bar(aes(fill = gender), position = dodge, stat = "identity") +
  #geom_bar(stat="identity",fill="white",color="black",position=dodge) +
  #geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) + 
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  facet_wrap(~expression) +
  xlab("Order") +
  ylab("Mean certainty rating") +
  ggsave(f="graphs/mean-response-by-expression-and-order.pdf",height=10,width=6)

# by ID and order
str(cd.target$response)
str(cd.target$order)

agr = aggregate(response ~ ID + order, data=cd.target, FUN="mean")
agr$CILow = aggregate(response ~ ID + order, data=cd.target, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ ID + order, data=cd.target, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=order,y=response)) +
  geom_bar(stat="identity",position=dodge) +
  #geom_bar(aes(fill = gender), position = dodge, stat = "identity") +
  #geom_bar(stat="identity",fill="white",color="black",position=dodge) +
  #geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) + 
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  facet_wrap(~ID) +
  xlab("Order") +
  ylab("Mean certainty rating") +
  ggsave(f="graphs/mean-response-by-target-ID-and-order.pdf",height=10,width=6)

