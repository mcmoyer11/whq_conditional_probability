#  Stats for Con_Prob
# Date: June 2, 2019
############################
# Stats for Likert Study
############################
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(ggpubr)
library(psych)
library(likert)
library(reshape2)
library(lme4)
library(languageR)
library(ordinal)
library(lmerTest)
library(FSA)
library(lattice)
library(boot)
library(rcompanion)
library(MASS)
library(Hmisc)
library(reshape2)
library(foreign)
# First, set the working directory
setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Conditional_Probability/results/")
source("../../helpers.R")

# Then import the data in
d <- read.csv("answer.csv", header = TRUE)
d <- d %>% drop_na()
length(unique(d$subject)) # 238
d$likert.f <- as.factor(d$likert)
head(d)
# This just gets a table of all these things
lapply(d[, c("likert", "answer", "task", "modal")], table)

# this also makes a table
# it would be great to figure out how to just plot this
# three-way cross tabs
ftable(xtabs(~ task + likert + answer + modal, data = d))

# this is a box plot with the dots
ggplot(d, aes(x = answer, y = likert)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(task ~ modal, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



############################
############################
# REGRESSIONS
############################
############################
m <- clm(likert.f ~ answer + modal + wh, data = d, Hess=TRUE)
summary(m)

# get the odds ratio by exponentiating the coefficients
exp(coef(m))
# answerMO   answerMS modalnomod    whwhere      whwho 
# 0.5014465  0.4687628  0.5895296  1.3302566  1.2381135 
ci <- confint(m)
exp(cbind(OR = coef(m), ci))
#               OR     2.5 %    97.5 %
# answerMO   0.5014465 0.4250736 0.5911780
# "for a 1 unit increase in answerMO (???) we expect a .17 increase 
# in expected Likert value on the log odds scale, holding all other
# variable constant"

# answerMS   0.4687628 0.3957725 0.5548741
# modalnomod 0.5895296 0.5155667 0.6738690
# whwhere    1.3302566 1.1303574 1.5658225
# whwho      1.2381135 1.0522083 1.4571030



sim <- clm(likert.f ~ answer + modal + wh, data = d, link = "logit", Hess=TRUE)
summary(sim)
com <- clm(likert.f ~ answer + answer*modal, data = d, link = "logit", Hess=TRUE)
summary(com)
anova(sim, com)

# Trying to get the probabilities
# SOURCE: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
d$modal.f <- as.factor(d$modal)
d$wh.f <- as.factor(d$wh)
d$answer.f <- as.factor(d$answer)

likely <- subset(d, d$task=="likely")
accept <- subset(d, d$task=="accept")

m <- clm(likert.f ~ answer + modal + wh, data = likely, Hess=TRUE)
summary(m)

# in accept, wh was not a significant predictor
# but it was in likely
t <- clm(likert.f ~ answer*modal, data = d, link="logit", Hess=TRUE)
summary(t)


anova(t,m)


## Fitted values with standard errors and confidence intervals:
predict(m, se.fit=TRUE, interval=TRUE) # type="prob"
## class predictions for the observations:
predict(m, type="class")

newData <- with(d,expand.grid(answer = c("MO","MS","MA"),
                       modal = c("mod","nomod"),
                       wh = c("who","where","how")))

## Predicted probabilities in all five response categories for each of
## the four cases in newData:
p <- predict(m, newdata=newData, type="prob")
## now include standard errors and intervals:
p2 <- predict(m, newdata=newData, se.fit=TRUE, interval=TRUE, type="prob")


probs <- cbind(newData,p)
head(probs)
# i need to make the wide into long
probs_long <- gather(probs, likert_rating, Probability, fit.0:fit.5, factor_key=TRUE)

head(probs_long)

ggplot(probs_long, aes(x = modal, y = Probability, colour = likert_rating)) +
  geom_point() +
  facet_grid(wh ~ answer, labeller="label_both")


library(effects)
plot(allEffects(m))


t_like <- polr(likert.f~wh+answer+modal+answer*modal+answer*modal*wh, data = like, Hess=T)
summary(t_like)

coeffs <- coef(summary(t_like)) 
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))

t_acc <- polr(likert.f~wh+answer+modal, data = acc, Hess=T)
summary(t_acc)

taskModd <- clm(likert.f ~ wh + answer + modal + answer*modal + answer*modal*wh, link="logit", data=d)
summary(taskModd)



m_like <- clm(likert.f ~ wh + answer + modal + answer*modal + answer*modal*wh, link="logit", data=like)
summary(m_like)
m_like_0 <- clm(likert.f ~ wh + answer + modal, link="logit", data=like)
anova(m_like, m_like_0)

g <- glmer(likert.f ~ wh + answer + modal + (1|subject), family="binomial", data=like)
summary(g)

anova(g, m_like_0)

m_acc <- clm(likert.f ~ wh + answer + modal + answer*modal + answer*modal*wh, link="logit", data=acc)
summary(m_acc)

############################
############################
### LOOKING AT MEDIANS
############################
############################

agr = d %>%
  group_by(answer, modal, wh) %>%
  summarise(mean_likert = mean(likert), CILow = ci.low(likert), CIHigh = ci.high(likert)) %>%
  mutate(YMin = mean_likert - CILow, YMax = mean_likert + CIHigh)
dodge = position_dodge(.9)
  # summarise(median = median(likert), mean = mean(likert), variance = var(likert))
View(agr)
ggplot(agr, aes(answer,y=mean_likert,fill=modal)) +
  facet_wrap(~wh) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge)
 

# DEfailt boxplot
ggplot(d, aes(answer,y=likert,fill=modal)) +
  facet_wrap(~wh) +
  geom_boxplot()

vrnc = d %>%
  group_by(task,answer, modal, wh) %>%
  summarise(mean_likert = mean(likert), variance = var(likert))
dodge = position_dodge(.9)
View(vrnc)

ggplot(vrnc, aes(answer,y=variance,fill=modal)) +
  facet_grid(task ~ wh, margins = TRUE) +
  geom_bar(position=dodge,stat="identity") +
  ggtitle("Variance")
  # geom_bar()

min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# ggplot code
d %>%
  filter(modal %in% c("mod")) %>%
  ggplot(., aes(answer, y = likert)) +
    facet_wrap(~wh) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
    geom_jitter(position=position_jitter(width=.1), size=.5) + 
    ggtitle("Modal: Boxplot with mean, 95%CI, min and max.") + 
    xlab("Answer") + 
    ylab("Likert")


d %>%
  filter(modal %in% c("nomod")) %>%
  ggplot(., aes(answer, y = likert)) +
  facet_wrap(~wh) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
  geom_jitter(position=position_jitter(width=.1), size=.5) + 
  ggtitle("NoModal: Boxplot with mean, 95%CI, min and max.") + 
  xlab("Answer") + 
  ylab("Likert")

########################################################
########################################################
# TESTING VARIANCE - HOMOSCEDASTICITY
########################################################
########################################################
# Compute a Fligner-Killeen test (can violate assumptions of normality)
# http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/

########################################################
# FILGNER-KILLEEN TEST ACROSS ALL DATA
# http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/

fligner.test(likert~ modal, data = d)
# Fligner-Killeen:med chi-squared = 6.79, df = 1, p-value = 0.009167 **
fligner.test(likert~ wh, data = d)
# Fligner-Killeen:med chi-squared = 10.336, df = 2, p-value = 0.005697 **

fligner.test(likert~ task, data = d)
# Fligner-Killeen:med chi-squared = 2.3863, df = 1, p-value = 0.1224

fligner.test(likert ~ interaction(modal,wh), data = d)
# Fligner-Killeen:med chi-squared = 20.355, df = 5, p-value = 0.001072 **


mo = d %>%
  filter(answer %in% c("MO"))
fligner.test(likert ~ modal, data = mo)
# Fligner-Killeen:med chi-squared = 2.3765, df = 1, p-value = 0.1232
fligner.test(likert ~ wh, data = mo)
# Fligner-Killeen:med chi-squared = 9.2457, df = 2, p-value = 0.009825 ***
fligner.test(likert ~ task, data = mo)
# Fligner-Killeen:med chi-squared = 0.00092063, df = 1, p-value = 0.9758

fligner.test(likert ~ interaction(modal,wh), data = mo)
# Fligner-Killeen:med chi-squared = 4.5903, df = 5, p-value = 0.4679
fligner.test(likert ~ interaction(modal,task), data = mo)
# Fligner-Killeen:med chi-squared = 8.5125, df = 3, p-value = 0.03653 *
fligner.test(likert ~ interaction(wh,task), data = mo)
# Fligner-Killeen:med chi-squared = 14.141, df = 5, p-value = 0.01474 *

fligner.test(likert ~ interaction(modal,wh,task), data = mo)
# Fligner-Killeen:med chi-squared = 22.014, df = 11, p-value = 0.02427 *


ms = d %>%
  filter(answer %in% c("MS"))
fligner.test(likert ~ modal, data = ms)
# Fligner-Killeen:med chi-squared = 0.27684, df = 1, p-value = 0.5988
fligner.test(likert ~ wh, data = ms)
# Fligner-Killeen:med chi-squared = 3.9014, df = 2, p-value = 0.1422
fligner.test(likert ~ task, data = ms)
# Fligner-Killeen:med chi-squared = 0.0007582, df = 1, p-value = 0.978

fligner.test(likert ~ interaction(modal,wh), data = ms)
# Fligner-Killeen:med chi-squared = 8.7589, df = 5, p-value = 0.1191
fligner.test(likert ~ interaction(modal,task), data = ms)
# Fligner-Killeen:med chi-squared = 1.1632, df = 3, p-value = 0.7618
fligner.test(likert ~ interaction(wh,task), data = ms)
# Fligner-Killeen:med chi-squared = 6.9675, df = 5, p-value = 0.2231

fligner.test(likert ~ interaction(modal,wh,task), data = ms)
# Fligner-Killeen:med chi-squared = 15.564, df = 11, p-value = 0.1581

moma = d %>%
  filter(answer %in% c("MO","MA"))
fligner.test(likert~ answer, data = moma)
# Fligner-Killeen:med chi-squared = 3.8664, df = 1, p-value = 0.04926


msmo = d %>%
  filter(answer %in% c("MS","MO"))
fligner.test(likert ~ interaction(modal,wh), data = msmo)
# Fligner-Killeen:med chi-squared = 5.5405, df = 5, p-value = 0.3535
fligner.test(likert ~ modal, data = msmo)
# Fligner-Killeen:med chi-squared = 1.2521, df = 1, p-value = 0.2632
fligner.test(likert ~ wh, data = msmo)
# Fligner-Killeen:med chi-squared = 8.2388, df = 2, p-value = 0.01625

########################################################
# Q1: is the variance in non-modal who-questions LESS than the variance in modal who-questions?
########################################################

########################################################
# FILGNER-KILLEEN TEST
# http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/
whomsmo = d %>%
  filter(wh %in% c("who") & answer %in% c("MS","MO"))
fligner.test(likert ~ modal, data = whomsmo)
# Fligner-Killeen:med chi-squared = 0.39531, df = 1, p-value = 0.5295

agr_whomsmo = whomsmo %>%
  group_by(task, modal) %>%
  summarise(median = median(likert), variance = var(likert))
View(agr_whomsmo)

########################################################
# MOOD TEST 
# https://stats.stackexchange.com/questions/36330/question-about-homoscedasticity-test
modwho = d %>%
  filter(wh %in% c("who") & modal %in% c("mod") & answer %in% c("MO", "MS"))
nomodwho = d%>%
  filter(wh %in% c("who") & modal %in% c("nomod") & answer %in% c("MO", "MS"))


mood.test(modwho$likert.f, modwho$likert.f, alternative = "less")
# data:  modwho$likert.f and modwho$likert.f
# Z = 0.27626, p-value = 0.7823
# alternative hypothesis: two.sided



########################################################
# Q2: is the variance in modal how-questions LESS than the variance in non-modal how-questions?
########################################################

########################################################
# Filgner-KilLeen Test
howmsmo = d %>%
  filter(wh %in% c("how") & answer %in% c("MS","MO"))
fligner.test(likert ~ modal, data = howmsmo)
# Fligner-Killeen:med chi-squared = 0.04828, df = 1, p-value = 0.8261

agr_howmsmo = howmsmo %>%
  group_by(task, modal, answer) %>%
  summarise(median = mean(likert), variance = var(likert))
View(agr_howmsmo)

########################################################
# MOOD TEST
modhow = d %>%
  filter(wh %in% c("how") & modal %in% c("mod") & answer %in% c("MO", "MS"))
nomodhow = d%>%
  filter(wh %in% c("how") & modal %in% c("nomod") & answer %in% c("MO", "MS"))
mood.test(modhow$likert.f, modhow$likert.f)
# data:  modhow$likert.f and modhow$likert.f
# Z = -4.5555, p-value = 5.226e-06
# alternative hypothesis: two.sided



# Subsetting to other conditions
wheremsmo = d %>%
  filter(wh %in% c("where") & answer %in% c("MS","MO"))
fligner.test(likert ~ modal, data = wheremsmo)
# Fligner-Killeen:med chi-squared = 0.23799, df = 1, p-value = 0.6257

################################################################
# TURNS OUT VARIANCE IS SD FOR MA ANSWERS.....
ma = d %>%
  filter(answer %in% c("MA"))
fligner.test(likert ~ modal, data = ma)
# Fligner-Killeen:med chi-squared = 6.5618, df = 1, p-value = 0.0104 *
fligner.test(likert ~ wh, data = ma)
# Fligner-Killeen:med chi-squared = 5.1021, df = 2, p-value = 0.078 
fligner.test(likert ~ interaction(modal,wh), data = ma)
# Fligner-Killeen:med chi-squared = 12.357, df = 5, p-value = 0.03021 *


wherema = ma %>%
  filter(wh %in% c("where"))
fligner.test(likert ~ modal, data = wherema)
# Fligner-Killeen:med chi-squared = 0.50461, df = 1, p-value = 0.4775
whoma = ma %>%
  filter(wh %in% c("who"))
fligner.test(likert ~ modal, data = whoma)
# Fligner-Killeen:med chi-squared = 0.09484, df = 1, p-value = 0.7581
howma = ma %>%
  filter(wh %in% c("how"))
fligner.test(likert ~ modal, data = howma)
# Fligner-Killeen:med chi-squared = 8.3967, df = 1, p-value = 0.003759 **
hist(howma$likert)

howmamod = howma %>%
  filter(modal %in% c("mod"))
howmanomod = howma %>%
  filter(modal %in% c("nomod"))

hist(howmamod$likert, col="pink")
hist(howmanomod$likert, col="pink")
# check to make sure they have the same number of observations --- yes they do
nrow(howmamod)
nrow(howmamod)

agr_howma = howma %>%
  group_by(task, modal) %>%
  summarise(median = median(likert))
View(agr_howma)
############################
############################
# Pairwise comparisons
############################
############################
kruskal.test(likert~answer, data = d)
# Kruskal-Wallis chi-squared = 90.291, df = 2, p-value < 2.2e-16
pairwise.wilcox.test(d$likert, d$answer, p.adjust.method = "BH")
#     MA      MO  
# MO 2.6e-14 -   
# MS < 2e-16 0.21

kruskal.test(likert~modal, data = d)
# Kruskal-Wallis chi-squared = 57.125, df = 1, p-value = 4.09e-14

kruskal.test(likert~wh, data = d)
# Kruskal-Wallis chi-squared = 11.893, df = 2, p-value = 0.002615

d_ms <- subset(d, d$answer=="MS")
d_mo <- subset(d, d$answer=="MO")
d_msmo <- rbind(d_ms, d_mo)
d_ma <- subset(d, d$answer=="MA")
d_msma = rbind(d_ms, d_ma)

d_mo_nomod = subset(d_mo, d_mo$modal=="nomod")
d_mo_mod = subset(d_mo, d_mo$modal=="mod")

d_ms_nomod = subset(d_ms, d_ms$modal=="nomod")
d_ms_mod = subset(d_ms, d_ms$modal=="mod")

d_ma_nomod = subset(d_ma, d_ma$modal=="nomod")
d_ma_mod = subset(d_ma, d_ma$modal=="mod")

str(d)
##############task between modal
kruskal.test(likert~task, data = d_mo_nomod)
# Kruskal-Wallis chi-squared = 15.105, df = 1, p-value = 0.0001017
kruskal.test(likert~task, data = d_mo_mod)
# Kruskal-Wallis chi-squared = 53.221, df = 1, p-value = 2.98e-13

kruskal.test(likert~task, data = d_ms_nomod)
# Kruskal-Wallis chi-squared = 0.12794, df = 1, p-value = 0.7206
kruskal.test(likert~task, data = d_ms_mod)
# Kruskal-Wallis chi-squared = 1.4354, df = 1, p-value = 0.2309

kruskal.test(likert~task, data = d_ma_nomod)
# Kruskal-Wallis chi-squared = 70.854, df = 1, p-value < 2.2e-16
kruskal.test(likert~task, data = d_ma_mod)
# Kruskal-Wallis chi-squared = 53.221, df = 1, p-value = 2.98e-13

########## MS.MO

kruskal.test(likert~answer, data = d_msmo)
# W = 479252, p-value = 0.2098
# Kruskal-Wallis chi-squared = 1.5726, df = 1, p-value = 0.2098

kruskal.test(likert~answer, data = d_msma)
# Kruskal-Wallis chi-squared = 77.884, df = 1, p-value < 2.2e-16

###### MODAL in MS/MO
wilcox.test(likert~modal, data = d_ms)
# W = 113189, p-value = 0.00152
wilcox.test(likert~modal, data = d_mo)
# W = 175122, p-value < 2.2e-16
wilcox.test(likert~modal, data = d_msmo)
# W = 574676, p-value < 2.2e-16

###### WH in MS/MO
kruskal.test(likert~wh, data = d_ms)
# Kruskal-Wallis chi-squared = 2.6723, df = 2, p-value = 0.2629
kruskal.test(likert~wh, data = d_mo)
# Kruskal-Wallis chi-squared = 6.1802, df = 2, p-value = 0.0455
kruskal.test(likert~wh, data = d_msmo)
# Kruskal-Wallis chi-squared = 8.5171, df = 2, p-value = 0.01414


########## MA
wilcox.test(likert~modal, data = d_ma)
# W = 107706, p-value = 0.7186
kruskal.test(likert~wh, data = d_ma)
# Kruskal-Wallis chi-squared = 9.6732, df = 2, p-value = 0.007934
wilcox.test(likert~modal, data = d_ma)


#########################
##### LIKELY TASK #######

# But separaing by task
like_ms = subset(likely, likely$answer=="MS")
like_mo = subset(likely, likely$answer=="MO")
like_ma = subset(likely, likely$answer=="MA")
like_msmo = rbind(like_ms, like_mo)
like_msma = rbind (like_ms, like_ma)


like_mo_nomod = subset(like_mo, like_mo$modal=="nomod")
like_mo_mod = subset(like_mo, like_mo$modal=="mod")

like_ms_nomod = subset(like_ms, like_ms$modal=="nomod")
like_ms_mod = subset(like_ms, like_ms$modal=="mod")

like_ma_nomod = subset(like_ma, like_ma$modal=="nomod")
like_ma_mod = subset(like_ma, like_ma$modal=="mod")

kruskal.test(likert~answer, data = likely)
# Kruskal-Wallis chi-squared = 17.261, df = 2, p-value = 0.0001786 ***

interLMA <- interaction(likely$modal, likely$answer)
kruskal.test(likert~interLMA, data = likely)
# Kruskal-Wallis chi-squared = 73.137, df = 5, p-value = 2.276e-14 ***

interLMAW <- interaction(likely$modal, likely$answer, likely$wh)
kruskal.test(likert~interLMAW, data = likely)
# Kruskal-Wallis chi-squared = 107.39, df = 17, p-value = 3.726e-15 ***


kruskal.test(likert~answer, data = like_msmo)
# Kruskal-Wallis chi-squared = 7.8094, df = 1, p-value = 0.005197
kruskal.test(likert~answer, data = like_msma)
# Kruskal-Wallis chi-squared = 1.4519, df = 1, p-value = 0.2282



########## MODAL
wilcox.test(likert~modal, data = likely)
# W = 327883, p-value = 3.846e-11
kruskal.test(likert~modal, data = likely)
# Kruskal-Wallis chi-squared = 43.691, df = 1, p-value = 3.844e-11

# kruskal.test(likert~modal, data = like_ms)
# W = 29620, p-value = 0.007707 **
# Kruskal-Wallis chi-squared = 7.1023, df = 1, p-value = 0.007698

# kruskal.test(likert~modal, data = like_mo)
# W = 50357, p-value = 5.806e-11 ***
# Kruskal-Wallis chi-squared = 42.889, df = 1, p-value = 5.795e-11

wilcox.test(likert~modal, data = like_msmo)
# W = 157035, p-value = 3.5e-10 ***
kruskal.test(likert~modal, data = like_ma)
# W = 31306, p-value = 0.005207 **
# Kruskal-Wallis chi-squared = 7.8079, df = 1, p-value = 0.005202


########## WH
kruskal.test(likert~wh, data = likely)
# Kruskal-Wallis chi-squared = 19.206, df = 2, p-value = 6.751e-05

# kruskal.test(likert~wh, data = like_ms)
# Kruskal-Wallis chi-squared = 5.3104, df = 2, p-value = 0.07028
# kruskal.test(likert~wh, data = like_mo)
# Kruskal-Wallis chi-squared = 2.4228, df = 2, p-value = 0.2978
kruskal.test(likert~wh, data = like_msmo)
# Kruskal-Wallis chi-squared = 4.7487, df = 2, p-value = 0.09308
kruskal.test(likert~wh, data = like_ma)
# Kruskal-Wallis chi-squared = 22.555, df = 2, p-value = 1.265e-05 ***


# kruskal.test(likert~wh, data = like_ma_mod)
# Kruskal-Wallis chi-squared = 7.3343, df = 2, p-value = 0.02555
pairwise.wilcox.test(like_ma_mod$likert, like_ma_mod$wh, p.adjust.method = "BH")
#       how   where
#   where 0.056 -    
#   who   0.036 0.628

kruskal.test(likert~wh, data = like_ma_nomod)
# Kruskal-Wallis chi-squared = 17.093, df = 2, p-value = 0.0001943

pairwise.wilcox.test(like_ma_nomod$likert, like_ma_nomod$wh, p.adjust.method = "BH")
#         how     where  
#   where 0.01208 -      
#   who   0.00019 0.11631

# kruskal.test(likert~wh, data = like_mo_mod)
# Kruskal-Wallis chi-squared = 1.8596, df = 2, p-value = 0.3946
# kruskal.test(likert~wh, data = like_mo_nomod)
# Kruskal-Wallis chi-squared = 1.1008, df = 2, p-value = 0.5767

# kruskal.test(likert~wh, data = like_ms_mod)
# Kruskal-Wallis chi-squared = 3.3526, df = 2, p-value = 0.1871
# pairwise.wilcox.test(like_ms_nomod$likert, like_ms_nomod$wh, p.adjust.method = "BH")
#       how  where
#   where 0.33 -    
#   who   0.33 0.92

# kruskal.test(likert~wh, data = like_ms_nomod)
# Kruskal-Wallis chi-squared = 2.1725, df = 2, p-value = 0.3375
# pairwise.wilcox.test(like_ms_mod$likert, like_ms_mod$wh, p.adjust.method = "BH")
#         how  where
#   where 0.43 -    
#   who   0.17 0.43 

#########################
##### ACCEPT TASK #######

acc_ms = subset(accept, accept$answer=="MS")
acc_mo = subset(accept, accept$answer=="MO")
acc_ma = subset(accept, accept$answer=="MA")
acc_msmo = rbind(acc_ms, acc_mo)
acc_msma = rbind(acc_ms, acc_ma)

acc_mo_nomod = subset(acc_mo, acc_mo$modal=="nomod")
acc_mo_mod = subset(acc_mo, acc_mo$modal=="mod")

acc_ms_nomod = subset(acc_ms, acc_ms$modal=="nomod")
acc_ms_mod = subset(acc_ms, acc_ms$modal=="mod")

acc_ma_nomod = subset(acc_ma, acc_ma$modal=="nomod")
acc_ma_mod = subset(acc_ma, acc_ma$modal=="mod")

kruskal.test(likert~answer, data = accept)
# Kruskal-Wallis chi-squared = 129.52, df = 2, p-value < 2.2e-16

kruskal.test(likert~answer, data = acc_msma)
# Kruskal-Wallis chi-squared = 132.28, df = 1, p-value < 2.2e-16


kruskal.test(likert~answer, data = acc_msmo)
# Kruskal-Wallis chi-squared = 25.278, df = 1, p-value = 4.963e-07
kruskal.test(likert~modal, data = acc_msmo)
# Kruskal-Wallis chi-squared = 53.879, df = 1, p-value = 2.133e-13

interAMA <- interaction(accept$modal, accept$answer)
kruskal.test(likert~interAMA, data = accept)
# Kruskal-Wallis chi-squared = 208.55, df = 5, p-value < 2.2e-16

interAMAW <- interaction(accept$modal, accept$answer, accept$wh)
kruskal.test(likert~interAMAW, data = accept)
# Kruskal-Wallis chi-squared = 222.11, df = 17, p-value < 2.2e-16


########## MODAL
kruskal.test(likert~modal, data = accept)
# Kruskal-Wallis chi-squared = 24.87, df = 1, p-value = 6.134e-07

# kruskal.test(likert~modal, data = acc_ms)
# W = 26893, p-value = 0.08209
# Kruskal-Wallis chi-squared = 3.0243, df = 1, p-value = 0.08203

# kruskal.test(likert~modal, data = acc_mo)
# W = 39004, p-value < 2.2e-16 ***
# Kruskal-Wallis chi-squared = 70.108, df = 1, p-value < 2.2e-16


wilcox.test(likert~modal, data = acc_msmo)
# W = 132130, p-value = 2.135e-13 ***
kruskal.test(likert~modal, data = acc_ma)
# W = 23277, p-value = 0.05648
# Kruskal-Wallis chi-squared = 3.6394, df = 1, p-value = 0.05643


########## WH
kruskal.test(likert~wh, data = accept)
# Kruskal-Wallis chi-squared = 1.6813, df = 2, p-value = 0.4314


# kruskal.test(likert~wh, data = acc_ms)
# Kruskal-Wallis chi-squared = 3.9988, df = 2, p-value = 0.1354
# kruskal.test(likert~wh, data = acc_mo)
# Kruskal-Wallis chi-squared = 3.2193, df = 2, p-value = 0.2
# pairwise.wilcox.test(acc_mo$likert,acc_mo$wh, p.adjust.method = "BH")
#         how   where
#   where 0.191 -    
#   who   0.534 0.093


kruskal.test(likert~wh, data = acc_msmo)
# Kruskal-Wallis chi-squared = 5.3384, df = 2, p-value = 0.06931
kruskal.test(likert~wh, data = acc_ma)
# Kruskal-Wallis chi-squared = 0.65125, df = 2, p-value = 0.7221


# kruskal.test(likert~wh, data = acc_ma_mod)
# Kruskal-Wallis chi-squared = 2.0695, df = 2, p-value = 0.3553

# kruskal.test(likert~wh, data = acc_ma_nomod)
# Kruskal-Wallis chi-squared = 0.47291, df = 2, p-value = 0.7894


# kruskal.test(likert~wh, data = acc_mo_mod)
# Kruskal-Wallis chi-squared = 4.1299, df = 2, p-value = 0.1268

# kruskal.test(likert~wh, data = acc_mo_nomod)
# Kruskal-Wallis chi-squared = 4.8851, df = 2, p-value = 0.08694
# pairwise.wilcox.test(acc_mo_nomod$likert, acc_mo_nomod$wh, p.adjust.method = "BH")

# kruskal.test(likert~wh, data = acc_ms_mod)
# Kruskal-Wallis chi-squared = 1.2139, df = 2, p-value = 0.545
# pairwise.wilcox.test(acc_ms_mod$likert, acc_ms_mod$wh, p.adjust.method = "BH")
#       how  where
#   where 0.54 -    
#   who   0.85 0.54

# kruskal.test(likert~wh, data = acc_ms_nomod)
# Kruskal-Wallis chi-squared = 5.6656, df = 2, p-value = 0.05885
# pairwise.wilcox.test(acc_ms_nomod$likert, acc_ms_nomod$wh, p.adjust.method = "BH")
#       how  where
#   where 0.95 -    
#   who   0.06 0.06 



# Looking at task overall
barplot(table(d_mo$task,d_mo$likert), beside=T,
        cex.names=0.7,legend.text=c("likely","accept"),
        args.legend=list(x=9,y=200,cex=0.8),
        col=c("pink","turquoise"))

# Overall effect of task
wilcox.test(likert~task,data=d)
# W = 1200000, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
kruskal.test(likert.f ~ task, data = d)

like = subset(d, d$task=="likely")
acc = subset(d, d$task=="accept")

wilcox.test(likert~modal, data = like)
# W = 327883, p-value = 3.846e-11
wilcox.test(likert~modal, data = acc)
# W = 267826, p-value = 6.137e-07


# Overall effect of Answer
kruskal.test(likert.f~answer, data = d)
# Kruskal-Wallis chi-squared = 90.291, df = 2, p-value < 2.2e-16
pairwise.wilcox.test(d$likert, d$answer, p.adjust.method = "BH")
  #     MA      MO  
  # MO 2.6e-14 -   
  # MS < 2e-16 0.21


interTA <- interaction(d$task, d$answer)
kruskal.test(likert~interTA, data = d)
# Kruskal-Wallis chi-squared = 217.54, df = 5, p-value < 2.2e-16


# Looking at modal overall
barplot(table(d$modal,d$likert), beside=T,
        cex.names=0.7,legend.text=c("modal","nomodal"),
        args.legend=list(x=9,y=300,cex=0.8),
        col=c("pink","light blue"))

# Overall effect of modal
wilcox.test(likert~modal,data=d)
# W = 1178500, p-value = 4.09e-14

interTM <- interaction(d$task, d$modal)
kruskal.test(likert~interTM, data = d)
# Kruskal-Wallis chi-squared = 140.53, df = 3, p-value < 2.2e-16

# Looking at ANSWER overall
barplot(table(d$answer,d$likert), beside=T,
        cex.names=0.7,legend.text=c("MO", "MS","MA"),
        args.legend=list(x=9,y=300,cex=0.8),
        col=c("pink","light blue","red"))

# Overall effect of answer
kruskal.test(likert~answer, data=d)
# Kruskal-Wallis chi-squared = 90.291, df = 2, p-value < 2.2e-16

# Finding exactly what is driving the difference
pairwise.wilcox.test(d$likert,d$answer, p.adjust.method = "BH")
# Pairwise comparisons using Wilcoxon rank sum test 
#       MA      MO  
#   MO 2.6e-14 -   
#   MS < 2e-16 0.21
# 
# P value adjustment method: BH

# Looking at overall WH
barplot(table(d$wh,d$likert), beside=T,
        cex.names=0.7,legend.text=c("who", "where","how"),
        args.legend=list(x=9,y=300,cex=0.8),
        col=c("pink","light blue","red"))

kruskal.test(likert~wh, data=d)
# Kruskal-Wallis chi-squared = 11.893, df = 2, p-value = 0.002615

# Finding exactly what is driving the difference
pairwise.wilcox.test(d$likert,d$wh, p.adjust.method = "BH")
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  d$likert and d$wh 
# 
#         how    where 
#   where 0.0031 -     
#   who   0.0160 0.4692
# 
# P value adjustment method: BH 


############################
# Chi-Squared tests
############################


tab_ans <- table(d$likert.f,d$answer)
tab_wh <- table(d$likert.f,d$wh)
tab_mod <- table(d$likert.f,d$modal)
tab_task <- table(d$likert.f,d$task)
tab_interTA <- table(d$likert.f,interTA)

chisq.test(tab_interTA)

# Chi-squared for task
chisq.test(tab_task)
# Pearson's Chi-squared test
# 
# data:  tab_task
# X-squared = 91.44, df = 5, p-value < 2.2e-16

# chi squared for ANSWER
chisq.test(tab_ans)
# Pearson's Chi-squared test
# 
# data:  tab_ans
# X-squared = 117.52, df = 10, p-value < 2.2e-16

# Chi squared for WH
chisq.test(tab_wh)
# #	Pearson's Chi-squared test
# 
# data:  tab_wh
# X-squared = 24.878, df = 10, p-value = 0.005582

# Chi squared for MODAL
chisq.test(tab_mod)
# Pearson's Chi-squared test
# 
# data:  tab_mod
# X-squared = 62.962, df = 5, p-value = 2.964e-12

################################################
########## Ordinal regressions 
################################################

################################################
# Using the Ordinal package

likely <- subset(d, d$task=="likely")
accept <- subset(d, d$task=="accept")
library(ordinal)

# If we want to instead get the intercept for the grand mean, we need to center frequency first:
#you take the original vector, and you make the mean of the vector the zero-point. So mu[1 2 5 8] = 16/4. The centered vector is each number minus n: [-3 -2 1 4].
centered = cbind(d,myCenter(d[,c("likert.f","answer")]))
View(centered)

ggplot(centered, aes(answer, y=clikert.f)) +
  geom_boxplot() +
  coord_flip()

ggplot(centered, aes(x = answer, y = clikert.f)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(task ~ modal, margins = TRUE) +
  coord_flip() +
  ylim(-4,2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

glmerFit <- glmer(clikert.f ~ answer + modal + wh + (1|subject), family="binomial", data=centered)






clmFit <- clm(likert.f ~ answer + modal + wh, link="probit", data=d)
summary(clmFit)
# formula: likert.f ~ answer + modal + wh
# data:    d
# 
# link  threshold nobs logLik   AIC     niter max.grad cond.H 
# logit flexible  2855 -4214.88 8449.77 5(0)  7.51e-09 9.3e+01
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#   answerMO   -0.69026    0.08414  -8.204 2.33e-16 ***
#   answerMS   -0.75766    0.08619  -8.790  < 2e-16 ***
#   modalnomod -0.52843    0.06830  -7.736 1.02e-14 ***
#   whwhere     0.28537    0.08312   3.433 0.000597 ***
#   whwho       0.21359    0.08304   2.572 0.010111 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#   E stimate Std. Error z value
# 0|1 -4.58417    0.15921 -28.794
# 1|2 -3.28111    0.11227 -29.226
# 2|3 -2.27645    0.09756 -23.335
# 3|4 -1.08559    0.08990 -12.076 
# 4|5  0.10947    0.08708   1.257


# formula: likert.f ~ canswer + modal + wh
# data:    centered
# 
# link   threshold nobs logLik   AIC     niter max.grad cond.H 
# probit flexible  2855 -4230.48 8478.96 5(0)  1.19e-09 5.2e+01
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# canswer    -0.21041    0.02525  -8.332  < 2e-16 ***
#   modalnomod -0.29418    0.04018  -7.321 2.46e-13 ***
#   whwhere     0.16328    0.04900   3.332 0.000861 ***
#   whwho       0.14210    0.04900   2.900 0.003734 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 0|1 -2.13883    0.06441 -33.208
# 1|2 -1.56582    0.05004 -31.292
# 2|3 -1.04588    0.04475 -23.374
# 3|4 -0.34769    0.04210  -8.259
# 4|5  0.38507    0.04204   9.160


# This is for grpahing the probit model
clmpFit <- clm(likert.f ~ answer + modal + wh, link="probit", data=d)
summary(clmpFit)


# formula: likert.f ~ answer + modal + wh
# data:    d
# 
# link   threshold nobs logLik   AIC     niter max.grad cond.H 
# probit flexible  2855 -4220.44 8460.89 5(0)  9.47e-10 7.8e+01
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#   answerMO   -0.39967    0.04926  -8.113 4.92e-16 ***
#   answerMS   -0.42604    0.05065  -8.412  < 2e-16 ***
#   modalnomod -0.30477    0.04028  -7.566 3.84e-14 ***
#   whwhere     0.16381    0.04902   3.342 0.000833 ***
#   whwho       0.14285    0.04903   2.914 0.003573 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#       Estimate Std. Error z value
# 0|1 -2.43063    0.07235 -33.594
# 1|2 -1.85572    0.05983 -31.017
# 2|3 -1.33322    0.05524 -24.136
# 3|4 -0.63254    0.05249 -12.050
# 4|5  0.10292    0.05144   2.001



ggplot(d, aes(x=answer, y=likert)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)


clmFit1 <- clm(likert.f ~ answer, link="logit", data=d)
summary(clmFit1)

# formula: likert.f ~ answer
# data:    d
# 
# link  threshold nobs logLik   AIC     niter max.grad cond.H 
# logit flexible  2855 -4251.07 8516.15 5(0)  5.04e-09 6.4e+01
# 
# Coefficients:
#             Estimate  Std. Error z value Pr(>|z|)    
#   answerMO -0.65987    0.08374  -7.880  3.26e-15 ***
#   answerMS -0.75758    0.08568  -8.842  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1 -4.43901    0.14668 -30.262
# 1|2 -3.13884    0.09366 -33.515
# 2|3 -2.14195    0.07561 -28.328
# 3|4 -0.97033    0.06617 -14.665
# 4|5  0.20069    0.06334   3.168

# With random effects for subjects
clmmFit <- clmm(likert.f ~ answer + (1|subject), link="logit", data=d)
summary(clmmFit)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: likert.f ~ answer + (1 | subject)
# data:    d
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H
# logit flexible  2855 -3629.59 7275.18 692(5656) 2.76e+01 NaN   
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# subject (Intercept) 3.817    1.954   
# Number of groups:  subject 238 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# answerMO   -1.007         NA      NA       NA
# answerMS   -1.129         NA      NA       NA
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 0|1  -6.0948         NA      NA
# 1|2  -4.6430         NA      NA
# 2|3  -3.3721         NA      NA
# 3|4  -1.6793         NA      NA
# 4|5   0.2799         NA      NA



clmmFit2 <- clmm(likert.f ~ answer + (1|subject), link="logit", data=d, control =
                  clmm.control(innerCtrl = "noWarn"))
summary(clmmFit2)



library(MASS)
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
p <- polr(likert.f ~ answer + modal + wh, data=d, Hess=TRUE)
summary(p)
# Call:
#   polr(formula = likert.f ~ answer + modal + wh, data = d, Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error t value
# answerMO   -0.6903    0.08414  -8.204
# answerMS   -0.7577    0.08619  -8.790
# modalnomod -0.5284    0.06830  -7.736
# whwhere     0.2854    0.08312   3.433
# whwho       0.2136    0.08304   2.572
# 
# Intercepts:
#   Value    Std. Error t value 
# 0|1  -4.5842   0.1592   -28.7930
# 1|2  -3.2811   0.1123   -29.2256
# 2|3  -2.2765   0.0976   -23.3347
# 3|4  -1.0856   0.0899   -12.0759
# 4|5   0.1095   0.0871     1.2569
# 
# Residual Deviance: 8429.769 
# AIC: 8449.769 

# Calculate pvalues
# store coefficient table
p.coef <- data.frame(coef(summary(p)))
# calculate pvals and bind to table
p.coef$pval = round((pnorm(abs(p.coef$t.value),lower.tail=FALSE)*2),2)
p.coef
#             Value Std..Error    t.value pval
# answerMO   -0.6902585 0.08413861  -8.203825 0.00
# answerMS   -0.7576584 0.08619109  -8.790449 0.00
# modalnomod -0.5284303 0.06830406  -7.736440 0.00
# whwhere     0.2853719 0.08312468   3.433058 0.00
# whwho       0.2135889 0.08304355   2.572010 0.01
# 0|1        -4.5841819 0.15921158 -28.793017 0.00
# 1|2        -3.2811192 0.11226879 -29.225567 0.00
# 2|3        -2.2764532 0.09755648 -23.334719 0.00
# 3|4        -1.0855930 0.08989769 -12.075872 0.00
# 4|5         0.1094578 0.08708337   1.256931 0.21

# Confidence intervals
# If the 95% CI does not cross 0, the parameter estimate is statistically significant.
(ci <- confint(p)) # default method gives profiled CIs
#               2.5 %     97.5 %
# answerMO   -0.85549299 -0.5256382
# answerMS   -0.92691567 -0.5890141
# modalnomod -0.66248862 -0.3947195
# whwhere     0.12253385  0.4484113
# whwho       0.05089114  0.3764502

confint.default(p) # CIs assuming normality
#                 2.5 %     97.5 %
# answerMO   -0.85516710 -0.5253498
# answerMS   -0.92658985 -0.5887270
# modalnomod -0.66230378 -0.3945568
# whwhere     0.12245049  0.4482932
# whwho       0.05082652  0.3763513


library(stargazer)
stargazer(me, type="text", out="me.htm")

m <- polr(likert.f~answer + modal + wh, data=d, Hess=T)
summary(m)

#  baseline is answerMA, modalmod, whhow
# Coefficients:
#             Value   Std. Error t value
# answerMO   -0.5059    0.11368  -4.450
# answerMS   -0.1322    0.11994  -1.102
# modalnomod -0.6617    0.09467  -6.990
# whwhere     0.4281    0.11438   3.743
# whwho       0.4536    0.11425   3.970
# 
# Intercepts:
#       Value    Std. Error t value 
# 0|1  -4.2893   0.2205   -19.4539
# 1|2  -2.8358   0.1479   -19.1757
# 2|3  -1.7168   0.1279   -13.4267
# 3|4  -0.5029   0.1201    -4.1878
# 4|5   0.8290   0.1211     6.8481
# 
# Residual Deviance: 4522.074 
# AIC: 4542.074 

# Standard  interpretation  of  the  ordered  log-odds  coefficient  is  
# that for  a  one  unit  increase  in  the  predictor,  the response 
# variable level is expected to change by its respective regression coefficient 
# in the ordered log-odds scale while the other variables in the model are held constant.

# then calculating the p-values for the coefficients (to 3 decimals)
coeffs <- coef(summary(m))
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))
#               Value   Std. Error    t value   p value
# answerMO   -0.5058786 0.11368288  -4.449910    0.00
# answerMS   -0.1321970 0.11993920  -1.102200    0.27
# modalnomod -0.6616987 0.09466507  -6.989894    0.00
# whwhere     0.4280959 0.11437923   3.742777    0.00
# whwho       0.4536015 0.11424804   3.970322    0.00
# 0|1        -4.2893104 0.22048551 -19.453934    0.00
# 1|2        -2.8357671 0.14788306 -19.175740    0.00
# 2|3        -1.7168361 0.12786712 -13.426720    0.00
# 3|4        -0.5029330 0.12009597  -4.187759    0.00
# 4|5         0.8290076 0.12105658   6.848100    0.00

# "null  hypothesis  that  an  individual  predictor’s  re-gression 
# coefficient is zero given that the rest of the predictors are in the model"


# the odds
exp(coef(m))

# answerMO   answerMS   modalnomod    whwhere      whwho 
# 0.6029756  0.8761684  0.5159741  1.5343332  1.5739707


############################################################
# WIThe LME4 Package

# NB: I don't think that  this next test is the right one, because it assumes that the
# variable is not multi-nomial
d <- d %>% drop_na()
me = glmer(likert.f ~ answer + (1|subject), family="binomial", data=d)
# No difference between this:
# me = glmer(likert.f ~ answer + (1|subject), family="binomial"(link="logit"), data=d)
summary(me)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: likert.f ~ answer + (1 | subject)
# Data: d
# 
# AIC      BIC   logLik deviance df.resid 
# 272.8    294.0   -132.4    264.8     1484 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.6679  0.0673  0.0715  0.0772  0.5362 
# 
# Random effects:
# Groups  Name        Variance Std.Dev.
# subject (Intercept) 2.784    1.669   
# Number of obs: 1488, groups:  subject, 124
# 
# Fixed effects:
#               Estimate  Std. Error  z value Pr(>|z|)    
#   (Intercept)   4.9235     0.6933   7.102 1.23e-12 ***
#   answerMO      0.1826     0.6484   0.282    0.778    
#   answerMS      0.3218     0.6982   0.461    0.645    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) answMO
# answerMO -0.516       
# answerMS -0.479  0.487



################################
# trying to center the data using myCenter from the helpers function
library(bootstrap)
centered = cbind(d,myCenter(d[,c("likert","modal","answer","wh")]))
head(centered)
summary(centered)

mc = glmer(likert ~ cFrequency + (1|Subject) + (1|Word), family="binomial", data=centered)
summary(mc)


