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

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(plyr)){install.packages("plyr")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}

setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Conditional_Probability/results/")
source("helpers.R")

d <- read.csv("answer_likely1_reformat.csv", header = TRUE)
head(d)













mod <- subset(d, d$modal=="mod")
nomod <-subset(d, d$modal=="nomod")
summary(mo)

barplot(table(mod$answer,mod$likert), beside=T,
        cex.names=0.7,legend.text=c("MA","MS","MO"),
        args.legend=list(x=9,y=150,cex=0.8),
        col=c("pink","light blue","red"),
        xlab="Modal")

barplot(table(nomod$answer,nomod$likert), beside=T,
        cex.names=0.7,legend.text=c("MA","MS","MO"),
        args.legend=list(x=9,y=100,cex=0.8),
        col=c("pink","light blue","red"),
        xlab="Nomodal")

mo <- subset(d, d$answer=="MO")
ms <- subset(d, d$answer=="MS")
ma <- subset(d, d$answer=="MA")

barplot(table(mo$modal,mo$likert), beside=T,
        cex.names=0.7,legend.text=c("mod","nomod"),
        args.legend=list(x=9,y=100,cex=0.8),
        col=c("pink","light blue"),
        xlab="Mention-One")

barplot(table(ms$modal,ms$likert), beside=T,
        cex.names=0.7,legend.text=c("mod","nomod"),
        args.legend=list(x=9,y=100,cex=0.8),
        col=c("pink","light blue"),
        xlab="Mention-Som")

barplot(table(ma$modal,ma$likert), beside=T,
        cex.names=0.7,legend.text=c("mod","nomod"),
        args.legend=list(x=9,y=100,cex=0.8),
        col=c("pink","light blue"),
        xlab="Mention-All")

barplot(table(mo$wh,mo$likert), beside=T,
        cex.names=0.7,legend.text=c("who","where","how"),
        args.legend=list(x=11,y=74,cex=0.8),
        col=c("pink","light blue","red"),
        xlab="Mention-One")





# mo$likert <- as.factor(mo$likert)
# nomo$likert <- as.factor(nomo$likert)


ggplot(mod, aes(answer,y=likert,fill=wh)) +
  geom_boxplot() +
  coord_flip() +
  # geom_jitter() +
  ggtitle(label = "Likely, Modal")

ggplot(nomod, aes(answer,y=likert,fill=wh)) +
  geom_boxplot() +
  coord_flip() +
  # geom_jitter() +
  ggtitle(label = "Likely, No Modal")

mod_where <- subset(mod, mod$wh=="where")
mod_where %>%
  group_by(answer,likert) %>%
  mutate(count=n())



#####################
# stats
m = glmer(likert ~ answer  + (1|subject), family="binomial", data=mo)




library(ggthemes)
library(extrafont)
library(plyr)
library(scales)


head(mo_where)

mo <- mo %>% 
  gather(answer, wh, likert, -subject,-variable,-task,-modal,-story) %>% 
  group_by(likert, wh, answer) %>% 
  tally %>% 
  mutate(n = n/sum(n)*100) %>%
  ungroup()

head(mo)

ggplot(agr) + geom_bar(aes(y = n, x = answer, fill = as.factor(likert)), 
                    stat="identity")



  ggplot(aes(x=answer, y==as.factor(likert),fill=wh)) + 
  geom_col() + 
  geom_text(aes(label=n), position=position_stack(.5)) + 
  coord_flip()



p <- ggplot(mo, aes(answer,y=likert,fill=wh)) 
p
library(sjPlot)
likert_2 <- data.frame(as.factor(sample(1:2, 500, replace=T, prob=c(0.3,0.7))),
                       as.factor(sample(1:2, 500, replace=T, prob=c(0.6,0.4))),
                       as.factor(sample(1:2, 500, replace=T, prob=c(0.25,0.75))),
                       as.factor(sample(1:2, 500, replace=T, prob=c(0.9,0.1))),
                       as.factor(sample(1:2, 500, replace=T, prob=c(0.35,0.65))))
levels_2 <- list(c("Disagree", "Agree"))
items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))
sjp.likert(likert_2, legendLabels=levels_2, axisLabels.x=items, orderBy="neg")


# d$Likert.f = factor(d$likert,
#                        ordered = TRUE,
#                        levels = c("1", "2", "3", "4", "5")
# )
# 
# summary(d$Likert.f)
# 
# DT = xtabs(~ Likert.f,
#            data=d)
# 
# likert = d$likert
# answer = d$answer
# #  general boxplot
# boxplot(likert~answer, 
#         # data=d,
#         # col="dark gray",
#         xlab="answer",
#         ylab="likert rating",
#         col="light blue",
#         border="dark blue")

#################
# 
# o <- read.csv("raw/answer_likely_first_raw.csv", header = TRUE)
# s <- read.csv("raw/answer_likely_second_raw.csv", header = TRUE)
# t <- read.csv("raw/answer_likely_third_raw.csv", header = TRUE)
# nomod <- rbind(o,s,t)
# 
# summary(o)
# 
# 
# fo <- read.csv("raw/answer_likely_fourth_raw.csv", header = TRUE)
# fi <- read.csv("raw/answer_likely_fifth_raw.csv", header = TRUE)
# s <- read.csv("raw/answer_likely_sixth_raw.csv", header = TRUE)
# mod <- rbind(fo,fi,s)




#################


# converting from long to wide to use the likert package
# this is not working
# mo_wide <- dcast(mo,subject ~ wh + answer, value.var = "likert")
# nomo_wide <- dcast(nomo,subject ~ wh + answer, value.var = "likert")
# View(mo_wide)

mo_wider <- mo %>% 
  nest(, a, .key = 'value_col') %>%
  spread(key = likert, value = value_col)
  unnest(wh, answer, .sep = '_')

View(mo_wider)
# Change the likert scores to factors
# chang ehte 0/1 in the modal to strings
write.csv(mo_wide,file = "mo_wide.csv")
write.csv(nomo_wide,file = "nomo_wide.csv")

mo_df <- read.csv("mo_wide.csv", header = TRUE)
nomo_df <- read.csv("nomo_wide.csv", header = TRUE)


str(mo_df)
# HOW

mo_df$how_MA = factor(mo_df$how_MA,
                    ordered = TRUE,
                    levels = c("1", "2", "3", "4", "5")
                    )

mo_df$how_MS = factor(mo_df$how_MS,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)    
mo_df$how_MO = factor(mo_df$how_MO,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)
# WHO
mo_df$who_MA = factor(mo_df$who_MA,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)

mo_df$who_MS = factor(mo_df$who_MS,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)    
mo_df$who_MO = factor(mo_df$who_MO,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)
# WHERE
mo_df$where_MA = factor(mo_df$where_MA,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)

mo_df$where_MS = factor(mo_df$where_MS,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)    
mo_df$where_MO = factor(mo_df$where_MO,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)



# NOMOD
# nomore how
nomo_df$how_MA = factor(nomo_df$how_MA,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)

nomo_df$how_MS = factor(nomo_df$how_MS,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)    
nomo_df$how_MO = factor(nomo_df$how_MO,
                       ordered = TRUE,
                       levels = c("1", "2", "3", "4", "5")
)




# nomore who
nomo_df$who_MA = factor(nomo_df$who_MA,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)

nomo_df$who_MS = factor(nomo_df$who_MS,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)    
nomo_df$who_MO = factor(nomo_df$who_MO,
                         ordered = TRUE,
                         levels = c("1", "2", "3", "4", "5")
)


# nomore where
nomo_df$where_MA = factor(nomo_df$where_MA,
                         ordered = TRUE,
                         levels = c("0", "1", "2", "3", "4", "5")
)

nomo_df$where_MS = factor(nomo_df$where_MS,
                         ordered = TRUE,
                         levels = c("0", "1", "2", "3", "4", "5")
)    
nomo_df$where_MO = factor(nomo_df$where_MO,
                         ordered = TRUE,
                         levels = c("0", "1", "2", "3", "4", "5")
)
                    
View(mo_df)
str(mo_df)
summary(mo_df)


library(likert)


# remove that very first column that python put in
df$X <- NULL

df <- as.data.frame(df)

# the following helpful stuff is from
# https://github.com/jbryer/likert/blob/master/demo/UnusedLevels.R
mylevels <- c('1', '2', '3', '4','5')

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(df)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})
sapply(df, class) #Verify that all the columns are indeed factors
sapply(df, function(x) { length(levels(x)) } ) # The number of levels in each factor

for(i in seq_along(df)) {
  df[,i] <- factor(df[,i], levels=mylevels)
}

result = likert(df)
summary(result)

plot(result,
     type="bar")

plot(result,
     type="heat",
     low.color = "white",
     high.color = "blue",
     text.color = "black",
     text.size = 4,
     wrap = 50)



  # facet_wrap(~wh)
