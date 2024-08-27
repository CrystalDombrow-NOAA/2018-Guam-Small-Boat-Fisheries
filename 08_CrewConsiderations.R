#######################
# 2018 Guam small boat fishery cost-earnings survey
# CREW CONSIDERATIONS section
# Calculations for section paragraphs
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)


#Run functions used in the analyses below
source("Functions/DataSummariesFunction.R")
source("Functions/DistributionFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------
# General crew stats

crew.stats <- guam.data.cleaned %>% 
  select(Q13B, Survey, boat.owner, sell.fish, vendor, Q14A:Q14F, Q16A:Q16G, Q22,
         Q35A:Q35E, Q38A:Q38E, Q42:Q53) %>% 
  filter(Q13B > 4) #Over 50% of the time fishing as crew from Q13B


#---------------------------------------------------------------------------

# Q13. In the past 12 months, how many of your fishing trips did you fish as
# captain and/or crew?

#DISTRIBUTION
q13a.distr <- distribution.function(guam.data.cleaned, q.number = "Q13A", 
                                    categories = 1:6)

q13b.distr <- distribution.function(guam.data.cleaned, q.number = "Q13B", 
                                    categories = 1:6)


#DATA SUMMARIES
q13a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q13A.mid")

q13b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q13B.mid")


#---------------------------------------------------------------------------

# Q14. In the past 12 months, how were the catch distributed among fisher(men) 
# in a fishing trip?

q14.data <- guam.data.cleaned %>% 
  select(Q13B, Q14A:Q14F, Q14.multiple.responses) %>%
  filter(Q13B > 4)

q14.percents <- guam.data.cleaned %>% 
  select(Q13B, Q14A:Q14F, Q14.multiple.responses) %>%
  filter(Q13B > 4) %>% #Over 50% of the time fishing as crew from Q13B
  mutate(q14a.per = round(sum(Q14A) / nrow(q14.data) * 100, 1),
         q14b.mean = round(mean(Q14B, na.rm = T), 1),
         q14d.per = round(sum(Q14D) / nrow(q14.data) * 100, 1),
         q14e.per = round(sum(Q14E) / nrow(q14.data) * 100, 1),
         q14.mult.per = round(sum(Q14.multiple.responses) / nrow(q14.data) * 100, 1),
         kept.fish = ifelse(Q14A == 1 | !is.na(Q14B), 1, 0),
         kept.fish.per = round(sum(kept.fish) / nrow(q14.data) * 100, 1))

