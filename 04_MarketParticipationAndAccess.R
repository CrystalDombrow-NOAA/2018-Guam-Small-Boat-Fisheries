#######################
# 2018 Guam small boat fishery cost-earnings survey
# Market Participation and Access Section
# Calculations for tables
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
source("Functions/DistributionFunction_MultipleVariables_FishValue.R")
source("Functions/DistributionFunction_MultipleVariables_WhereSell.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q12. In the past 12 months, how many of your fishing trips did you fish for a 
# roadside vendor, and/or as an independent fisherman?

#ROADSIDE VENDOR
q12a.distr <- distribution.function(guam.data.cleaned, q.number = "Q12A", 
                                    categories = 1:6)

#INDEPENDENT FISHERMAN
q12b.distr <- distribution.function(guam.data.cleaned, q.number = "Q12B", 
                                    categories = 1:6)


#--------------------------------------------------------------------------

# Q15. In the past 12 months, about what percentage of your fishing trips did 
# you sell a portion of your catch?

#DATA SUMMARIES
q15.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q15.mid")

#DISTRIBUTION
q15.distr <- distribution.function(guam.data.cleaned, q.number = "Q15", 
                                   categories = 1:6) 


#--------------------------------------------------------------------------

# Q17. Where did you sell your catch?

#------------
#DISTRIBUTION
#------------
#GFCA
q17a.distr <- distribution.function(guam.data.cleaned, q.number = "Q17A", 
                                    categories = 1:6) 

#ROADSIDE VENDOR
q17b.distr <- distribution.function(guam.data.cleaned, q.number = "Q17B", 
                                    categories = 1:6) 

#RETAIL MARKETS/STORES
q17c.distr <- distribution.function(guam.data.cleaned, q.number = "Q17C", 
                                    categories = 1:6)

#HOTELS/RESTAURANTS
q17d.distr <- distribution.function(guam.data.cleaned, q.number = "Q17D", 
                                    categories = 1:6)

#FRIENDS/NEIGHBORS/COWORKERS
q17e.distr <- distribution.function(guam.data.cleaned, q.number = "Q17E", 
                                    categories = 1:6)  

#WHOLESALER
q17f.distr <- distribution.function(guam.data.cleaned, q.number = "Q17F", 
                                    categories = 1:6) 

#OTHER
q17g.distr <- distribution.function(guam.data.cleaned, q.number = "Q17G", 
                                    categories = 1:6) 


#------------
#DISTRIBUTION
#------------
#FULL SAMPLE
q17.full.sample <- distribution.function.where.sell(guam.data.cleaned, 
                                                    breakdown = "Full.sample")

#GFCA
q17.gfca <- distribution.function.where.sell(guam.data.cleaned, 
                                             breakdown = "GFCA.member")

#SELL FISH
q17.sell.fish <- distribution.function.where.sell(guam.data.cleaned, 
                                                  breakdown = "sell.fish.chr")

#HIGHLINER
q17.highliner <- distribution.function.where.sell(guam.data.cleaned, 
                                                  breakdown = "highliner")

#PRIMARY TARGET
q17.primary.target <- distribution.function.where.sell(guam.data.cleaned, 
                                                       breakdown = "primary.target")

#BOAT OWNER
q17.boat.owner <- distribution.function.where.sell(guam.data.cleaned, 
                                                   breakdown = "boat.owner")



#Create output table, just need to get number of observations from above objects
q17.where.sell.table <- rbind(q17.full.sample,
                             q17.gfca,
                             q17.sell.fish,
                             q17.highliner,
                             q17.primary.target,
                             q17.boat.owner)

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner", 
               "not highliner", "did not sell fish", "pelagic", "bottomfish", 
               "reef", "no primary", "other", "boat owner", "not boat owner")

q17.where.sell.table <- q17.where.sell.table %>%
  slice(match(row.order, brk.down))


write.csv(q17.where.sell.table, 
          paste("Tables/Q17_distribution_multiplevariables.csv"), 
          row.names = F)


#------------------
#MARKET UTILIZATION
#------------------
#GUAM COOP
q17a.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17A.yesno", 
                                    categories = 1:2)

#ROADSIDE VENDOR
q17b.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17B.yesno", 
                                    categories = 1:2) 

#RETAIL MARKETS/STORES
q17c.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17C.yesno", 
                                    categories = 1:2) 

#HOTELS/RESTAURANTS
q17d.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17D.yesno", 
                                    categories = 1:2) 

#FRIENDS/NEIGHBORS/COWORKERS
q17e.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17E.yesno", 
                                    categories = 1:2) 

#WHOLESALER
q17f.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17F.yesno", 
                                    categories = 1:2) 

#OTHER
q17g.yesno <- distribution.function(guam.data.cleaned, q.number = "Q17G.yesno", 
                                    categories = 1:2) 


#------------------------------------
#DISTRIBUTION -- # OF MARKET CHANNELS
#------------------------------------
q17.dummy.distr <- distribution.function(guam.data.cleaned, 
                                         q.number = "Q17.dummy.total", 
                                         categories = 1:4) 
#V1 = 1, V2 = 2, V3 = 3, V4 = 4 (number of market channels used)


#--------------------------------------------------------------------------

# Q18. Can you usually sell all the fish that you want to sell?

#PELAGIC
q18a <- distribution.function(guam.data.cleaned, q.number = "Q18A", 
                              categories = 1:3)

#BOTTOMFISH
q18b <- distribution.function(guam.data.cleaned, q.number = "Q18B", 
                              categories = 1:3)

#REEF FISH
q18c <- distribution.function(guam.data.cleaned, q.number = "Q18C", 
                              categories = 1:3)


#--------------------------------------------------------------------------

# Q19. If no in Q18, why not?

q19.data <- guam.data.cleaned %>% 
  select(Q18A:Q18C, Q19A:Q19G) %>%
  mutate(Q19.ifelse = ifelse(Q18A == 2 | Q18B == 2 | Q18C == 2, 1, 0)) %>%
  filter(Q19.ifelse == 1)

q19.percents <- q19.data %>% 
  select(Q19.ifelse, Q19A:Q19F) %>%
  filter(Q19.ifelse == 1) %>% 
  mutate(q19a.per = round(sum(Q19A, na.rm = T) / nrow(q19.data) * 100, 1),
         q19b.per = round(sum(Q19B, na.rm = T) / nrow(q19.data) * 100, 1),
         q19c.per = round(sum(Q19C, na.rm = T) / nrow(q19.data) * 100, 1),
         q19d.per = round(sum(Q19D, na.rm = T) / nrow(q19.data) * 100, 1),
         q19e.per = round(sum(Q19E, na.rm = T) / nrow(q19.data) * 100, 1),
         q19f.per = round(sum(Q19F, na.rm = T) / nrow(q19.data) * 100, 1))


#--------------------------------------------------------------------------

# Q20. In the past 12 months, what was the approximate value of all the fish you 
# sold?

#DATA SUMMARIES
q20.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q20.mid.ifelse")

#DISTRIBUTION
q20.distr <- distribution.function(guam.data.cleaned, q.number = "Q20A", 
                                   categories = 1:6) 


#--------------------------------------------------------------------------

# Q21. In the past 12 months, what percent of the value of fish sold came from:

#PELAGIC
q21a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q21A.mid")
q21a.distr <- distribution.function(guam.data.cleaned, q.number = "Q21A", 
                                    categories = 1:6)

#BOTTOMFISH
q21b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q21B.mid")
q21b.distr <- distribution.function(guam.data.cleaned, q.number = "Q21B", 
                                    categories = 1:6) 

#REEF FISH
q21c.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q21C.mid")
q21c.distr <- distribution.function(guam.data.cleaned, q.number = "Q21C", 
                                    categories = 1:6)


#------------
#DISTRIBUTION
#------------
#FULL SAMPLE
q21.full.sample <- distribution.function.fish.value(guam.data.cleaned, 
                                                    breakdown = "Full.sample")

#GFCA
q21.gfca <- distribution.function.fish.value(guam.data.cleaned, 
                                             breakdown = "GFCA.member")

#SELL FISH
q21.sell.fish <- distribution.function.fish.value(guam.data.cleaned, 
                                                  breakdown = "sell.fish.chr")

#HIGHLINER
q21.highliner <- distribution.function.fish.value(guam.data.cleaned, 
                                                  breakdown = "highliner")

#PRIMARY TARGET
q21.primary.target <- distribution.function.fish.value(guam.data.cleaned, 
                                                       breakdown = "primary.target")

#BOAT OWNER
q21.boat.owner <- distribution.function.fish.value(guam.data.cleaned, 
                                                   breakdown = "boat.owner")



#Create output table, just need to get number of observations from above objects
q21.mult.vars.table <- rbind(q21.full.sample,
                             q21.gfca,
                             q21.sell.fish,
                             q21.highliner,
                             q21.primary.target,
                             q21.boat.owner)

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner", 
               "not highliner", "did not sell fish", "pelagic", "bottomfish", 
               "reef", "no primary", "other", "boat owner", "not boat owner")

q21.mult.vars.table <- q21.mult.vars.table %>%
  slice(match(row.order, brk.down))


write.csv(q21.mult.vars.table, 
          paste("Tables/Q21_distribution_multiplevariables.csv"), 
          row.names = F)


#--------------------------------------------------------------------------

# Q22. In the past 12 months, what percent of your personal income came from the 
# sale of fish?

#DATA SUMMARIES
q22.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q22.mid")

#DISTRIBUTION
q22.distr <- distribution.function(guam.data.cleaned, q.number = "Q22", 
                                   categories = 1:6) 



