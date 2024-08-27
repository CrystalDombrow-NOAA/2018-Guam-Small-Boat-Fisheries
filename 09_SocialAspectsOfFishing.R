#######################
# 2018 Guam small boat fishery cost-earnings survey
# SOCIAL ASPECTS OF FISHING section
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
source("Functions/DistributionFunction_MultipleVariables_CatchDisposition.R")
source("Functions/DistributionFunction_MultipleVariables_PrimaryMotivation.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q10. To be a considered a commercial fisher(man), I feel that someone must:
#(note: percent of respondents, respondents can select multiple answers, so %'s do not add up to 100)

q10a.distr <- distribution.function(guam.data.cleaned, q.number = "Q10A", 
                                    categories = 1:2) 

q10b.distr <- distribution.function(guam.data.cleaned, q.number = "Q10B", 
                                    categories = 1:2) 

q10c.distr <- distribution.function(guam.data.cleaned, q.number = "Q10C", 
                                    categories = 1:2) 

q10d.distr <- distribution.function(guam.data.cleaned, q.number = "Q10D", 
                                    categories = 1:2) 

q10e.distr <- distribution.function(guam.data.cleaned, q.number = "Q10E", 
                                    categories = 1:2) 

q10f.distr <- distribution.function(guam.data.cleaned, q.number = "Q10F", 
                                    categories = 1:2) 

q10g.distr <- distribution.function(guam.data.cleaned, q.number = "Q10G", 
                                    categories = 1:2) 

q10h.distr <- distribution.function(guam.data.cleaned, q.number = "Q10H", 
                                    categories = 1:2) 

q10j.distr <- distribution.function(guam.data.cleaned, q.number = "Q10J", 
                                   categories = 1:2) 



#-------------------------------------------------------------------------

# Q11. What is your primary motivation for fishing?

#------------
#DISTRIBUTION
#------------
#Measures the distribution of respondents participating in each motivations for
#fishing.

q11a.distr <- distribution.function(guam.data.cleaned, q.number = "Q11A.yesno", 
                                    categories = 1:2) 

q11b.distr <- distribution.function(guam.data.cleaned, q.number = "Q11B.yesno", 
                                    categories = 1:2) 

q11c.distr <- distribution.function(guam.data.cleaned, q.number = "Q11C.yesno", 
                                    categories = 1:2) 

q11d.distr <- distribution.function(guam.data.cleaned, q.number = "Q11D.yesno", 
                                    categories = 1:2) 

q11e.distr <- distribution.function(guam.data.cleaned, q.number = "Q11E.yesno", 
                                    categories = 1:2) 

q11f.distr <- distribution.function(guam.data.cleaned, q.number = "Q11F.yesno", 
                                    categories = 1:2) 

q11g.distr <- distribution.function(guam.data.cleaned, q.number = "Q11G.yesno", 
                                    categories = 1:2) 



#--------------------------------------
#DISTRIBUTION ACROSS MULTIPLE VARIABLES
#--------------------------------------

#FULL SAMPLE
q11.full.sample <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                            breakdown = "Full.sample")

#GFCA
q11.gfca <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                     breakdown = "GFCA.member")

#SELL FISH
q11.sell.fish <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                          breakdown = "sell.fish.chr")

#HIGHLINER
q11.highliner <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                          breakdown = "highliner")

#PRIMARY TARGET
q11.primary.target <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                               breakdown = "primary.target")

#BOAT OWNER
q11.boat.owner <- distribution.function.primary.motivation(guam.data.cleaned, 
                                                           breakdown = "boat.owner")


#Create output table, just need to get number of observations from above objects
q11.primary.motivation.table <- rbind(q11.full.sample,
                                      q11.gfca,
                                      q11.sell.fish,
                                      q11.highliner,
                                      q11.primary.target,
                                      q11.boat.owner) %>%
  unique()

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner",
               "not highliner", "did not sell fish", "pelagic", "bottomfish",
               "reef", "no primary", "other", "boat owner", "not boat owner")

q11.primary.motivation.table <- q11.primary.motivation.table %>%
  slice(match(row.order, brk.down))


write.csv(q11.primary.motivation.table,
          paste("Tables/Q11_distribution_multiplevariables.csv"),
          row.names = F)


#--------------------------------------------------------------------------

# Q16. In the past 12 months, what percent of your catch was:

#SOLD
q16a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q16A.mid")
q16a.distr <- distribution.function(guam.data.cleaned, q.number = "Q16A", 
                                    categories = 1:6) 

#CONSUMED AT HOME
q16b.data.sum <- data.summaries.function(guam.data.cleaned,q.number = "Q16B.mid")
q16b.distr <- distribution.function(guam.data.cleaned, q.number = "Q16B", 
                                    categories = 1:6) 

#GIVEN TO FAMILY MEMBERS
q16c.data.sum <- data.summaries.function(guam.data.cleaned,q.number = "Q16C.mid")
q16c.distr <- distribution.function(guam.data.cleaned, q.number = "Q16C", 
                                    categories = 1:6)

#GIVEN TO FRIENDS/NEIGHBORS
q16d.data.sum <- data.summaries.function(guam.data.cleaned,q.number = "Q16D.mid")
q16d.distr <- distribution.function(guam.data.cleaned, q.number = "Q16D", 
                                    categories = 1:6)

#CAUGHT FOR FIESTAS OR OTHER COMMUNITY/CULTURAL EVENTS
q16e.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q16E.mid")
q16e.distr <- distribution.function(guam.data.cleaned, q.number = "Q16E", 
                                    categories = 1:6)  

#TRADED FOR GOODS AND SERVICES
q16f.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q16F.mid")
q16f.distr <- distribution.function(guam.data.cleaned, q.number = "Q16F", 
                                    categories = 1:6) 

#CAUGHT AND RELEASED
q16g.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q16G.mid")
q16g.distr <- distribution.function(guam.data.cleaned, q.number = "Q16G", 
                                    categories = 1:6) 


#--------------------------------------
#DISTRIBUTION ACROSS MULTIPLE VARIABLES
#--------------------------------------
#FULL SAMPLE
q16.full.sample <- distribution.function.catch.disposition(guam.data.cleaned, 
                                                  breakdown = "Full.sample")

#GFCA
q16.gfca <- distribution.function.catch.disposition(guam.data.cleaned, 
                                           breakdown = "GFCA.member")

#SELL FISH
q16.sell.fish <- distribution.function.catch.disposition(guam.data.cleaned, 
                                                breakdown = "sell.fish.chr")
#HIGHLINER
q16.highliner <- distribution.function.catch.disposition(guam.data.cleaned, 
                                                breakdown = "highliner")

#PRIMARY TARGET
q16.primary.target <- distribution.function.catch.disposition(guam.data.cleaned, 
                                                     breakdown = "primary.target")

#BOAT OWNER
q16.boat.owner <- distribution.function.catch.disposition(guam.data.cleaned, 
                                                 breakdown = "boat.owner")


#Create output table, just need to get number of observations from above objects
q16.catch.disposition.table <- rbind(q16.full.sample,
                                     q16.gfca,
                                     q16.sell.fish,
                                     q16.highliner,
                                     q16.primary.target,
                                     q16.boat.owner) %>%
  unique()

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner", 
               "not highliner", "did not sell fish", "pelagic", "bottomfish", 
               "reef", "no primary", "other", "boat owner", "not boat owner")

q16.catch.disposition.table <- q16.catch.disposition.table %>%
  slice(match(row.order, brk.down))


write.csv(q16.catch.disposition.table, 
          paste("Tables/Q16_distribution_multiplevariables.csv"), 
          row.names = F)


#--------------------------------------------------------------------------

# Q23. How important is the fish you catch as a source of food for your family?

q23.distr <- distribution.function(guam.data.cleaned, q.number = "Q23", 
                                   categories = 1:5)


#DISTRIBUTION ACROSS CATCH GROUPS IN Q8
q23.pelagics <- distribution.function(guam.data.cleaned, 
                                      q.number = "Q23.pelagics.yesno", 
                                      categories = 1:2) 
# 1 = yes, 2 = no

q23.bottomfish <- distribution.function(guam.data.cleaned, 
                                        q.number = "Q23.bottomfish.yesno", 
                                        categories = 1:2) 
# 1 = yes, 2 = no

q23.reef <- distribution.function(guam.data.cleaned, 
                                  q.number = "Q23.reef.yesno",
                                  categories = 1:2)
# 1 = yes, 2 = no


#--------------------------------------------------------------------------

# Q47. Are you a member of a fishing club/association/group?
#------------
#DISTRIBUTION
#------------
q47.distr <- distribution.function(guam.data.cleaned, q.number = "Q47", 
                                    categories = 1:2) 


q47a.distr <- distribution.function(guam.data.cleaned, q.number = "Q47A", 
                                   categories = 1:2) 

q47b.distr <- distribution.function(guam.data.cleaned, q.number = "Q47B", 
                                   categories = 1:2) 

q47c.distr <- distribution.function(guam.data.cleaned, q.number = "Q47C", 
                                   categories = 1:2) 

q47d.distr <- distribution.function(guam.data.cleaned, q.number = "Q47D", 
                                   categories = 1:2) 

q47e.distr <- distribution.function(guam.data.cleaned, q.number = "Q47E", 
                                   categories = 1:2) 

q47f.distr <- distribution.function(guam.data.cleaned, q.number = "Q47F", 
                                   categories = 1:2) 

q47g.distr <- distribution.function(guam.data.cleaned, q.number = "Q47G", 
                                   categories = 1:2) 

q47h.distr <- distribution.function(guam.data.cleaned, q.number = "Q47H", 
                                    categories = 1:2) 


#--------------------------------------------------------------------------

# Q58. Please state how much you agree or disagree with the following 
  # statements:

#Q58A. As someone who fishes I am respected by the community.
q58a.distr <- distribution.function(guam.data.cleaned, q.number = "Q58A", 
                                    categories = 1:5)

#Q58B. Fishing is an important part of who I am.
q58b.distr <- distribution.function(guam.data.cleaned, q.number = "Q58B", 
                                    categories = 1:5)

#Q58C. Fishing is an important part of my culture.
q58c.distr <- distribution.function(guam.data.cleaned, q.number = "Q58C", 
                                    categories = 1:5)


