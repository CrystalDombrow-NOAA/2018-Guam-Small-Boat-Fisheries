#######################
# 2018 Guam small boat fishery cost-earnings survey
# FISHER PERSPECTIVES section
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


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q54. In the next year, do you think in the next year more people will be 
  # involved in…
    #*Limit to those reporting catch of each species group

#Pelagic fishing
q54a.distr <- distribution.function(guam.data.cleaned, q.number = "Q54A.caught", 
                                    categories = 1:2) 

#Bottomfish fishing
q54b.distr <- distribution.function(guam.data.cleaned, q.number = "Q54B.caught", 
                                    categories = 1:2) 

#Reef fishing
q54c.distr <- distribution.function(guam.data.cleaned, q.number = "Q54C.caught", 
                                    categories = 1:2) 


#---------------------------

# Q55. Why do you feel this way?
q55.open.ended <- guam.data.cleaned %>%
  select(Q54A:Q54C, Q55)

write.csv(q55.open.ended, "Tables/Q55_openended.csv", row.names = F)


#---------------------------------------------------------------------------

# Q59. How important are the following for managing fisheries in 
  # the Marianas?


# Q59A. Rules are followed and enforced
q59a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59A")
q59a.distr <- distribution.function(guam.data.cleaned, q.number = "Q59A", 
                                    categories = 1:5) 

# Q59B. My voice is included in decision making
q59b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59B")
q59b.distr <- distribution.function(guam.data.cleaned, q.number = "Q59B",
                                    categories = 1:5) 

# Q59C. We know how many fish there are
q59c.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59C")
q59c.distr <- distribution.function(guam.data.cleaned, q.number = "Q59C", 
                                    categories = 1:5) 

# Q59D. We know how healthy the reef / other habitats are
q59d.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59D")
q59d.distr <- distribution.function(guam.data.cleaned, q.number = "Q59D", 
                                    categories = 1:5) 

# Q59E. We know about the fisher(men) and fishing community 
  # (income, culture, etc.)
q59e.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59E")
q59e.distr <- distribution.function(guam.data.cleaned, q.number = "Q59E", 
                                    categories = 1:5) 

# Q59F. We build or maintain fisheries infrastructure 
  # (boat ramps, harbors, etc.)
q59f.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q59F")
q59f.distr <- distribution.function(guam.data.cleaned, q.number = "Q59F", 
                                    categories = 1:5) 

# Q59G. Other, please specify:
q59g.distr <- distribution.function(guam.data.cleaned, q.number = "Q59G", 
                                    categories = 1:5) 



#Explanation
q59h.other.explain <- guam.data.cleaned %>%
  select(Q59H)

write.csv(q59h.other.explain, "Tables/Q59H_other.csv", row.names = F)


#-------------------------------------------------------------------------

# Q60. Please state how much you agree or disagree that following management 
  # is being done well


# Q60A. Rules are followed and enforced
q60a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60A")
q60a.distr <- distribution.function(guam.data.cleaned, q.number = "Q60A", 
                                    categories = 1:5) 

# Q60B. My voice is included in decision making
q60b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60B")
q60b.distr <- distribution.function(guam.data.cleaned, q.number = "Q60B", 
                                    categories = 1:5) 

# Q60C. We know how many fish there are
q60c.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60C")
q60c.distr <- distribution.function(guam.data.cleaned, q.number = "Q60C", 
                                    categories = 1:5) 

# Q60D. We know how healthy the reef / other habitats are
q60d.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60D")
q60d.distr <- distribution.function(guam.data.cleaned, q.number = "Q60D", 
                                    categories = 1:5) 

# Q60E. We know about the fisher(men) and fishing community 
  # (income, culture, etc.)
q60e.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60E")
q60e.distr <- distribution.function(guam.data.cleaned, q.number = "Q60E", 
                                    categories = 1:5) 

# Q60F. We build or maintain fisheries infrastructure 
  # (boat ramps, harbors, etc.)
q60f.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q60F")
q60f.distr <- distribution.function(guam.data.cleaned, q.number = "Q60F", 
                                    categories = 1:5) 

# Q60G. Other
q60g.distr <- distribution.function(guam.data.cleaned, q.number = "Q60G", 
                                    categories = 1:5) 



#Explanation
q60h.other.explain <- guam.data.cleaned %>%
  select(Q60H)

write.csv(q60h.other.explain, "Tables/Q60H_other.csv", row.names = F)

#---------------------------------------------------------------------------

# Comments. Do you have any suggestions for how the Marianas fisheries should be 
  # managed or topics you feel need further study?


comments.open.ended <- guam.data.cleaned %>%
  select(comment)

write.csv(comments.open.ended, "Tables/comments_openended.csv", row.names = F)


