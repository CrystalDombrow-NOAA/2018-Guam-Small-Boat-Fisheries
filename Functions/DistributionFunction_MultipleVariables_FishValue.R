#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.fish.value <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.mult.vars.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.mult.vars <- guam.mult.vars.cleaned %>%
    select(any_of(c("Q21A.mid", "Q21B.mid", "Q21C.mid", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.pelagic = sum(Q21A.mid),
           total.bottomfish = sum(Q21B.mid),
           total.reef = sum(Q21C.mid)) %>%
    mutate(total.q21 = (total.pelagic + total.bottomfish + total.reef)) %>%
    mutate(pelagic.perc = round(total.pelagic / total.q21 * 100, 1),
           bottomfish.perc = round(total.bottomfish / total.q21 * 100, 1),
           reef.perc = round(total.reef / total.q21 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.mult.vars <- dist.mult.vars  %>% 
    select(-Q21A.mid, -Q21B.mid, -Q21C.mid, -total.pelagic, -total.bottomfish, 
           -total.reef, -total.q21) %>%
    ungroup() %>%
    unique()
  
  
  #Return final dataframe.
  return(dist.mult.vars)
  
}
