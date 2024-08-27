#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.trip.days <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.mult.vars.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.mult.vars <- guam.mult.vars.cleaned %>%
    select(any_of(c("Q5A.mid", "Q5B.mid", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.single.day = sum(Q5A.mid),
           total.multi.day = sum(Q5B.mid)) %>%
    mutate(total.q5 = (total.single.day + total.multi.day)) %>%
    mutate(single.day.perc = round(total.single.day / total.q5 * 100, 1),
           multi.day.perc = round(total.multi.day / total.q5 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.mult.vars <- dist.mult.vars  %>% 
    select(-Q5A.mid, -Q5B.mid, -total.single.day, -total.multi.day, -total.q5) %>%
    ungroup() %>%
    unique()
  
  
  #Return final dataframe.
  return(dist.mult.vars)
  
}
