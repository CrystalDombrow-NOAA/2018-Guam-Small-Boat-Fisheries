#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.gear.trips <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.mult.vars.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.mult.vars <- guam.mult.vars.cleaned %>%
    select(any_of(c("Q2A.mid", "Q2B.mid", "Q2C.mid", "Q2D.mid", "Q2E.mid", 
                    "Q2F.mid", "Q2G.mid", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.trolling = sum(Q2A.mid),
           total.deepbf = sum(Q2B.mid),
           total.shallowbf = sum(Q2C.mid),
           total.atulai = sum(Q2D.mid),
           total.reefspear = sum(Q2E.mid),
           total.reefnet = sum(Q2F.mid),
           total.other = sum(Q2G.mid)) %>%
    mutate(total.q2 = (total.trolling + total.deepbf + total.shallowbf +
                       total.atulai + total.reefspear + total.reefnet +
                       total.other)) %>%
    mutate(trolling.perc = round(total.trolling / total.q2 * 100, 1),
           deepbf.perc = round(total.deepbf / total.q2 * 100, 1),
           shallowbf.perc = round(total.shallowbf / total.q2 * 100, 1),
           atulai.perc = round(total.atulai / total.q2 * 100, 1),
           reefspear.perc = round(total.reefspear / total.q2 * 100, 1),
           reefnet.perc = round(total.reefnet / total.q2 * 100, 1),
           other.perc = round(total.other / total.q2 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.mult.vars <- dist.mult.vars  %>% 
    select(-Q2A.mid, -Q2B.mid, -Q2C.mid, -Q2D.mid, -Q2E.mid, -Q2F.mid, -Q2G.mid, 
           -total.trolling, -total.deepbf, -total.shallowbf, -total.atulai,
           -total.reefspear, -total.reefnet, -total.other, -total.q2) %>%
    ungroup() %>%
    unique()
  
  
  #Return final dataframe.
  return(dist.mult.vars)
  
}
