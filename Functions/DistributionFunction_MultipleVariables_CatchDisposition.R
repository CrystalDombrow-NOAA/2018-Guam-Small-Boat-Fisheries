#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.catch.disposition <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.catch.dis.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.catch.disposition <- guam.catch.dis.cleaned %>%
    select(any_of(c("Q16A.mid", "Q16B.mid", "Q16C.mid", "Q16D.mid", "Q16E.mid", 
                    "Q16F.mid", "Q16G.mid", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.sold = sum(Q16A.mid),
           total.home = sum(Q16B.mid),
           total.family = sum(Q16C.mid),
           total.fr.neighbors = sum(Q16D.mid),
           total.fiestas.events = sum(Q16E.mid),
           total.traded = sum(Q16F.mid),
           total.released = sum(Q16G.mid)) %>%
    mutate(total.q16 = (total.sold + total.home + total.family + 
                          total.fr.neighbors + total.fiestas.events + 
                          total.traded + total.released)) %>%
    mutate(sold.perc = round(total.sold / total.q16 * 100, 1),
           home.perc = round(total.home / total.q16 * 100, 1),
           family.perc = round(total.family / total.q16 * 100, 1),
           fr.neighbors.perc = round(total.fr.neighbors / total.q16 * 100, 1),
           fiestas.events.perc = round(total.fiestas.events / total.q16 * 100, 1),
           traded.perc = round(total.traded / total.q16 * 100, 1),
           released.perc = round(total.released / total.q16 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.catch.disposition <- dist.catch.disposition  %>% 
    select(-Q16A.mid, -Q16B.mid, -Q16C.mid, -Q16D.mid, -Q16E.mid, -Q16F.mid, 
           -Q16G.mid, -total.sold, -total.home, -total.family, -total.fr.neighbors, 
           -total.fiestas.events, -total.traded, -total.released, -total.q16) %>%
    ungroup()
  
  
  #Return final dataframe.
  return(dist.catch.disposition)
  
}

