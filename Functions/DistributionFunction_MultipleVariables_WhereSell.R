#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.where.sell <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.where.sell.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.where.sell <- guam.where.sell.cleaned %>%
    select(any_of(c("Q17A.mid", "Q17B.mid", "Q17C.mid", "Q17D.mid", "Q17E.mid", 
                    "Q17F.mid", "Q17G.mid", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.gfcoop = sum(Q17A.mid),
           total.rdvendor = sum(Q17B.mid),
           total.marketstores = sum(Q17C.mid),
           total.hotelsrest = sum(Q17D.mid),
           total.friendsetc = sum(Q17E.mid),
           total.wholsesaler = sum(Q17F.mid),
           total.other = sum(Q17G.mid)) %>%
    mutate(total.q17 = (total.gfcoop + total.rdvendor + total.marketstores + 
                        total.hotelsrest + total.friendsetc + total.wholsesaler 
                        + total.other)) %>%
    mutate(gfcoop.perc = round(total.gfcoop / total.q17 * 100, 1),
           rdvendor.perc = round(total.rdvendor / total.q17 * 100, 1),
           marketstores.perc = round(total.marketstores / total.q17 * 100, 1),
           hotelsrest.perc = round(total.hotelsrest / total.q17 * 100, 1),
           friendsetc.perc = round(total.friendsetc / total.q17 * 100, 1),
           wholsesaler.perc = round(total.wholsesaler / total.q17 * 100, 1),
           other.perc = round(total.other / total.q17 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.where.sell <- dist.where.sell  %>% 
    select(-Q17A.mid, -Q17B.mid, -Q17C.mid, -Q17D.mid, -Q17E.mid, -Q17F.mid, 
           -Q17G.mid, -total.gfcoop, -total.rdvendor, -total.marketstores, 
           -total.hotelsrest, -total.friendsetc, -total.wholsesaler,
           -total.other, -total.q17) %>%
    ungroup() %>%
    unique()
  
  
  #Return final dataframe.
  return(dist.where.sell)
  
}
