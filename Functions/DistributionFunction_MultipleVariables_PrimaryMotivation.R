#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
distribution.function.primary.motivation <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.pri.mot.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Distribution calculation
  dist.primary.motivation <- guam.pri.mot.cleaned %>%
    select(any_of(c("Q11A.primarymotiv", "Q11B.primarymotiv", 
                    "Q11C.primarymotiv", "Q11D.primarymotiv", 
                    "Q11E.primarymotiv", "Q11F.primarymotiv", 
                    "Q11G.primarymotiv", "Q11I", "brk.down"))) %>%
    drop_na() %>%
    group_by(brk.down) %>%
    mutate(total.pure.rec = sum(Q11A.primarymotiv),
           total.rec.expense = sum(Q11B.primarymotiv),
           total.subsistence = sum(Q11C.primarymotiv),
           total.cultural = sum(Q11D.primarymotiv),
           total.pt.comm = sum(Q11E.primarymotiv),
           total.ft.comm = sum(Q11F.primarymotiv),
           total.other = sum(Q11G.primarymotiv),
           total.mult.mot = sum(Q11I)) %>%
    mutate(total.q11 = (total.pure.rec + total.rec.expense + total.subsistence + 
                          total.cultural + total.pt.comm + total.ft.comm 
                        + total.other + total.mult.mot)) %>%
    mutate(pure.rec.perc = round(total.pure.rec / total.q11 * 100, 1),
           rec.expense.perc = round(total.rec.expense / total.q11 * 100, 1),
           subsistence.perc = round(total.subsistence / total.q11 * 100, 1),
           cultural.perc = round(total.cultural / total.q11 * 100, 1),
           pt.comm.perc = round(total.pt.comm / total.q11 * 100, 1),
           ft.comm.perc = round(total.ft.comm / total.q11 * 100, 1),
           other.perc = round(total.other / total.q11 * 100, 1),
           mult.mot.perc = round(total.mult.mot / total.q11 * 100, 1))
  
  
  #Rename brk.down, remove unneeded columns, set up dataframe to later rbind in 
  #section script for table.
  dist.primary.motivation <- dist.primary.motivation  %>% 
    select(-Q11A.primarymotiv, -Q11B.primarymotiv, -Q11C.primarymotiv, 
           -Q11D.primarymotiv, -Q11E.primarymotiv, -Q11F.primarymotiv, 
           -Q11G.primarymotiv, -Q11I, -total.pure.rec, 
           -total.rec.expense, -total.subsistence, -total.cultural, 
           -total.pt.comm, -total.ft.comm, -total.other, -total.mult.mot,
           -total.q11) %>%
    ungroup()
  
  
  #Return final dataframe.
  return(dist.primary.motivation)
  
}

