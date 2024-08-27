#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.no.table.function <- function(guam.data.cleaned, q.number){
  
  #Rename argument for group_by to work below
  guam.cleaned.dist.tables.fun <- guam.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #FULL SAMPLE
  
  #Create object to calculate percentages below
  n.full.sample <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("q.num"))) %>%
    drop_na()
  
  #Calculate full sample distribution
  q.full.sample.dist <-  guam.cleaned.dist.tables.fun %>%
    select(any_of(c("q.num"))) %>%
    drop_na() %>%
    group_by(q.num) %>%
    mutate(percent = round(100 * n() / nrow(n.full.sample), 1)) %>%
    arrange(q.num)
  
  
  #-----------------------------------------------------
  #GFCA MEMBERSHIP
  
  #Create objects to calculate n rows
  n.gfca <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(GFCA.member == "gfca") 
  
  n.not.gfca <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(GFCA.member == "non-gfca") 
  
  
  #Calculate distribution
  q.gfca.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("GFCA.member", "q.num"))) %>%
    filter(GFCA.member == "gfca") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.gfca), 1)) %>%
    arrange(q.num)
  
  q.not.gfca.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("GFCA.member", "q.num"))) %>%
    filter(GFCA.member == "non-gfca") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.gfca), 1)) %>%
    arrange(q.num)

  
  #-----------------------------------------------------
  #SELL FISH
  
  #Create objects to calculate n rows
  n.sell.fish <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "sold fish") 
  
  n.did.not.sell.fish <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "did not sell fish") 
  
  
  #Calculate distribution
  q.sell.fish.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "sold fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.sell.fish), 1)) %>%
    arrange(q.num)
  
  q.did.not.sell.fish.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "did not sell fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.did.not.sell.fish), 1)) %>%
    arrange(q.num)

  
  
  #-----------------------------------------------------
  #HIGHLINER
  
  #Create objects to calculate n rows
  n.highliner <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "highliner") 
  
  n.not.highliner <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "not highliner") 
  
  
  #Calculate distribution
  q.highliner.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.highliner), 1)) %>%
    arrange(q.num)
  
  q.not.highliner.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "not highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.highliner), 1)) %>%
    arrange(q.num)

  
  
  #-----------------------------------------------------
  #PRIMARY TARGET, PART I
  
  #Create objects to calculate n rows
  n.pelagic <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "pelagic") 
  
  n.bottomfish <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "bottomfish") 
  
  
  #Calculate distribution
  q.pelagic.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "pelagic") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.pelagic), 1)) %>%
    arrange(q.num)
  
  q.bottomfish.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "bottomfish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.bottomfish), 1)) %>%
    arrange(q.num)
  
  
  
  #-----------------------------------------------------
  #PRIMARY TARGET, PART II
  
  #Create objects to calculate n rows
  n.reef <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "reef") 
  
  n.no.primary <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "no primary") 
  
  
  #Calculate distribution
  q.reef.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "reef") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.reef), 1)) %>%
    arrange(q.num)
  
  q.no.primary.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "no primary") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.no.primary), 1)) %>%
    arrange(q.num)
  
  
  
  #-----------------------------------------------------
  #BOAT OWNERSHIP
  
  #Create objects to calculate n rows
  n.boat.owner <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "boat owner") 
  
  n.not.boat.owner <- guam.cleaned.dist.tables.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "not boat owner") 
  
  
  #Calculate distribution
  q.boat.owner.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "boat owner") %>% 
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.boat.owner), 1)) %>%
    arrange(q.num)
  
  q.not.boat.owner.dist <- guam.cleaned.dist.tables.fun %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "not boat owner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.boat.owner), 1)) %>%
    arrange(q.num)
  
  
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #Return final list of above objects
  q.dist.table.list <- list("q.full.sample.dist" = q.full.sample.dist,
                            "q.gfca.dist" = q.gfca.dist,
                            "q.not.gfca.dist" = q.not.gfca.dist,
                            "q.sell.fish.dist" = q.sell.fish.dist,
                            "q.did.not.sell.fish.dist" = q.did.not.sell.fish.dist,
                            "q.highliner.dist" = q.highliner.dist,
                            "q.not.highliner.dist" = q.not.highliner.dist,
                            "q.pelagic.dist" = q.pelagic.dist,
                            "q.bottomfish.dist" = q.bottomfish.dist,
                            "q.reef.dist" = q.reef.dist,
                            "q.no.primary.dist" = q.no.primary.dist,
                            "q.boat.owner.dist" = q.boat.owner.dist,
                            "q.not.boat.owner.dist" = q.not.boat.owner.dist)
  
  
  return(q.dist.table.list)
  
}

