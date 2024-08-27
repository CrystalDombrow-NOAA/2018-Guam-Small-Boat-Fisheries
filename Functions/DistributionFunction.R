#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
  #2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.function <- function(guam.data.cleaned, q.number, categories){
  
  #Rename argument for group_by to work below
  guam.data.cleaned <- guam.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #FULL SAMPLE
  
  #Create object to calculate percentages below
  n.full.sample <- guam.data.cleaned %>%
    select(any_of(c("q.num"))) %>%
    drop_na()
  
  #Calculate full sample distribution
  q.full.sample.dist <-  guam.data.cleaned %>%
    select(any_of(c("q.num"))) %>%
    drop_na() %>%
    group_by(q.num) %>%
    mutate(percent = round(100 * n() / nrow(n.full.sample), 1)) %>%
    arrange(q.num)
    
  
  
  #Create data frame to later rbind into a table
  q.full.sample.dist <- q.full.sample.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(full.sample = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1, 2)) %>%
    mutate(n = nrow(n.full.sample), .before = V1)
  
  
  #-----------------------------------------------------
  #GFCA MEMBERSHIP
  
  #Create objects to calculate n rows
  n.gfca <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(GFCA.member == "gfca") 
  
  n.not.gfca <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(GFCA.member == "non-gfca") 
  
  
  #Calculate distribution
  q.gfca.dist <- guam.data.cleaned %>%
    select(any_of(c("GFCA.member", "q.num"))) %>%
    filter(GFCA.member == "gfca") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.gfca), 1)) %>%
    arrange(q.num)
  
  q.not.gfca.dist <- guam.data.cleaned %>%
    select(any_of(c("GFCA.member", "q.num"))) %>%
    filter(GFCA.member == "non-gfca") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.gfca), 1)) %>%
    arrange(q.num)
  
  
  
  #Create data frame to later rbind into a table 
  q.gfca.dist <- q.gfca.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(gfca = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.gfca), .before = V1)
  
  q.not.gfca.dist <- q.not.gfca.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(not.gfca = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.not.gfca), .before = V1)
  
  
  #-----------------------------------------------------
  #SELL FISH
  
  #Create objects to calculate n rows
  n.sell.fish <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "sold fish") 
  
  n.did.not.sell.fish <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "did not sell fish") 
  
  
  #Calculate distribution
  q.sell.fish.dist <- guam.data.cleaned %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "sold fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.sell.fish), 1)) %>%
    arrange(q.num)
  
  q.did.not.sell.fish.dist <- guam.data.cleaned %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "did not sell fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.did.not.sell.fish), 1)) %>%
    arrange(q.num)
  
  
  
  #Create data frame to later rbind into a table 
  q.sell.fish.dist <- q.sell.fish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(sold.fish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.sell.fish), .before = V1)
  
  q.did.not.sell.fish.dist <- q.did.not.sell.fish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(did.not.sell.fish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.did.not.sell.fish), .before = V1)
  
  
  #-----------------------------------------------------
  #HIGHLINER
  
  #Create objects to calculate n rows
  n.highliner <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "highliner") 
  
  n.not.highliner <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "not highliner") 
  
  
  #Calculate distribution
  q.highliner.dist <- guam.data.cleaned %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.highliner), 1)) %>%
    arrange(q.num)
  
  q.not.highliner.dist <- guam.data.cleaned %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "not highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.highliner), 1)) %>%
    arrange(q.num)
  
  
  
  #Create data frame to later rbind into a table 
  q.highliner.dist <- q.highliner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(highliner. = ifelse(is.na(percent), 0, percent)) %>%  #extra . in name to preserve breakdown for rbind table
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.highliner), .before = V1)
  
  q.not.highliner.dist <- q.not.highliner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(not.highliner = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.not.highliner), .before = V1)
  
  
  
  #-----------------------------------------------------
  #PRIMARY TARGET, PART I
  
  #Create objects to calculate n rows
  n.pelagic <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "pelagic") 
  
  n.bottomfish <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "bottomfish") 
  
  
  #Calculate distribution
  q.pelagic.dist <- guam.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "pelagic") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.pelagic), 1)) %>%
    arrange(q.num)
  
  q.bottomfish.dist <- guam.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "bottomfish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.bottomfish), 1)) %>%
    arrange(q.num)
  
  
  
  #Create data frame to later rbind into a table 
  q.pelagic.dist <- q.pelagic.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(pelagic = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.pelagic), .before = V1)
  
  q.bottomfish.dist <- q.bottomfish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.bottomfish), .before = V1)
  
  
  
  #-----------------------------------------------------
  #PRIMARY TARGET, PART II
  
  #Create objects to calculate n rows
  n.reef <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "reef") 
  
  n.no.primary <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "no primary") 
  
  
  #Calculate distribution
  q.reef.dist <- guam.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "reef") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.reef), 1)) %>%
    arrange(q.num)
  
  q.no.primary.dist <- guam.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "no primary") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.no.primary), 1)) %>%
    arrange(q.num)
  
  
  
  #Create data frame to later rbind into a table 
  q.reef.dist <- q.reef.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(reef = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n =  nrow(n.reef), .before = V1)
  
  q.no.primary.dist <- q.no.primary.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(no.primary = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.no.primary), .before = V1)
  
  
  
  #-----------------------------------------------------
  #BOAT OWNERSHIP
  
  #Create objects to calculate n rows
  n.boat.owner <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "boat owner") 
  
  n.not.boat.owner <- guam.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "not boat owner") 
  
  
  #Calculate distribution
  q.boat.owner.dist <- guam.data.cleaned %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "boat owner") %>% 
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.boat.owner), 1)) %>%
    arrange(q.num)
  
  q.not.boat.owner.dist <- guam.data.cleaned %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "not boat owner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.boat.owner), 1)) %>%
    arrange(q.num)

  
  
  #Create data frame to later rbind into a table 
  q.boat.owner.dist <- q.boat.owner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(boat.owner. = ifelse(is.na(percent), 0, percent)) %>% #extra . in name to preserve breakdown for rbind table
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n =  nrow(n.boat.owner), .before = V1)
  
  q.not.boat.owner.dist <- q.not.boat.owner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(not.boat.owner = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.not.boat.owner), .before = V1)
  
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  #Rbind together, for glory
  q.distribution.table <- rbind(q.full.sample.dist, 
                                q.gfca.dist,
                                q.not.gfca.dist,
                                q.sell.fish.dist,
                                q.highliner.dist,
                                q.not.highliner.dist,
                                q.did.not.sell.fish.dist,
                                q.pelagic.dist,
                                q.bottomfish.dist, 
                                q.reef.dist, 
                                q.no.primary.dist, 
                                q.boat.owner.dist,
                                q.not.boat.owner.dist) 
  
  
  #Save final table 
  write.csv(q.distribution.table, na = "0.0", 
            paste("Tables/", q.number, sep = "", "_distribution.csv"))
  
  
  
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

