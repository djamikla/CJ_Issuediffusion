install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")

library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)

################################################################################
#### Climate Justice ###
################################################################################

# read all rds files (created by data collection ccj_a - ccj_J, use name of rds files as name 
# for dataframes)

ccj_a <- readRDS("Data/Twitter/RDS_gesamt/ccj_a.rds")
ccj_b <- readRDS("Data/Twitter/RDS_gesamt/ccj_b.rds")
ccj_c <- readRDS("Data/Twitter/RDS_gesamt/ccj_c.rds")
ccj_d <- readRDS("Data/Twitter/RDS_gesamt/ccj_d.rds")
ccj_e <- readRDS("Data/Twitter/RDS_gesamt/ccj_e.rds")
ccj_f <- readRDS("Data/Twitter/RDS_gesamt/ccj_f.rds")
ccj_g <- readRDS("Data/Twitter/RDS_gesamt/ccj_g.rds")
ccj_h <- readRDS("Data/Twitter/RDS_gesamt/ccj_h.rds")
ccj_I1 <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1.rds")
ccj_I1a <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1a.rds")
ccj_I1b <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1b.rds")
ccj_I1c <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1c.rds")
ccj_I1d <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1d.rds")
ccj_I1e <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1e.rds")
ccj_I1f <- readRDS("Data/Twitter/RDS_gesamt/ccj_I1f.rds")
ccj_I2 <- readRDS("Data/Twitter/RDS_gesamt/ccj_I2.rds")
ccj_J <- readRDS("Data/Twitter/RDS_gesamt/ccj_J.rds")



# unnest relevant nested variables in each dataframe
# variables needed: text, id, public_metrics (nested: retweet_count, reply_count, like_count, 
# quote_count, impression_count), author_id, created_at, lang, withheld 
# (nested: country_codes), geo (double nested: place_id,types, coordinates)  

CCJ_a <- unnest(ccj_a, cols = c(public_metrics, withheld))
CCJ_b <- unnest(ccj_b, cols = c(public_metrics, withheld, geo))
CCJ_b <- unnest(CCJ_b, cols = c(coordinates))
CCJ_c <- unnest(ccj_c, cols = c(public_metrics, geo))
CCJ_c <- unnest(CCJ_c, cols = c(coordinates))
CCJ_d <- unnest(ccj_d, cols = c(public_metrics, geo, withheld))
CCJ_d <- unnest(CCJ_d, cols = c(coordinates))
CCJ_e <- unnest(ccj_e, cols = c(public_metrics, geo, withheld))
CCJ_e <- unnest(CCJ_e, cols = c(coordinates))
CCJ_f <- unnest(ccj_f, cols = c(public_metrics, geo, withheld))
CCJ_f <- unnest(CCJ_f, cols = c(coordinates))
CCJ_g <- unnest(ccj_g, cols = c(public_metrics, geo, withheld))
CCJ_g <- unnest(CCJ_g, cols = c(coordinates))
CCJ_h <- unnest(ccj_h, cols = c(public_metrics, geo, withheld))
CCJ_h <- unnest(CCJ_h, cols = c(coordinates))
CCJ_I1 <- unnest(ccj_I1, cols = c(public_metrics, geo, withheld))
CCJ_I1 <- unnest(CCJ_I1, cols = c(coordinates))
CCJ_I1a <- unnest(ccj_I1a, cols = c(public_metrics, geo))
CCJ_I1a <- unnest(CCJ_I1a, cols = c(coordinates))
CCJ_I1b <- unnest(ccj_I1b, cols = c(public_metrics, geo, withheld))
CCJ_I1b <- unnest(CCJ_I1b, cols = c(coordinates))
CCJ_I1c <- unnest(ccj_I1c, cols = c(public_metrics, geo, withheld))
CCJ_I1c <- unnest(CCJ_I1c, cols = c(coordinates))
CCJ_I1d <- unnest(ccj_I1d, cols = c(public_metrics, geo, withheld))
CCJ_I1d <- unnest(CCJ_I1d, cols = c(coordinates))
CCJ_I1e <- unnest(ccj_I1e, cols = c(public_metrics, geo, withheld))
CCJ_I1e <- unnest(CCJ_I1e, cols = c(coordinates))
CCJ_I1f <- unnest(ccj_I1f, cols = c(public_metrics, geo, withheld))
CCJ_I1f <- unnest(CCJ_I1f, cols = c(coordinates))
CCJ_I2 <- unnest(ccj_I2, cols = c(public_metrics, geo))
CCJ_I2 <- unnest(CCJ_I2, cols = c(coordinates))
CCJ_J <- unnest(ccj_J, cols = c(public_metrics, geo, withheld))
CCJ_J <- unnest(CCJ_J, cols = c(coordinates))


# select only needed variables for each dataframe: text, id, retweet_count, reply_count, 
# like_count, quote_count, impression_count, author_id, created_at, lang, country_codes,
# place_id,types, coordinates 

# combine all dataframes in one big dataframe ("twitter"), containing only the needed variables 
# dataframes in which relevant variables are not existent: add NA as value for theses variables 
 Twitter_CJ<- bind_rows(CCJ_a,CCJ_b,CCJ_c,CCJ_d,CCJ_e,CCJ_f,CCJ_g,
            CCJ_h,CCJ_I1,CCJ_I1a,CCJ_I1b,CCJ_I1c,
            CCJ_I1d,CCJ_I1e,CCJ_I1f,CCJ_I2,CCJ_J) %>% 
   select(c(text, id, retweet_count, reply_count,
            like_count, quote_count, impression_count,
            author_id, created_at, lang, country_codes,
            place_id, type, coordinates))
    #1301192 cases
 
#remove unused dataframes
rm(ccj_a, ccj_b,ccj_c,ccj_d,ccj_e,ccj_f,ccj_g,ccj_h,ccj_I1,ccj_I1a,ccj_I1b,ccj_I1c,ccj_I1d,ccj_I1e,ccj_I1f,ccj_I2,ccj_J)
rm(CCJ_a, CCJ_b,CCJ_c,CCJ_d,CCJ_e,CCJ_f,CCJ_g,CCJ_h,CCJ_I1,CCJ_I1a,CCJ_I1b,CCJ_I1c,CCJ_I1d,CCJ_I1e,CCJ_I1f,CCJ_I2,CCJ_J)
 
#### identify and exclude duplicates according to id (cause by overlappting data aquisition periods)
Twitter_CJ <- Twitter_CJ %>%
  distinct(id, .keep_all = TRUE)
  # 1255723

## Dataframe twitter is only to be including the terms: "climate justice", "climatejustice" etc. (not "climate" and "justice")
Twitter_CJ2 <- Twitter_CJ %>% filter(str_detect(text, "climate justice|Climate Justice|climatejustice|ClimateJustice|Climatejustice|Climate justice|climate Justice |climateJustice | CLIMATEJUSTICE |CLIMATE JUSTICE"))
  # 832021 cases


#################################################################################
### Klimagerechtigkeit###
#################################################################################

# read dataframe "klimagerechtigkeit"
twitter_KG <- readRDS("Data/Twitter/KG.rds")
  #50408 cases


# unnesting of relevant nested variables
Twitter_KG <- unnest(twitter_KG, cols = c(public_metrics, withheld, geo))
Twitter_KG <- unnest(Twitter_KG, cols = c(coordinates))

# excluding unrelevant variables and appoint NA to fill non existent Data
Twitter_KG <- bind_rows(Twitter_KG) %>% 
  select(c(text, id, retweet_count, reply_count,
           like_count, quote_count, impression_count,
           author_id, created_at, lang, country_codes,
           place_id, type, coordinates))

################################################################################
### Justicia Climatica ###
################################################################################

# read dataframe "justicia climatica"
twitter_JC <- readRDS("Data/Twitter/JC.rds")
  #57972 cases

# unnesting of relevant nested variables
Twitter_JC <- unnest(twitter_JC, cols = c(public_metrics, withheld, geo))
Twitter_JC <- unnest(Twitter_JC, cols = c(coordinates))

# excluding unrelevant variables and appoint NA to fill non existent Data
Twitter_JC <- bind_rows(Twitter_JC) %>% 
  select(c(text, id, retweet_count, reply_count,
           like_count, quote_count, impression_count,
           author_id, created_at, lang, country_codes,
           place_id, type, coordinates))

## Dataframe twitter is only to be including the terms: "justicia climatica", "justiciaclimatica" etc. (not "justicia" and "climatica") justiciaclimática
Twitter_JC <- Twitter_JC %>% filter(str_detect(text, "justicia climatica|justicia climática|Justicia Climatica|Justicia Climática|justiciaclimatica|justiciaclimática|JusticiaClimatica|JusticiaClimática|Justiciaclimatica|Justiciaclimática|Justicia climatica|Justicia climática|justicia Climatica|justicia Climática|justiciaClimatica|justiciaClimática|JUSTICIA CLIMÁTICA|JUSTICIA CLIMATICA"))
  # 42784 cases

#remove unused dataframes
rm(twitter_JC, twitter_KG)


#### create joint dataframe for KG, JC and CJ####
# Add source column to each dataframe
Twitter_CJ$source <- "CJ"
Twitter_JC$source <- "JC"
Twitter_KG$source <- "KG"

# Combine the dataframes
joint_twitter <- bind_rows(Twitter_CJ, Twitter_JC, Twitter_KG)
  #1348915

#### check for duplicates ####
# Identify duplicates (excluding "source" column)
duplicates <- joint_twitter %>%
  select(-source) %>%  # Exclude the "source" column
  duplicated()

# Subset the dataframe to show only duplicates
duplicated_data <- joint_twitter[duplicates, ]

# exclude duplicates
joint_twitter <- anti_join(joint_twitter, duplicated_data, by = "id")
  # 872626 cases

#### export final dataframe ####

save(joint_twitter, file = "Data//joint_twitter.RData")
