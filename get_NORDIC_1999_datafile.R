

#The purpose is to create new figs to Party PReference.
# EO 30.10.2020
# EO 09.11.2020

remove(list = ls())

library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(readxl)
library(svMisc)
library(janitor)
library(hablar)
library(googledrive)
library(googlesheets4)
library(dplyr)


source("../Startup/eeros_functions.R")

googledrive::drive_auth(email = "eero.olli@gmail.com")
googlesheets4::gs4_auth(token = drive_token())

setwd("../CT and LR with Brendon")


partylist_l <- c("Socialist Left", "Social Democrat", "Conservative" ,
                 "Liberal",        "Agrarian"  ,      "Christian"   ,             
                 "Progress" ,      "Green")      


partylist_n <- c("lsoc",  "socdem", "cons"   , 
                 "liber", "agrar" , "christ" , 
                 "progr", "green" )

#### GET DATA ----------

mydata <-
  read_from_gsheet(googlesheet_key = "1HHTxRCQ5_MnP5kPqDeEpyGwuIurmxCq60h6TTN7NwAw",
                   sheet_name_to_read = "Party_family_preference_by_CB_LR",
                   columnname_indicator = "Top 2 CB"
  ) %>% 
  janitor::remove_empty(which = c("rows", "cols")) %>% 
  janitor::clean_names() %>% 
  hablar::retype() %>% 
  filter(!is.na(top_2_cb)) 

# remove non significant numbers in alternative data
mydata_na <- mydata
for (i in 2:ncol(mydata)) {
  for (j in 1:nrow(mydata)) {
    if (is.na(mydata[j, i]))  {
      mydata_na[j, i - 1] <- NA  # drop the value in the column on left side
    }
  }
}

# replace the NA in main
mydata <- mydata %>% 
  mutate(across(.cols = ends_with("_sig"), ~str_replace_na(., replacement = ""))) %>%  # Make all NA sig to "not sign."
  mutate(across(.cols = ends_with("_sig"), ~as_numeric(str_length(.)))) %>%
  # transmute(across(.cols = ends_with("_sig"), ~str_length(.))) %>%
  set_labels(., ends_with("_sig"), labels = c("non sign." = 0  , 
                                              "0.05"      = 1        ,
                                              "0.01"      = 2        ,
                                              "0.001"     = 3   ) ,
             force.labels = TRUE ) %>% 
  as_factor(., ends_with("_sig"), add.non.labelled = TRUE) 

str(mydata)
names(mydata)
Hmisc::contents(mydata)


# make labels-factors
mydata <- 
  rename(mydata, 
         socialist_left_sig = sl_sig,
         soc_dem_sig = sd_sig,
         agrarian_sig = ag_sig,
         liberal_sig = lib_sig,
         christian_sig = chr_sig,
         conservative_sig = con_sig,
         progress_sig = pro_sig,
         green_sig = gr_sig,
         other_sig = o_sig) 


str(mydata)  
names(mydata)
Hmisc::contents(mydata)



#### All 19 Cases -------------
# Totparty19 <- mydata_na %>%     # only the signifcant cases
Totparty19 <- mydata %>% # All 19 cases
  dplyr::slice_max(., order_by = n, n = 19) %>% 
  remove_col_if_ends_with("sig") %>% 
  arrange(mean_left_right)

summarise(Totparty19, n, top_2_cb, mean_left_right )  # a check   

names(Totparty19)
write.csv2(Totparty19, "Totparty19_preference_by_cultural_bias_sig.csv")

# Totparty19 <-  read.csv2("Totparty19_preference_by_cultural_bias_sig.csv")  

partylist <- c(
  "socialist_left" ,
  "soc_dem" ,
  "conservative" ,
  "liberal"     ,
  "agrarian"    ,
  "christian"  ,
  "progress"   ,
  "green"
)

#names(Totparty19[2:9]  # these are in wrong order!


#### All 57 cases are used to create the LloWess Soothed line

Totparty57 <- mydata %>%
  dplyr::slice_max(., order_by = n, n = 57) %>% 
  arrange(mean_left_right) 

summarise(Totparty57, n, top_2_cb, mean_left_right )  # a check   


names(Totparty57)
write.csv2(Totparty57, "Totparty57_preference_by_cultural_bias_sig.csv")

# Totparty57 <-  read.csv2("Totparty57_preference_by_cultural_bias_sig.csv")  



