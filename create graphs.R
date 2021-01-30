

#The purpose is to create new figs to Party PReference.
# EO 30.10.2020
# EO 09.11.2020

remove(list = ls())

library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(readxl)
library(ggplot2)
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

mydata <-
 read_from_gsheet(googlesheet_key = "1HHTxRCQ5_MnP5kPqDeEpyGwuIurmxCq60h6TTN7NwAw",
                 sheet_name_to_read = "Party_family_preference_by_CB_LR",
                 columnname_indicator = "Top 2 CB"
                 ) %>% 
  janitor::remove_empty(which = c("rows", "cols")) %>% 
  janitor::clean_names() %>% 
  hablar::retype() %>% 
  filter(!is.na(top_2_cb)) 



# remove non significant numbers
mydata_na <- mydata
for (i in 2:ncol(mydata)) {
  for (j in 1:nrow(mydata)) {
    if (is.na(mydata[j, i]))  {
      mydata_na[j, i - 1] <- NA
    }
  }
}


totparty <- mydata_na %>%
  dplyr::slice_max(., order_by = n, n = 19) %>% 
  remove_col_if_ends_with("sig") %>% 
  arrange(mean_left_right)

summarise(totparty, n, top_2_cb, mean_left_right )  # a check   


names(totparty)
write.csv2(totparty, "totparty_preference_by_cultural_bias_sig.csv")

# totparty <-  read.csv2("totparty_preference_by_cultural_bias_sig.csv")  

partylist <- names(totparty[2:9])


totparty_long <-
gather(totparty, 
       socialist_left, soc_dem, agrarian  , liberal ,
       christian , conservative, progress, green  , 
       key = "party_family",
       value = "support_pct" )






# socialist_left  
ggplot2::ggplot(totparty, aes(mean_left_right, socialist_left)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Socialist Left Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "socialist_left LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  



# soc_dem  
ggplot2::ggplot(totparty, aes(mean_left_right, soc_dem)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Social Democratic Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "soc_dem LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  


# agrarian  
ggplot2::ggplot(totparty, aes(mean_left_right, agrarian)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Agrarian Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "agrarian LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  

# Liberal  
ggplot2::ggplot(totparty, aes(mean_left_right, liberal)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Liberal Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "Liberal LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  

# Christian  
ggplot2::ggplot(totparty, aes(mean_left_right, christian)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Christian Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "Christian LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  



# Green  
ggplot2::ggplot(totparty, aes(mean_left_right, green)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Green Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "Green LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  


# Conservative  
ggplot2::ggplot(totparty, aes(mean_left_right, conservative)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Conservative Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "Conservative LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  

# Progress  
ggplot2::ggplot(totparty, aes(mean_left_right, progress)) + 
  ggplot2::geom_point(color = "grey", aes(size = n))               +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Progress Party Family Supporters (percent)",
                caption ="Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "Progress LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  

partylist

# socialist_left  and Conservative

totparty_long %>% 
  filter(str_detect(party_family,"socialist_left|conservative")) %>% 
  ggplot(., aes(mean_left_right, support_pct)) + 
  ggplot2::geom_point(aes(size = n, shape = party_family)) +
  scale_shape_manual(values = c(1,4)) +
    scale_shape_manual(values = c(1,4)) +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  labs(x = "Mean Left-Right Position",
                y = "Party Family Supporters (percent)",
                caption = "Only significant deviations in party preference are included.")  +
  theme_gray()


   #  geom_text(aes(label = top_2_cb, size = n, color = party_family)) +  

ggplot2::ggsave(filename = "socialist_conservative LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  




totparty_long %>% 
  filter(str_detect(party_family,"progress|soc_dem")) %>% 
  ggplot(., aes(mean_left_right, support_pct)) + 
  ggplot2::geom_point(aes(size = n, shape = party_family)) +
  scale_shape_manual(values = c(1,4,22)) +
  ggrepel::geom_label_repel(aes(label = top_2_cb))        +
  labs(x = "Mean Left-Right Position",
       y = "Party Family Supporters (percent)",
       caption = "Only significant deviations in party preference are included.")  +
  theme_gray()


# 
# for (partynm in partylist) {
#   yaxis <- partynm
#   print(yaxis)
#   yaxislabel <- snakecase::to_title_case(paste0(partynm, " Party Party Supporters (percent)"))
#   filenm <- paste0(yaxislabel, ".png")
#   print(
#   ggplot2::ggplot(totparty, aes(mean_left_right, yaxis)) + 
#   ggplot2::geom_point(color = "grey", aes(size = n))               +
#   ggrepel::geom_label_repel(aes(label = top_2_cb))        +
#   ggplot2::labs(x = "Mean Left-Right Position",
#        y = yaxislabel,
#        caption ="Only significant deviations in party preference are included."
#        )
# 
# 
#   )
#   
#   Sys.sleep(5)
#   
#   ggplot2::ggsave(filename = filenm,
#                   width = 12,
#                   units = "cm",
#                   dpi = 300)  
# }
# 


  

  
  
