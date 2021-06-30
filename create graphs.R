

#The purpose is to create new figs to Party PReference.
# EO 30.10.2020
# EO 09.11.2020



#### Intro -------------
remove(list = ls())

library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(readr)
library(svMisc)
library(janitor)
library(hablar)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(patchwork)


source("../Startup/eeros_functions.R")

googledrive::drive_auth(email = "eero.olli@gmail.com")
googlesheets4::gs4_auth(token = drive_token())

setwd("../cultural-bias-and-left-right-paper")


# Lists of party names must stay in same order

partylist_l <- c("Socialist Left", "Social Democrat", "Conservative" ,
                 "Liberal",        "Agrarian"  ,      "Christian"   ,             
                 "Progress" ,      "Green")      


partylist_n <- c("lsoc",  "socdem", "cons"   , 
                 "liber", "agrar" , "christ" , 
                 "progr", "green" )

partylist <- c(
    "socialist_left" ,     "soc_dem"     ,     "conservative" ,
    "liberal"        ,     "agrarian"    ,     "christian"  ,
    "progress"       ,     "green"   )



#### READ the DATA


Totparty19 <- read.csv2("Totparty19_preference_by_cultural_bias_sig.csv")   %>%
  hablar::retype() %>% 
  select(!starts_with("X")) %>% 
  set_labels(., ends_with("_sig"), 
             labels = c("non sign." = 0        ,
                        "0.05"      = 1        ,
                        "0.01"      = 2        ,
                        "0.001"     = 3   )    ,
             force.labels = TRUE ) %>%
  as_factor(., ends_with("_sig"), add.non.labelled = TRUE)

 
   
View(Totparty19)


Totparty57 <- read_csv2("Totparty57_preference_by_cultural_bias_sig.csv") %>%  
  hablar::retype() %>% 
  select(!starts_with("X")) %>% 
  set_labels(., ends_with("_sig"), 
             labels = c("non sign." = 0        ,
                        "0.05"      = 1        ,
                        "0.01"      = 2        ,
                        "0.001"     = 3   )    ,
             force.labels = TRUE ) %>%
  as_factor(., ends_with("_sig"), add.non.labelled = TRUE)


View(Totparty57)



#### Data into Long format -----------

Totparty19_long <- Totparty19 %>% 
  select(!ends_with("sig")) %>%
  select(!ends_with("sum")) %>%
  pivot_longer( 
    cols = socialist_left:other  , 
    names_to = "party_family",
    values_to = "support_pct" )
  


Totparty57_long <- Totparty57 %>% 
  select(!ends_with("sig")) %>%
  select(!ends_with("sum")) %>%
  pivot_longer( 
    cols = socialist_left:other  , 
    names_to = "party_family",
    values_to = "support_pct" )



# MAKE Graphs ------------
# NO LINE 
# 
# for (i in seq_along(partylist)) {
#   # linedata <-  Totparty57_long  %>% 
#   #   filter(str_detect(party_family,partylist[i])) %>% 
#   #   select(mean_left_right, support_pct)
#   
#   Totparty19_long %>% 
#     filter(str_detect(party_family,partylist[i])) %>% 
#     ggplot(., aes(mean_left_right, support_pct)) + 
#     ggplot2::geom_point(aes(size = n)) +
#     labs(title = str_to_title(paste0(partylist_l[i], " Party Family")),
#          caption = "Only statistically significant of the 19 most frequent top 2 cultural biases are included.")  +
#     xlab("Mean Left-Right Position") +
#     ylab(paste0(partylist_l[i], " Party Family Supporters (percent)")) +
#     ggrepel::geom_label_repel(aes(label = top_2_cb))        +
#     # ggplot2::stat_smooth(data = linedata,
#                          # method = "loess", 
#                          # fullrange = FALSE,
#                          # se = FALSE,
#                          # span = 4) +
#     theme_gray() + 
#     theme(plot.caption = element_text(size = rel(0.55)))
#   
#   print(partylist_l[i])
#   
#   ggplot2::ggsave(filename = paste0(partylist[i],"_loess_noline_dots19.png"),
#                   width = 12,
#                   units = "cm",
#                   dpi = 300)  
# }




#### Graphs with 57 cases for the line and 19 dots ----

# knitr::opts_chunk$set(echo = TRUE,
#                      fig.height = 9,
#                      fig.width = 9)


plots_19 = list() # must be defined before the for() 

for (i in seq_along(partylist)) {
linedata <-  Totparty57_long  %>%   
  filter(str_detect(party_family,partylist[i])) %>% 
  select(mean_left_right, support_pct)
  
the_plot <-
Totparty19_long %>% 
  filter(str_detect(party_family,partylist[i])) %>% 
  ggplot(., aes(mean_left_right, support_pct)) + 
  ggplot2::geom_point(aes(size = n)) +
  labs(title = str_to_title(paste0(partylist_l[i])))  + # , " Party Family"
  xlab("Mean Left-Right") +
  ylab("") +  #Supporters (percent)
  ggplot2::stat_smooth(data = linedata,
                       method = "loess", 
                       fullrange = FALSE,
                       se = FALSE,
                       span = 4) +
  ggrepel::geom_label_repel(aes(label = top_2_cb),
                            label.padding = 0.15,
                            size = 2)        +
  theme_gray() + 
      theme(plot.caption = element_text(size = rel(0.55)))

  print(partylist_l[i])
  
  plots_19[[partylist_l[i]]] <- the_plot
  
  # plots_19 <- the_plot
  
  ggplot2::ggsave(filename = paste0(partylist[i],"_loess_line57_dots19.png"),
                  width = 12,
                  units = "cm",
                  dpi = 600)  
}


#### Collect all plots on one page. ------------
#### using the Patchwork package
plots_19[["Socialist Left"]] +
  plots_19[["Social Democrat"]] +
  plots_19[["Agrarian"]] +
  plots_19[["Christian"]] +
  plots_19[["Conservative"]]  +
  plots_19[["Progress"]] +
  plots_19[["Green"]] +
  plots_19[["Liberal"]] +
  guide_area() +
  plot_layout(
    ncol = 3,
    nrow = 3,
    byrow = TRUE,
    guides =  "collect"
  ) +
  plot_annotation(caption = "Dots show the 19 most frequent and the Loess smoothed line uses all 57 Top Two Cultural Biases.")
# "Dots show the 19 most frequent Top Two Cultural Biases. Loess smoothed line.")
# title = "Support for Party Family (percent)"   # removed because the Caption is text in the paper.

ggplot2::ggsave(
  filename = "8_Partyfamilies_loess_line57_dots19.png",
  width = 21,
  height = 27,
  units = "cm",
  dpi = 600
)

  


#### Graphs with 57 cases ----
# 
# tabyl(Totparty57_long, green_sig)
# 
# for (i in seq_along(partylist)) {
#  linedata <-  Totparty57_long  %>% 
#    filter(str_detect(party_family,partylist[i])) %>% 
#    select(mean_left_right, support_pct)
#   
#   Totparty57_long %>% 
#     filter(str_detect(party_family,partylist[i])) %>% 
#     ggplot(., aes(x = mean_left_right, 
#                   y = support_pct)) +
#     guides(fill = "none") +
#     ggplot2::geom_point(size = 4.5, color = "#707070") +
#     labs(title = str_to_title(partylist_l[i]),
#          caption = "All 57 cultural combinations are included. Loess smoothed line.")  +
#     xlab("Mean Left-Right Position") +
#     ylab("Party Family Supporters (percent)") +
#    # scale_shape_manual(values = c(1,4,22)) +
#     ggrepel::geom_label_repel(aes(label = top_2_cb))        +
#     ggplot2::stat_smooth(method = "loess", 
#                          fullrange = FALSE,
#                          se = FALSE,
#                          span = 3) +
#     theme_gray() + 
#     theme(plot.caption = element_text(size = rel(0.55))) 
# 
#   print(partylist[i])
#   
#   
#   ggplot2::ggsave(filename = paste0(partylist[i],"_loess_line57_dots57.png"),
#                   width = 12,
#                   units = "cm",
#                   dpi = 600)  
# }

# #### Graphs with 57 cases ----
# 
# tabyl(Totparty57_long, green_sig)
# 
# for (i in seq_along(partylist)) {
#   linedata <-  Totparty57_long  %>% 
#     filter(str_detect(party_family,partylist[i])) %>% 
#     select(mean_left_right, support_pct)
#   
#   Totparty57_long %>% 
#     filter(str_detect(party_family,partylist[i])) %>% 
#     ggplot(., aes(x = mean_left_right, 
#                   y = support_pct)) +
#     guides(fill = "none") +
#     ggplot2::geom_point(aes_string(col = paste0(partylist[i], "_sig")), size = 5) +
#     scale_colour_grey(
#       start = 1,
#       end = 0.3,
#       aesthetics = "colour",
#       labels = c("non sign.", "0.05" , "0.01", "0.001"),
#       limits = c(0, 1, 2, 3))  +
#     
#     labs(title = str_to_title(partylist[i]),
#          caption = "All 57 cultural combinations are included. Loess smoothed line.")  +
#     xlab("Mean Left-Right Position") +
#     ylab("Party Family Supporters (percent)") +
#     # scale_shape_manual(values = c(1,4,22)) +
#     ggrepel::geom_label_repel(aes(label = top_2_cb))        +
#     ggplot2::stat_smooth(method = "loess", 
#                          fullrange = FALSE,
#                          se = FALSE,
#                          span = 3) +
#     theme_gray() + 
#     theme(plot.caption = element_text(size = rel(0.55))) 
#   
#   print(partylist[i])
#   
#   ggplot2::ggsave(filename = paste0(partylist[i],"_loess_line57_dots57_sign.png"),
#                   width = 12,
#                   units = "cm",
#                   dpi = 300)  
# }
# 

  

  
  
