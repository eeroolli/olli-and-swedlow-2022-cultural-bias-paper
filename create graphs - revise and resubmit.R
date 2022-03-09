

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

partylist_0 <- c("Socialist Left", "Social Democrat", "Conservative" ,
                 "Liberal",        "Agrarian"  ,      "Christian"   ,             
                 "Progress" ,      "Green"     ,      "Other"
                 )      



partylist_n <- c("lsoc",  "socdem", "cons"   , 
                 "liber", "agrar" , "christ" , 
                 "progr", "green" )

partylist <- c(
    "socialist_left" ,     "soc_dem"     ,     "conservative" ,
    "liberal"        ,     "agrarian"    ,     "christian"  ,
    "progress"       ,     "green"   )




#### READ the aggregated DATA


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
  arrange(mean_left_right)

Totparty19$top_2_cb


 
   
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



# The weighted means for the party family LR ----
partyfamily_lr <-
Totparty57_long %>%
  group_by(party_family) %>% 
  summarise(across("mean_left_right",
          ~weighted.mean(.x, support_pct*n/100, na.rm = TRUE)),
          .groups = "keep")

partyfamily_support <-
  Totparty57_long %>%
  group_by(party_family) %>% 
  summarise(across("support_pct",
                   ~weighted.mean(.x, support_pct*n/100, na.rm = TRUE)),
            .groups = "keep")

partyfamily_support <-  partyfamily_support %>%   # from thesis table 40. n = 3085.
     add_column(support_thesis = c(17.9, 5.6, 21.7, 3.6, 5.4, 6.1, 4.8, 25.6, 9.1))

sum(partyfamily_support$support_thesis)

# The weighted mean for the sample
LRmean <-
  Totparty19_long %>%
  summarise(weighted.mean(Totparty19_long$mean_left_right, Totparty19_long$n * Totparty19_long$support_pct/100) ,
            .groups = "keep")  # each party family 
LRmean




# MAKE Graphs ------------
#
# The Left-Right graph
# 

# "Figure 2: The Mean Left-Right Orientation by Top Two Cultural Bias"
# 
# 

# layout
caption_text_size = 1.3  #this is used by several figures.



party19 <- Totparty19 %>%
  arrange(mean_left_right) %>% 
  select(mean_left_right, top_2_cb, n)

party19$order <- 1:nrow(party19)  

as_factor(party19$top_2_cb)

party19

lr_plot <-
  ggplot(party19, aes(mean_left_right, order, top_2_cb, n)) +
  geom_point(aes(size = n)) +
  scale_y_discrete(name = "Top Two Cultural Biases", 
                   breaks = 1:19,
                   labels = party19$top_2_cb[1:19],
                   limits = factor(1:19)) +
  labs(x = "Mean Left-Right Orientation",
       size = "n of 
respondents") +
  geom_vline(xintercept = 0.53) +   # the Sample mean for the selected 19 T2CB
  theme_gray() +
    theme(plot.caption = element_text(size = rel(caption_text_size)),
          plot.subtitle = element_text(size = rel(0.8))  )  +
    plot_annotation(caption = "The vertical line shows the Mean Left-Right position among among all respondents.
                    ") 


last_plot()

ggplot2::ggsave(
  filename = "Figure 2 dots19 by T2CB.png",
  width = 12,
  height = 18,
  units = "cm",
  dpi = 600
)






#### Graphs with 57 cases for the line and 19 dots ----

# knitr::opts_chunk$set(echo = TRUE,
#                      fig.height = 9,
#                      fig.width = 9)


plots_19 = list() # must be defined before the for() 


library(grid)


for (i in seq_along(partylist)) {
  
  linedata <-  
    Totparty57_long  %>%
    filter(str_detect(party_family, partylist[i])) %>%
    select(mean_left_right, support_pct)
  
  print(i)

  
  
  ylabel <- 
    if (str_detect(paste0("socialist_left ", "agrarian ", "green", "conservative"), partylist[i])) {
    "T2CB Support for Party Family (%)" }  else {
    "  "}  
  
  x_lr_support           <- partyfamily_lr$mean_left_right[partyfamily_lr$party_family == partylist[i]]
  y_party_family_support <- partyfamily_support$support_thesis[partyfamily_support$party_family == partylist[i]]

  the_plot <-
    Totparty19_long %>%
    filter(str_detect(party_family, partylist[i])) %>%
    ggplot(., aes(mean_left_right, support_pct)) +
    ggplot2::geom_point(aes(size = n)) +
      ylim(0,70) +
    labs(title = str_to_title(paste0(partylist_l[i])),
        x = "Mean Left-Right",
        y = ylabel)  + # " Party Family"
    ggplot2::stat_smooth(
      data = linedata,
      method = "loess",
      fullrange = FALSE,
      se = FALSE,
      span = 4
    ) +
    ggrepel::geom_label_repel(aes(label = top_2_cb),
                              label.padding = 0.15,
                              size = 4)        +
    geom_vline(xintercept = x_lr_support) +   # the Sample mean 
    geom_hline(yintercept = y_party_family_support) +
    theme_gray()
  
  print(partylist_l[i])
  
  
  plots_19[[partylist_l[i]]] <- the_plot
  
  # plots_19 <- the_plot
  
  ggplot2::ggsave(filename = paste0(partylist[i],"_loess_line57_dots19.png"),
                  width = 12,
                  units = "cm",
                  dpi = 600)  
}

last_plot()

#### Collect all plots on two pages. ------------
#### using the Patchwork package
#### 

# "Dots show the 19 most frequent Top Two Cultural Biases. Loess smoothed line.")
# title = "Figure 4: Support for Party Family (percent)"   # removed because the Caption is text in the paper.


plot_a <-
plots_19[["Socialist Left"]] +
  plots_19[["Social Democrat"]] +
  plots_19[["Green"]] +
  plots_19[["Liberal"]] +
  plot_layout(
    ncol = 2,
    nrow = 2,
    byrow = TRUE,
    guides =  "collect"
  ) +
  plot_annotation(
                  caption = "Horisontal lines show the level of Support for a Party Family among all respondents
                  and the vertical the Mean Left-Right position among supporters of one Party Family.") +
  theme(plot.caption = element_text(size = rel(caption_text_size)),
        plot.caption.position = "panel",
        plot.subtitle = element_text(size = rel(0.8))  ) 

last_plot()

ggplot2::ggsave(
  filename = "Figure_3a_Partyfamilies_with_v&h_line.png",
  width = 21,
  height = 30,
  units = "cm",
  dpi = 600
)




plot_b <-
plots_19[["Agrarian"]] +
  plots_19[["Christian"]] +
  plots_19[["Conservative"]]  +
  plots_19[["Progress"]] +
  plot_layout(
    ncol = 2,
    nrow = 2,
    byrow = TRUE,
    guides =  "collect"
  ) +
plot_annotation(
  caption  = "The Loess smoothed line uses all 57, while only the dots for
  the 19 most common Top Two Cultural Biases are shown.") +
  theme(plot.caption = element_text(size = rel(caption_text_size)),
        plot.caption.position = "panel",
        plot.subtitle = element_text(size = rel(0.8))  ) 


last_plot()


ggplot2::ggsave(
  filename = "Figure_3b_Partyfamilies_with_v&h_line.png",
  width = 21,
  height = 30,
  units = "cm",
  dpi = 600
)




# ****************************************
#### EXPERIMENTING **********************

partyfamily_support

names(Totparty19_long)

tabyl(Totparty19_long, top_2_cb, party_family) %>% 
  spineplot()

Totparty19_pct <-
  Totparty19_long %>% 
  groupby( )scale(Totparty19, FALSE, colSums(Totparty19))

Totparty19_long %>%
  ggplot(., aes(x = top_2_cb, y = support_pct , fill = party_family)) +
  ggplot2::geom_bar(stat = "identity")
last_plot()

+
  #      ylim(0,60) +
  labs(title = str_to_title(paste0(partylist_l[i])),
       x = "Mean Left-Right",
       y = ylabel)  + # " Party Family"
  ggplot2::stat_smooth(
    data = linedata,
    method = "loess",
    fullrange = FALSE,
    se = FALSE,
    span = 4
  ) +
  ggrepel::geom_label_repel(aes(label = top_2_cb),
                            label.padding = 0.15,
                            size = 4)        +
  geom_vline(xintercept = partyfamily_lr$mean_left_right[partyfamily_lr$party_family == partylist[i]]) +   # the Sample mean 
  geom_hline(yintercept = partyfamily_support$support_thesis[partyfamily_support$party_family == partylist[i]]) +
  theme_gray()

  
  
# *********************************************

ggplot(Totparty19_long, aes(top_2_cb)) +
  geom_col(aes(support_pct, party_family, fill = top_2_cb))

ggplot(Totparty19_long, aes(top_2_cb)) +
  geom_col(aes(support_pct, top_2_cb, fill = party_family)) +
  labs(x = "percent") +
  theme_gray()

ggplot(Totparty19_long, aes(top_2_cb)) +
  geom_col(aes(support_pct, top_2_cb, fill = party_family)) +
  labs(x = "percent") +
  theme_gray()

ggplot(Totparty19_long, aes(top_2_cb)) +
  geom_bar(aes(fill=support_pct), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Party support by T2CB", 
       subtitle="Manufacturer across Vehicle Classes") 
