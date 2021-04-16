
#The purpose is to create new figs to Party PReference using the full dataset
# to create the function for line between LR and party preference.

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

setwd("../cultural-bias-and-left-right-paper")




#### GET DATA ----------

mydata <- 
  foreign::read.spss("E:/data/Nordic1999_Nordic_All3_tri3565_bin50_c10_n40_recoded.sav",
                     to.data.frame = TRUE) %>% 
  janitor::remove_empty(which = c("rows", "cols")) %>% 
  janitor::clean_names() %>% 
  hablar::retype() 

mydata$left_right01 <- hablar::as_reliable_num(mydata$left_right01)
#mydata$left_right01_b10 <- bin




names(mydata)  
str(mydata)
get_labels(mydata$totparty)

#recode()
partylist_l <- c("Socialist Left", "Social Democrat", "Conservativ" ,
                 "Liberal",        "Agrarian"  ,      "Christian"   ,             
                 "Progress" ,      "Greens")      
 


partylist_n <- c("lsoc",  "socdem", "cons"   , 
                 "liber", "agrar" , "christ" , 
                 "progr", "green" )


partylist_dummies <-  c("lsoc_dummy"  , "socdem_dummy",   "cons_dummy" ,
                "liber_dummy" , "agrar_dummy"  ,  "christ_dummy" ,
                "progr_dummy",  "green_dummy" ,  "dontknow_dummy" ,
                "novote_dummy" ,  "noanswer_dummy"   )

tabyl(mydata, "lsoc_dummy")

for (i in seq_along(partylist_l)) {
  mydata
}
mydata 

test <-   
  mutate(mydata, ends_with("dummy"), 
                       c("This Party" = "1", "Other party" = "0" ))


  

linedata  <-  mydata  %>%
  group_by(totparty)  %>% 
  select(totparty,country, left_right01, lsoc:noanswer_dummy, cult_rank_tri_a2) %>% 
  rename(top2bias = cult_rank_tri_a2) %>% 
  filter(!totparty == "NA") %>% 
  filter(!totparty == "Other") %>% 
  filter(!is.na(left_right01)) 

Hmisc::contents(mydata)
  

linedata$partyfam <- as.factor(linedata$totparty)



recode_key <- c("this party" = "1", "other party" = "0")
test <- plyr::revalue(linedata$lsoc_dummy, !!!recode_key) 

str(linedata$lsoc_dummy)
factor(linedata$lsoc_dummy)
table(linedata$lsoc_dummy)


countrydata  <-  mydata  %>%
  select(totparty,country, left_right01, lsoc:noanswer_dummy, cult_rank_tri_a2) %>% 
  rename(top2bias = cult_rank_tri_a2) %>% 
  filter(!totparty == "NA") %>% 
  filter(!totparty == "Other") %>% 
  filter(!is.na(left_right01))


countrydata %>%
 group_by(totparty, country) %>% 
 summarise(lr = mean(left_right01,
           ps_n = n()
           )
           
           
           
           ,
            ps_n = count(totparty))

country_data$lr_mean <- countrydata %>%  
  mean(left_right01) 


###### MODEL Multinominal Logistic ############ -----------------

# Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(linedata, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- linedata[-sample_id,]


# Setting the basline 
train$partyfam <- relevel(train$partyfam, ref = "Social Democrat")
train


# Training the multinomial model
multinom.fit <- nnet::multinom(partyfam ~ left_right01, data = train)

# Checking the model
summary(multinom.fit)

head(probability.table <- fitted(multinom.fit))


# Predicting the values for train dataset
train$predicted <- predict(multinom.fit, newdata = train, "class")
tabyl(train, predicted)

# Building classification table
ctable <- table(train$partyfam, train$predicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

### Test ------------
# Predicting the values for train dataset
test$predicted <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable <- table(test$partyfam, test$predicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
print("This is a bad model. very low predicting ability")





##LEARN purrr::map ###########-----------------------------------------  
linedata %>%
#   LEARN purrr::map(
#     
#     
#     
#   )
# map_dfr(  
# case_when(
#   str_detect(linedata$lsoc_dummy, "this party")  ~ 1L ,
#   str_detect(linedata$lsoc_dummy, "other party") ~ 0L ,
#   TRUE                                           ~ NA_integer_ )
# 
#   char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
#   level_key <- c(a = "apple", b = "banana", c = "carrot")
#   recode(char_vec, !!!level_key)
  
  
  



tabyl(linedata, "partyfam")  
  
tabyl(mydata, "totparty")  

  

# recode(mydata, partylist_dummies, lowest = 0, highest = 1)


############# GGPLOT2 ------------------------------------------- 
str(linedata)
party_family_support <- tibble(partyname = NA_character_, 
                               support_pct = NA_real_,
                               left_right01 = NA_real_,
                               country = NA_character_,
                               .rows = 8)

for (i in seq_along(partylist_n)) {
  party_family_support$partyname[[i]] <- partylist_l[[i]]
  n_of_cases <- nrow(linedata)
  support_n <- 
    length(which(linedata$totparty == partylist_l[i] ))
  party_family_support$support_pct[[i]]  <- support_n / n_of_cases * 100
  party_family_support$left_right01[[i]] <- 
     mean(linedata$left_right01[linedata$totparty == partylist_l[i]])
  }   

country_party_family <- mydata %>% 
  group_by(totparty, country) %>% 
  mutate(
    
  )
  spread(mydata, key ) 

party_family_support

for (i in seq_along(partylist_n)) {
  party <- partylist_l[[i]]
  print(party)
  n_of_cases <- nrow(linedata)
  print(n_of_cases)
  support_n <- length(which(linedata$totparty == partylist_l[i] ))
  support_pct <- support_n / n_of_cases * 100
  print(support_pct)
  linedata$support_pct <-
  linedata %>% 
     filter(str_detect(totparty,partylist_l[i])) %>% 
     mutate(support_pct = length(which(totparty ==partylist_l[i] )) / n_of_cases) %>% 
     select(left_right01, support_pct) 
  print(linedata$support_pct)   
}   
  


ggplot(party_family_support, aes(left_right01, support_pct)) + 
  ggplot2::geom_point(size = 4) +
  ggrepel::geom_label_repel(aes(label = partyname)) +
  labs(title = "Which Party would you vote for if there was a parliamentary election today? ")  +
  xlab("Mean Left-Right Position") +
  ylab("Party Family Supporters (percent)" )
  

for (i in seq_along(partylist_l)) {
  mydata %>% 
    filter(str_detect(totparty,partylist_l[i])) %>% 
    ggplot(party_family_support, aes(left_right01, support_pct)) + 
    ggplot2::geom_point(aes(shape = partyname)) +
    labs(title = str_to_title(partylist_l[i]))  +
    xlab("Mean Left-Right Position") +
    ylab("Party Family Supporters (percent)") +
    #  scale_shape_manual(values = c(1,4,22)) +
    ggrepel::geom_label_repel(aes(label = top_2_cb))        +
    ggplot2::stat_smooth(data = linedata,
                         method = "loess", 
                         fullrange = FALSE,
                         se = FALSE,
                         span = 4) +
    theme_gray()
  
  print(partylist_l[i])

  
  ggplot2::ggsave(filename = paste0(partylist_n[i],"_ind_linedata.png"),
                  width = 12,
                  units = "cm",
                  dpi = 300)  
}


ggplot2::ggplot(linedata, aes(left_right01)) + 
  stat_count(left_right01, geom = "bar") +
  ggplot2::geom_bar(position = "fill") +
  xlab("Left-Right Self Placement") +
  ylab("Party Family Preference")  


# soc_dem  
ggplot2::ggplot(linedata, aes(left_right01, totparty)) + 
  ggplot2::geom_violin(scale = "area" ) +
  xlab("Left-Right Self Placement") +
  ylab("Party Family Preference")  

ggplot2::ggplot(linedata, aes(left_right01, totparty)) + 
  ggplot2::geom_boxplot(outlier.shape = NA,
                        notch = FALSE) +
  xlab("Left-Right Self Placement") +
  ylab("Party Family Preference") 


ggplot2::ggplot(linedata, aes(left_right01, totparty)) + 
  ggplot2::geom_count() +
  ggplot2::scale_size_area()








ggplot2::ggplot(linedata, aes(left_right01, totparty)) + 
  ggplot2::geom_dotplot(binaxis = totparty) 



ggplot2::ggplot(linedata, aes(left_right01, totparty)) + 
  ggplot2::geom_pointrange() +  
  geom_jitter(height = 0.2, width = 0)

  ggplot2::geom_line(color = "grey", aes(group = n))               +
  ggplot2::labs(x = "Mean Left-Right Position",
                y = "Social Democratic Party Family Supporters (percent)",
                caption = "Only significant deviations in party preference are included.")  

ggplot2::ggsave(filename = "soc_dem LF.png",
                width = 12,
                units = "cm",
                dpi = 300)  




