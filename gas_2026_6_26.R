# Code for gas_v?.tex titled: "Paying at the Pump: Evidence from a Consumer Payments Survey"
# This code uses the 2021 to 2025 data

# Packages used
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library(ggplot2); theme_set(theme_bw())# for graphics
library(scales)# to have % in ggplot axes and tables and for commas , in data frame numbers
#library(nnet)# multinomial regressions
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
#library(ineq)# GINI coefficient
#library(Hmisc)# for cutting data into bins

# regression packages
#library(gtools)# for stars.pval function
#library(brglm2)# modifies glm. Useful when glm warns about fitting 0 and 1
#library(logistf)# stronger bias reduction to eliminate the glm warning (when brglm2 cannot eliminate it)
library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displays multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 

# machine learning packages
#library(caret)#for confusionMatrix. Note: it flips the orientation so it makes actual (reference) as columns and predicted (data) as rows => unconventional. 
# ML packages
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library("randomForest")
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
library(missRanger)# for imputations based on "amnt" and "merch only

#++++++++++++++ 

#Begin load data by year####
#2025 data load
setwd("~/SDCPC/2025_SDCPC")
dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel-public.rds")
dim(trans2025_1.df)
names(trans2025_1.df)
(sampled_num_trans_2025 = nrow(trans2025_1.df))

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel-public.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)
(sampled_num_resp_2025 = nrow(indiv2025_1.df))

#2024 data load
setwd("~/SDCPC/2024_SDCPC")# 
dir()
# Read transaction dataset
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)
(sampled_num_trans_2024 = nrow(trans2024_1.df))

# Read individual dataset
indiv2024_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(indiv2024_1.df)
names(indiv2024_1.df)
(sampled_num_resp_2024 = nrow(indiv2024_1.df))

#2023 data load
setwd("~/SDCPC/2023_SDCPC")
dir()
# Read transaction dataset
trans2023_1.df = readRDS("dcpc-2023-tranlevel-public.rds")
dim(trans2023_1.df)
names(trans2023_1.df)
(sampled_num_trans_2023 = nrow(trans2023_1.df))

# Read individual dataset
indiv2023_1.df = readRDS("dcpc-2023-indlevel-public.rds")
dim(indiv2023_1.df)
names(indiv2023_1.df)
(sampled_num_resp_2023 = nrow(indiv2023_1.df))

#2022 data load
setwd("~/SDCPC/2022_DCPC")
dir()
# Read transaction dataset
trans2022_1.df = readRDS("dcpc-2022-tranlevel-public.rds")
dim(trans2022_1.df)
names(trans2022_1.df)
(sampled_num_trans_2022 = nrow(trans2022_1.df))

# Read individual dataset
indiv2022_1.df = readRDS("dcpc-2022-indlevel-public.rds")
dim(indiv2022_1.df)
names(indiv2022_1.df)
(sampled_num_resp_2022 = nrow(indiv2022_1.df))

#2021 data load
setwd("~/SDCPC/2021_DCPC")
dir()
# Read transaction dataset
trans2021_1.df = readRDS("dcpc-2021-tranlevel-public.rds")
dim(trans2021_1.df)
names(trans2021_1.df)
(sampled_num_trans_2021 = nrow(trans2021_1.df))

# Read individual dataset
indiv2021_1.df = readRDS("dcpc-2021-indlevel-public.rds")
dim(indiv2021_1.df)
names(indiv2021_1.df)
(sampled_num_resp_2021 = nrow(indiv2021_1.df))

#End loading data by year####

#+++++++++++++++

#Start merging and refining data by year####

# delete unneeded variables from indiv dataset
i2025_2.df = subset(indiv2025_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards, urban_cat, gender))
#
i2024_2.df = subset(indiv2024_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards))
#
i2023_2.df = subset(indiv2023_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards))
#
i2022_2.df = subset(indiv2022_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards))
#
i2021_2.df = subset(indiv2021_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards))
#

# Select gas station spending only merch=2 & in-person payment
t2025_2.df = subset(trans2025_1.df, merch==2 & in_person==1)
dim(t2025_2.df)
#
t2024_2.df = subset(trans2024_1.df, merch==2 & in_person==1)
dim(t2024_2.df)
#
t2023_2.df = subset(trans2023_1.df, merch==2 & in_person==1)
dim(t2023_2.df)
#
t2022_2.df = subset(trans2022_1.df, merch==2 & in_person==1)
dim(t2022_2.df)
#
t2021_2.df = subset(trans2021_1.df, merch==2 & in_person==1)
dim(t2021_2.df)

# select variables to delete
t2025_3.df = subset(t2025_2.df, select = c(id, pi, amnt, device, mobile_app, cc_discount, dc_rewards, accept_cash, accept_card, discount, cc_surcharge))
dim(t2025_3.df)
#
t2024_3.df = subset(t2024_2.df, select = c(id, pi, amnt, device, mobile_app, cc_discount, dc_rewards, accept_cash, accept_card, discount))
dim(t2024_3.df)
#
t2023_3.df = subset(t2023_2.df, select = c(id, pi, amnt, device, mobile_app, cc_discount, dc_rewards, accept_cash, accept_card, discount))
dim(t2023_3.df)
#
t2022_3.df = subset(t2022_2.df, select = c(id, pi, amnt, device, mobile_app, cc_discount, dc_rewards, accept_cash, accept_card, discount))
dim(t2022_3.df)
#
t2021_3.df = subset(t2021_2.df, select = c(id, pi, amnt, device, mobile_app, cc_discount, dc_rewards, accept_cash, accept_card, discount))
dim(t2021_3.df)

# merge indiv data into trans data
m2025_1.df = left_join(t2025_3.df, i2025_2.df, by = "id")
dim(m2025_1.df)
names(m2025_1.df)
#
m2024_1.df = left_join(t2024_3.df, i2024_2.df, by = "id")
dim(m2024_1.df)
names(m2024_1.df)
#
m2023_1.df = left_join(t2023_3.df, i2023_2.df, by = "id")
dim(m2023_1.df)
names(m2023_1.df)
#
m2022_1.df = left_join(t2022_3.df, i2022_2.df, by = "id")
dim(m2022_1.df)
names(m2022_1.df)
#
m2021_1.df = left_join(t2021_3.df, i2021_2.df, by = "id")
dim(m2021_1.df)
names(m2021_1.df)
#

# remove payments with missing obs of: pi, amnt,ind_weight_all. Also, delete transactions with amount = 0 (if any)
sum(is.na(m2025_1.df$pi))
sum(is.na(m2025_1.df$amnt))
nrow(subset(m2025_1.df, amnt <=0))
sum(is.na(m2025_1.df$ind_weight_all))
dim(m2025_1.df)
m2025_2.df = subset(m2025_1.df, !is.na(m2025_1.df$pi) & !is.na(m2025_1.df$amnt) & m2025_1.df$amnt >0 & !is.na(m2025_1.df$ind_weight_all))
nrow(m2025_2.df)-nrow(m2025_1.df)# num obs removed
#
sum(is.na(m2024_1.df$pi))
sum(is.na(m2024_1.df$amnt))
nrow(subset(m2024_1.df, amnt <=0))
sum(is.na(m2024_1.df$ind_weight_all))
dim(m2024_1.df)
m2024_2.df = subset(m2024_1.df, !is.na(m2024_1.df$pi) & !is.na(m2024_1.df$amnt) & m2024_1.df$amnt >0  & !is.na(m2024_1.df$ind_weight_all) )
nrow(m2024_2.df)-nrow(m2024_1.df)# num obs removed
#
sum(is.na(m2023_1.df$pi))
sum(is.na(m2023_1.df$amnt))
nrow(subset(m2023_1.df, amnt <=0))
sum(is.na(m2023_1.df$ind_weight_all))
dim(m2023_1.df)
m2023_2.df = subset(m2023_1.df, !is.na(m2023_1.df$pi) & !is.na(m2023_1.df$amnt) & m2023_1.df$amnt >0  & !is.na(m2023_1.df$ind_weight_all) )
nrow(m2023_2.df)-nrow(m2023_1.df)# num obs removed
#
sum(is.na(m2022_1.df$pi))
sum(is.na(m2022_1.df$amnt))
nrow(subset(m2022_1.df, amnt <=0))
sum(is.na(m2022_1.df$ind_weight_all))
dim(m2022_1.df)
m2022_2.df = subset(m2022_1.df, !is.na(m2022_1.df$pi) & !is.na(m2022_1.df$amnt) & m2022_1.df$amnt >0  & !is.na(m2022_1.df$ind_weight_all) )
nrow(m2022_2.df)-nrow(m2022_1.df)# num obs removed
#
sum(is.na(m2021_1.df$pi))
sum(is.na(m2021_1.df$amnt))
nrow(subset(m2021_1.df, amnt <=0))
sum(is.na(m2021_1.df$ind_weight_all))
dim(m2021_1.df)
m2021_2.df = subset(m2021_1.df, !is.na(m2021_1.df$pi) & !is.na(m2021_1.df$amnt) & m2021_1.df$amnt >0 & !is.na(m2021_1.df$ind_weight_all) )
nrow(m2021_2.df)-nrow(m2021_1.df)# num obs removed

# rescale the weights for year to sum up to num obs: add column w
nrow(m2025_2.df)
sum(m2025_2.df$ind_weight_all)
m2025_2.df$w = nrow(m2025_2.df) * m2025_2.df$ind_weight_all/sum(m2025_2.df$ind_weight_all)
sum(m2025_2.df$w)# verify w sum to num obs
#
nrow(m2024_2.df)
sum(m2024_2.df$ind_weight_all)
m2024_2.df$w = nrow(m2024_2.df) * m2024_2.df$ind_weight_all/sum(m2024_2.df$ind_weight_all)
sum(m2024_2.df$w)# verify w sum to num obs
#
nrow(m2023_2.df)
sum(m2023_2.df$ind_weight_all)
m2023_2.df$w = nrow(m2023_2.df) * m2023_2.df$ind_weight_all/sum(m2023_2.df$ind_weight_all)
sum(m2023_2.df$w)# verify w sum to num obs
#
nrow(m2022_2.df)
sum(m2022_2.df$ind_weight_all)
m2022_2.df$w = nrow(m2022_2.df) * m2022_2.df$ind_weight_all/sum(m2022_2.df$ind_weight_all)
sum(m2022_2.df$w)# verify w sum to num obs
#
nrow(m2021_2.df)
sum(m2021_2.df$ind_weight_all)
m2021_2.df$w = nrow(m2021_2.df) * m2021_2.df$ind_weight_all/sum(m2021_2.df$ind_weight_all)
sum(m2021_2.df$w)# verify w sum to num obs

# reclassify PI as cash, credit card, debit card, and other
table(m2025_2.df$pi)
m2025_3.df = m2025_2.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
  ))
table(m2025_3.df$pi)
sum(table(m2025_3.df$pi))# num payments
dim(m2025_3.df)# verify
m2025_3.df$pi = as.factor(m2025_3.df$pi)
str(m2025_3.df$pi)
(levels(m2025_3.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m2025_3.df$pi)
#
table(m2024_2.df$pi)
m2024_3.df = m2024_2.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
  ))
table(m2024_3.df$pi)
sum(table(m2024_3.df$pi))# num payments
dim(m2024_3.df)# verify
m2024_3.df$pi = as.factor(m2024_3.df$pi)
str(m2024_3.df$pi)
(levels(m2024_3.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m2024_3.df$pi)
#
table(m2023_2.df$pi)
m2023_3.df = m2023_2.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
  ))
table(m2023_3.df$pi)
sum(table(m2023_3.df$pi))# num payments
dim(m2023_3.df)# verify
m2023_3.df$pi = as.factor(m2023_3.df$pi)
str(m2023_3.df$pi)
(levels(m2024_3.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m2023_3.df$pi)
#
table(m2022_2.df$pi)
m2022_3.df = m2022_2.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
  ))
table(m2022_3.df$pi)
sum(table(m2022_3.df$pi))# num payments
dim(m2022_3.df)# verify
m2022_3.df$pi = as.factor(m2022_3.df$pi)
str(m2022_3.df$pi)
(levels(m2022_3.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m2022_3.df$pi)
#
table(m2021_2.df$pi)
m2021_3.df = m2021_2.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
  ))
table(m2021_3.df$pi)
sum(table(m2021_3.df$pi))# num payments
dim(m2021_3.df)# verify
m2021_3.df$pi = as.factor(m2021_3.df$pi)
str(m2021_3.df$pi)
(levels(m2021_3.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m2021_3.df$pi)

#End merging and refining data by year####

#+++++++++++++++

#Begin: Figure 1: Fraction of PI by year####

# Compute fraction of PI used (unweighted)
(cash2025_frac = nrow(subset(m2025_3.df, m2025_3.df$pi == "Cash"))/nrow(m2025_3.df))
(credit2025_frac = nrow(subset(m2025_3.df, m2025_3.df$pi == "Credit"))/nrow(m2025_3.df))
(debit2025_frac = nrow(subset(m2025_3.df, m2025_3.df$pi == "Debit"))/nrow(m2025_3.df))
(other2025_frac = nrow(subset(m2025_3.df, m2025_3.df$pi == "Other"))/nrow(m2025_3.df))
# Check sum up to 1
cash2025_frac+credit2025_frac+debit2025_frac+other2025_frac
#
(cash2024_frac = nrow(subset(m2024_3.df, m2024_3.df$pi == "Cash"))/nrow(m2024_3.df))
(credit2024_frac = nrow(subset(m2024_3.df, m2024_3.df$pi == "Credit"))/nrow(m2024_3.df))
(debit2024_frac = nrow(subset(m2024_3.df, m2024_3.df$pi == "Debit"))/nrow(m2024_3.df))
(other2024_frac = nrow(subset(m2024_3.df, m2024_3.df$pi == "Other"))/nrow(m2024_3.df))
# Check sum up to 1
cash2024_frac+credit2024_frac+debit2024_frac+other2024_frac
#
(cash2023_frac = nrow(subset(m2023_3.df, m2023_3.df$pi == "Cash"))/nrow(m2023_3.df))
(credit2023_frac = nrow(subset(m2023_3.df, m2023_3.df$pi == "Credit"))/nrow(m2023_3.df))
(debit2023_frac = nrow(subset(m2023_3.df, m2023_3.df$pi == "Debit"))/nrow(m2023_3.df))
(other2023_frac = nrow(subset(m2023_3.df, m2023_3.df$pi == "Other"))/nrow(m2023_3.df))
# Check sum up to 1
cash2023_frac+credit2023_frac+debit2023_frac+other2023_frac
#
(cash2022_frac = nrow(subset(m2022_3.df, m2022_3.df$pi == "Cash"))/nrow(m2022_3.df))
(credit2022_frac = nrow(subset(m2022_3.df, m2022_3.df$pi == "Credit"))/nrow(m2022_3.df))
(debit2022_frac = nrow(subset(m2022_3.df, m2022_3.df$pi == "Debit"))/nrow(m2022_3.df))
(other2022_frac = nrow(subset(m2022_3.df, m2022_3.df$pi == "Other"))/nrow(m2022_3.df))
# Check sum up to 1
cash2022_frac+credit2022_frac+debit2022_frac+other2022_frac
#
(cash2021_frac = nrow(subset(m2021_3.df, m2021_3.df$pi == "Cash"))/nrow(m2021_3.df))
(credit2021_frac = nrow(subset(m2021_3.df, m2021_3.df$pi == "Credit"))/nrow(m2021_3.df))
(debit2021_frac = nrow(subset(m2021_3.df, m2021_3.df$pi == "Debit"))/nrow(m2021_3.df))
(other2021_frac = nrow(subset(m2021_3.df, m2021_3.df$pi == "Other"))/nrow(m2021_3.df))
# Check sum up to 1
cash2021_frac+credit2021_frac+debit2021_frac+other2021_frac

# the above fraction weighted
# verify weights sum up to num payments
nrow(m2025_3.df)
sum(m2025_3.df$w)
(cash2025_frac_w = sum(subset(m2025_3.df, m2025_3.df$pi == "Cash")$w)/sum(m2025_3.df$w))
(credit2025_frac_w = sum(subset(m2025_3.df, m2025_3.df$pi == "Credit")$w)/sum(m2025_3.df$w))
(debit2025_frac_w = sum(subset(m2025_3.df, m2025_3.df$pi == "Debit")$w)/sum(m2025_3.df$w))
(other2025_frac_w = sum(subset(m2025_3.df, m2025_3.df$pi == "Other")$w)/sum(m2025_3.df$w))
# Check sum up to 1
cash2025_frac_w+credit2025_frac_w+debit2025_frac_w+other2025_frac_w
#
# verify weights sum up to num payments
nrow(m2024_3.df)
sum(m2024_3.df$w)
(cash2024_frac_w = sum(subset(m2024_3.df, m2024_3.df$pi == "Cash")$w)/sum(m2024_3.df$w))
(credit2024_frac_w = sum(subset(m2024_3.df, m2024_3.df$pi == "Credit")$w)/sum(m2024_3.df$w))
(debit2024_frac_w = sum(subset(m2024_3.df, m2024_3.df$pi == "Debit")$w)/sum(m2024_3.df$w))
(other2024_frac_w = sum(subset(m2024_3.df, m2024_3.df$pi == "Other")$w)/sum(m2024_3.df$w))
# Check sum up to 1
cash2024_frac_w+credit2024_frac_w+debit2024_frac_w+other2024_frac_w
#
# verify weights sum up to num payments
nrow(m2023_3.df)
sum(m2023_3.df$w)
(cash2023_frac_w = sum(subset(m2023_3.df, m2023_3.df$pi == "Cash")$w)/sum(m2023_3.df$w))
(credit2023_frac_w = sum(subset(m2023_3.df, m2023_3.df$pi == "Credit")$w)/sum(m2023_3.df$w))
(debit2023_frac_w = sum(subset(m2023_3.df, m2023_3.df$pi == "Debit")$w)/sum(m2023_3.df$w))
(other2023_frac_w = sum(subset(m2023_3.df, m2023_3.df$pi == "Other")$w)/sum(m2023_3.df$w))
# Check sum up to 1
cash2023_frac_w+credit2023_frac_w+debit2023_frac_w+other2023_frac_w
#
# verify weights sum up to num payments
nrow(m2022_3.df)
sum(m2022_3.df$w)
(cash2022_frac_w = sum(subset(m2022_3.df, m2022_3.df$pi == "Cash")$w)/sum(m2022_3.df$w))
(credit2022_frac_w = sum(subset(m2022_3.df, m2022_3.df$pi == "Credit")$w)/sum(m2022_3.df$w))
(debit2022_frac_w = sum(subset(m2022_3.df, m2022_3.df$pi == "Debit")$w)/sum(m2022_3.df$w))
(other2022_frac_w = sum(subset(m2022_3.df, m2022_3.df$pi == "Other")$w)/sum(m2022_3.df$w))
# Check sum up to 1
cash2022_frac_w+credit2022_frac_w+debit2022_frac_w+other2022_frac_w
#
# verify weights sum up to num payments
nrow(m2021_3.df)
sum(m2021_3.df$w)
(cash2021_frac_w = sum(subset(m2021_3.df, m2021_3.df$pi == "Cash")$w)/sum(m2021_3.df$w))
(credit2021_frac_w = sum(subset(m2021_3.df, m2021_3.df$pi == "Credit")$w)/sum(m2021_3.df$w))
(debit2021_frac_w = sum(subset(m2021_3.df, m2021_3.df$pi == "Debit")$w)/sum(m2021_3.df$w))
(other2021_frac_w = sum(subset(m2021_3.df, m2021_3.df$pi == "Other")$w)/sum(m2021_3.df$w))
# Check sum up to 1
cash2021_frac_w+credit2021_frac_w+debit2021_frac_w+other2021_frac_w

# Data frame for the fraction by year
(year.vec = as.factor(2021:2025))
#cash
(cash_frac.vec = c(cash2021_frac, cash2022_frac, cash2023_frac, cash2024_frac, cash2025_frac))
(cash_cagr = (cash2025_frac/cash2021_frac)^(1/4) -1)#CAGR
(cash_frac_w.vec = c(cash2021_frac_w, cash2022_frac_w, cash2023_frac_w, cash2024_frac_w, cash2025_frac_w))
(cash_cagr_w = (cash2025_frac_w/cash2021_frac_w)^(1/4) -1)#CAGR
#credit
(credit_frac.vec = c(credit2021_frac, credit2022_frac, credit2023_frac, credit2024_frac, credit2025_frac))
(credit_cagr = (credit2025_frac/credit2021_frac)^(1/4) -1)#CAGR
(credit_frac_w.vec = c(credit2021_frac_w, credit2022_frac_w, credit2023_frac_w, credit2024_frac_w, credit2025_frac_w))
(credit_cagr_w = (credit2025_frac_w/credit2021_frac_w)^(1/4) -1)#CAGR
#debit
(debit_frac.vec = c(debit2021_frac, debit2022_frac, debit2023_frac, debit2024_frac, debit2025_frac))
(debit_cagr = (debit2025_frac/debit2021_frac)^(1/4) -1)#CAGR
(debit_frac_w.vec = c(debit2021_frac_w, debit2022_frac_w, debit2023_frac_w, debit2024_frac_w, debit2025_frac_w))
(debit_cagr_w = (debit2025_frac_w/debit2021_frac_w)^(1/4) -1)#CAGR
#other
(other_frac.vec = c(other2021_frac, other2022_frac, other2023_frac, other2024_frac, other2025_frac))
(other_cagr = (other2025_frac/other2021_frac)^(1/4) -1)#CAGR
(other_frac_w.vec = c(other2021_frac_w, other2022_frac_w, other2023_frac_w, other2024_frac_w, other2025_frac_w))
(other_cagr_w = (other2025_frac_w/other2021_frac_w)^(1/4) -1)#CAGR

(frac.df = data.frame(year.vec, cash_frac.vec, cash_frac_w.vec, credit_frac.vec, credit_frac_w.vec, debit_frac.vec, debit_frac_w.vec, other_frac.vec, other_frac_w.vec))

#plot 5 years of PI shares
ggplot(frac.df, aes(x=year.vec, y=cash_frac_w.vec, group = 1)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=credit_frac_w.vec), linewidth = 1.0, linetype="dotdash", color="red") +geom_point(aes(y=credit_frac_w.vec), size=1.9, color="red") +geom_line(aes(y=debit_frac_w.vec), linewidth = 1.0, linetype = "longdash", color="blue") +geom_point(aes(y=debit_frac_w.vec), size=1.9, color="blue") +geom_line(aes(y=other_frac_w.vec), linewidth = 1.0, linetype = "dotted", color="darkgreen") +geom_point(aes(y=other_frac_w.vec), size=1.9, color="darkgreen") + scale_y_continuous(breaks = seq(0, 0.5, 0.05), labels = percent) +labs(x="Year", y="Percentage of payment method used") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2, y = 0.26, label = "Cash", angle = 0,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2, y = 0.05, label = "Other", angle = 0,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2, y = 0.34, label = "Credit card", angle = 0,  color = "red", size = 6, hjust = 0)  +annotate("text", x = 2, y = 0.43, label = "Debit card", angle = 0,  color = "blue", size = 6, hjust = 0) +annotate("text", x = 3.5, y = 0.32, label = paste("Debit CAGR =", round(100*debit_cagr_w,1),"%"), angle = 0,  color = "blue", size = 6, hjust = 0) +annotate("text", x = 3.5, y = 0.30, label = paste("Credit CAGR =", round(100*credit_cagr_w,1),"%"), angle = 0,  color = "red", size = 6, hjust = 0) +annotate("text", x = 3.5, y = 0.28, label = paste("Cash CAGR =", round(100*cash_cagr_w,1),"%"), angle = 0,  color = "black", size = 6, hjust = 0)  +annotate("text", x = 3.5, y = 0.26, label = paste("Other CAGR =", round(100*other_cagr_w,1),"%"), angle = 0,  color = "darkgreen", size = 6, hjust = 0)  

# Info for Figure 1:
round(100*cash_frac_w.vec,1)# cash frac
round(100*(cash_frac_w.vec[5] -cash_frac_w.vec[1])/cash_frac_w.vec[1],1)#% change
#
round(100*credit_frac_w.vec,1)# credit frac
round(100*(credit_frac_w.vec[5] -credit_frac_w.vec[1])/credit_frac_w.vec[1],1)#% change
#
round(100*debit_frac_w.vec,1)# debit frac
round(100*(debit_frac_w.vec[5] -debit_frac_w.vec[1])/debit_frac_w.vec[1],1)#% change
#
round(100*other_frac_w.vec,1)# other frac
round(100*(other_frac_w.vec[5] -other_frac_w.vec[1])/other_frac_w.vec[1],1)#% change


# place in a footnote to this figure
# number of payments by year
nrow(m2021_3.df)# num payments
length(m2021_3.df$id)# num resp
#
nrow(m2022_3.df)# num payments
length(m2022_3.df$id)# num resp
#
nrow(m2023_3.df)# num payments
length(m2023_3.df$id)# num resp
#
nrow(m2024_3.df)# num payments
length(m2024_3.df$id)# num resp
#
nrow(m2025_3.df)# num payments
length(m2025_3.df$id)# num resp


#End: Figure 1: Fraction of PI by year####

#+++++++++++++++

#Begin: (not in paper) Figure: Dollar spending on gas by year####

# Compute average (per-resp) $ spending (3-days, unweighted)
(total_spend2025 = sum(m2025_3.df$amnt))
(num_resp2025 = length(unique(m2025_3.df$id)))
(num_trans2025 = nrow(m2025_3.df))
(avg_spend2025_per_resp =  total_spend2025/num_resp2025)
(avg_spend2025_per_resp_daily =  total_spend2025/(3*num_resp2025))
(avg_spend2025_per_trans =  total_spend2025/num_trans2025)
#
(total_spend2024 = sum(m2024_3.df$amnt))
(num_resp2024 = length(unique(m2024_3.df$id)))
(num_trans2024 = nrow(m2024_3.df))
(avg_spend2024_per_resp =  total_spend2024/num_resp2024)
(avg_spend2024_per_resp_daily =  total_spend2024/(3*num_resp2024))
(avg_spend2024_per_trans =  total_spend2024/num_trans2024)
#
(total_spend2023 = sum(m2023_3.df$amnt))
(num_resp2023 = length(unique(m2023_3.df$id)))
(num_trans2023 = nrow(m2023_3.df))
(avg_spend2023_per_resp =  total_spend2023/num_resp2023)
(avg_spend2023_per_resp_daily =  total_spend2023/(3*num_resp2023))
(avg_spend2023_per_trans =  total_spend2023/num_trans2023)
#
(total_spend2022 = sum(m2022_3.df$amnt))
(num_resp2022 = length(unique(m2022_3.df$id)))
(num_trans2022 = nrow(m2022_3.df))
(avg_spend2022_per_resp =  total_spend2022/num_resp2022)
(avg_spend2022_per_resp_daily =  total_spend2022/(3*num_resp2022))
(avg_spend2022_per_trans =  total_spend2022/num_trans2022)
#
(total_spend2021 = sum(m2021_3.df$amnt))
(num_resp2021 = length(unique(m2021_3.df$id)))
(num_trans2021 = nrow(m2021_3.df))
(avg_spend2021_per_resp =  total_spend2021/num_resp2021)
(avg_spend2021_per_resp_daily =  total_spend2021/(3*num_resp2021))
(avg_spend2021_per_trans =  total_spend2021/num_trans2021)

# gas prices. Source: U.S. Energy Information Administration (EIA), based on their “All Grades All Formulations Retail Gasoline Prices” dataset
# https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emm_epm0_pte_nus_dpg&f=a, 
# https://www.eia.gov/petroleum/
#Regular gas (87 Octane)
gas87 = data.frame(
  year  = 2021:2025,
  price = c(3.008, 3.951, 3.519, 3.304, 3.097)
)

# Make is a data frame
(Avg_per_fill.vec = c(avg_spend2021_per_trans, avg_spend2022_per_trans, avg_spend2023_per_trans, avg_spend2024_per_trans, avg_spend2025_per_trans))
#
(Avg_daily.vec = c(avg_spend2021_per_resp_daily, avg_spend2022_per_resp_daily, avg_spend2023_per_resp_daily, avg_spend2024_per_resp_daily, avg_spend2025_per_resp_daily))
#

#compute CAGR for the above averages
(Avg_per_fill_CAGR = (tail(Avg_per_fill.vec,1) / head(Avg_per_fill.vec,1))^(1/4) - 1)
#
(Avg_daily_CAGR = (tail(Avg_daily.vec,1) / head(Avg_daily.vec,1))^(1/4) - 1)

(spend.df = data.frame(Year = as.factor(2021:2025), Avg_per_fill = Avg_per_fill.vec, Avg_daily = Avg_daily.vec, Price_per_gallon = gas87$price, Avg_per_fill_CAGR, Avg_daily_CAGR))

#plot 5 years of $ spending
ggplot(spend.df, aes(x=Year, y=Avg_per_fill.vec, group = 1)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=Avg_daily.vec), linewidth = 1.0, linetype="longdash", color="red") +geom_point(aes(y=Avg_daily.vec), size=1.9, color="red")  + scale_y_continuous(breaks = seq(0, 40, 5), labels = dollar) +labs(x="Year", y="Average consumer spending on gasoline") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 3, y = 35, label = "Average spending per gas fill", angle = 0,  color = "black", size = 6, hjust = 0) +annotate("text", x = 3, y = 16, label = "Average per-person daily spending", angle = 0,  color = "red", size = 6, hjust = 0) +annotate("text", x = 3, y = 31, label =  paste("CAGR =", round(100*Avg_per_fill_CAGR,1),"%"), angle = 0,  color = "black", size = 6, hjust = 0) +annotate("text", x = 3, y = 12.5, label =  paste("CAGR =", round(100*Avg_daily_CAGR,1),"%"), angle = 0,  color = "red", size = 6, hjust = 0) 

#Median and avg in 2025: Discussion in Footnote 3 at the end of section 2 in the paper
round(Avg_per_fill.vec,2)
median(m2025_3.df$amnt)

#End: (not in paper) Figure: Dollar spending on gas by year####

#+++++++++++++++++

#Start: regression on demog and trans-specific variables####
names(m2025_3.df)
table(m2025_3.df$pi)
# remove unneeded variables (not used in the regressions)
m2025_4.df = subset(m2025_3.df, select = c(id, pi, amnt, cc_surcharge, income_hh, hhincome, age, urban_cat, gender, cc_adopt, dc_adopt, discount, cc_rewards))
names(m2025_4.df)

# ensure factor variables are indeed factors
m2025_4.df$cc_surcharge = as.factor(m2025_4.df$cc_surcharge)
str(m2025_4.df$cc_surcharge)
m2025_4.df$discount = as.factor(m2025_4.df$discount)
str(m2025_4.df$discount)
m2025_4.df$cc_adopt = as.factor(m2025_4.df$cc_adopt)
str(m2025_4.df$cc_adopt)
m2025_4.df$dc_adopt = as.factor(m2025_4.df$dc_adopt)
str(m2025_4.df$dc_adopt)
m2025_4.df$cc_rewards = as.factor(m2025_4.df$cc_rewards)
str(m2025_4.df$cc_rewards)
m2025_4.df$age = as.integer(m2025_4.df$age)
str(m2025_4.df$age)
m2025_4.df$hhincome = as.factor(m2025_4.df$hhincome)# 16 HH income bins
str(m2025_4.df$hhincome)
m2025_4.df$urban_cat = as.factor(m2025_4.df$urban_cat)# Urban cat
str(m2025_4.df$urban_cat)
m2025_4.df$cc_surcharge = as.factor(m2025_4.df$gender)
str(m2025_4.df$cc_surcharge)

# check missing obs
nrow(m2025_4.df)#total number of payments (gas stations)
table(m2025_4.df$discount, useNA = "always")#trans level
nrow(subset(m2025_4.df, is.na(income_hh)))#indiv level
nrow(subset(m2025_4.df, is.na(cc_adopt)))#indiv level
nrow(subset(m2025_4.df, is.na(dc_adopt)))#indiv level
nrow(subset(m2025_4.df, is.na(amnt)))#trans level
table(m2025_4.df$cc_rewards, useNA = "always")
nrow(subset(m2025_4.df, is.na(age)))#indv level
table(m2025_4.df$urban_cat, useNA = "always")
nrow(subset(m2025_4.df, is.na(urban_cat)))#indv level
table(m2025_4.df$gender, useNA = "always")
nrow(subset(m2025_4.df, is.na(gender)))#indv level

m2025_5.df = m2025_4.df

# impute missing "discount" values 
with(m2025_5.df, table(pi, discount, useNA = "always"))
# => almost most of the NAs are in other. => set these to 0. 
#m2025_5.df = m2025_5.df %>% mutate(discount = if_else(pi == "Other", as.factor(0), as.factor(discount)))
#with(m2025_5.df, table(pi, discount, useNA = "always"))
# impute the remaining missing discount values
m2025_5.df = missRanger(data = m2025_5.df, formula =  discount ~ pi + amnt)
with(m2025_5.df, table(pi, discount, useNA = "always"))

# # impute missing "cc_surcharge" values (don't use missRanger because it sets 0 for cash payments, which must be incorrect.) => Variable is NOT in the paper, ignore
# with(m2025_5.df, table(pi, cc_surcharge, useNA = "always"))
# # => almost most of the NAs are in debit and cash.
# # set cc_surcharge=0 for respondents who do NOT own credit cards
# m2025_5.df = m2025_5.df %>% mutate(cc_surcharge = if_else(cc_adopt == 0, as.factor(0), as.factor(cc_surcharge)))
# with(m2025_5.df, table(pi, cc_surcharge, useNA = "always"))
# # impute the remaining missing discount values
# #m2025_5.df = missRanger(data = m2025_5.df, formula =  cc_surcharge ~ pi + amnt +urban_cat)
# #use proportion from credit, for all other payments
# (cc_surcharge_frac = round(nrow(subset(m2025_5.df, pi=="Credit" & cc_surcharge==1))/nrow(subset(m2025_5.df, pi=="Credit")),2))
# #find row numbers for remaining NA
# cc_surcharge_row_num = which(is.na(m2025_5.df$cc_surcharge))# which rows
# length(cc_surcharge_row_num)#number of rows with NAs cc_charge
# set.seed(1955)
# (picked = cc_surcharge_row_num[runif(length(cc_surcharge_row_num)) < 0.05])
# not_picked = setdiff(cc_surcharge_row_num, picked)
# length(picked)
# length(not_picked)
# length(picked) + length(not_picked)
# #
# m2025_5.df[picked, ]$cc_surcharge = "1"# assign 1 to 5% of the NAs
# with(m2025_5.df, table(pi, cc_surcharge, useNA = "always"))
# m2025_5.df[not_picked, ]$cc_surcharge = "0"# assign 0 to 95% of the NAs
# with(m2025_5.df, table(pi, cc_surcharge, useNA = "always"))

# imputing missing "cc_adopt" values based on HH income
with(m2025_5.df, table(pi, cc_adopt, useNA = "always"))
m2025_5.df$cc_adopt = as.factor(m2025_5.df$cc_adopt)
# impute the missing cc_adopt values
m2025_5.df = missRanger(data = m2025_5.df, formula =  cc_adopt ~ income_hh + age +urban_cat)
with(m2025_5.df, table(pi, cc_adopt, useNA = "always"))

# imputing missing "cc_rewards" (indiv dataset)
m2025_5.df$cc_rewards = as.factor(m2025_5.df$cc_rewards)
m2025_5.df$cc_adopt = as.factor(m2025_5.df$cc_adopt)
table(m2025_5.df$cc_rewards, useNA = "always")
# check missing rewards by cc_adopt
table(m2025_5.df$cc_rewards, m2025_5.df$cc_adopt, useNA = "always")
table(m2025_5.df$cc_adopt, m2025_5.df$cc_rewards, useNA = "always")
# => most NAs are by consumers who do not have credit cards => set cc_reward to 0 for consumers with cc_adopt = 0
m2025_6.df = m2025_5.df %>% mutate(cc_rewards = if_else(cc_adopt=="0", as.factor(0), cc_rewards))
str(m2025_6.df$cc_rewards)
str(m2025_5.df$cc_adopt)
table(m2025_6.df$cc_adopt, m2025_6.df$cc_rewards, useNA = "always")
# impute the renaming NAs for consumers who have credit cards by income and age
m2025_6.df = missRanger(data = m2025_6.df, formula =  cc_rewards ~ age + income_hh +urban_cat)
table(m2025_6.df$cc_adopt, m2025_6.df$cc_rewards, useNA = "always")

# Define new variables: paycash, paycredit, paydebit all 0,1, 
m2025_6.df$Paid_cash = ifelse(m2025_6.df$pi=="Cash",1,0)
m2025_6.df$Paid_cash = as.factor(m2025_6.df$Paid_cash)
m2025_6.df$Paid_credit = ifelse(m2025_6.df$pi=="Credit",1,0)
m2025_6.df$Paid_credit = as.factor(m2025_6.df$Paid_credit)
m2025_6.df$Paid_debit = ifelse(m2025_6.df$pi=="Debit",1,0)
m2025_6.df$Paid_debit = as.factor(m2025_6.df$Paid_debit)

# verify variable types and no NAs
str(m2025_6.df)
sum(is.na(m2025_6.df))
names(m2025_6.df)[colSums(is.na(m2025_6.df)) > 0]
# delete trans with missing HH income
dim(m2025_6.df)
m2025_7.df = subset(m2025_6.df, !is.na(m2025_6.df$income_hh))
dim(m2025_7.df)
sum(is.na(m2025_7.df))
# divide HH income by 10,000
m2025_7.df$income_hh = m2025_7.df$income_hh/10000
# pi should be a factor
str(m2025_7.df$pi)
m2025_7.df$pi = as.factor(m2025_7.df$pi)

#rename variables
m2025_8.df = m2025_7.df %>% rename("Amount" = "amnt", "Own_credit" = "cc_adopt", "Credit_reward" = "cc_rewards",  "Own_debit" = "dc_adopt", "Discount" = "discount", "HH_income" = "income_hh", "PI" = "pi", "Age" = "age", "Urban" = "urban_cat", "Gender" = "gender")
names(m2025_8.df)

# Set factor levels (R regressions use the first as a reference level)
str(m2025_8.df$HH_income)
str(m2025_8.df$Age)
str(m2025_8.df$Amount)
#
str(m2025_8.df$Urban)
table(m2025_8.df$Urban, useNA = "always")
(levels(m2025_8.df$Urban) = c("Rural", "Mixed", "Urban"))
#
str(m2025_8.df$Own_credit)
levels(m2025_8.df$Own_credit)
table(m2025_8.df$Own_credit, useNA = "always")
(levels(m2025_8.df$Own_credit) = c("No", "Yes"))
#
str(m2025_8.df$Credit_reward)
levels(m2025_8.df$Credit_reward)
table(m2025_8.df$Credit_reward, useNA = "always")
(levels(m2025_8.df$Credit_reward) = c("No", "Yes"))
#
str(m2025_8.df$Own_debit)
levels(m2025_8.df$Own_debit)
table(m2025_8.df$Own_debit, useNA = "always")
(levels(m2025_8.df$Own_debit) = c("No", "Yes"))
#
str(m2025_8.df$Discount)
levels(m2025_8.df$Discount)
table(m2025_8.df$Discount, useNA = "always")
(levels(m2025_8.df$Discount) = c("No", "Yes"))
#
str(m2025_8.df$cc_surcharge)
levels(m2025_8.df$cc_surcharge)
table(m2025_8.df$cc_surcharge, useNA = "always")
(levels(m2025_8.df$cc_surcharge) = c("No", "Yes"))
#
str(m2025_8.df$Gender)
m2025_8.df$Gender = as.factor(m2025_8.df$Gender)
levels(m2025_8.df$Gender)
table(m2025_8.df$Gender, useNA = "always")
(levels(m2025_8.df$Gender) = c("Female", "Male"))

# 3 logit regression models: Cash, Credit, and Debit
cash.model = Paid_cash ~ HH_income + Age +Gender +Urban + Own_credit + Credit_reward + Own_debit + Amount + Discount #+ cc_surcharge
#
credit.model = Paid_credit ~ HH_income + Age +Gender +Urban + Own_credit + Credit_reward + Own_debit + Amount + Discount #+ cc_surcharge 
#
debit.model = Paid_debit ~ HH_income + Age +Gender +Urban + Own_credit + Credit_reward + Own_debit + Amount + Discount #+ cc_surcharge 


#cash regression R mfx yielding 0,1 warning
(cash.reg = logitmfx(cash.model, data = m2025_8.df, atmean = F))

#cash regression R built-in glm yielding 0,1 warning
#cash.reg = glm(cash.model, data = m2025_7.df, family = binomial(link = "logit"), method = "brglmFit")
#summary(cash.reg)
#compare logistf package to eliminate warning 0,1 fittings
#cash_logist.reg = logistf(cash.model, data = m2025_7.df)
#summary(cash_logist.reg)
# => minor difference in coefficients => use glm

#credit regression R mfx yielding 0,1 warninig
(credit.reg = logitmfx(credit.model, data = m2025_8.df, atmean = F))

#credit regression R built-in glm yielding 0,1 warning
#credit.reg = glm(credit.model, data = m2025_7.df, family = binomial(link = "logit"), method = "brglmFit")
#summary(credit.reg)
#compare logistf package to eliminate warning 0,1 fittings
#credit_logist.reg = logistf(credit.model, data = m2025_7.df)
#summary(credit_logist.reg)
# => minor difference in coefficients => use glm

#debit regression using mfx yielding 0,1 warninig
(debit.reg = logitmfx(debit.model, data = m2025_8.df, atmean = F))# avg marginal effects

#debit regression R built-in glm yielding 0,1 warninig
#debit.reg = glm(debit.model, data = m2025_7.df, family = binomial(link = "logit"), method = "brglmFit")
#summary(debit.reg)
#compare logistf package to eliminate warning 0,1 fittings
#debit_logist.reg = logistf(debit.model, data = m2025_7.df)
#summary(debit_logist.reg)
# => minor difference in coefficients => use glm

#construct a LaTeX table
screenreg(list(cash.reg, credit.reg, debit.reg), digits = 3,
          stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
          symbol = ".",
          custom.model.names = c("Cash", "Credit", "Debit"))#texreg package => screen
#
texreg(list(cash.reg, credit.reg, debit.reg), digits = 3,
       stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
       symbol = ".",
       custom.model.names = c("Cash", "Credit", "Debit"))#texreg package => LaTeX

# Reference catogories (first level)
levels(m2025_8.df$Urban)

#End regression on demog and trans-specific variables####

#+++++++++++++++++++++

#Begin: (not in paper) Classification tree####
#m2025_8.df = m2025_7.df

names(m2025_8.df)
sum(is.na(m2025_8.df))
table(m2025_8.df$PI, useNA = "always")
table(m2025_8.df$Paid_cash, useNA = "always")
table(m2025_8.df$Paid_credit, useNA = "always")
table(m2025_8.df$Paid_debit, useNA = "always")

#rename variables to make them readable on the tree figure
#m2025_8.df = m2025_8.df %>% rename("Amount" = "amnt", "Own_credit" = "cc_adopt", "Credit_reward" = "cc_rewards",  "Own_debit" = "dc_adopt", "Discount" = "discount", "HH_income" = "income_hh", "PI" = "pi")
names(m2025_8.df)

# rename factor values 0 and 1 to No and Yes
with(m2025_8.df, table(Own_credit, useNA = "always"))
with(m2025_8.df, levels(Own_credit))
levels(m2025_8.df$Own_credit) = c("No", "Yes")
with(m2025_8.df, table(Own_credit, useNA = "always"))
#
with(m2025_8.df, table(Own_debit, useNA = "always"))
with(m2025_8.df, levels(Own_debit))
levels(m2025_8.df$Own_debit) = c("No", "Yes")
with(m2025_8.df, table(Own_debit, useNA = "always"))
#
with(m2025_8.df, table(Discount, useNA = "always"))
with(m2025_8.df, levels(Discount))
levels(m2025_8.df$Discount) = c("No", "Yes")
with(m2025_8.df, table(Discount, useNA = "always"))
#
with(m2025_8.df, table(Credit_reward, useNA = "always"))
with(m2025_8.df, levels(Credit_reward))
levels(m2025_8.df$Credit_reward) = c("No", "Yes")
with(m2025_8.df, table(Credit_reward, useNA = "always"))
#
with(m2025_8.df, table(Paid_cash, useNA = "always"))
with(m2025_8.df, levels(Paid_cash))
levels(m2025_8.df$Paid_cash) = c("No", "Yes")
with(m2025_8.df, table(Paid_cash, useNA = "always"))
#
with(m2025_8.df, table(Paid_credit, useNA = "always"))
with(m2025_8.df, levels(Paid_credit))
levels(m2025_8.df$Paid_credit) = c("No", "Yes")
with(m2025_8.df, table(Paid_credit, useNA = "always"))
#
with(m2025_8.df, table(Paid_debit, useNA = "always"))
with(m2025_8.df, levels(Paid_debit))
levels(m2025_8.df$Paid_debit) = c("No", "Yes")
with(m2025_8.df, table(Paid_debit, useNA = "always"))
#
with(m2025_8.df, table(Urban, useNA = "always"))
str(m2025_8.df$Urban)
with(m2025_8.df, levels(Urban))
levels(m2025_8.df$Urban) = c("Rural", "Mixed", "Urban")
with(m2025_8.df, table(Urban, useNA = "always"))

# model
pi.model = PI ~ HH_income + Age +Gender + Urban + Own_credit + Own_debit + Amount + Discount + Credit_reward

# Tree on entire sample (not just training) to generate Fig.2 in paper, & Not tuning to Optimal tree cp, just to demonstrate. For confusion table, see below
set.seed(1955)# to be able to reproduce the rpart CV below
tree1 = rpart(pi.model, data = m2025_8.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
prp(tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(tree1)# plot cp: 
#names(tree1)
tree1$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning. There are 2 methods:
#Method 1: Use the plotcp and pick the highest relative error below the dashed line => may lead to a larger tree than needed.
#
#Method 2 (1-SE rule, preferred): Pick the lowest xerror in the cptable. Add the corresponding xstd. Then, pick the cp in the cptable with xerror < or = to this sum.
#
(cp.choice = tree1$cptable[4, "CP"]) # Corresponds to 9 splits (just for demonstration)
prune1 = prune.rpart(tree1, cp=cp.choice)
# plot prunned tree
prp(prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, tweak = 1.0, cex = 1.0, fallen.leaves = T, branch = 0.2)
#Note: Maximize the Plots window in RStudio before prp
#faclen = 0 → no abbreviation (print full factor level names)
#faclen = n → abbreviate factor levels to n characters tweak for char size
#tweak also controls the fontsize (in addition or replacement for cex = 1.x, but it looks good this way)
#varlen = -1 (default) → use full variable names
#varlen = 0 → shortest unique abbreviation
#varlen = n → truncate variable names to at most n characters
# extra = 0 → show the predicted class only (default)
#extra = 1 → show number of observations in the node
#extra = 2 → show class probabilities
#extra = 4 → show class percentages
#extra = 5 → show class and probability
#extra = 6 → show class, probability, and N
#extra = 7 → show class, N, and percentage
# For regression trees:  extra = 101 → show fitted value; extra = 104 → show fitted value and number of observations

# Information for the classification tree figure
nrow(m2025_8.df)
length(unique(m2025_8.df$id))

#End: (not in paper) Classification tree####

#++++++++++++++++++++++

#Begin: Figure 4: Random Forest####
set.seed(1955)
# recall model
pi.model

(forest_output =randomForest(pi.model, data=m2025_8.df, importance=T, na.action=na.roughfix))
# defaults: mtry = rounded downwards sqrt(#predictors), nodesize=1, na.action =na.roughtfix => imputations. 
forest_output$type# verify classification tree (not regression tree)
#forest_output$confusion based on the OOB sample (Instead, I use train-test subsamples for the confusion matrix)

# Table of variable importance for the entire sample
forest_importance.df =importance(forest_output) 
str(forest_importance.df)
(forest_importance2.df = as.data.frame(forest_importance.df))
# Below, Plot of variable importance (not in paper)
#varImpPlot(random_forest_output, type = 1, main ='', bg = "blue", cex=2)#default type 1&2, 

# Rename rows
(forest_importance3.df = round(forest_importance2.df, digits = 2))
row.names(forest_importance3.df)
#(row.names(forest_importance3.df) = c("Preferred payment method",  "Spending/merchant category", "Spending amount", "Own credit card", "Credit card reward", "Own debit card", "Card accepted", "Cash accepted", "Credit card surcharge", "Discount for payment method"))
#forest_importance3.df
#
# Delete the GINI MDA from the importance table
names(forest_importance3.df)
dim(forest_importance3.df)
(forest_importance4.df = forest_importance3.df[,1:5])
#
print(xtable(forest_importance4.df))
# info about the above table notes
nrow(m2025_8.df)# num of payments
nrow(m2025_8.df[unique(m2025_8.df$id), ])# num respondents

#End: Figure 4: Random Forest####

#++++++++++++++++++++++

#Begin: Figure 2: fraction PI by HH income####
names(m2025_8.df)
table(m2025_8.df$hhincome)
#
(cash_frac_by_income.df = m2025_8.df %>% group_by(hhincome) %>% summarise(frac_cash = mean(PI=="Cash")))
(frac_cash.vec = cash_frac_by_income.df$frac_cash)# make it a vector
#
(credit_frac_by_income.df = m2025_8.df %>% group_by(hhincome) %>% summarise(frac_credit = mean(PI=="Credit")))
(frac_credit.vec = credit_frac_by_income.df$frac_credit)# make it a vector
#
(debit_frac_by_income.df = m2025_8.df %>% group_by(hhincome) %>% summarise(frac_debit = mean(PI=="Debit")))
(frac_debit.vec = debit_frac_by_income.df$frac_debit)# make it a vector
#
(other_frac_by_income.df = m2025_8.df %>% group_by(hhincome) %>% summarise(frac_other = mean(PI=="Other")))
(frac_other.vec = other_frac_by_income.df$frac_other)# make it a vector
#
# verify sum up to 1
frac_cash.vec + frac_credit.vec + frac_debit.vec + frac_other.vec

#make it a data frame
HH_income_group.vec = as.factor(1:16)
#
(frac_pi_by_income.df = data.frame(HH_income_group.vec, Cash = frac_cash.vec, Credit = frac_credit.vec, Debit = frac_debit.vec, Other =frac_other.vec))

ggplot(frac_pi_by_income.df, aes(x=HH_income_group.vec, y=Cash, group = 1)) + geom_line(linetype="solid", linewidth=1.2, color="black") +geom_point(size=3, color="black")+geom_line(aes(y=Debit), linetype="longdash", linewidth=1.2, color="blue") +geom_point(aes(y=Debit), size=3, color="blue") +geom_line(aes(y=Credit), linetype="dotdash", linewidth=1.2, color="red") +geom_point(aes(y=Credit), size=3, color="red") +geom_line(aes(y=Other), linetype="dotted", linewidth=1.2, color="darkgreen") +geom_point(aes(y=Other), size=3, color="darkgreen") +scale_x_discrete(breaks = seq(0,16,1)) + scale_y_continuous(breaks = seq(0, 1, 0.05), labels = percent)+labs(x="Annual household income group (low to high)", y="Preferred payment method (%)") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  +annotate("text", x = 12, y = 0.52, angle=-24, label = "Debit card", size = 8, color="blue")  +annotate("text", x = 13, y = 0.20, label = "Cash", size = 8, color="black") +annotate("text", x = 11, y = 0.35, label = "Credit card", size = 8, color="red") +annotate("text", x = 11.5, y = 0.06, label = "Other", size = 8, color="darkgreen")


#Info for figure 2: PI by HH income
nrow(m2025_8.df)
length(unique(m2025_8.df$id))
#
(round(100*frac_cash.vec,1))# cash use
(round(100*frac_credit.vec,1))# credit use
(round(100*frac_debit.vec,1))# debit use

#End: Figure 2: fraction PI by HH income####

#++++++++++++++++++++++

#Begin: Figure 3: fraction PI by Age####
names(m2025_8.df)
summary(m2025_8.df$Age)
sum(is.na(m2025_8.df$Age))

# Construct and age groups
m2025_9.df =m2025_8.df %>%
  mutate(
    age_group = cut(
      Age,
      breaks = c(18, 24, 34, 44, 54, 64, 75, Inf),
      labels = c("18–24","25–34","35–44","45–54","55–64", "65–74","75+"),
      right = F   # include 18.
    )
  )
table(m2025_9.df$age_group, useNA = "always")
sum(table(m2025_9.df$age_group))
nrow(m2025_9.df)
names(m2025_9.df)
class(m2025_9.df$age_group)

# number resp who prefer cash by age group
(cash_by_age.df = m2025_9.df %>% group_by(age_group) %>% summarise(pay_cash = sum(PI=="Cash")))
#
(credit_by_age.df = m2025_9.df %>% group_by(age_group) %>% summarise(pay_credit = sum(PI=="Credit")))
#
(debit_by_age.df = m2025_9.df %>% group_by(age_group) %>% summarise(pay_debit = sum(PI=="Debit")))
#
(other_by_age.df = m2025_9.df %>% group_by(age_group) %>% summarise(pay_other = sum(PI=="Other")))

# count resp in each age group
(count_by_age.df = m2025_9.df %>% count(age_group))

# make a data frame
(pi_by_age.df = data.frame(Age = count_by_age.df$age_group, N = count_by_age.df$n, Cash_num = cash_by_age.df$pay_cash, Credit_num = credit_by_age.df$pay_credit, Debit_num = debit_by_age.df$pay_debit, Other_num = other_by_age.df$pay_other))

# frac of each PI
(pi_by_age.df$Cash_frac = pi_by_age.df$Cash_num/pi_by_age.df$N)
(pi_by_age.df$Credit_frac = pi_by_age.df$Credit_num/pi_by_age.df$N)
(pi_by_age.df$Debit_frac = pi_by_age.df$Debit_num/pi_by_age.df$N)
(pi_by_age.df$Other_frac = pi_by_age.df$Other_num/pi_by_age.df$N)

names(pi_by_age.df)
ggplot(pi_by_age.df, aes(x=Age, y=Cash_frac)) +geom_point(size=3, color="black") +geom_path(aes(group=1), linetype="solid", linewidth=1.2, color="black") +geom_point(aes(y=Debit_frac), size=3, color="blue") +geom_path(aes(y=Debit_frac, group=1), linetype="longdash", linewidth=1.2, color="blue") +geom_point(aes(y=Credit_frac), size=3, color="red") +geom_path(aes(y=Credit_frac, group=1), linetype="dotdash", linewidth=1.2, color="red") +geom_point(aes(y=Other_frac), size=3, color="darkgreen") +geom_path(aes(y=Other_frac, group = 1), linetype="dotted", linewidth=1.2, color="darkgreen") +scale_x_discrete(breaks = pi_by_age.df$Age) + scale_y_continuous(breaks = seq(0,1,0.10), labels = percent(seq(0,1,0.10))) +labs(x="Age group", y="Preferred payment method (%)") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2, y = 0.51, label = "Debit card", size = 8, color="blue") +annotate("text", x = 2, y = 0.39, label = "Credit card", size = 8, color="red") +annotate("text", x = 2, y = 0.14, label = "Cash", size = 8, color="black") +annotate("text", x = 2, y = 0.03, label = "Other", size = 8, color="darkgreen") 

# Information for Figure 3 
nrow(m2025_8.df)# num payments
length(unique(m2025_8.df$id))
#
round(100*pi_by_age.df$Cash_frac,1)
round(100*pi_by_age.df$Credit_frac,1)
round(100*pi_by_age.df$Debit_frac,1)

#End: Figure 3: fraction PI by Age####

#+++++++++++++++++

#Begin: Table 1: fraction PI by Urban\rural & gender####

names(m2025_8.df)
table(m2025_8.df$Urban, useNA = "always")
#
(cash_frac_by_urban.df = m2025_8.df %>% group_by(Urban) %>% summarise(frac_cash = mean(PI=="Cash")))
(frac_cash_urban.vec = cash_frac_by_urban.df$frac_cash)# make it a vector
(perc_cash_urban.vec = 100*round(cash_frac_by_urban.df$frac_cash,3))# make it %
#
(credit_frac_by_urban.df = m2025_8.df %>% group_by(Urban) %>% summarise(frac_credit = mean(PI=="Credit")))
(frac_credit_urban.vec = credit_frac_by_urban.df$frac_credit)# make it a vector
(perc_credit_urban.vec = 100*round(credit_frac_by_urban.df$frac_credit,3))# make it %
#
(debit_frac_by_urban.df = m2025_8.df %>% group_by(Urban) %>% summarise(frac_debit = mean(PI=="Debit")))
(frac_debit_urban.vec = debit_frac_by_urban.df$frac_debit)# make it a vector
(perc_debit_urban.vec = 100*round(debit_frac_by_urban.df$frac_debit,3))# make it %
#
(other_frac_by_urban.df = m2025_8.df %>% group_by(Urban) %>% summarise(frac_other = mean(PI=="Other")))
(frac_other_urban.vec = other_frac_by_urban.df$frac_other)# make it a vector
(perc_other_urban.vec = 100*round(other_frac_by_urban.df$frac_other,3))# make it %

# make it a data frame
(perc_urban_total.vec = c(perc_cash_urban.vec[1] +perc_credit_urban.vec[1] +perc_debit_urban.vec[1] +perc_other_urban.vec[1], perc_cash_urban.vec[2] +perc_credit_urban.vec[2] +perc_debit_urban.vec[2] +perc_other_urban.vec[2],  perc_cash_urban.vec[3] +perc_credit_urban.vec[3] +perc_debit_urban.vec[3] +perc_other_urban.vec[3]))

(urban.df = data.frame(Area = c("Rural", "Mixed", "Urban"), Cash = perc_cash_urban.vec, Credit = perc_credit_urban.vec, Debit = perc_debit_urban.vec, Other = perc_other_urban.vec, Total = perc_urban_total.vec))

# start gender (to be added to urban.df)

table(m2025_8.df$Gender, useNA = "always")
#
(cash_frac_by_gender.df = m2025_8.df %>% group_by(Gender) %>% summarise(frac_cash = mean(PI=="Cash")))
(frac_cash_gender.vec = cash_frac_by_gender.df$frac_cash)# make it a vector
(perc_cash_gender.vec = 100*round(cash_frac_by_gender.df$frac_cash,3))# make it %
#
(credit_frac_by_gender.df = m2025_8.df %>% group_by(Gender) %>% summarise(frac_credit = mean(PI=="Credit")))
(frac_credit_gender.vec = credit_frac_by_gender.df$frac_credit)# make it a vector
(perc_credit_gender.vec = 100*round(credit_frac_by_gender.df$frac_credit,3))# make it %
#
(debit_frac_by_gender.df = m2025_8.df %>% group_by(Gender) %>% summarise(frac_debit = mean(PI=="Debit")))
(frac_debit_gender.vec = debit_frac_by_gender.df$frac_debit)# make it a vector
(perc_debit_gender.vec = 100*round(debit_frac_by_gender.df$frac_debit,3))# make it %
#
(other_frac_by_gender.df = m2025_8.df %>% group_by(Gender) %>% summarise(frac_other = mean(PI=="Other")))
(frac_other_gender.vec = other_frac_by_gender.df$frac_other)# make it a vector
(perc_other_gender.vec = 100*round(other_frac_by_gender.df$frac_other,3))# make it %

# make gender a data frame
(perc_gender_total.vec = c(perc_cash_gender.vec[1] +perc_credit_gender.vec[1] +perc_debit_gender.vec[1] +perc_other_gender.vec[1], perc_cash_gender.vec[2] +perc_credit_gender.vec[2] +perc_debit_gender.vec[2] +perc_other_gender.vec[2]))

(gender.df = data.frame(Gender = c("Female", "Male"), Cash = perc_cash_gender.vec, Credit = perc_credit_gender.vec, Debit = perc_debit_gender.vec, Other = perc_other_gender.vec, Total = perc_gender_total.vec))

# Append gender data frame below urban data frame
dim(urban.df)
dim(gender.df)
# rename 1st columns (prep for binding the 2 data frames)
names(urban.df)
urban2.df = urban.df %>% rename("Category" = "Area")
names(urban2.df)
names(gender.df)
gender2.df = gender.df %>% rename("Category" = "Gender")
names(gender2.df)

(urban_gender.df = bind_rows(urban2.df, gender2.df))

# LaTeX table
print(xtable(urban_gender.df, digits=1), include.rownames = F, hline.after = c(0,3))

# Info for Table 1: urban/rural & gender
nrow(m2025_8.df)# num payments
table(m2025_8.df$Urban)
sum(table(m2025_8.df$Urban))
table(m2025_8.df$Gender)
sum(table(m2025_8.df$Gender))
# num resp by urban cat
length(unique(m2025_8.df$id))# num resp
urban_2025.df = subset(m2025_8.df, Urban=="Urban")
mixed_2025.df = subset(m2025_8.df, Urban=="Mixed")
rural_2025.df = subset(m2025_8.df, Urban=="Rural")
length(unique(urban_2025.df$id))# num urban resp
length(unique(mixed_2025.df$id))# num mixed resp
length(unique(rural_2025.df$id))# num rural resp
length(unique(urban_2025.df$id)) +length(unique(mixed_2025.df$id)) +length(unique(rural_2025.df$id))# total resp
# num resp by gender
length(unique(m2025_8.df$id))# num resp
female_2025.df = subset(m2025_8.df, Gender=="Female")
male_2025.df = subset(m2025_8.df, Gender=="Male")
length(unique(female_2025.df$id)) +length(unique(male_2025.df$id)) #total resp

#End: Table 1: fraction PI by Urban & gender####

#++++++++++++++++

#Appendix A: Imputations of NAs####
#See below line 583 above