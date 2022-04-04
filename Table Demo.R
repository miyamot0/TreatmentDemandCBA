library(beezdemand)
library(broom)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lattice)
library(latticeExtra)
library(nlme)
library(qualtRics)
library(readtext)
library(scales)
library(tidyr)

mSurveyResults <- readRDS("offlineDataCBA.rds")

mSurveyResults.clean <- mSurveyResults %>%
  select(c("ResponseId",
           "irQ_bpt_1",  "irQ_bpt_2", "IR_Comb_BPT",
           "irQ_pcit_1", "irQ_pcit_1", "IR_Comb_PCIT",
           "irQ_pmt_1", "irQ_pmt_2", "IR_Comb_PMT",
           "irQ_ppp_1", "irQ_ppp_2", "IR_Comb_PPP",
           "irQ_incredibleyears_1", "irQ_incredibleyears_2", "IR_Comb_IncredibleYears",
           "irQ_essentialoils_1", "irQ_essentialoils_2", "IR_Comb_EssentialOils",
           "irQ_grounding_1", "irQ_grounding_2", "IR_Comb_Grounding",
           "irQ_hbot_1", "irQ_hbot_2", "IR_Comb_HBOT",
           "irQ_chelation_1", "irQ_chelation_2", "IR_Comb_Chelation",
           "irQ_dietarysup_1", "irQ_dietarysup_2", "IR_Comb_DietarySupp",
           "irQ_musictherapy_1", "irQ_musictherapy_2", "IR_Comb_Music_Therapy",
           "irQ_millermethod_1", "irQ_millermethod_2", "IR_Comb_MillerMethod",
           "irQ_rpp_1", "irQ_rpp_2", "IR_Comb_RPP",
           "irQ_gfcf_1", "irQ_gfcf_2", "IR_Comb_GlutenFree",
           "irQ_sit_1", "irQ_sit_2", "IR_Comb_SIT",
           "1_demandAlone_1", "2_demandAlone_1",
           "3_demandAlone_1", "4_demandAlone_1",
           "5_demandAlone_1", "6_demandAlone_1",
           "7_demandAlone_1", "8_demandAlone_1",
           "1_crossPrice_1", "1_crossPrice_2", "1_crossPrice_3",
           "2_crossPrice_1", "2_crossPrice_2", "2_crossPrice_3",
           "3_crossPrice_1", "3_crossPrice_2", "3_crossPrice_3",
           "4_crossPrice_1", "4_crossPrice_2", "4_crossPrice_3",
           "5_crossPrice_1", "5_crossPrice_2", "5_crossPrice_3",
           "6_crossPrice_1", "6_crossPrice_2", "6_crossPrice_3",
           "7_crossPrice_1", "7_crossPrice_2", "7_crossPrice_3",
           "8_crossPrice_1", "8_crossPrice_2", "8_crossPrice_3",
           "demo_BirthYear", "demo_Education", "demo_Gender", "demo_Marital",
           "demo_Income", "demo_Race",
           "attnCheck_1_1",
           "attnCheck_2_1",
           "attnCheck_2_2",
           "nChildren",
           "qualifier_Concern",
           "TRT_High_UR_High_IR", "TRT_High_UR_Low_IR", "TRT_Low_UR_High_IR")) %>%
  filter(nChildren > 0 & !is.na(attnCheck_2_2)) # Exclusion criterion here

### Edit for coding error
### R_3eajAPXM63YgJu0. That person didn't do year, did age (30) instead
mSurveyResults.clean <- mSurveyResults.clean %>%
  mutate(demo_BirthYear = ifelse(demo_BirthYear == 30,
                                 1990, demo_BirthYear))

pureDemandFrame <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_demandAlone_1',
           '2_demandAlone_1',
           '3_demandAlone_1',
           '4_demandAlone_1',
           '5_demandAlone_1',
           '6_demandAlone_1',
           '7_demandAlone_1',
           '8_demandAlone_1',
           demo_Education,
           demo_Gender,
           demo_Marital,
           nChildren,
           demo_BirthYear
  )) %>%
  rename(`50`= `1_demandAlone_1`,
         `100`=`2_demandAlone_1`,
         `150`=`3_demandAlone_1`,
         `200`=`4_demandAlone_1`,
         `250`=`5_demandAlone_1`,
         `300`=`6_demandAlone_1`,
         `350`=`7_demandAlone_1`,
         `400`=`8_demandAlone_1`)

pureDemandFrame.long <-pureDemandFrame %>%
  gather(Price, Consumption,
         `50`:`400`,
         -demo_Education,
         -demo_Gender,
         -demo_Marital,
         -nChildren,
         -demo_BirthYear,
         factor_key=TRUE) %>%
  mutate(Price= as.numeric(as.character(Price))) %>%
  mutate(Consumption=as.numeric(Consumption)) %>%
  as.data.frame()

fromNormalToIhs <- function(x) {
  (log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10))
}

fromIhsToNormal <- function(x) {
  (1/10^(1*x))*((10^(2*x)) - 1)
}

custom_ihs_trans <- trans_new(
  name = "IHS",
  transform = fromNormalToIhs,
  inverse = fromIhsToNormal
)

mSurveyRes <- mSurveyResults.clean %>%
  mutate(FamilySize = ifelse(nChildren > 1, "Multiple", "Single")) %>%
  mutate(Age = 2020 - demo_BirthYear)

mSurveyRes %>%
  summarise(
    Ave = mean(Age),
    SD  = sd(Age),
    Q1  = quantile(Age, probs = c(0.25)),
    Q2  = quantile(Age, probs = c(0.5)),
    Q3  = quantile(Age, probs = c(0.75)),
    n = n()
  ) %>%
  print(n = 100)

mSurveyRes %>%
  group_by(demo_Gender) %>%
  summarise(
    Count = n(),
    Pct = Count / 104
  ) %>%
  print(n = 100)

mSurveyRes %>%
  #  group_by(Income) %>%
  summarise(
    Ave = mean(demo_Income),
    SD  = sd(demo_Income),
    Q1  = quantile(demo_Income, probs = c(0.25)),
    Q2  = quantile(demo_Income, probs = c(0.5)),
    Q3  = quantile(demo_Income, probs = c(0.75))
  ) %>%
  print(n = 100)


mSurveyRes %>%
  group_by(qualifier_Concern) %>%
  summarise(
    Count = n(),
    Pct = Count / 104
  ) %>%
  print(n = 100)

mSurveyRes %>%
  group_by(demo_Marital) %>%
  summarise(
    Count = n(),
    Pct = Count / 104
  ) %>%
  print(n = 100)

mSurveyRes %>%
  summarise(
    Ave = mean(nChildren),
    SD  = sd(nChildren),
    Q1  = quantile(nChildren, probs = c(0.25)),
    Q2  = quantile(nChildren, probs = c(0.5)),
    Q3  = quantile(nChildren, probs = c(0.75))
  ) %>%
  print(n = 100)

mSurveyRes %>%
  group_by(demo_Education) %>%
  summarise(
    Count = n(),
    Pct = Count / 104
  ) %>%
  print(n = 100)

mSurveyRes %>%
  group_by(demo_Race) %>%
  summarise(
    Count = n(),
    Pct = Count / 104
  ) %>%
  print(n = 100)

