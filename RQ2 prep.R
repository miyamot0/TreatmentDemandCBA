# Shawn Gilroy, Louisiana State University - Copyright 2021
# GPLv2+

suppressPackageStartupMessages(library(beezdemand))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggsci))
suppressPackageStartupMessages(library(jtools))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(nlme))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(tidyverse))

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1 * y))*((10^(2 * y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyResults <- readRDS("offlineDataCBA.rds")

mSurveyResults.clean <- mSurveyResults %>%
  filter(nChildren > 0 & !is.na(attnCheck_2_1) & !is.na(demo_BirthYear))

### Edit for coding error
### R_3eajAPXM63YgJu0. That person didn't do year, did age (30) instead
mSurveyResults.clean <- mSurveyResults.clean %>%
  mutate(demo_BirthYear = ifelse(demo_BirthYear == 30,
                                 1990, demo_BirthYear))

demandAlone <- mSurveyResults.clean %>%
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
           nChildren)) %>%
  #rename some of the columns selected, using proices as a new name
  rename(`50`= `1_demandAlone_1`,
         `100`=`2_demandAlone_1`,
         `150`=`3_demandAlone_1`,
         `200`=`4_demandAlone_1`,
         `250`=`5_demandAlone_1`,
         `300`=`6_demandAlone_1`,
         `350`=`7_demandAlone_1`,
         `400`=`8_demandAlone_1`) %>%
  mutate(Prospect = "Demand-Alone")

crossPriceOwn <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_crossPrice_1',
           '2_crossPrice_1',
           '3_crossPrice_1',
           '4_crossPrice_1',
           '5_crossPrice_1',
           '6_crossPrice_1',
           '7_crossPrice_1',
           '8_crossPrice_1',
           demo_Education,
           demo_Gender,
           demo_Marital,
           nChildren)) %>%
  #rename some of the columns selected, using proices as a new name
  rename(`50`= `1_crossPrice_1`,
         `100`=`2_crossPrice_1`,
         `150`=`3_crossPrice_1`,
         `200`=`4_crossPrice_1`,
         `250`=`5_crossPrice_1`,
         `300`=`6_crossPrice_1`,
         `350`=`7_crossPrice_1`,
         `400`=`8_crossPrice_1`) %>%
  mutate(Prospect = "Demand-Own")

combinedFrame = rbind(demandAlone, crossPriceOwn)

combinedFrame.gather <- combinedFrame %>%
  gather(Price, Consumption,
         -ResponseId,
         -demo_Education,
         -demo_Gender,
         -demo_Marital,
         -nChildren,
         -Prospect)

pureDemandGroupFrame <- combinedFrame.gather %>%
  mutate(Price = as.numeric(Price),
         Consumption = as.numeric(Consumption)) %>%
  group_by(Prospect, Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  ) %>%
  ungroup()

combinedFrame.gather$Price = as.numeric(combinedFrame.gather$Price)

zbeFit.full.closed <- nls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
    k *
    (exp(-alpha * q0 * Price) - 1),
  start   = list(q0    = max(pureDemandGroupFrame$ConsumptionAve),
                 k     = 1,
                 alpha = 0.001),
  control = nls.control(maxiter = 5000,
                        warnOnly = TRUE),
  data    = combinedFrame.gather)

zbeFit.abbrev.closed <- nls(formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
                              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),

                            control = nls.control(maxiter = 5000,
                                                  warnOnly = TRUE),
                            data = combinedFrame.gather,
                            start   = list(q0    = max(pureDemandGroupFrame$ConsumptionAve),
                                           alpha = 0.001))

zbeFit.null.closed <- nls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10),
  start   = list(q0    = max(pureDemandGroupFrame$ConsumptionAve)),
  data    = combinedFrame.gather)

ss1 <- sum(resid(zbeFit.abbrev.closed)^2)
ss2 <- sum(resid(zbeFit.full.closed)^2)
df1 <- df.residual(zbeFit.abbrev.closed)
df2 <- df.residual(zbeFit.full.closed)

Fval <- ((ss1 - ss2)/ss2)/((df1 - df2)/df2)
pval <- 1 - pf(Fval, (df1 - df2), df2)
critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

print("Null hypothesis: No Specific Span Parameter Necessary (Q0 == K)")
print("Alternative hypothesis: K is DIFFERENT from overall span")
print(paste0("Conclusion: ", if (pval < .05) "reject" else "fail to reject", " the null hypothesis"))
print(paste0("F(", (df1 - df2), ",", df2, ") = ", round(Fval, 4), ", p = ", round(pval, 4)))

# go two param!

ss1 <- sum(resid(zbeFit.null.closed)^2)
ss2 <- sum(resid(zbeFit.abbrev.closed)^2)
df1 <- df.residual(zbeFit.null.closed)
df2 <- df.residual(zbeFit.abbrev.closed)

Fval <- ((ss1 - ss2)/ss2)/((df1 - df2)/df2)
pval <- 1 - pf(Fval, (df1 - df2), df2)
critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

print("Null hypothesis: No Specific Span Parameter Necessary (Q0 == K)")
print("Alternative hypothesis: K is DIFFERENT from overall span")
print(paste0("Conclusion: ", if (pval < .05) "reject" else "fail to reject", " the null hypothesis"))
print(paste0("F(", (df1 - df2), ",", df2, ") = ", round(Fval, 4), ", p = ", round(pval, 4)))

# Stick with two param

combinedFrame.gather <- combinedFrame.gather %>%
  mutate(demo_Education = recode(demo_Education,
                                 `Professional degree (JD, MD) (8)`          = "Bachelors+",
                                 `Doctoral degree (7)`                       = "Bachelors+",
                                 `Master's degree (6)`                       = "Bachelors+",
                                 `Bachelor's degree in college (4-year) (5)` = "Bachelors+",
                                 `Associate degree in college (2-year) (4)`  = "NoBachelors",
                                 `Some college but no degree (3)`            = "NoBachelors",
                                 `High school graduate (high school diploma) (2)` = "NoBachelors",
                                 `Less than high school degree (1)` = "NoBachelors")) %>%
  mutate(demo_Education = factor(as.character(demo_Education))) %>%
  mutate(FamilySize = ifelse(nChildren > 1, "Multiple Children", "Single Child")) %>%
  mutate(FamilySize = factor(FamilySize)) %>%
  mutate(demo_Gender = recode(demo_Gender,
                              `Man (cisgender)` = "Male",
                              `Woman (cisgender)` = "Female")) %>%
  mutate(demo_Gender = factor(as.character(demo_Gender)),
         Price = as.numeric(Price))

combinedFrame.gather$Prospect = factor(combinedFrame.gather$Prospect)

out <- NULL
out <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
            data = combinedFrame.gather,
            na.action = na.omit,
            params = list(q0 + alpha ~ Prospect),
            start = list( fixed = c(q0 = rep(10, 2),
                                    alpha = rep(0.0001, 2))),
            weights = varPower(),
            control = gnlsControl(msMaxIter = 200,
                                  maxIter = 300,
                                  tolerance = 1,
                                  nlsTol = 1,
                                  apVar = F,
                                  opt = "optim"))

firstModelTest <- nlme(log((Consumption * 0.5) +
                             ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                         (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
                         (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) *
                                                             (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
                       data    = combinedFrame.gather,
                       na.action = na.omit,
                       fixed   = list(
                         q0    ~ Prospect,
                         alpha ~ Prospect
                       ),
                       random  = pdBlocked(list(
                         pdSymm(q0 + alpha ~ 1),
                         pdDiag(q0 + alpha ~ Prospect - 1)
                       )),
                       weights = varPower(),
                       method  = "ML",
                       groups  = ~ ResponseId,
                       start   = coef(out),
                       control = list(msMaxIter = 5000,
                                      niterEM   = 5000,
                                      maxIter   = 5000,
                                      pnlsTol   = .001,
                                      tolerance = 1),
                       verbose = F)

# RE's way worth it
anova(out, firstModelTest)

combinedFrame.gather$pred  <- tn$inverse(predict(firstModelTest, level = 0))
combinedFrame.gather$predi <- tn$inverse(predict(firstModelTest, level = 1))

save(combinedFrame.gather, file = "combinedFrameGather.RData")
