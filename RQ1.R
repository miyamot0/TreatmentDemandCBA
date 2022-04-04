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
           nChildren
           )) %>%
  rename(`50`  = `1_demandAlone_1`,
         `100` = `2_demandAlone_1`,
         `150` = `3_demandAlone_1`,
         `200` = `4_demandAlone_1`,
         `250` = `5_demandAlone_1`,
         `300` = `6_demandAlone_1`,
         `350` = `7_demandAlone_1`,
         `400` = `8_demandAlone_1`)

pureDemandFrame.long <- pureDemandFrame %>%
                   gather(Price, Consumption,
                          `50`:`400`,
                          -demo_Education,
                          -demo_Gender,
                          -demo_Marital,
                          -nChildren,
                          factor_key = TRUE) %>%
                   mutate(Price = as.numeric(as.character(Price))) %>%
                   mutate(Consumption = as.numeric(Consumption)) %>%
                   as.data.frame()

## Flaggings the good, systematic data
passing.ids <- pureDemandFrame.long %>%
  rename(x  = Price,
         y  = Consumption,
         id = ResponseId) %>%
  beezdemand::CheckUnsystematic() %>%
  filter(TotalPass == 3) %>%
  select(id) %>%
  pull()

length(passing.ids)/104

pureDemandGroupFrame <- pureDemandFrame.long %>%
  filter(ResponseId %in% passing.ids) %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  ) %>%
  ungroup()

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
  data    = pureDemandFrame.long)

zbeFit.abbrev.closed <- nls(formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
                              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
                            data = pureDemandFrame.long,
                            start   = list(q0    = max(pureDemandGroupFrame$ConsumptionAve),
                                           alpha = 0.001))

zbeFit.null.closed <- nls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10),
  start   = list(q0    = max(pureDemandGroupFrame$ConsumptionAve)),
  data    = pureDemandFrame.long)

ss1 <- sum(resid(zbeFit.abbrev.closed)^2)
ss2 <- sum(resid(zbeFit.full.closed)^2)
df1 <- df.residual(zbeFit.abbrev.closed)
df2 <- df.residual(zbeFit.full.closed)

Fval <- ((ss1 - ss2)/ss2)/((df1 - df2)/df2)
pval <- 1 - pf(Fval, (df1 - df2), df2)
critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

print("Null hypothesis: (Restricted is Good, fewer) No Specific Span Parameter Necessary (Q0 == K)")
print("Alternative hypothesis: (Unrestricted actually better, more) K is DIFFERENT from overall span")
print(paste0("Conclusion: ", if (pval < .05) "reject" else "fail to reject", " the null hypothesis"))
print(paste0("F(", (df1 - df2), ",", df2, ") = ", round(Fval, 4), ", p = ", round(pval, 4)))

# Support 2 param

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

# reject 1 param, 2 param best!

pureDemandFrame.long <- pureDemandFrame.long %>%
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
  mutate(FamilySize = factor(FamilySize,
                             levels = c("Single Child", "Multiple Children"))) %>%
  mutate(demo_Gender = recode(demo_Gender,
                         `Man (cisgender)` = "Male",
                         `Woman (cisgender)` = "Female")) %>%
  mutate(demo_Gender = factor(as.character(demo_Gender)))

out <- NULL
out <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
            data = pureDemandFrame.long,
            na.action = na.omit,
            params = list(q0 + alpha ~ demo_Education + demo_Gender + FamilySize),
            start = list( fixed = c(q0 = rep(10, 4),
                                    alpha = rep(0.0001, 4))),
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
                       data    = pureDemandFrame.long,
                       fixed   = list(
                         q0    ~ demo_Education + demo_Gender + FamilySize,
                         alpha ~ demo_Education + demo_Gender + FamilySize
                       ),
                       random  = list(pdDiag(q0 + alpha ~ 1)),
                       method  = "ML",
                       groups  = ~ ResponseId,
                       start   = coef(out),
                       control = list(msMaxIter = 5000,
                                      niterEM   = 5000,
                                      maxIter   = 5000,
                                      pnlsTol   = .1,
                                      tolerance = .01,
                                      apVar     = T,
                                      minScale  = .00001,
                                      opt       = "nlminb"),
                       verbose = F)

anova(out, firstModelTest)

# way better with RE's

pureDemandGroupFrame <- pureDemandFrame.long %>%
  filter(ResponseId %in% passing.ids) %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  ) %>%
  ungroup()

pureDemandFrame.long$pred  <- tn$inverse(predict(firstModelTest, level = 0))
pureDemandFrame.long$predi <- tn$inverse(predict(firstModelTest, level = 1))

plot <-
  ggplot(pureDemandFrame.long, aes(Price,
                                   pred,
                                   lty   = FamilySize)) +
  geom_line(size = 1) +
  geom_line(mapping = aes(Price, predi,
                          lty = FamilySize,
                          group = ResponseId),
            size  = 0.4,
            alpha = 0.25) +
  scale_x_continuous(breaks = c(0, 1, 10, 50, 100, 200, 400, 500, 1000, 5000, 10000),
                     labels = c(0, 1, 10, 50, 100, 200, 400, 500, 1000, 5000, 10000),
                     limits = c(50, 400),
                     trans = tn) +
  scale_y_continuous(breaks = c(0, 1, 5, 10, 20),
                     labels = c(0, 1, 5, 10, 20),
                     limits = c(0, 20),
                     trans = tn) +
  theme_bw() +
  scale_color_grey() +
  xlab("Price/Hour of Treatment") +
  ylab("Predicted Treatment Consumption") +
  theme(legend.position  = "bottom",
        strip.background = element_blank(),
        strip.text       = element_text(colour = 'black',
                                        face   = 'bold'),
        text             = element_text(family = "Times New Roman",
                                        size   = 10),
        panel.spacing    = unit(1.25, "lines"),
        axis.title.y     = element_text(margin = margin(t = 0,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0)),
        axis.title.x     = element_text(margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0))) +
  beezdemand::theme_apa() +
  facet_grid(demo_Gender ~ demo_Education) +
  labs(lty = "Family Size")

ggsave("Figure 1.png",
       plot = plot,
       units = "in",
       height = 6,
       width = 9, dpi = 600)
