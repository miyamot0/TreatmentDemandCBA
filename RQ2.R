suppressPackageStartupMessages(library(beezdemand))
suppressPackageStartupMessages(library(emmeans))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(geepack))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(jtools))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(MuMIn))
suppressPackageStartupMessages(library(tidyverse))

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1 * y))*((10^(2 * y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyResults <- readRDS("offlineDataCBA.rds")

mSurveyResults.clean <- mSurveyResults %>%
  filter(nChildren > 0 & !is.na(attnCheck_2_1) & !is.na(demo_BirthYear))

colnames(mSurveyResults.clean)

crossPriceAlone <- mSurveyResults.clean %>%
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
  mutate(Prospect = "HighUR/HighIR")

crossPriceHighUR <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_crossPrice_2',
           '2_crossPrice_2',
           '3_crossPrice_2',
           '4_crossPrice_2',
           '5_crossPrice_2',
           '6_crossPrice_2',
           '7_crossPrice_2',
           '8_crossPrice_2',
           demo_Education,
           demo_Gender,
           demo_Marital,
           nChildren)) %>%
  #rename some of the columns selected, using proices as a new name
  rename(`50`= `1_crossPrice_2`,
         `100`=`2_crossPrice_2`,
         `150`=`3_crossPrice_2`,
         `200`=`4_crossPrice_2`,
         `250`=`5_crossPrice_2`,
         `300`=`6_crossPrice_2`,
         `350`=`7_crossPrice_2`,
         `400`=`8_crossPrice_2`) %>%
  mutate(Prospect = "HighUR/LowIR")

crossPriceHighIR <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_crossPrice_3',
           '2_crossPrice_3',
           '3_crossPrice_3',
           '4_crossPrice_3',
           '5_crossPrice_3',
           '6_crossPrice_3',
           '7_crossPrice_3',
           '8_crossPrice_3',
           demo_Education,
           demo_Gender,
           demo_Marital,
           nChildren)) %>%
  #rename some of the columns selected, using proices as a new name
  rename(`50`= `1_crossPrice_3`,
         `100`=`2_crossPrice_3`,
         `150`=`3_crossPrice_3`,
         `200`=`4_crossPrice_3`,
         `250`=`5_crossPrice_3`,
         `300`=`6_crossPrice_3`,
         `350`=`7_crossPrice_3`,
         `400`=`8_crossPrice_3`) %>%
  mutate(Prospect = "LowUR/HighIR")

combineSeries <- rbind(crossPriceAlone,
                       crossPriceHighUR,
                       crossPriceHighIR)

combineSeries.gather <- combineSeries %>%
  gather(Price, Consumption,
         -ResponseId,
         -demo_Education,
         -demo_Gender,
         -demo_Marital,
         -nChildren,
         -Prospect)

geeFittingFrame <- as_tibble(combineSeries.gather) %>%
  filter(Prospect != "HighUR/HighIR") %>%
  mutate(NewID = as.numeric(factor(ResponseId, levels = unique(ResponseId))),
         demo_Gender = factor(as.character(demo_Gender)),
         Prospect = factor(Prospect),
         Price = as.numeric(Price)) %>%
  select(Prospect, NewID, Price, Consumption, demo_Gender, demo_Education) %>%
  arrange(Prospect, NewID, Price, Consumption, demo_Gender, demo_Education)


fit.full <- geeglm(Consumption ~ Price * Prospect * demo_Gender,
                   id = NewID,
                   data = geeFittingFrame,
                   family = gaussian,
                   corstr = "exchangeable")

fit.1 <- geeglm(Consumption ~ Price * Prospect + demo_Gender,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.2 <- geeglm(Consumption ~ Price * Prospect,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.3 <- geeglm(Consumption ~ Price + Prospect,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

model.sel(fit.full,
          fit.1,
          fit.2,
          fit.3,
          rank = "QIC")

# Model 3 fits best

emmeaner <- emmeans(fit.3, specs = pairwise ~ Prospect | Price, type = "response",
                    at = list(Price = c(50,
                                        100,
                                        150,
                                        200,
                                        250,
                                        300,
                                        400)))

plotData <- emmip(fit.3, Prospect ~ Price,
                  CIs = TRUE,
                  plotit = FALSE,
                  at = list(Price = c(50,
                                      100,
                                      150,
                                      200,
                                      250,
                                      300,
                                      350,
                                      400)))

fits.prospect <- summary(emmeans(fit.3, ~ Price | Prospect,
                                   at = list(Price = c(50, 100,
                                                            150, 200,
                                                            250, 300,
                                                            350, 400))))

plotData$Prospect = recode(plotData$Prospect,
                           "HighUR/LowIR" = "Alternative (High UR)",
                           "LowUR/HighIR" = "Alternative (High IR)")

font_import()
loadfonts(device = "win")
fonts()

load("combinedFrameGather.RData")

plotData = plotData %>%
  mutate(Task  = "EBP + Alternatives") %>%
  rename(pred = yvar)

plotData$Task = factor(plotData$Task,
                       levels = c("EBP Alone",
                                  "EBP + Alternatives"))

plotData2 = combinedFrame.gather

plotData2$Task = ifelse(plotData2$Prospect == "Demand-Alone",
                        "EBP Alone",
                        "EBP + Alternatives")

plotData2$Task = factor(plotData2$Task,
                        levels = c("EBP Alone",
                                   "EBP + Alternatives"))

plotData2$Prospect = recode(plotData2$Prospect,
                            "Demand-Alone" = "EBP",
                            "Demand-Own"   = "EBP")

plotData$Prospect = factor(plotData$Prospect,
                            levels = c("EBP",
                                       "Alternative (High UR)",
                                       "Alternative (High IR)"))

plotData2$Prospect = factor(plotData2$Prospect,
                            levels = c("EBP",
                                       "Alternative (High UR)",
                                       "Alternative (High IR)"))

plot <- ggplot(plotData, aes(Price, pred,
                             color = Prospect,
                             group = interaction(Task, Prospect),
                             lty   = Prospect)) +
  geom_line(size = 1.25,
            color = "black") +
  geom_ribbon(data = plotData,
              aes(ymin = pred - 1.96*SE,
                  ymax = pred + 1.96*SE,
                  x = Price),
              alpha = 0.1,
              show.legend = FALSE,
              color = NA,
              fill = "black") +
  geom_line(data = plotData2,
            size = 1.25,
            color = "black") +
  # indiv lines
  geom_line(size = 0.8,
            alpha = 0.25,
            data = plotData2,
            inherit.aes = FALSE,
            color = "gray",
            mapping = aes(Price, predi,
                          group = interaction(Task, ResponseId))) +
  scale_linetype_manual(values = c("EBP" = "solid",
                                   "Alternative (High UR)" = "dashed",
                                   "Alternative (High IR)" = "dotted")) +
  scale_x_continuous(breaks = c(0, 1, 10, 50,
                                100, 200, 400,
                                500, 1000, 5000, 10000),
                     labels = c(0, 1, 10, 50,
                                100, 200, 400,
                                500, 1000, 5000, 10000),
                     limits = c(50, 400),
                     trans  = tn) +
  scale_y_continuous(breaks = c(0, 1, 5, 10, 20),
                     labels = c(0, 1, 5, 10, 20),
                     limits = c(0, 20),
                     trans  = tn) +
  scale_color_grey() +
  xlab("Price/Hour of EBPs") +
  ylab("Predicted Consumption") +
  beezdemand::theme_apa() +
  facet_grid( ~ Task) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(colour = 'black',
                                    face   = 'bold'),
    text = element_text(family = "Times New Roman", size = 10)
  )

ggsave("Figure 2.png",
       plot = plot,
       units = "in",
       height = 6,
       width = 9, dpi = 600)

