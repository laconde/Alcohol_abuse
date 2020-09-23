library(dplyr)
library(parsnip)
library(stringr)
library(tidyverse)
library(ggplot2)
neds2016_CORE = rename(`NEDS_2016_CORE.sample.random.(1)`, AGE = V1,  AMONTH = V2,    AWEEKEND = V3,  DIED_VISIT = V4,        DISCWT = V5,    DISP_ED = V6,   DQTR = V7,      DXVER = V8,     
                       EDEVENT = V9,      FEMALE = V10,   HCUPFILE = V11, HOSP_ED = V12,  I10_DX1 = V13,  I10_DX2 = V14,  I10_DX3 = V15,  I10_DX4 = V16,  I10_DX5 = V17, 
                       I10_DX6 = V18, I10_DX7 = V19,      I10_DX8 = V20,  I10_DX9 = V21,  I10_DX10 = V22, I10_DX11 = V23, I10_DX12 = V24, I10_DX13 = V25, I10_DX14 = V26, I10_DX15 = V27,
                       I10_DX16 = V28, I10_DX17 = V29,    I10_DX18 = V30, I10_DX19 = V31, I10_DX20 = V32, I10_DX21 = V33, I10_DX22 = V34, I10_DX23 = V35, I10_DX24 = V36, I10_DX25 = V37, I10_DX26 = V38,
                       I10_DX27 = V39,    I10_DX28 = V40, I10_DX29 = V41, I10_DX30 = V42, I10_ECAUSE1 = V43,      I10_ECAUSE2 = V44,      I10_ECAUSE3 = V45,      I10_ECAUSE4 = V46,      I10_NDX = V47, 
                       I10_NECAUSE = V48, KEY_ED = V49,   NEDS_STRATUM = V50, PAY1 = V51, PAY2 = V52, PL_NCHS = V53, TOTCHG_ED = V54, YEAR = V55, ZIPINC_QRTL = V56)
neds2016_CORE$Alcohol_Abuse <- ifelse(neds2016_CORE$I10_DX1 == "F1010"| neds2016_CORE$I10_DX1 == "F1011"| neds2016_CORE$I10_DX1 == "F10120"| neds2016_CORE$I10_DX1 == "F10121"| neds2016_CORE$I10_DX1 == "F10129"| neds2016_CORE$I10_DX1 == "F1014"| neds2016_CORE$I10_DX1 == "F10150"| neds2016_CORE$I10_DX1 == "F10151"| neds2016_CORE$I10_DX1 == "F10159"| neds2016_CORE$I10_DX1 == "F10180"| neds2016_CORE$I10_DX1 == "F10181"| neds2016_CORE$I10_DX1 == "F10182"| neds2016_CORE$I10_DX1 == "F10188"| neds2016_CORE$I10_DX1 == "F1019",1,0)
neds2016_CORE <- neds2016_CORE %>%
  filter(ZIPINC_QRTL == 1 | ZIPINC_QRTL == 2 | ZIPINC_QRTL == 3 | ZIPINC_QRTL == 4)
neds2016_CORE$AMONTH <- ifelse(neds2016_CORE$AMONTH == -9, 9, neds2016_CORE$AMONTH)
neds2016_CORE <- neds2016_CORE %>%
  filter(FEMALE == "0" | FEMALE == "1" )
# Converting FEMALE, AGE, SAlcohol_Abuse, ZIPINC_QRTL, YEAR
neds2016_CORE$FEMALE <- as.factor(neds2016_CORE$FEMALE)
neds2016_CORE$AGE <- as.numeric(neds2016_CORE$AGE)
neds2016_CORE$Alcohol_Abuse <- as.factor(neds2016_CORE$Alcohol_Abuse)
neds2016_CORE$ZIPINC_QRTL <- as.factor(neds2016_CORE$ZIPINC_QRTL)
neds2016_CORE$YEAR <- as.factor(neds2016_CORE$YEAR)
neds2016_CORE$AMONTH <- as.factor(neds2016_CORE$AMONTH)
summary(neds2016_CORE)
neds2016_time <- neds2016_CORE %>%
  select(Alcohol_Abuse, YEAR) %>%
  filter(Alcohol_Abuse == 1)
ggplot(data = neds2016_time) +
  geom_bar(mapping = aes(x=YEAR))
neds2016_sex <- neds2016_CORE %>%
  select(Alcohol_Abuse, FEMALE) %>%
  filter(Alcohol_Abuse == 1)
summary(neds2016_sex)
ggplot(data = neds2016_sex) +
  geom_bar(mapping = aes(x=FEMALE))
neds2016_CORE$Age_type <- ifelse(neds2016_CORE$AGE <= 44,"18-44 years","45-64 years")
neds2016_CORE$Age_type <- ifelse(neds2016_CORE$AGE >= 65, "65 years or older", neds2016_CORE$Age_type)
neds2016_CORE$Age_type <- as.factor(neds2016_CORE$Age_type)
neds2016_Age_type <- neds2016_CORE %>%
  select(Alcohol_Abuse, Age_type) %>%
  filter(Alcohol_Abuse == 1)
summary(neds2016_Age_type)
ggplot(data = neds2016_Age_type) +
  geom_bar(mapping = aes(x=Age_type))
neds2016_Quartile <- neds2016_CORE %>%
  select(Alcohol_Abuse, ZIPINC_QRTL) %>%
  filter(Alcohol_Abuse == 1)
summary(neds2016_Quartile)
ggplot(data = neds2016_Quartile) +
  geom_bar(mapping = aes(x=ZIPINC_QRTL))
neds2016_AMONTH <- neds2016_CORE %>%
  select(Alcohol_Abuse, AMONTH) %>%
  filter(Alcohol_Abuse == 1)
summary(neds2016_CORE$AMONTH)
summary(neds2016_AMONTH$AMONTH)
ggplot(data = neds2016_AMONTH) +
  geom_bar(mapping = aes(x=AMONTH))
#Choosing the variables
neds2016_1 <- neds2016_CORE %>%
  select(FEMALE,ZIPINC_QRTL, Alcohol_Abuse,Age_type,AMONTH)
str(neds2016_1)
#glm model
logistic <- glm(Alcohol_Abuse ~.,data = neds2016_1, family = "binomial")
summary(logistic)
#Alcohol_Abuse=5.879135+
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null - ll.proposed)/ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
predicted.data <- data.frame(probability.of.alcohol_abuse=logistic$fitted.values,Alcohol_Abuse=neds2016_1$Alcohol_Abuse)
predicted.data <- predicted.data[order(predicted.data$probability.of.alcohol_abuse,decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.alcohol_abuse))+
  geom_point(aes(color=Alcohol_Abuse), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted probability of alcohol abuse")
ggsave("Alcohol_abuse_probabilities.pdf")

summary(neds2016_Age_type)