rm(list=ls())
library(dplyr)
library(tidyr)
library(lmerTest)
library(lme4)
data = read.csv(file="maine_geno_pheno_updated.csv",header=T)
data <- data %>%
  filter(!is.na(Vehicle),!is.na(Aten.15)) %>%
  mutate(CTXdif = Vehicle - Aten.15) %>%
  mutate(CTXavg = (Vehicle + Aten.15)/2)
table(data$adrb2_I_think,exclude=F)
hist(data$CTXdif)
hist(data$CTXavg)
# long <- data %>% 
#   tidyr::pivot_longer(
#   cols = c(Vehicle,Aten.15),
#   names_to = "Treatment",
#   values_to = "CTX"
# )
#t.test(long$CTX~long$Treatment,paired=T)
#lmer(CTX~Treatment + (1 | indID),data=long)
t.test(data$CTXdif)
summary(lm(FNBMDH~CTXdif,data=data))
summary(lm(FNBMDH~CTXavg,data=data))
summary(lm(FNBMDH~Vehicle,data=data))
summary(lm(FNBMDH~Aten.15,data=data))
summary(lm(FNBMDH~Vehicle,data=data))
summary(lm(Aten.15~FNBMDH,data=data))

summary(lm(FNBMDH~Aten.15,data=data))
mod = lm(data$FNBMDH~data$Aten.15)
plot(data$FNBMDH~data$Aten.15)
lines(mod$fitted.values~data$Aten.15)


summary(lm(HTOTBMDH~Aten.15,data=data))
summary(lm(STOTBMDH~Aten.15,data=data))
summary(lm(R13BMD~Aten.15,data=data))
summary(lm(RUBMD~Aten.15,data=data))
plot(data$FNBMDH~data$Vehicle)
summary(lm(Aten.15~FNBMDH,data=data))
summary(lm(Aten.15~VS1AGE,data=data))
summary(lm(Aten.15~YRSMENO,data=data))

plot(data$Vehicle,data$Aten.15)
cor.test(data$Vehicle,data$Aten.15)
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(data$Vehicle,data$Aten.15,data$FNBMDH)
summary(lm(FNBMDH~Aten.15 + Vehicle,data=data))
summary(lm(Aten.15~Vehicle + FNBMDH,data=data))
summary(lm(VS1DBPCALC~adrb2_I_think,data=data))
summary(lm(VS1SBPCALC~adrb2_I_think,data=data))
summary(lm(VS1HRCALC~adrb2_I_think,data=data))
boxplot(VS1SBPCALC~adrb2_I_think,data=data)
boxplot(VS1SBPCALC~rs1801252_a_g,data=data)
summary(lm(VS1SBPCALC~rs1801252_a_g,data=data))

