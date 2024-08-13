rm(list=ls())
setwd(":/~")

########################################################################################
########################################################################################
# This file shows the main analyses for the study of "Consumers Modulate Effects of Plant Diversity on Community Stability".
# According to the manuscripts, the code for figures 2-3, S2-S6, tables S1-S3, 
# as well as the structural equation models (figures 4 & S6, tables S4-s7) are provided.
########################################################################################
########################################################################################

# loading packages
library(nlme)
library(MuMIn)
library(multcomp)
library(ggplot2)
library(cowplot)

my_theme<- theme(legend.position = "none",
                 strip.text = element_text(size = 14, family = "Arial", color = "black"),
                 axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
                 axis.text.y = element_text(size = 18, family = "Arial", color = "black"),
                 axis.title.x = element_text(size = 20, family = "Arial", color = "black"),
                 axis.title.y = element_text(size = 20, family = "Arial", color = "black"),
                 axis.ticks.length = unit(1, "mm"),
                 axis.line = element_line(color = "black", size = 0.5),
                 plot.tag = element_text(size = 24, family = "Arial", color = "black"))

# read the data
Consumers_div_sta.data <- read.csv('Consumers_div_sta.csv',header=T)
variable.names(Consumers_div_sta.data)

Consumers_div_sta.data$vertical_complexity <- factor(Consumers_div_sta.data$vertical_complexity,levels=c("Low vertical complexity",
                                                                                   "Moderate vertical complexity",
                                                                                   "High vertical complexity"))
#Consumers_div_sta.data$horizontal_complexity <- factor(Consumers_div_sta.data$horizontal_complexity,levels=c("1", "4", "16"))
low_vertical_complex.data <- Consumers_div_sta.data[grep("Low vertical complexity", Consumers_div_sta.data$vertical_complexity),]
mode_vertical_complex.data <- Consumers_div_sta.data[grep("Moderate vertical complexity", Consumers_div_sta.data$vertical_complexity),]
high_vertical_complex.data <- Consumers_div_sta.data[grep("High vertical complexity", Consumers_div_sta.data$vertical_complexity),]

#####################################################################################
##################################### Figure 1 ######################################
#####################################################################################
# Community stability
com_sta_lme.fit <- lme(com_sta ~ horizontal_complexity*vertical_complexity, random = ~1|PlotID/Trts_consumers_removal, data = Consumers_div_sta.data, na.action = na.omit)
summary(com_sta_lme.fit)
anova(com_sta_lme.fit)
r.squaredGLMM(com_sta_lme.fit)

# Assess normality of residuals
hist(com_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(com_sta_lme.fit, which=1:4)
plot(com_sta_lme.fit$fitted, com_sta_lme.fit$residuals)
#plot(full_model_one.fit, which=1:4)
qqnorm(resid(com_sta_lme.fit))
qqline(resid(com_sta_lme.fit))


com_sta.plot <- ggplot() +
  geom_point(mapping = aes(x = log2(horizontal_complexity), y = com_sta, color = vertical_complexity), 
             data = Consumers_div_sta.data, 
             size = 3.5, position = position_dodge(0.5), 
             shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = log2(horizontal_complexity), y = com_sta, color = vertical_complexity), 
              data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, size = 1.5, alpha = 1, se=F) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_x_continuous(breaks = c(0, 2, 4), limits = c(-1, 5),labels = c("1","4","16")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab(bquote(atop(paste("Horizontal complexity"), NULL["(Log"[2 ]]["(no. of plant species))"]))) +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Species stability
spe_sta_lme.fit <- lme(spe_sta ~ horizontal_complexity*vertical_complexity, random = ~1|PlotID/Trts_consumers_removal, data = Consumers_div_sta.data, na.action = na.omit)
summary(spe_sta_lme.fit)
anova(spe_sta_lme.fit)
r.squaredGLMM(spe_sta_lme.fit)

# Assess normality of residuals
hist(spe_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_sta_lme.fit, which=1:4)
plot(spe_sta_lme.fit$fitted, spe_sta_lme.fit$residuals)
#plot(full_model_one.fit, which=1:4)
qqnorm(resid(spe_sta_lme.fit))
qqline(resid(spe_sta_lme.fit))

pop_sta.plot <- ggplot() +
  geom_point(mapping = aes(x = log2(horizontal_complexity), y = spe_sta, color = vertical_complexity), 
             data = Consumers_div_sta.data, 
             size = 3.5, position = position_dodge(0.5), 
             shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = log2(horizontal_complexity), y = spe_sta, color = vertical_complexity), 
              data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, size = 1.5, alpha = 1, se=F) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_x_continuous(breaks = c(0, 2, 4), limits = c(-1, 5),labels = c("1","4","16")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x), limits = c(0.3, 2)) +
  ylab("Species stabilty") +
  xlab(bquote(atop(paste("Horizontal complexity"), NULL["(Log"[2 ]]["(no. of plant species))"]))) +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Species asynchrony
spe_asy_lme.fit <- lme(spe_asy ~ horizontal_complexity*vertical_complexity, random = ~1|PlotID/Trts_consumers_removal, data = Consumers_div_sta.data, na.action = na.omit)
summary(spe_asy_lme.fit)
anova(spe_asy_lme.fit)
r.squaredGLMM(spe_asy_lme.fit)

# Assess normality of residuals
hist(spe_asy_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_asy_lme.fit, which=1:4)
plot(spe_asy_lme.fit$fitted, spe_asy_lme.fit$residuals)
#plot(full_model_one.fit, which=1:4)
qqnorm(resid(spe_asy_lme.fit))
qqline(resid(spe_asy_lme.fit))

pop_asy.plot <- ggplot() +
  geom_point(mapping = aes(x = log2(horizontal_complexity), y = spe_asy, color = vertical_complexity), 
             data = Consumers_div_sta.data, 
             size = 3.5, position = position_dodge(0.5), 
             shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = log2(horizontal_complexity), y = spe_asy, color = vertical_complexity), 
              data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 1, se=F) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_x_continuous(breaks = c(0, 2, 4), limits = c(-1, 5),labels = c("1","4","16")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab(bquote(atop(paste("Species asynchrony"), NULL[("Community stabilty/Species stabilty")]))) +
  xlab(bquote(atop(paste("Horizontal complexity"), NULL["(Log"[2 ]]["(no. of plant species))"]))) +
  labs(tag = "C") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.2 <- plot_grid(com_sta.plot, pop_sta.plot, pop_asy.plot, ncol = 3, align= "hv") 


#####################################################################################
##################################### Figure 3 ######################################
#####################################################################################
# com_sta ~ inverse_Simpson
inverse_Simpson_com_sta_lme.fit <- lme(com_sta ~ inverse_Simpson*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                               data = Consumers_div_sta.data, na.action = na.omit)
summary(inverse_Simpson_com_sta_lme.fit)
anova(inverse_Simpson_com_sta_lme.fit)
r.squaredGLMM(inverse_Simpson_com_sta_lme.fit)

# Assess normality of residuals
hist(inverse_Simpson_com_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson_com_sta_lme.fit, which=1:4)
plot(inverse_Simpson_com_sta_lme.fit$fitted, inverse_Simpson_com_sta_lme.fit$residuals)
#plot(inverse_Simpson_com_sta_lme.fit, which=1:4)
qqnorm(resid(inverse_Simpson_com_sta_lme.fit))
qqline(resid(inverse_Simpson_com_sta_lme.fit))

summary(lm(com_sta ~ inverse_Simpson, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ inverse_Simpson, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ inverse_Simpson, data = high_vertical_complex.data, na.action = na.omit))

com_sta_inverse_Simpson.plot <- ggplot() +
  geom_point(mapping = aes(x = inverse_Simpson, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = inverse_Simpson, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab(bquote(atop(paste(italic("ENS"["PIE"])), NULL[("1/Simpson index")]))) +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# spe_sta ~ inverse_Simpson
inverse_Simpson_spe_sta_lme.fit <- lme(spe_sta ~ inverse_Simpson*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                               data = Consumers_div_sta.data, na.action = na.omit)
summary(inverse_Simpson_spe_sta_lme.fit)
anova(inverse_Simpson_spe_sta_lme.fit)
r.squaredGLMM(inverse_Simpson_spe_sta_lme.fit)

# Assess normality of residuals
hist(inverse_Simpson_spe_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson_spe_sta_lme.fit, which=1:4)
plot(inverse_Simpson_spe_sta_lme.fit$fitted, inverse_Simpson_spe_sta_lme.fit$residuals)
#plot(inverse_Simpson_spe_sta_lme.fit, which=1:4)
qqnorm(resid(inverse_Simpson_spe_sta_lme.fit))
qqline(resid(inverse_Simpson_spe_sta_lme.fit))

summary(lm(spe_sta ~ inverse_Simpson, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(spe_sta ~ inverse_Simpson, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(spe_sta ~ inverse_Simpson, data = high_vertical_complex.data, na.action = na.omit))

spe_sta_inverse_Simpson.plot <- ggplot() +
  geom_point(mapping = aes(x = inverse_Simpson, y = spe_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = inverse_Simpson, y = spe_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Species stabilty") +
  xlab(bquote(atop(paste(italic("ENS"["PIE"])), NULL[("1/Simpson index")]))) +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# spe_asy ~ inverse_Simpson
inverse_Simpson_spe_asy_lme.fit <- lme(spe_asy ~ inverse_Simpson*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                               data = Consumers_div_sta.data, na.action = na.omit)
summary(inverse_Simpson_spe_asy_lme.fit)
anova(inverse_Simpson_spe_asy_lme.fit)
r.squaredGLMM(inverse_Simpson_spe_asy_lme.fit)

# Assess normality of residuals
hist(inverse_Simpson_spe_asy_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson_spe_asy_lme.fit, which=1:4)
plot(inverse_Simpson_spe_asy_lme.fit$fitted, inverse_Simpson_spe_asy_lme.fit$residuals)
#plot(inverse_Simpson_spe_asy_lme.fit, which=1:4)
qqnorm(resid(inverse_Simpson_spe_asy_lme.fit))
qqline(resid(inverse_Simpson_spe_asy_lme.fit))

summary(lm(spe_asy ~ inverse_Simpson, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(spe_asy ~ inverse_Simpson, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(spe_asy ~ inverse_Simpson, data = high_vertical_complex.data, na.action = na.omit))

spe_asy_inverse_Simpson.plot <- ggplot() +
  geom_point(mapping = aes(x = inverse_Simpson, y = spe_asy, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = inverse_Simpson, y = spe_asy, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Species asynchrony") +
  xlab(bquote(atop(paste(italic("ENS"["PIE"])), NULL[("1/Simpson index")]))) +
  labs(tag = "C") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.3 <- plot_grid(com_sta_inverse_Simpson.plot, spe_sta_inverse_Simpson.plot, spe_asy_inverse_Simpson.plot, ncol = 3, align= "hv") 


#####################################################################################
##################################### Figure S2 #####################################
#####################################################################################
Consumers_div_pro.data <- read.csv('Consumers_div_pro.csv',header=T)
variable.names(Consumers_div_pro.data)

Consumers_div_pro.data$vertical_complexity <- factor(Consumers_div_pro.data$vertical_complexity,levels=c("Low vertical complexity",
                                                                                                         "Moderate vertical complexity",
                                                                                                         "High vertical complexity"))
#Consumers_div_pro.data$horizontal_complexity <- factor(Consumers_div_pro.data$horizontal_complexity,levels=c("1", "4", "16"))

figure.S2 <- ggplot() +
  geom_point(mapping = aes(x = Year, y = log(total_biomass_planted+1), color = vertical_complexity), 
             data = Consumers_div_pro.data, 
             size = 2, position = position_dodge(0.5), 
             shape = 16, alpha = 0.3) +
  stat_smooth(mapping = aes(x = Year, y = log(total_biomass_planted+1), color = vertical_complexity), 
              data = Consumers_div_pro.data, method = 'loess', formula = y ~ x, size = 1.5, alpha = 0.5) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  facet_grid(.~horizontal_complexity) +
  scale_x_continuous(breaks = seq(2009, 2021, by = 2), limits = c(2008, 2022)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab(bquote(atop(paste("Plant aboveground biomass"), NULL[(log~(~g~"."~m^-2*" +1"))]))) +
  xlab("Year") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

#####################################################################################
##################################### Table S1 ######################################
#####################################################################################
# The Effective Number of Species (inverse_Simpson)
inverse_Simpson.lme <- lme(inverse_Simpson ~ horizontal_complexity*vertical_complexity*Year, random=list(~1|PlotID), 
                           data = Consumers_div_pro.data, na.action = na.omit)
summary(inverse_Simpson.lme)
anova(inverse_Simpson.lme)
r.squaredGLMM(inverse_Simpson.lme)
# Assess normality of residuals
hist(inverse_Simpson.lme$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson.lme, which=1:4)
plot(inverse_Simpson.lme$fitted, inverse_Simpson.lme$residuals)
#plot(inverse_Simpson.lme, which=1:4)
qqnorm(resid(inverse_Simpson.lme))
qqline(resid(inverse_Simpson.lme))


# plant evenness (inverse_Simpson/plant_richness)
plant_evenness.lme <- lme(plant_evenness ~ horizontal_complexity*vertical_complexity*Year, random=list(~1|PlotID), 
                        data = Consumers_div_pro.data, na.action = na.omit)
summary(plant_evenness.lme)
anova(plant_evenness.lme)
r.squaredGLMM(plant_evenness.lme)
# Assess normality of residuals
hist(plant_evenness.lme$residuals)
# Inspect the model diagnostic metrics
plot(plant_evenness.lme, which=1:4)
plot(plant_evenness.lme$fitted, plant_evenness.lme$residuals)
#plot(plant_evenness.lme, which=1:4)
qqnorm(resid(plant_evenness.lme))
qqline(resid(plant_evenness.lme))


# The Effective Number of Species (Shannon index) 
shannon.lme <- lme(shannon ~ horizontal_complexity*vertical_complexity*Year, random=list(~1|PlotID), 
                   data = Consumers_div_pro.data, na.action = na.omit)
summary(shannon.lme)
anova(shannon.lme)
r.squaredGLMM(shannon.lme)
# Assess normality of residuals
hist(shannon.lme$residuals)
# Inspect the model diagnostic metrics
plot(shannon.lme, which=1:4)
plot(shannon.lme$fitted, shannon.lme$residuals)
#plot(shannon.lme, which=1:4)
qqnorm(resid(shannon.lme))
qqline(resid(shannon.lme))


# planted species biomass
biomass.lme <- lme(total_biomass_planted ~ horizontal_complexity*vertical_complexity*Year, random=list(~1|PlotID), 
                   data = Consumers_div_pro.data, na.action = na.omit)
summary(biomass.lme)
anova(biomass.lme)
r.squaredGLMM(biomass.lme)
# Assess normality of residuals
hist(biomass.lme$residuals)
# Inspect the model diagnostic metrics
plot(biomass.lme, which=1:4)
plot(biomass.lme$fitted, biomass.lme$residuals)
#plot(biomass.lme, which=1:4)
qqnorm(resid(biomass.lme))
qqline(resid(biomass.lme))



#####################################################################################
##################################### Figure S3 #####################################
#####################################################################################
# Temporal mean of biomass
tmp_mean_lme.fit <- lme(tmp_mean ~ horizontal_complexity*vertical_complexity, random = ~1|PlotID/Trts_consumers_removal, data = Consumers_div_sta.data, na.action = na.omit)
summary(tmp_mean_lme.fit)
anova(tmp_mean_lme.fit)
r.squaredGLMM(tmp_mean_lme.fit)

# Assess normality of residuals
hist(tmp_mean_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(tmp_mean_lme.fit, which=1:4)
plot(tmp_mean_lme.fit$fitted, tmp_mean_lme.fit$residuals)
#plot(full_model_one.fit, which=1:4)
qqnorm(resid(tmp_mean_lme.fit))
qqline(resid(tmp_mean_lme.fit))

pro_mean.plot <- ggplot() +
  geom_point(mapping = aes(x = log2(horizontal_complexity), y = tmp_mean, color = vertical_complexity), 
             data = Consumers_div_sta.data, 
             size = 3.5, position = position_dodge(0.5), 
             shape = 16, alpha = 0.3) +
  stat_smooth(mapping = aes(x = log2(horizontal_complexity), y = tmp_mean, color = vertical_complexity), 
              data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, size = 1.5, alpha = 1, se=F) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_x_continuous(breaks = c(0, 2, 4), limits = c(-1, 5),labels = c("1","4","16")) +
  ylab(bquote(paste("Temporal mean of biomass ( "*g~"."~m^-2*")"))) +
  xlab(bquote(atop(paste("Horizontal complexity"), NULL["(Log"[2 ]]["(no. of plant species))"]))) +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Temporal SD of biomass
tmp_sd_lme.fit <- lme(tmp_sd ~ horizontal_complexity*vertical_complexity, random = ~1|PlotID/Trts_consumers_removal, data = Consumers_div_sta.data, na.action = na.omit)
summary(tmp_sd_lme.fit)
anova(tmp_sd_lme.fit)
r.squaredGLMM(tmp_sd_lme.fit)

# Assess normality of residuals
hist(tmp_sd_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(tmp_sd_lme.fit, which=1:4)
plot(tmp_sd_lme.fit$fitted, tmp_sd_lme.fit$residuals)
#plot(full_model_one.fit, which=1:4)
qqnorm(resid(tmp_sd_lme.fit))
qqline(resid(tmp_sd_lme.fit))

pro_SD.plot <- ggplot() +
  geom_point(mapping = aes(x = log2(horizontal_complexity), y = tmp_sd, color = vertical_complexity), 
             data = Consumers_div_sta.data, 
             size = 3.5, position = position_dodge(0.5), 
             shape = 16, alpha = 0.3) +
  stat_smooth(mapping = aes(x = log2(horizontal_complexity), y = tmp_sd, color = vertical_complexity), 
              data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, size = 1.5, alpha = 1, se=F) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_x_continuous(breaks = c(0, 2, 4), limits = c(-1, 5),labels = c("1","4","16")) +
  ylab(bquote(paste("Temporal SD of biomass ( "*g~"."~m^-2*")"))) +
  xlab(bquote(atop(paste("Horizontal complexity"), NULL["(Log"[2 ]]["(no. of plant species))"]))) +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.S3 <- plot_grid(pro_mean.plot, pro_SD.plot, nrow = 1, align= "hv")

#####################################################################################
##################################### Figure S4 #####################################
#####################################################################################
low_vertical_complex.data <- Consumers_div_sta.data[grep("Low vertical complexity", Consumers_div_sta.data$vertical_complexity),]
mode_vertical_complex.data <- Consumers_div_sta.data[grep("Moderate vertical complexity", Consumers_div_sta.data$vertical_complexity),]
high_vertical_complex.data <- Consumers_div_sta.data[grep("High vertical complexity", Consumers_div_sta.data$vertical_complexity),]

# com_sta ~ spe_asy
summary(lm(com_sta ~ spe_asy, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ spe_asy, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ spe_asy, data = high_vertical_complex.data, na.action = na.omit))

com_sta_spe_asy.plot <- ggplot() +
  geom_point(mapping = aes(x = spe_asy, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = spe_asy, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab("Species asynchrony") +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# com_sta ~ spe_sta
summary(lm(com_sta ~ spe_sta, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ spe_sta, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ spe_sta, data = high_vertical_complex.data, na.action = na.omit))

com_sta_spe_sta.plot <- ggplot() +
  geom_point(mapping = aes(x = spe_sta, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = spe_sta, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab("Species stabilty") +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# com_sta ~ temp_mean
summary(lm(com_sta ~ tmp_mean, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ tmp_mean, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ tmp_mean, data = high_vertical_complex.data, na.action = na.omit))

com_sta_temp_mean.plot <- ggplot() +
  geom_point(mapping = aes(x = tmp_mean, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = tmp_mean, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  #scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab("Temporal mean of biomass") +
  labs(tag = "C") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# com_sta ~ temp_sd
summary(lm(com_sta ~ tmp_sd, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ tmp_sd, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ tmp_sd, data = high_vertical_complex.data, na.action = na.omit))

com_sta_temp_sd.plot <- ggplot() +
  geom_point(mapping = aes(x = tmp_sd, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = tmp_sd, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  #scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab("Temporal SD of biomass") +
  labs(tag = "D") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.S4 <- plot_grid(com_sta_spe_asy.plot, com_sta_spe_sta.plot, 
                       com_sta_temp_mean.plot, com_sta_temp_sd.plot, ncol = 2, align= "hv") 

#####################################################################################
##################################### Figure S5 #####################################
#####################################################################################
# tmp_mean ~ inverse_Simpson
inverse_Simpson_tmp_mean_lme.fit <- lme(tmp_mean ~ inverse_Simpson*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                data = Consumers_div_sta.data, na.action = na.omit)
summary(inverse_Simpson_tmp_mean_lme.fit)
anova(inverse_Simpson_tmp_mean_lme.fit)
r.squaredGLMM(inverse_Simpson_tmp_mean_lme.fit)

# Assess normality of residuals
hist(inverse_Simpson_tmp_mean_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson_tmp_mean_lme.fit, which=1:4)
plot(inverse_Simpson_tmp_mean_lme.fit$fitted, inverse_Simpson_tmp_mean_lme.fit$residuals)
#plot(inverse_Simpson_tmp_mean_lme.fit, which=1:4)
qqnorm(resid(inverse_Simpson_tmp_mean_lme.fit))
qqline(resid(inverse_Simpson_tmp_mean_lme.fit))

summary(lm(tmp_mean ~ inverse_Simpson, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_mean ~ inverse_Simpson, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_mean ~ inverse_Simpson, data = high_vertical_complex.data, na.action = na.omit))

tmp_mean_inverse_Simpson.plot <- ggplot() +
  geom_point(mapping = aes(x = inverse_Simpson, y = tmp_mean, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = inverse_Simpson, y = tmp_mean, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Temporal mean of biomass") +
  xlab(bquote(atop(paste(italic("ENS"["PIE"])), NULL[("1/Simpson index")]))) +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# tmp_sd ~ inverse_Simpson
inverse_Simpson_tmp_sd_lme.fit <- lme(tmp_sd ~ inverse_Simpson*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                              data = Consumers_div_sta.data, na.action = na.omit)
summary(inverse_Simpson_tmp_sd_lme.fit)
anova(inverse_Simpson_tmp_sd_lme.fit)
r.squaredGLMM(inverse_Simpson_tmp_sd_lme.fit)

# Assess normality of residuals
hist(inverse_Simpson_tmp_sd_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(inverse_Simpson_tmp_sd_lme.fit, which=1:4)
plot(inverse_Simpson_tmp_sd_lme.fit$fitted, inverse_Simpson_tmp_sd_lme.fit$residuals)
#plot(inverse_Simpson_tmp_sd_lme.fit, which=1:4)
qqnorm(resid(inverse_Simpson_tmp_sd_lme.fit))
qqline(resid(inverse_Simpson_tmp_sd_lme.fit))

summary(lm(tmp_sd ~ inverse_Simpson, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_sd ~ inverse_Simpson, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_sd ~ inverse_Simpson, data = high_vertical_complex.data, na.action = na.omit))

tmp_sd_inverse_Simpson.plot <- ggplot() +
  geom_point(mapping = aes(x = inverse_Simpson, y = tmp_sd, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = inverse_Simpson, y = tmp_sd, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Temporal SD of biomass") +
  xlab(bquote(atop(paste(italic("ENS"["PIE"])), NULL[("1/Simpson index")]))) +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# tmp_mean ~ div_shannon
div_shannon_tmp_mean_lme.fit <- lme(tmp_mean ~ div_shannon*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                    data = Consumers_div_sta.data, na.action = na.omit)
summary(div_shannon_tmp_mean_lme.fit)
anova(div_shannon_tmp_mean_lme.fit)
r.squaredGLMM(div_shannon_tmp_mean_lme.fit)

# Assess normality of residuals
hist(div_shannon_tmp_mean_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(div_shannon_tmp_mean_lme.fit, which=1:4)
plot(div_shannon_tmp_mean_lme.fit$fitted, div_shannon_tmp_mean_lme.fit$residuals)
#plot(div_shannon_tmp_mean_lme.fit, which=1:4)
qqnorm(resid(div_shannon_tmp_mean_lme.fit))
qqline(resid(div_shannon_tmp_mean_lme.fit))

summary(lm(tmp_mean ~ div_shannon, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_mean ~ div_shannon, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_mean ~ div_shannon, data = high_vertical_complex.data, na.action = na.omit))

tmp_mean_div_shannon.plot <- ggplot() +
  geom_point(mapping = aes(x = div_shannon, y = tmp_mean, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = div_shannon, y = tmp_mean, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Temporal mean of biomass") +
  xlab("Shannon H′") +
  labs(tag = "C") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# tmp_sd ~ div_shannon
div_shannon_tmp_sd_lme.fit <- lme(tmp_sd ~ div_shannon*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                  data = Consumers_div_sta.data, na.action = na.omit)
summary(div_shannon_tmp_sd_lme.fit)
anova(div_shannon_tmp_sd_lme.fit)
r.squaredGLMM(div_shannon_tmp_sd_lme.fit)

# Assess normality of residuals
hist(div_shannon_tmp_sd_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(div_shannon_tmp_sd_lme.fit, which=1:4)
plot(div_shannon_tmp_sd_lme.fit$fitted, div_shannon_tmp_sd_lme.fit$residuals)
#plot(div_shannon_tmp_sd_lme.fit, which=1:4)
qqnorm(resid(div_shannon_tmp_sd_lme.fit))
qqline(resid(div_shannon_tmp_sd_lme.fit))

summary(lm(tmp_sd ~ div_shannon, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_sd ~ div_shannon, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(tmp_sd ~ div_shannon, data = high_vertical_complex.data, na.action = na.omit))

tmp_sd_div_shannon.plot <- ggplot() +
  geom_point(mapping = aes(x = div_shannon, y = tmp_sd, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = div_shannon, y = tmp_sd, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Temporal SD of biomass") +
  xlab("Shannon H′") +
  labs(tag = "D") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.S5 <- plot_grid(tmp_mean_inverse_Simpson.plot, tmp_sd_inverse_Simpson.plot, 
                       tmp_mean_div_shannon.plot, tmp_sd_div_shannon.plot, ncol = 2, align= "hv") 

#####################################################################################
##################################### Figure S6 #####################################
#####################################################################################
# com_sta ~ div_shannon
div_shannon_com_sta_lme.fit <- lme(com_sta ~ div_shannon*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                   data = Consumers_div_sta.data, na.action = na.omit)
summary(div_shannon_com_sta_lme.fit)
anova(div_shannon_com_sta_lme.fit)
r.squaredGLMM(div_shannon_com_sta_lme.fit)

# Assess normality of residuals
hist(div_shannon_com_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(div_shannon_com_sta_lme.fit, which=1:4)
plot(div_shannon_com_sta_lme.fit$fitted, div_shannon_com_sta_lme.fit$residuals)
#plot(div_shannon_com_sta_lme.fit, which=1:4)
qqnorm(resid(div_shannon_com_sta_lme.fit))
qqline(resid(div_shannon_com_sta_lme.fit))

summary(lm(com_sta ~ div_shannon, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ div_shannon, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(com_sta ~ div_shannon, data = high_vertical_complex.data, na.action = na.omit))

com_sta_div_shannon.plot <- ggplot() +
  geom_point(mapping = aes(x = div_shannon, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = div_shannon, y = com_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Community stabilty") +
  xlab("Shannon H′") +
  labs(tag = "A") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# spe_sta ~ div_shannon
div_shannon_spe_sta_lme.fit <- lme(spe_sta ~ div_shannon*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                   data = Consumers_div_sta.data, na.action = na.omit)
summary(div_shannon_spe_sta_lme.fit)
anova(div_shannon_spe_sta_lme.fit)
r.squaredGLMM(div_shannon_spe_sta_lme.fit)

# Assess normality of residuals
hist(div_shannon_spe_sta_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(div_shannon_spe_sta_lme.fit, which=1:4)
plot(div_shannon_spe_sta_lme.fit$fitted, div_shannon_spe_sta_lme.fit$residuals)
#plot(div_shannon_spe_sta_lme.fit, which=1:4)
qqnorm(resid(div_shannon_spe_sta_lme.fit))
qqline(resid(div_shannon_spe_sta_lme.fit))

summary(lm(spe_sta ~ div_shannon, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(spe_sta ~ div_shannon, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(spe_sta ~ div_shannon, data = high_vertical_complex.data, na.action = na.omit))

spe_sta_div_shannon.plot <- ggplot() +
  geom_point(mapping = aes(x = div_shannon, y = spe_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = div_shannon, y = spe_sta, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Species stabilty") +
  xlab("Shannon H′") +
  labs(tag = "B") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# spe_asy ~ div_shannon
div_shannon_spe_asy_lme.fit <- lme(spe_asy ~ div_shannon*vertical_complexity + vertical_complexity -1, random = ~1|PlotID/Trts_consumers_removal, 
                                   data = Consumers_div_sta.data, na.action = na.omit)
summary(div_shannon_spe_asy_lme.fit)
anova(div_shannon_spe_asy_lme.fit)
r.squaredGLMM(div_shannon_spe_asy_lme.fit)

# Assess normality of residuals
hist(div_shannon_spe_asy_lme.fit$residuals)
# Inspect the model diagnostic metrics
plot(div_shannon_spe_asy_lme.fit, which=1:4)
plot(div_shannon_spe_asy_lme.fit$fitted, div_shannon_spe_asy_lme.fit$residuals)
#plot(div_shannon_spe_asy_lme.fit, which=1:4)
qqnorm(resid(div_shannon_spe_asy_lme.fit))
qqline(resid(div_shannon_spe_asy_lme.fit))

summary(lm(spe_asy ~ div_shannon, data = low_vertical_complex.data, na.action = na.omit))
summary(lm(spe_asy ~ div_shannon, data = mode_vertical_complex.data, na.action = na.omit))
summary(lm(spe_asy ~ div_shannon, data = high_vertical_complex.data, na.action = na.omit))

spe_asy_div_shannon.plot <- ggplot() +
  geom_point(mapping = aes(x = div_shannon, y = spe_asy, color = vertical_complexity), data = Consumers_div_sta.data, 
             size = 3.5, shape = 16, alpha = 0.5) +
  stat_smooth(mapping = aes(x = div_shannon, y = spe_asy, color = vertical_complexity), data = Consumers_div_sta.data, 
              method = 'lm', formula = y ~ x, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values=c('#009E73','#0072B2','#D55E00')) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Species asynchrony") +
  xlab("Shannon H′") +
  labs(tag = "C") +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  my_theme

# Saving figures
figure.S5 <- plot_grid(com_sta_div_shannon.plot, spe_sta_div_shannon.plot, spe_asy_div_shannon.plot, ncol = 3, align= "hv") 

#####################################################################################
######################### SEMs (figures 4 & S6, tables S4-S7) #######################
#####################################################################################
# According to the hypothesized structural equation modeling (Fig. S1), we have 
# accounted for all reasonable pathways in our SEMs. This approach provided comparable 
# path coefficients among the SEMs, allowing us to compare the differences.

# Next, we constructed the same pathways and fitted them as a multigroup SEM to 
# statistically test the differences among these pathways.


library(piecewiseSEM)
# The Effective Number of Species (inverse_Simpson)
sem_low_simp.fit <- psem(
  lme(inverse_Simpson ~ horizontal_complexity, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = low_vertical_complex.data, na.action = na.omit),
  data = low_vertical_complex.data)

# To evaluate the model
fisherC(sem_low_simp.fit)
summary(sem_low_simp.fit)
AIC(sem_low_simp.fit)
dSep(sem_low_simp.fit)


sem_mode_simp.fit <- psem(
  lme(inverse_Simpson ~ horizontal_complexity, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = mode_vertical_complex.data, na.action = na.omit),
  data = mode_vertical_complex.data)

# To evaluate the model
fisherC(sem_mode_simp.fit)
summary(sem_mode_simp.fit)
AIC(sem_mode_simp.fit)
dSep(sem_mode_simp.fit)


sem_high_simp.fit <- psem(
  lme(inverse_Simpson ~ horizontal_complexity, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + inverse_Simpson, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = high_vertical_complex.data, na.action = na.omit),
  data = high_vertical_complex.data)

# To evaluate the model
fisherC(sem_high_simp.fit)
summary(sem_high_simp.fit)
AIC(sem_high_simp.fit)
dSep(sem_high_simp.fit)


# The Effective Number of Species (Shannon index) 
sem_low_shannon.fit <- psem(
  lme(shannon ~ horizontal_complexity, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + shannon, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + shannon, random = ~1|PlotID, data = low_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = low_vertical_complex.data, na.action = na.omit),
  data = low_vertical_complex.data)

# To evaluate the model
fisherC(sem_low_shannon.fit)
summary(sem_low_shannon.fit)
AIC(sem_low_shannon.fit)
dSep(sem_low_shannon.fit)

sem_mode_shannon.fit <- psem(
  lme(shannon ~ horizontal_complexity, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + shannon, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + shannon, random = ~1|PlotID/Trts_consumers_removal, data = mode_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = mode_vertical_complex.data, na.action = na.omit),
  data = mode_vertical_complex.data)

# To evaluate the model
fisherC(sem_mode_shannon.fit)
summary(sem_mode_shannon.fit)
AIC(sem_mode_shannon.fit)
dSep(sem_mode_shannon.fit)

sem_high_shannon.fit <- psem(
  lme(shannon ~ horizontal_complexity, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lme(spe_sta ~ horizontal_complexity + shannon, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lme(spe_asy ~ horizontal_complexity + shannon, random = ~1|PlotID, data = high_vertical_complex.data, na.action = na.omit),
  lm(com_sta ~ spe_sta + spe_asy, data = high_vertical_complex.data, na.action = na.omit),
  data = high_vertical_complex.data)

# To evaluate the model
fisherC(sem_high_shannon.fit)
summary(sem_high_shannon.fit)
AIC(sem_high_shannon.fit)
dSep(sem_high_shannon.fit)



#####################################################################################
#################################### multigroup SEM #################################
#####################################################################################

library(lavaan)
# This code is to analyze the effects of plant diversity (horizontal complexity) on 
# plant community stability across three levels of vertical complexity in the enemy 
# removal experiment nested in bigbio (e244) at Cedar Creek Ecosystem Science Reserve 
# with multigroup SEM in lavaan. Project PI: Maowei Liang. The data used for the SEM, 
# has been assembled by Maowei Liang. For this start with a fully unconstrained model. 
# This means, all paths can vary between the different levels of vertical complexity. 
# Then check path by path, whether it can be constrained (forced to be identical) 
# across different vertical levels without making the model fit the data less well 
# than in the fully unconstrained model.


## Check paths for constraining
Consumers_div_sta_scaled.data <- Consumers_div_sta.data[,c("com_sta", "spe_sta", "spe_asy", "horizontal_complexity", "inverse_Simpson", "shannon", "vertical_complexity")]
Consumers_div_sta_scaled.data$com_sta <- scale(Consumers_div_sta_scaled.data$com_sta)
Consumers_div_sta_scaled.data$spe_sta <- scale(Consumers_div_sta_scaled.data$spe_sta)
Consumers_div_sta_scaled.data$spe_asy <- scale(Consumers_div_sta_scaled.data$spe_asy)
Consumers_div_sta_scaled.data$horizontal_complexity <- scale(Consumers_div_sta_scaled.data$horizontal_complexity)
Consumers_div_sta_scaled.data$inverse_Simpson <- scale(Consumers_div_sta_scaled.data$inverse_Simpson)
Consumers_div_sta_scaled.data$shannon <- scale(Consumers_div_sta_scaled.data$shannon)

# inverse_Simpson model
mod_constrained_simp <- '
 com_sta ~ c("a1", "a1", "a1") * spe_sta  + c("b1", "b2", "b2") * spe_asy  + c("g1", "g1", "g1") * inverse_Simpson
 spe_sta ~ c("c1", "c1", "c1") * horizontal_complexity + c("e1", "e1", "e1") * inverse_Simpson
 spe_asy ~ c("d1", "d1", "d1") * horizontal_complexity + c("f1", "f2", "f2") * inverse_Simpson
 inverse_Simpson ~ c("h1", "h1", "h1") * horizontal_complexity

 indirect_via_spe_sta             := a1 * c1
 indirect_via_simp_spe_sta        := a1 * e1 * h1
 indirect_via_spe_all             := a1 * c1 + a1 * e1 * h1

 indirect_via_simp                := h1 * g1

 indirect_via_spe_asy_high        := b1 * d1
 indirect_via_simp_spe_asy_high   := b1 * f1 * h1
 indirect_via_spe_all_high        := b1 * d1 + b1 * f1 * h1

 indirect_via_spe_asy_medlow      := b2 * d1
 indirect_via_simp_spe_asy_medlow := b2 * f2 * h1
 indirect_via_spe_all_medlow      := b2 * d1 + b2 * f2 * h1'

mod_constrained_simp.fit <- sem(mod_constrained_simp, data = Consumers_div_sta_scaled.data, group = "vertical_complexity")
fitMeasures(mod_constrained_simp.fit, c("df", "AIC", "pvalue", "RMSEA", "CFI", "SRMR"))
summary(mod_constrained_simp.fit, rsquare=TRUE) 

# Shannon model
mod_constrained_shan <- '
 com_sta ~ c("a1", "a1", "a1") * spe_sta  + c("b1", "b2", "b2") * spe_asy  + c("g1", "g2", "g2") * shannon
 spe_sta ~ c("c1", "c1", "c1") * horizontal_complexity + c("e1", "e1", "e1") * shannon
 spe_asy ~ c("d1", "d2", "d2") * horizontal_complexity + c("f1", "f2", "f2") * shannon
 shannon ~ c("h1", "h1", "h1") * horizontal_complexity

 indirect_via_spe_sta             := a1 * c1
 indirect_via_shan_spe_sta        := a1 * e1 * h1
 indirect_via_spe_all             := a1 * c1 + a1 * e1 * h1

 indirect_via_shan_high           := h1 * g1
 indirect_via_shan_medlow         := h1 * g2

 indirect_via_spe_asy_high        := b1 * d1
 indirect_via_shan_spe_asy_high   := b1 * f1 * h1
 indirect_via_spe_all_high        := b1 * d1 + b1 * f1 * h1

 indirect_via_spe_asy_medlow      := b2 * d2
 indirect_via_shan_spe_asy_medlow := b2 * f2 * h1
 indirect_via_spe_all_medlow      := b2 * d2 + b2 * f2 * h1'

mod_constrained_shan.fit <- sem(mod_constrained_shan, data = Consumers_div_sta_scaled.data, group = "vertical_complexity")
fitMeasures(mod_constrained_shan.fit, c("df", "AIC", "pvalue", "RMSEA", "CFI", "SRMR"))
summary(mod_constrained_shan.fit, rsquare=TRUE) 
