#### Data Visualisation ####
#### Tobias Ruettenauer ####
#### 2024/ 02 / 25      ####



# Working Directory
setwd("C:/work/Lehre/Dissertation_module/SOCS0075_2023/Example_project/02_Data")


library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(ggeffects)

library(tidyverse)
library(broom)
library(dplyr)

library(sjPlot)
library(sjlabelled)
library(sjmisc)

#####################
### Load the data ###
#####################

data(mpg)



#### --------- Some descriptive examples --------- ####


#####################
### Easy examples ###
#####################


### Basic plot

p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
p1

# Export
png(file = paste("../03_Output/", "Plot1.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p1
dev.off()


### Basic Plot by group

p2 <- ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

p2

# Export
png(file = paste("../03_Output/", "Plot2.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p2
dev.off()


### Basic Plot by group divided by facet

p3 <- ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() +
  facet_wrap(~year)

p3

# Export
png(file = paste("../03_Output/", "Plot3.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p3
dev.off()



### Basic Plot by group + smoother

p4 <- ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() +
  facet_wrap(~year) +
  geom_smooth(aes(group = 1)) # group argument for overall smoother
p4

# Export
png(file = paste("../03_Output/", "Plot4.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p4
dev.off()




#######################
### Boxplot example ###
#######################

### Basic boxplot
p5 <- ggplot(mpg, aes(drv, hwy)) + 
  geom_point()
p5

# Export
png(file = paste("../03_Output/", "Plot5.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p5
dev.off()



### Boxplot with jittering
p6_1 <- ggplot(mpg, aes(drv, hwy)) + geom_jitter()
p6_2 <- ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
p6_3 <- ggplot(mpg, aes(drv, hwy)) + geom_violin()

# Export
png(file = paste("../03_Output/", "Plot6.png", sep = ""), 
    width = 8, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
ggarrange(p6_1, p6_2, p6_3,
          ncol = 3)
dev.off()



########################
### Barchart example ###
########################

### Basic boxplot
p7 <- ggplot(mpg, aes(hwy)) + 
  geom_histogram()
p7

# Export
png(file = paste("../03_Output/", "Plot7.png", sep = ""), 
    width = 6, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
p7
dev.off()




### Basic barchart by group
p8_1 <- ggplot(mpg, aes(displ, colour = as.factor(year))) + 
  geom_freqpoly(binwidth = 0.5)
p8_2 <- ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

# Export
png(file = paste("../03_Output/", "Plot8.png", sep = ""), 
    width = 8, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
ggarrange(p8_1, p8_2,
          ncol = 2)
dev.off()



### Basic barchart by group with theme
p9_1 <- ggplot(mpg, aes(displ, colour = as.factor(year))) + 
  geom_freqpoly(binwidth = 0.5) +
  theme_classic()
p9_2 <- ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1) +
  theme_classic()

# Export
png(file = paste("../03_Output/", "Plot9.png", sep = ""), 
    width = 8, height = 4, 
    units = "in", res = 300)
par(mar = c(0,0,0,0))
ggarrange(p9_1, p9_2,
          ncol = 2)
dev.off()



#######################
### Overall example ###
#######################

# Prepare vars

mpg$class2 <- NA
mpg$class2[which(mpg$class %in% c("2seater",
                                  "compact",
                                  "midsize",
                                  "minivan",
                                  "subcompact"))] <- "Compact"
mpg$class2[which(mpg$class %in% c("pickup"))] <- "Pickup"
mpg$class2[which(mpg$class %in% c("suv"))] <- "SUV"

mpg$drv <- factor(mpg$drv, levels = c("f", "r", "4"), 
                  labels = c("front", "rear", "4wd"))



### Plot
# p10_1 <- ggplot(mpg, aes(drv, hwy)) + 
#   geom_violin(aes(fill = drv), draw_quantiles = c(0.5), colour = "deeppink1", 
#               size = 1.2, alpha = 0.7) + 
#   geom_jitter(aes(colour = class2), alpha = 0.5) + 
#   scale_colour_viridis_d() +                # colour for jitter points
#   scale_fill_grey(start = 0.4) +            # colour for violin fill
#   theme_classic() +  theme(legend.position = "none") +
#   xlab("Drive train") +  ylab("Miles per gallon")

p10_1 <- ggplot(mpg, aes(drv, hwy)) + 
  geom_boxplot(aes(fill = drv), outlier.shape = NA) + 
  geom_jitter(aes(colour = class2),height=0, width =  .15) + 
  stat_summary(fun = mean, geom = "point", size = 4, color = "deeppink1") +
  scale_colour_viridis_d() +                      # colour for jitter points
  scale_fill_viridis_d(option = "G", begin = .3, direction = -1) +            # colour for violin fill
  theme_classic() +  theme(legend.position = "none") +
  xlab("Drive train") +  ylab("Miles per gallon")

p10_2 <- ggplot(mpg, aes(displ, fill = class2)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~class2, ncol = 1) +
  scale_fill_viridis_d() +
  theme_classic() +  theme(legend.position = "none") +
  ylab("Frequency") +  xlab("Engine Displacement (litres)") 

p10_3 <- ggplot(mpg, aes(displ, hwy, colour = class2, shape = class2)) + 
  geom_jitter() +
  scale_color_viridis_d() +
  geom_smooth(aes(group = 1)) +
  theme_classic() + xlab("Engine Displacement (litres)") +  ylab("Miles per gallon")

# Export
png(file = paste("../03_Output/", "Plot10.png", sep = ""), 
    width = 10, height = 4, 
    units = "in", res = 300)
ggarrange(p10_1, p10_2, p10_3, ncol = 3)
dev.off()








#### --------- Some regression examples --------- ####

# Let's use the last time

load("ISSP_prep.RData")


### Estimate Regressions

# NOTE THE VARYING N!!!
# we want to correct this by getting the obs included in the models with smallest N

omitted <- re3.lm$na.action
sample <- which(! rownames(issp.df) %in% omitted)

### Model1
re1.lm <- lm(envir_willigness ~ female,
             data = issp.df[sample, ])

### Model2
re2.lm <- lm(envir_willigness ~ female + age + education + TOPBOT,
             data = issp.df[sample, ])

### Model2
re3.lm <- lm(envir_willigness ~ female + as.factor(country),
             data = issp.df[sample, ])

### Model3
re4.lm <- lm(envir_willigness ~ female + age + education + TOPBOT + as.factor(country),
             data = issp.df[sample, ])



###########################
### Plot a single model ###
###########################


### Extract model information via tidy()

(results <- tidy(re2.lm, conf.int = TRUE, conf.level = 0.95))

results <- results %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

### Plot
rp1 <- ggplot(results, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = Variable, 
                      y = Coefficient,
                      ymin = conf.low,
                      ymax = conf.high)) +
  coord_flip()

rp1


# Export
png(file = paste("../03_Output/", "Plot11.png", sep = ""), 
    width = 6, height = 5, 
    units = "in", res = 300)
rp1
dev.off()


# Label and select
map <- list("female1"                                   = "Female",
            "age"                                       = "Age",
            "educationNo education, incomplete primary" = "No education",
            "educationPrimary education"                = "Primary Education",
            "educationLower secondary education"        = "Lower Secondary",
            "educationPost secondary, non-tertiary"     = "Post Secondary",
            "educationShort-cycle tertiary"             = "Short Tertiary",
            "educationLower tertiary, BA"               = "Lower Tertiary",
            "educationUpper tertiary, MA"               = "Upper Tertiary",
            "educationPhD, Post tertiary specialization"= "Post Tertiary",
            "TOPBOT"                                    = "SES self-rating")

results$Variable <- factor(results$Variable,
                           levels = rev(names(map)), # reverse ordering
                           labels = rev(map))

# Re-do plot
rp1 <- ggplot(results, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = Variable, 
                      y = Coefficient,
                      ymin = conf.low,
                      ymax = conf.high)) +
  ggtitle("Effect on WTP for environment") +
  ylab("Coefficient with 95% confidence interval") +
  coord_flip()

rp1


# Export
png(file = paste("../03_Output/", "Plot12.png", sep = ""), 
    width = 6, height = 5, 
    units = "in", res = 300)
rp1
dev.off()



### Extract model information for multiple models

mod.list <- list("Model 1" = re1.lm,
                 "Model 2" = re2.lm,
                 "Model 3" = re3.lm,
                 "Model 4" = re4.lm)
res.ls <- lapply(mod.list, 
                 FUN = function(x) tidy(x, conf.int = TRUE, conf.level = 0.95))
results <- bind_rows(res.ls, .id = "model")
head(results)


# Repare and filder
results <- results %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)",
         !str_detect(Variable, "country")) # drop anything that contains country


# Label and select
map <- list("female1"                                   = "Female",
            "age"                                       = "Age",
            "educationNo education, incomplete primary" = "No education",
            "educationPrimary education"                = "Primary Education",
            "educationLower secondary education"        = "Lower Secondary",
            "educationPost secondary, non-tertiary"     = "Post Secondary",
            "educationShort-cycle tertiary"             = "Short Tertiary",
            "educationLower tertiary, BA"               = "Lower Tertiary",
            "educationUpper tertiary, MA"               = "Upper Tertiary",
            "educationPhD, Post tertiary specialization"= "Post Tertiary",
            "TOPBOT"                                    = "SES self-rating")

results$Variable <- factor(results$Variable,
                           levels = rev(names(map)), # reverse ordering
                           labels = rev(map))

# Re-do plot
rp2 <- ggplot(results, aes(x = Variable, y = Coefficient, 
                           shape = model, colour = model)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = Variable, 
                      y = Coefficient,
                      ymin = conf.low,
                      ymax = conf.high),
                  position = position_dodge(0.7)) + # offset for coefficients
  scale_color_viridis_d() +
  ggtitle("Effect on WTP for environment") +
  ylab("Coefficient with 95% confidence interval") +
  coord_flip() +
  theme_light() + theme(axis.text.y = element_text(colour = "black"))

rp2



# Export
png(file = paste("../03_Output/", "Plot13.png", sep = ""), 
    width = 6, height = 5, 
    units = "in", res = 300)
rp2
dev.off()






#### --------- Interaction effects --------- ####



#############################
### Predicted values plot ###
#############################

mod1.lm <- lm(DE_INC ~ female*age + female*I(age*age),
              data = issp.df) 
summary(mod1.lm)



### Plot the effect using package ggeffect
ip1 <- plot_model(mod1.lm, type = "pred", terms = c("age", "female"))
ip1 + theme_bw()

# Export
png(file = paste("../03_Output/", "Plot14.png", sep = ""), 
    width = 6, height = 5, 
    units = "in", res = 300)
ip1 + theme_bw()
dev.off()





#############################
### Marginal effects plot ###
#############################


mod2.lm <- lm(TOPBOT ~ DE_INC * age + female * age,
              data = issp.df) 
summary(mod2.lm)



### Plot the effect using package interactions
ip2 <- sim_slopes(mod2.lm, pred = DE_INC, modx = age, jnplot = TRUE)
ip2


# Export
png(file = paste("../03_Output/", "Plot15.png", sep = ""), 
    width = 6, height = 5, 
    units = "in", res = 300)
ip2$jnplot + theme_bw()
dev.off()
