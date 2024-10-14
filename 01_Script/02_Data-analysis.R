#### Data Analysis      ####
#### Tobias Ruettenauer ####
#### 2020/ 01 / 31      ####

# Packages
library(WDI)
library(ggplot2)
library(texreg)

# Working Directory
setwd("C:/work/Lehre/Dissertation_module/SOCS0075_2023/Example_project/02_Data")


# Load data 
load("WDI_short.RData")




########################
### Regression model ###
########################

### Estimate linear regression with quadratic term for year 1

# Model 1
mod1.lm <- lm(log(co2_pc) ~ log(gdp_pc) + I(log(gdp_pc)^2),
              data = wd.df[wd.df$year == 2019,])

summary(mod1.lm)


# Model 2
mod2.lm <- lm(log(co2_pc) ~ log(gdp_pc) + I(log(gdp_pc)^2) + population + gini,
              data = wd.df[wd.df$year == 2019,])

summary(mod2.lm)


### Export models to word in Output folder
# could use custom.coef.names or custom.coef.map to rename coefficients
wordreg(list(mod1.lm, mod2.lm),
        file = "../03_Output/Regression.docx", # "../" means "go back one folder"
        custom.model.names = c("Model 1", "Model 2"),
        dcolumn = TRUE, caption.above = TRUE, digits = 3,
        caption = "Regression models. Outcome variable: CO2 emission per capita.")


#####################
### Visualisation ###
#####################


# Calculate maximum (turning point)
b1 <- mod1.lm$coefficients["log(gdp_pc)"]
b2 <- mod1.lm$coefficients["I(log(gdp_pc)^2)"]
tp <- -b1 / (2 * b2)

# Plot 2019 data
pl1 <- ggplot(wd.df[wd.df$year == 2019,],
              aes(
                x = gdp_pc,
                y = co2_pc,
                size = population,
                color = region
              )) +
  geom_smooth(aes(group = 1), show.legend = "none") + 
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = exp(tp), linetype="dotted") +
  annotate(geom="text", x = 10000,  y = 0.1,
           label = paste0("Turning point: ", 
                          format(round(exp(tp), 0), big.mark = ","), 
                          " USD")) +
  theme_minimal() + 
  scale_y_log10() +  
  scale_x_log10(labels = scales::dollar_format()) +
  labs(y = "CO2 emissions per capita (log-transformed)", 
       x = "GDP per capita (log-transformed)",
       title = "Year: 2019")

pl1


### Export as jpeg
jpeg(file = "../03_Output/Figure1.jpeg", width = 8, height = 6, 
     units = "in", res = 300)
pl1
dev.off()