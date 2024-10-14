#### Data Handling      ####
#### Tobias Ruettenauer ####
#### 2024/ 01 / 08      ####



# Working Directory
setwd("C:/work/Lehre/Dissertation_module/SOCS0075_2023/Example_project/02_Data")


library(haven)
library(psych)
library(fastDummies)
library(corrtable)
library(stargazer)
library(texreg)


#####################
### Load the data ###
#####################

issp.df <- read_dta("ZA7650_v2-0-0.dta")



######################
### Missing values ###
######################

# Check descriptives of sex
table(issp.df$SEX) # Sex == -9 is missing.
summary(issp.df$SEX)

# Check descriptives of birth year
table(issp.df$BIRTH) # Again, -9 is missing.
summary(issp.df$BIRTH)

# Recode -9 to missing
issp.df$SEX[which(issp.df$SEX == -9)] <- NA

# Check if this worked
summary(issp.df$SEX)

### You can do this for the entire dataset
# This only makes sense if there aren't any real negative values
issp.df[issp.df < 0] <- NA

table(issp.df$BIRTH)
summary(issp.df$BIRTH)



########################################
### Recode into meaningful variables ###
########################################

### Sex
# For sex, 2 = female. So we do female = SEX - 1, so that 1 = female and 0 = male
table(issp.df$SEX)
issp.df$female <- issp.df$SEX - 1
table(issp.df$female) # this worked well

# And we declare it as factor right away
issp.df$female <- as.factor(issp.df$female)


### Age (survey year - birth year)
table(issp.df$BIRTH)
issp.df$age <- 2020 - issp.df$BIRTH
table(issp.df$age)
summary(issp.df$age)


### Education
table(issp.df$EDULEVEL)

# Code as factor with labels
issp.df$education <- factor(issp.df$EDULEVEL,
                            levels = c(0:8), 
                            labels = c("No education, incomplete primary",
                                       "Primary education",
                                       "Lower secondary education",
                                       "Upper secondary education",
                                       "Post secondary, non-tertiary",
                                       "Short-cycle tertiary", 
                                       "Lower tertiary, BA",
                                       "Upper tertiary, MA",
                                       "PhD, Post tertiary specialization"))
table(issp.df$education)

# Define reference category (don't want to use "no education")
# We take the largest category
table(issp.df$education)[which(table(issp.df$education) == max(table(issp.df$education)))]

# Use relevel
issp.df$education <- relevel(issp.df$education, ref = "Upper secondary education")




####################################################
### Index construction: Environmental protextion ###
####################################################

# Willingness to Make Trade-Offs for Environment
# v26…Pay much higher prices
# v27…Pay much higher taxes
# v28…Cut your standard of living,

# Summary stats
summary(issp.df[, c("v26", "v27", "v28")])
table(issp.df$v26)

# NOTE! higher values (5): very unwilling 
# So, we recode: higher values more willing
issp.df$v26 <- 6 - issp.df$v26
issp.df$v27 <- 6 - issp.df$v27
issp.df$v28 <- 6 - issp.df$v28

table(issp.df$v26)

# Correlation matrix
cor(issp.df[, c("v26", "v27", "v28")], method = "pearson",
    use = "complete.obs") # listwise deletion (only those without any missing)

# Cronbachs Alpha
alpha(issp.df[, c("v26", "v27", "v28")])

# This looks all pretty good, very high Cronbachs Alpha (far above 0.6)
# Cronbachs Alpha would increase when dropping v28, but only slightly
# Rather keep it as it is a bit of a different dimension than other two.

# Construct a single composite index (using unweighted average)
issp.df$envir_willigness <- (issp.df$v26 + issp.df$v27 + issp.df$v28)/3

# Look at summary and distribution
summary(issp.df$envir_willigness)
hist(issp.df$envir_willigness)




##########################
### Summary statistics ###
##########################

# Define varnames
vars <- c("envir_willigness", "age", "female", "education", "TOPBOT")

# Convert to pure dataframe without tble (stargazer doesn't like)
issp.df <- as.data.frame(issp.df)

# Make dummy cols out of factors
sample.df <- issp.df[, vars]
sample.df <- dummy_cols(sample.df, ignore_na = TRUE, 
                        remove_selected_columns = TRUE)

# Create table
stargazer(sample.df, 
          style = "asr", digits = 3, type = "html", align = T, 
          out = "../03_Output/Summary_stats.html", # relatic path: "one folder back, than into 03_Output"
          summary.stat = c("n", "mean", "sd", "min", "max"))





##########################
### Correlation matrix ###
##########################

# simple corrtable
cor(sample.df, method = "pearson",
    use = "complete.obs")

# Or use corrtable to export
save_correlation_matrix(sample.df,
                        filename = "../03_Output/Corr_mat.csv",
                        digits = 3,
                        use = 'lower')






############################
### Estiamte some models ###
############################

### Model1
re1.lm <- lm(envir_willigness ~ female,
             data = issp.df)

### Model2
re2.lm <- lm(envir_willigness ~ female + age + education,
             data = issp.df)

### Model3
re3.lm <- lm(envir_willigness ~ female + age + education + TOPBOT + as.factor(country),
             data = issp.df)

### Compare

screenreg(list(re1.lm, re2.lm, re3.lm), digits = 3)



# NOTE THE VARYING N!!!
# we want to correct this by getting the obs included in the models with smallest N

omitted <- re3.lm$na.action
sample <- which(! rownames(issp.df) %in% omitted)

### Model1
re1.lm <- lm(envir_willigness ~ female,
             data = issp.df[sample, ])

### Model2
re2.lm <- lm(envir_willigness ~ female + age + education,
             data = issp.df[sample, ])

### Model3
re3.lm <- lm(envir_willigness ~ female + age + education + TOPBOT + as.factor(country),
             data = issp.df[sample, ])


### Export model table to word
# See ?wordreg on how to make this look nicer.

wordreg(list(re1.lm, re2.lm, re3.lm), 
        file = "../03_Output/Regression_output1.docx",
        custom.model.names = c("Mod 1", "Mod 2", "Mod 3"),
        digits = 3, include.nobs = TRUE, 
        omit.coef = c("country"),
        custom.gof.rows = list("Country effects" = c("No", "No", "Yes")))



save(issp.df, file = "ISSP_prep.RData")