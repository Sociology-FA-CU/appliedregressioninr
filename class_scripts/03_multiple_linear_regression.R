# Exercise of MLR, incl. standardization of coefficients
# Exercise of interactions

library(tidyverse)
library(here)
library(ggeffects)
countries <- read.csv(here("data", "countries.csv"))
un <- read.table(here("data", "UnitedNations.txt"))



# Exercise 1 - compare SLR and MLR -----------------------------------------------------------

# Regress per-capita GDP on the percentage of university educated (‘uni_prc’). 
# Then regress per-capita GDP on both percentage of university educated (‘uni_prc’) 
# and democratic index (‘dem_index’) in a single multiple regression model. Interpret the results.

countries <-
  countries %>% mutate(percapita_gdp = (gdp*1000000) / population)


mod1 <- lm(percapita_gdp ~ uni_prc, data = countries)
summary(mod1)

mod2 <- lm(percapita_gdp ~ uni_prc + dem_index, data = countries)
summary(mod2)

str(mod2)
mod2$coefficients

coef(mod2)
confint(mod2)



# Exercise 2 - compare betas ---------------------------------------

# interpretation - change from mean to mean + 1sd
mod3 <- lm(scale(percapita_gdp) ~ scale(uni_prc) + scale(dem_index), data = countries)
summary(mod3)

# Comment about standardization - it is still rough comparison only, 
# for different distributions, standard deviation can have somewhat different substantial meaning
# e.g. binary distribution 0 vs. 1 with probability 0.5 vs 0.5: after standardization: 
# comparing -1 with 1, i.e. less straightforward comparison, we often keep binaries un-standardized 
# (but comparing betas of standardized metric and non-standardized binary does no make much sense, then)
# or Gelman suggest standardization dividing by 2 SDs

# From Gelman:
# To see this, consider the simplest binary x variable, which takes on the values
# 0 and 1, each with probability 0.5. The standard deviation of x is then √ 0.5 ∗ 0.5 = 0.5, and so
# the standardized variable, (x − μx )/(2σx ), takes on the values ± 0.5, and its coefficient reflects
# comparisons between x = 0 and x = 1. In contrast, if we had divided by 1 standard deviation,
# the rescaled variable takes on the values ±1, and its coefficient corresponds to half the difference
# between the two possible values of x.


# Exercise 3 ---------------------------------------


mod4 <- lm(tfr ~ illiteracyFemale, data = un)
summary(mod4)

mod5 <- lm(tfr ~ illiteracyMale, data = un)
summary(mod5)

mod6 <- lm(tfr ~ illiteracyMale + illiteracyFemale, data = un)
summary(mod6)

mod7 <- lm(tfr ~ illiteracyFemale + illiteracyMale, data = un)
summary(mod7)

cor(un$illiteracyFemale, un$illiteracyMale, use = "complete")


# Exercise 4 - interactions simulation ---------------------------------------

# gender
women <- rep("woman", 1000)
men <- rep("man", 1000)
gender <- c(women, men)

n <- length(gender)

# years of education

set.seed(248)
yoe_women <- sample(10:20, n/2, replace = TRUE)
yoe_men <- sample(10:20, n/2, replace = TRUE)
yoe <- c(yoe_women, yoe_men)

# income for women
intercept_w <- 10000
beta_yoe_w <- 1000
sigma_w <- 500

income_w <- intercept_w + beta_yoe_w*yoe_women + sigma_w*rnorm(n/2)

# income for men
intercept_m <- 40000
beta_yoe_m <- -1000
sigma_m <- 500

income_m <- intercept_m + beta_yoe_m*yoe_men + sigma_m*rnorm(n/2)

mean(income_w)
mean(income_m)

income <- c(income_w, income_m)

# produce dataframe

fake <- data.frame(gender, yoe, income)

# visualize

fake %>% ggplot(aes(x=yoe, y = income))+
  geom_point() +
  geom_smooth(method = "lm")

fake %>% ggplot(aes(x=yoe, y = income, color = gender))+
  geom_point() +
  geom_smooth(method = "lm")


# model

fit1 <- lm(income ~ yoe + gender, data=fake)
summary(fit1)

fit2 <- lm(income ~ yoe*gender, data=fake)
summary(fit2)

# two sepearate models

fit3 <- lm(income ~ yoe, data=fake[1:1000,])
summary(fit3)

fit4 <- lm(income ~ yoe, data=fake[1001:2000,])
summary(fit4)

# centering variables for models with interactions

fake = 
  fake %>% mutate(yoe = yoe - mean(yoe),
                  gender_w = ifelse(gender == "woman", 1,0 ),
                  gender_w = gender_w - mean(gender_w))


fit5 <- lm(income ~ yoe*gender_w, data=fake)
summary(fit5)

# Exercise 5 - reshuffle the parameters (more realistic models) ---------------------------------------


# Visualizing regression models using specialized packages -------------------------------------------

# Show slides


# Exercise 6 plot marginal effects shown in slides ----------------------------------------

# first, build a model, the same as in slides

mdl1 = lm(life_exp ~ poly(dem_index, 2) * postsoviet,
          data = countries[!is.na(countries$dem_index),])
summary(mdl1)
coef(mdl1)

# predicted values for selected dem_index for postsoviet countries
ggpredict(mdl1, terms = c("dem_index [4,6,8]", "postsoviet [yes]"))

# predicted values without specification of values
ggpredict(mdl1, terms = c("dem_index", "postsoviet"))

# plot it

dfplot <- ggpredict(mdl1, terms = c("dem_index", "postsoviet"))

plot(dfplot)

ggplot(aes(x=x, y=predicted, color = group), data = dfplot) +
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)

# plot just one variable

plot(ggpredict(mdl1, terms = c("postsoviet")))

plot(ggpredict(mdl1, terms = c("postsoviet"), condition = c(dem_index = 9)))


# Exercise 7 plot marginal effects in a new model ----------------------------------------


un %>% glimpse()
un %>% summarise(across(.cols = everything(),
                        .fns = ~sum(is.na(.))))

ggplot(aes(x=infantMortality), data=un)+geom_density()
ggplot(aes(y=infantMortality, x=tfr), data=un) + geom_point()
ggplot(aes(y=infantMortality, x=log(GDPperCapita)), data=un) + geom_point()

un %>% count(region)

mdl2 <- lm(infantMortality ~ region*tfr + log(GDPperCapita), data = un)
summary(mdl2)


plot(ggpredict(mdl2, terms = c("region", "tfr")))


mdl3 <- lm(infantMortality ~ region + tfr*log(GDPperCapita), data = un)
summary(mdl3)


plot(ggpredict(mdl3, terms = c("GDPperCapita [exp, all]", "tfr")))
plot(ggpredict(mdl3, terms = c("tfr [all]", "GDPperCapita")))

plot(ggpredict(mdl3, terms = c("GDPperCapita", "tfr")))
plot(ggpredict(mdl3, terms = c("GDPperCapita [exp]", "tfr")))
plot(ggpredict(mdl3, terms = c("GDPperCapita", "tfr"), condition = c(region = "Europe")))
plot(ggpredict(mdl3, terms = c("GDPperCapita [exp]", "tfr",), condition = c(region = "Europe")))

# dependent on y, first independent on x, second color, third facet
plot(ggpredict(mdl3, terms = c("GDPperCapita", "tfr", "region")))

# interaction

browseURL("https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html")



