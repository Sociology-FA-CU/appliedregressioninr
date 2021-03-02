library(tidyverse)
library(here)


# Simulation (Gelman et al. pp. 82) ---------------------------------------


x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
set.seed(42)
y <- a + b*x + sigma*rnorm(n)


fake <- data.frame(x, y)

fit_1 <- lm(y ~ x, data=fake)
fit_1

# interpret full coefficient table + residual standard error 
# (residual standard error is sigma in our simulation, to create CI around estimate to predict where 68, 95 etc. of data point will be)
summary(fit_1)


fake %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm", se=TRUE)

# compare to R base approach
plot(y ~ x, data = fake)
abline(fit_1)

# extracting single parameters --------------------------------------------

str(fit_1)

fit_1$coefficients
fit_1$coefficients[1]
fit_1$coefficients[2]
fit_1$fitted.values
fit_1$residuals # response - fitted
fit_1$df.residual
fit_1$call
fit_1$model # actual data

fit_1$model[[1]] - fit_1$fitted.values

 

# real data example -------------------------------------------------------

countries <- read.csv(here("data", "countries.csv"))

countries %>% glimpse()

countries %>% ggplot(aes(x=dem_index, y=life_exp))+geom_point()

#quick alternative for inspection
countries %>% select(dem_index, life_exp) %>% plot()

fit_di <- lm(life_exp ~ dem_index, data = countries)
summary(fit_di)

# centering
countries <-
  countries %>% mutate(dem_index_c = dem_index - mean(dem_index, na.rm = TRUE))

# base alternative
countries$dem_index_c <- countries$dem_index - mean(countries$dem_index, na.rm = TRUE)

fit_di_c <- lm(life_exp ~ dem_index_c, data = countries)
summary(fit_di_c)

# binary predictor -------------------------------------------------------

countries %>% count(postsoviet)

countries$postsoviet_dummy <- ifelse(countries$postsoviet == "yes", 1, 0)

fit_postsoviet <- lm(life_exp ~ postsoviet_dummy, data = countries)
summary(fit_postsoviet)

fit_postsoviet <- lm(life_exp ~ postsoviet, data = countries)
summary(fit_postsoviet)


# exercise regression ----------------------------------------------------------------

# from the countries data set, regress dem_index on life expectancy, interpret the betas

lm(dem_index ~ life_exp, data = countries)

# center life expectancy and repeat, interpret the betas

countries <-
  countries %>% mutate(life_exp_c = life_exp - mean(life_exp, na.rm = TRUE))

lm(dem_index ~ life_exp_c, data = countries)

# regress per-capita gdp on eu_membership, interpret the betas

countries <-
  countries %>% mutate(percapita_gdp = (gdp*1000000) / population)
lm(percapita_gdp ~ eu_member, data = countries)

countries %>% group_by(eu_member) %>% summarise(mean = mean((gdp*1000000) / population, na.rm=TRUE))

countries %>% 
  mutate(eu_member = as.numeric(eu_member=="yes")) %>%
  ggplot(aes(x=eu_member, y = (gdp*1000000) / population)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# given that Czech Republic is in the EU, by how much is its percapita GDP bigger or smaller than predicted by the model?

eu_model <- lm(percapita_gdp ~ eu_member, data = countries)

countries # 3

eu_model$residuals[3]
eu_model$fitted.values[3]
eu_model$model[3,]



# basic stats concepts - recap --------------------------------------------

var(countries$uni_prc, na.rm = TRUE)
sd(countries$uni_prc, na.rm = TRUE)
sqrt(var(countries$uni_prc, na.rm = TRUE))

ggplot(aes(x=uni_prc), data=countries) + geom_histogram(bins = 10, color = "white")
ggplot(aes(x=uni_prc), data=countries) + geom_density(fill = "grey")
ggplot(aes(y=uni_prc, x=""), data=countries) + geom_boxplot()
ggplot(aes(y=uni_prc, x =""), data = countries) + geom_violin() +geom_boxplot(width = 0.3)


cov(countries$uni_prc, countries$dem_index, use="complete.obs")
cor(countries$uni_prc, countries$dem_index, use="complete.obs")

# fit_2 <- stan_glm(y ~ x, data=fake)
# print(fit_2, digits = 2)
