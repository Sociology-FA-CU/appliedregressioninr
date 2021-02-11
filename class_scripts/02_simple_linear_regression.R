
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
# (sigma in our simulation, to create CI around estimate to predict where 68, 95 etc. od data point will be)
summary(fit_1)


fake %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm", se=TRUE)

# compare to R base approach
plot(y ~ x, data = fake)
abline(fit_1)



# extracting single parameters --------------------------------------------

str(fit_1)

fit_1$coefficients[1]
fit_1$coefficients[2]
fit_1$fitted.values
fit_1$residuals # response - fitted
fit_1$df.residual
fit_1$call
fit_1$model

fit_1$model[[1]] - fit_1$fitted.values

 

# real data example -------------------------------------------------------

countries %>% glimpse()

countries %>% ggplot(aes(x=dem_index, y=life_exp))+geom_point()

#quick alternative for inspection
countries %>% select(dem_index, life_exp) %>% plot()

fit_di <- lm(life_exp ~ dem_index, data = countries)
summary(fit_di)

# centering
countries <-
  countries %>% mutate(dem_index_c = dem_index - mean(dem_index, na.rm = TRUE))

fit_di_c <- lm(life_exp ~ dem_index_c, data = countries)
summary(fit_di_c)

# binary predictor -------------------------------------------------------



fit_postsoviet <- lm(life_exp ~ postsoviet, data = countries)
summary(fit_postsoviet)


# standradized betas

browseURL("http://www.stat.columbia.edu/~gelman/standardize/")





# fit_2 <- stan_glm(y ~ x, data=fake)
# print(fit_2, digits = 2)
