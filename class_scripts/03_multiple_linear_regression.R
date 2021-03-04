# standradized betas

browseURL("http://www.stat.columbia.edu/~gelman/standardize/")


# interaction

browseURL("https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html")


# gender
women <- rep("woman", 1000)
men <- rep("man", 1000)
gender <- c(women, men)

n <- length(gender)

# years of education

runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=T)

yoe_women <- runifdisc(n/2, min = 10, max = 20)
yoe_men <- runifdisc(n/2, min = 10, max = 20)
yoe <- c(yoe_women, yoe_men)

# income for women
intercept_w <- 10000
beta_yoe_w <- 1000
sigma_w <- 200

income_w <- intercept_w + beta_yoe_w*yoe_women + sigma_w*rnorm(n/2)

# income for men
intercept_m <- 40000
beta_yoe_m <- -1000
sigma_m <- 200

income_m <- intercept_m + beta_yoe_m*yoe_men + sigma_m*rnorm(n/2)

mean(income_w)
mean(income_m)

income <- c(income_w, income_m)

# data

fake <- data.frame(gender, yoe, income)

fit1 <- lm(income ~ yoe + gender, data=fake)
summary(fit1)

fit2 <- lm(income ~ yoe + gender + yoe*gender, data=fake)
summary(fit2)

# two sepearate models

fit3 <- lm(income ~ yoe, data=fake[1:1000,])
summary(fit3)

fit4 <- lm(income ~ yoe, data=fake[1001:2000,])
summary(fit4)
