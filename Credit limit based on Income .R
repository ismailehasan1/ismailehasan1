


## R code for the credit Limit based on Income analysis.

##### Question A ###########################################################

library(ISLR)
data("Credit")
?Credit


df1 <- data.frame(income = log(Credit$Income), limit = log(Credit$Limit))
prf <- lm(limit ~ income, data = df1)
coef(prf)
seed <- 123
set.seed(seed)
library(dplyr)
credit_sample <- slice_sample(df1, n = 25, replace = TRUE)
srf = lm(limit ~ income, data = credit_sample)


library(ggplot2)
ggplot(df1, aes(x = income, y = limit)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  xlab("income") + ylab("limit") +
  ggtitle("PRF of Credit Limit") +
  labs(subtitle = "Credit limit based on income (both in logs)")


n <- 25
seed <- 123
set.seed(seed)
M <- 1000
# Set up empty matrix to store simulation results
coef_df_25 <- data.frame(matrix(NA, nrow = M, ncol = 2))
colnames(coef_df_25) <- c("beta_1", "beta_2")
for (i in 1:M) {
  auto_sample <- slice_sample(df1, n = n, replace = TRUE)
  sim_srf = lm(limit ~ income, data = auto_sample)
  coef_df_25$beta_1[i] <- sim_srf$coefficients[1]
  coef_df_25$beta_2[i] <- sim_srf$coefficients[2]
}
library(tidyr)
df_plot_25 <- pivot_longer(coef_df_25, 1:2, 
                           names_to = "coef")
df_plot_25$coef2 <- factor(df_plot_25$coef, 
                           labels = c("beta[1]", "beta[2]"))

ggplot(df_plot_25, aes(value)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + 
  facet_grid(. ~ coef2, scales = "free_x", 
             labeller = label_parsed) + 
  geom_vline(data = filter(df_plot_25, coef == "beta_1"), 
             aes(xintercept = coef(prf)[1]), color = "purple") +
  geom_vline(data = filter(df_plot_25, coef == "beta_2"), 
             aes(xintercept = coef(prf)[2]), color = "violet") +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Results of Monte-Carlo study for n = 25")

c(mean_b1 = mean(coef_df_25$beta_1), 
  sd_b1 = sd(coef_df_25$beta_1), 
  mean_b2 = mean(coef_df_25$beta_2), 
  sd_b2 = sd(coef_df_25$beta_2),
  var_b1 = var(coef_df_25$beta_1),
  var_b2 = var(coef_df_25$beta_2))


n <- 100
seed <- 123
set.seed(seed)
M <- 1000
# Set up empty matrix to store simulation results
coef_df_100 <- data.frame(matrix(NA, nrow = M, ncol = 2))
colnames(coef_df_100) <- c("beta_1", "beta_2")
for (i in 1:M) {
  auto_sample <- slice_sample(df1, n = n, replace = TRUE)
  sim_srf = lm(limit ~ income, data = auto_sample)
  coef_df_100$beta_1[i] <- sim_srf$coefficients[1]
  coef_df_100$beta_2[i] <- sim_srf$coefficients[2]
}
library(tidyr)
df_plot_100 <- pivot_longer(coef_df_100, 1:2, 
                            names_to = "coef")
df_plot_100$coef2 <- factor(df_plot_100$coef, 
                            labels = c("beta[1]", "beta[2]"))

ggplot(df_plot_100, aes(value)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + 
  facet_grid(. ~ coef2, scales = "free_x", 
             labeller = label_parsed) + 
  geom_vline(data = filter(df_plot_100, coef == "beta_1"), 
             aes(xintercept = coef(prf)[1]), color = "red") +
  geom_vline(data = filter(df_plot_100, coef == "beta_2"), 
             aes(xintercept = coef(prf)[2]), color = "blue") +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Results of Monte-Carlo study for n = 100")

c(mean_b1 = mean(coef_df_100$beta_1), 
  sd_b1 = sd(coef_df_100$beta_1), 
  mean_b2 = mean(coef_df_100$beta_2), 
  sd_b2 = sd(coef_df_100$beta_2), 
  var_b1 = var(coef_df_100$beta_1),
  var_b2 = var(coef_df_100$beta_2))

n <- 400
seed <- 123
set.seed(seed)
M <- 1000
# Set up empty matrix to store simulation results
coef_df_400 <- data.frame(matrix(NA, nrow = M, ncol = 2))
colnames(coef_df_400) <- c("beta_1", "beta_2")
for (i in 1:M) {
  auto_sample <- slice_sample(df1, n = n, replace = TRUE)
  sim_srf = lm(limit ~ income, data = auto_sample)
  coef_df_400$beta_1[i] <- sim_srf$coefficients[1]
  coef_df_400$beta_2[i] <- sim_srf$coefficients[2]
}
library(tidyr)
df_plot_400 <- pivot_longer(coef_df_400, 1:2, 
                            names_to = "coef")
df_plot_400$coef2 <- factor(df_plot_400$coef, 
                            labels = c("beta[1]", "beta[2]"))

ggplot(df_plot_400, aes(value)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + 
  facet_grid(. ~ coef2, scales = "free_x", 
             labeller = label_parsed) + 
  geom_vline(data = filter(df_plot_400, coef == "beta_1"), 
             aes(xintercept = coef(prf)[1]), color = "yellow") +
  geom_vline(data = filter(df_plot_400, coef == "beta_2"), 
             aes(xintercept = coef(prf)[2]), color = "green") +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Results of Monte-Carlo study for n = 400")

c(mean_b1 = mean(coef_df_400$beta_1), 
  sd_b1 = sd(coef_df_400$beta_1), 
  mean_b2 = mean(coef_df_400$beta_2), 
  sd_b2 = sd(coef_df_400$beta_2),
  var_b1 = var(coef_df_400$beta_1),
  var_b2 = var(coef_df_400$beta_2))


coef_df = data.frame(coef_df_25, coef_df_100, coef_df_400)
names(coef_df) = c("beta1_25", "beta2_25", "beta1_100", "beta2_100", "beta1_400", "beta2_400")
df_plot = pivot_longer(coef_df, cols = 1:6, names_sep = "_", 
                       names_to = c("coef", "sample_size"))
df_plot$coef2 <- factor(df_plot$coef, labels = c("beta[1]", "beta[2]"))

ggplot(df_plot, aes(value)) + geom_histogram(bins = 30) +
  facet_grid(sample_size ~ coef2, scales = "free_x", labeller = label_parsed) +
  geom_vline(data = filter(df_plot, coef == "beta1"), 
             aes(xintercept = coef(prf)[1]), color = "blue") +
  geom_vline(data = filter(df_plot, coef == "beta2"), 
             aes(xintercept = coef(prf)[2]), color = "red") +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Results of Monte-Carlo study for n = 400, 100, 25")

coef(srf)



##### Question B ###########################################################


##   QQ TEST FOR n=25   ####################################



credit_sample <- slice_sample(df1, n = 25, replace = TRUE)
srf_25 = lm(limit ~ income, data = credit_sample)



srf_25 <- lm(income ~ limit, data = credit_sample)
coef(srf_25)

srf_25_summary <- summary(srf_25)
r2 <- srf_25_summary$r.squared
RSS <- sum(srf_25$residuals^2)
TSS <- sum((credit_sample$income - mean(credit_sample$income))^2)
ESS = r2 / (1 - r2) * RSS      # calculate ESS from RSS and r^2
round(c(r.squared = r2, RSS = RSS, ESS = ESS), digits = 4)

library(ggpubr)
residuals_df <- data.frame(residuals = srf_25_summary$residuals,
                           limit = credit_sample$limit)

# Residual Plot
plot_1_res = ggplot(residuals_df, aes(limit, residuals)) + 
  geom_point(color = "black") +
  ggtitle("Residual Plot for n=25")

# QQ-plot of residuals
plot_1_qq = ggplot(residuals_df, aes(sample = residuals)) + 
  geom_qq(color = "black") + 
  geom_qq_line(color = "red") +
  ggtitle("QQ-Plot of residuals for n=25")
ggarrange(plot_1_res, plot_1_qq, nrow = 2, ncol = 1)


##   QQ TEST FOR n=100   ####################################


credit_sample <- slice_sample(df1, n = 100, replace = TRUE)
srf_100 = lm(limit ~ income, data = credit_sample)



srf_100 <- lm(income ~ limit, data = credit_sample)
coef(srf_100)

srf_100_summary <- summary(srf_100)
r2 <- srf_100_summary$r.squared
RSS <- sum(srf_100$residuals^2)
TSS <- sum((credit_sample$income - mean(credit_sample$income))^2)
ESS = r2 / (1 - r2) * RSS      # calculate ESS from RSS and r^2
round(c(r.squared = r2, RSS = RSS, ESS = ESS), digits = 4)

library(ggpubr)
residuals_df <- data.frame(residuals = srf_100_summary$residuals,
                           limit = credit_sample$limit)

# Residual Plot
plot_2_res = ggplot(residuals_df, aes(limit, residuals)) + 
  geom_point(color = "black") +
  ggtitle("Residual Plot for n=100")

# QQ-plot of residuals
plot_2_qq = ggplot(residuals_df, aes(sample = residuals)) + 
  geom_qq(color = "black") + 
  geom_qq_line(color = "red") +
  ggtitle("QQ-Plot of residuals for n=100")
ggarrange(plot_2_res, plot_2_qq, nrow = 2, ncol = 1)



##   QQ TEST FOR n=400   ####################################

credit_sample <- slice_sample(df1, n = 400, replace = TRUE)
srf_400 = lm(limit ~ income, data = credit_sample)



srf_400 <- lm(income ~ limit, data = credit_sample)
coef(srf_400)

srf_400_summary <- summary(srf_400)
r2 <- srf_400_summary$r.squared
RSS <- sum(srf_400$residuals^2)
TSS <- sum((credit_sample$income - mean(credit_sample$income))^2)
ESS = r2 / (1 - r2) * RSS      # calculate ESS from RSS and r^2
round(c(r.squared = r2, RSS = RSS, ESS = ESS), digits = 4)

library(ggpubr)
residuals_df <- data.frame(residuals = srf_400_summary$residuals,
                           limit = credit_sample$limit)

# Residual Plot
plot_3_res = ggplot(residuals_df, aes(limit, residuals)) + 
  geom_point(color = "black") +
  ggtitle("Residual Plot for n=400")

# QQ-plot of residuals
plot_3_qq = ggplot(residuals_df, aes(sample = residuals)) + 
  geom_qq(color = "black") + 
  geom_qq_line(color = "red") +
  ggtitle("QQ-Plot of residuals for n=400")
ggarrange(plot_3_res, plot_3_qq, nrow = 2, ncol = 1)

ggarrange(plot_1_res, plot_1_qq, plot_2_res, plot_2_qq, plot_3_res, plot_3_qq, nrow = 3, ncol = 2)




##### Question C ##########################################################

##   JB TEST FOR n=25   ####################################



res_c <- srf_25$residuals - mean(srf_25$residuals)          # centered residuals
n <- length(res_c)
S <- 1 / n * sum(res_c^3) / (1 / n * sum(res_c^2))^1.5  # empirical skewness
K <- 1 / n * sum(res_c^4) / (1 / n * sum(res_c^2))^2    # empirical kurtosis
JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
crit <- qchisq(0.95, 2)
c(JB = JB, crit = crit)


##   JB TEST FOR n=100   ####################################



res_c <- srf_100$residuals - mean(srf_100$residuals)          # centered residuals
n <- length(res_c)
S <- 1 / n * sum(res_c^3) / (1 / n * sum(res_c^2))^1.5  # empirical skewness
K <- 1 / n * sum(res_c^4) / (1 / n * sum(res_c^2))^2    # empirical kurtosis
JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
crit <- qchisq(0.95, 2)
c(JB = JB, crit = crit)

##   JB TEST FOR n=400   ####################################


res_c <- srf_400$residuals - mean(srf_400$residuals)          # centered residuals
n <- length(res_c)
S <- 1 / n * sum(res_c^3) / (1 / n * sum(res_c^2))^1.5  # empirical skewness
K <- 1 / n * sum(res_c^4) / (1 / n * sum(res_c^2))^2    # empirical kurtosis
JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
crit <- qchisq(0.95, 2)
c(JB = JB, crit = crit)


