# Chapter 18: Model Basics with modelr

setwd("/Users/noeljohnson/Dropbox/R Course/Learn_Git/R4ds_ch18")

library(tidyverse)
library(modelr)
library(skimr)
library(stargazer)
library(jtools)
library(viridis)
options(na.action = na.warn)

head(sim1)
skim(sim1)
cor(sim1)

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(1000, -20, 40),
  a2 = runif(1000, -5, 5)
)

head(models)
skim(models)
hist(models$a1)
hist(models$a2)

# ggplot(sim1, aes(x, y)) +
#   geom_abline(
#     aes(intercept = a1, slope = a2),
#     data = models, alpha = 1/4
#   ) +
#   geom_point()
# 
# model1 <- function(a, data) {
#   a[1] + data$x * a[2]
# }
# model1(c(7, 1.5), sim1)
# 
# measure_distance <- function(mod, data) {
#   diff <- data$y - model1(mod, data)
#   sqrt(mean(diff ^ 2))
# }
# measure_distance(c(7, 1.5), sim1)
# 
# 
# sim1_dist <- function(a1, a2) {
#   measure_distance(c(a1, a2), sim1)
# }
# models <- models %>%
#   mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
# models
# 
# ggplot(sim1, aes(x, y)) +
#   geom_point(size = 2, color = "grey30") +
#   geom_abline(
#     aes(intercept = a1, slope = a2, color = -dist),
#     data = filter(models, rank(dist) <= 10)
#   )
# 
# ggplot(models, aes(a1, a2)) +
#   geom_point(
#     data = filter(models, rank(dist) <= 10),
#     size = 4, color = "red"
#   ) +
#   geom_point(aes(colour = -dist))
# 
# grid <- expand.grid(
#   a1 = seq(-5, 20, length = 30),
#   a2 = seq(1, 3, length = 30)
# ) %>%
#   mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
# 
# grid %>%
#   ggplot(aes(a1, a2)) +
#   geom_point(
#     data = filter(grid, rank(dist) <= 10),
#     size = 4, colour = "red"
#   ) +
#   geom_point(aes(color = -dist))
# 
# ggplot(sim1, aes(x, y)) +
#   geom_point(size = 2, color = "grey30") +
#   geom_abline(
#     aes(intercept = a1, slope = a2, color = -dist),
#     data = filter(grid, rank(dist) <= 10)
#   )
# 
# best <- optim(c(0, 0), measure_distance, data = sim1)
# best$par
# 
# ggplot(sim1, aes(x, y)) +
#   geom_point(size = 2, color = "grey30") +
#   geom_abline(intercept = best$par[1], slope = best$par[2])
# 
# sim1_mod <- lm(y ~ x, data = sim1)
# coef(sim1_mod)
# predict(sim1_mod, sim1)
# sim1_mod
# names(sim1_mod)
# summary(sim1_mod)
# stargazer(sim1_mod, type = "text")
# summ(sim1_mod)
# summ(sim1_mod, robust = "HC1")
# summ(sim1_mod, scale = TRUE)
# summ(sim1_mod, confint = TRUE, ci.width = .95, digits = 3)
# plot_summs(sim1_mod)
# plot_summs(sim1_mod, scale = TRUE)
# plot_summs(sim1_mod, scale = TRUE, inner_ci_level = .9)
# plot_summs(sim1_mod, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# sim1 <- sim1 %>% mutate(xsquared = x^2)
# sim2_mod <- lm(y ~ x + xsquared, data = sim1)
# plot_summs(sim1_mod, sim2_mod, scale = TRUE)
# plot_summs(sim1_mod, sim2_mod, scale = TRUE, plot.distributions = TRUE)
# plot_summs(sim1_mod, sim1_mod, sim1_mod, scale = TRUE, robust = list(FALSE, "HC0", "HC3"),
#            model.names = c("OLS", "HC0", "HC3"))
# effect_plot(sim1_mod, pred = x, interval = TRUE)
# effect_plot(sim1_mod, pred = x, interval = TRUE, plot.points = TRUE)
# export_summs(sim1_mod, sim2_mod, scale = TRUE)
# 
# 
# grid <- sim1 %>%
#   data_grid(x)
# grid
# 
# grid <- grid %>%
#   add_predictions(sim1_mod)
# grid
# 
# ggplot(sim1, aes(x, y)) +
#   geom_point(size = 1, color = "grey30") +
# geom_point(
#   aes(x, pred),
#   data = grid,
#   colour = "red",
#   size = 3
# )
# 
# sim1 <- sim1 %>%
#   add_residuals(sim1_mod)
# sim1
# 
# ggplot(sim1, aes(resid)) +
#   geom_freqpoly(binwidth = 0.5)
# 
# ggplot(sim1, aes(x, resid)) +
#   geom_ref_line(h = 0) +
#   geom_point()
# 
# # Formulas and Model Families
# 
# df <- tribble(
#   ~y, ~x1, ~x2,
#   4, 2, 5,
#   5, 1, 6
# )
# df
# 
# model_matrix(df, y ~ x1)
# 
# model_matrix(df, y ~ x1 + x2)
# 
# model_matrix(df, y ~ x1 - 1)
# 
# df <- tribble(
#   ~ sex, ~ response,
#   "male", 1,
#   "female", 2,
#   "male", 1
# )
# model_matrix(df, response ~ sex)
# df
# 
# ggplot(sim2) +
#   geom_point(aes(x, y))
# 
# mod2 <- lm(y ~ x, data = sim2)
# summary(mod2)
# 
# grid <- sim2 %>%
#   data_grid(x) %>%
#   add_predictions(mod2)
# grid
# 
# ggplot(sim2, aes(x)) +
#   geom_point(aes(y = y)) +
#   geom_point(
#     data = grid,
#     aes(y = pred),
#     color = "red",
#     size = 4
#   )
# 
# ggplot(sim3, aes(x1, y)) +
#   geom_point(aes(color = x2))
# 
# mod1 <- lm(y ~ x1 + x2, data = sim3)
# mod2 <- lm(y ~ x1 * x2, data = sim3)
# 
# grid <- sim3 %>%
#   data_grid(x1, x2) %>%
#   gather_predictions(mod1, mod2)
# grid
# 
# ggplot(sim3, aes(x1, y, color = x2)) +
#   geom_point() +
#   geom_line(data = grid, aes(y = pred)) +
#   facet_wrap(~ model)
# 
# sim3 <- sim3 %>%
#   gather_residuals(mod1, mod2)
# 
# ggplot(sim3, aes(x1, resid, color = x2)) +
#   geom_point() +
#   facet_grid(model ~ x2)
# 
# mod1 <- lm(y ~ x1 + x2, data = sim4)
# mod2 <- lm(y ~ x1 * x2, data = sim4)
# 
# grid <- sim4 %>%
#   data_grid(
#     x1 = seq_range(x1, 5),
#     x2 = seq_range(x2, 5)
#   ) %>%
#   gather_predictions(mod1, mod2)
# grid
# 
# ggplot(grid, aes(x1, x2)) +
#   geom_tile(aes(fill = pred)) +
#   facet_wrap(~ model)
# 
# ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
#   geom_line() +
#   facet_wrap(~ model)
# ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
#   geom_line() +
#   facet_wrap(~ model)
# 
# # Transformations
# 
# df <- tribble(
#   ~y, ~x,
#   1, 1,
#   2, 2,
#   3, 3
# )
# model_matrix(df, y ~ x^2 + x)
# model_matrix(df, y ~ I(x^2) + x)
# 
# model_matrix(df, y ~ poly(x, 2))
# 
# library(splines)
# model_matrix(df, y ~ ns(x, 2))
# 
# sim5 <- tibble(
#   x = seq(0, 3.5 * pi, length = 50),
#   y = 4 * sin(x) + rnorm(length(x))
# )
# ggplot(sim5, aes(x, y)) +
#   geom_point()
# 
# mod1 <- lm(y ~ ns(x, 1), data = sim5)
# mod2 <- lm(y ~ ns(x, 2), data = sim5)
# mod3 <- lm(y ~ ns(x, 3), data = sim5)
# mod4 <- lm(y ~ ns(x, 4), data = sim5)
# mod5 <- lm(y ~ ns(x, 5), data = sim5)
# 
# grid <- sim5 %>%
#   data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
#   gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")
# 
# ggplot(sim5, aes(x, y)) +
#   geom_point() +
#   geom_line(data = grid, color = "red") +
#   facet_wrap(~ model)
# 
# # Missing Values
# 
# df <- tribble(
#   ~x, ~y,
#   1, 2.2,
#   2, NA,
#   3, 3.5,
#   4, 8.3,
#   NA, 10
# )
# 
# mod <- lm(y ~ x, data = df)
# 
# nobs(mod)

# End Code



















