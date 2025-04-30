################################################################################
## INQUISITION IN GIAVENO (1335)
## (3) Castellario: Gender study (Cox regression models)
## R script by Davor Salihovic (Uni of Antwerp) & Jose Luis Estevez (Uni of Helsinki)
## Date: Apr 29th, 2025
################################################################################

# R PACKAGES REQUIRED ----
library(data.table);library(tidyr);library(dplyr) # for data wrangling
library(ggplot2);library(ggpubr) # for visualizations
library(survival);library(coxme) # for event-history analyses
library(splines) # for adding splines
library(performance) # for assumption and performance checks
library(sjPlot) # for model's presentation
library(rstanarm);library(brms);library(bayesplot) # for Bayesian models
library(parallel) # to use several cores
library(posterior) # to extract posteriors

# DATA LOADING 
rm(list=ls())
data <- read.csv("data/data3.csv")

# Set theme 
source('0_Castellario_gender_theme.R')
theme_paper <- theme_paper + theme(
  text = element_text(family = "serif", size = 14),
  legend.title = element_blank(),
  legend.position = "bottom",
  legend.direction = "vertical",
  legend.text = element_text(size = 14),
  plot.title = element_text(size = 14, hjust = 0.5)
)
theme_set(theme_paper)

################################################################################

# VARIABLE TRANSFORMATION ----
d <- data %>% 
  mutate(day_std = scale(day),
         denunc_std = scale(denunc),
         kin_denunc_std = scale(kin_denunc),
         day_acc = day_accused + 1,
         day_acc_std = scale(day_acc),
         w_time = woman * time1, # gender int. with time
         w_ln_time = woman * log(time1), # gender int. with log-time
         d_time = day_acc_std * time1, # day of accusation int. with time
         d_ln_time = day_acc_std * log(time1)) # day of accusation int. with log time

# BASE MODEL ----
m1 <- coxph(Surv(time0, time1, summons) ~ woman + day_acc_std + denunc_std + 
              kin_denunc_std + martinus + franciscus + host, 
            data = d, ties = "efron")
summary(m1)

# Check proportionality
transform <- c("identity", "log", "km", "rank")
for (i in (transform)) {
  print(round(cox.zph(m1, transform = i)$table,3)) # rounded to 3 decimals
}

ref_lines <- data.frame(
  yintercept = c(0, coef(m1)[1], coef(m1)[2]),
  label = c("No effect (hazard ratio = 1)", "Estimated effect for gender", "Estimated effect for day of accusation")
)

# Gender
ph_ident <- cox.zph(m1, transform = "identity")[1] # behaviour across time      
ph_log <- cox.zph(m1, transform = "log")[1] # behaviour across log-time

fit_ident <- lm(ph_ident$y ~ ns(ph_ident$x, df = 3)) # use natural splines to smooth out the residuals
fit_ident <- predict.lm(fit_ident, interval = "confidence")
fit_ident <- as.data.frame(fit_ident) %>% mutate(t = ph_ident$x)

fit_log <- lm(ph_log$y ~ ns(ph_log$x, df = 3))
fit_log <- predict.lm(fit_log, interval = "confidence")
fit_log <- as.data.frame(fit_log) %>% mutate(t = ph_log$x)

# Visualization
schi <- ggplot(fit_ident, aes(x = t, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey", linewidth = .5, alpha = .5, col = "grey40", lty = 2) +
  geom_line(linewidth = 0.7) +
  geom_hline(data = ref_lines, aes(yintercept = yintercept, linetype = label), linewidth = 0.5) +
  scale_linetype_manual(values = c("No effect (hazard ratio = 1)" = "dashed", "Estimated effect for gender" = "dotted")) +
  labs(title="", x="Analysis time", y="Smoothed scaled Schoenfeld residuals") +
  coord_cartesian(xlim = c(1, 20), ylim = c(-4, 4))

schl <- ggplot(fit_log, aes(x = t, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey", linewidth = .5, alpha = .5, col = "grey40", lty = 2) +
  geom_line(linewidth = 0.7) +
  geom_hline(data = ref_lines, aes(yintercept = yintercept, linetype = label), linewidth = 0.5) +
  scale_linetype_manual(values = c("No effect (hazard ratio = 1)" = "dashed", "Estimated effect for gender" = "dotted")) +
  labs(title="", x="Log analysis time", y="") +
  coord_cartesian(xlim = c(0, 3), ylim = c(-4, 4))

# Day of accusation
ph_ident_day <- cox.zph(m1, transform = "identity")[2] # behaviour across time     
ph_log_day <- cox.zph(m1, transform = "log")[2] # behaviour across log-time

fit_ident_day <- lm(ph_ident_day$y ~ ns(ph_ident_day$x, df = 3))
fit_ident_day <- predict.lm(fit_ident_day, interval = "confidence")
fit_ident_day <- as.data.frame(fit_ident_day) %>% mutate(t = ph_ident_day$x)

fit_log_day <- lm(ph_log_day$y ~ ns(ph_log_day$x, df = 3))
fit_log_day <- predict.lm(fit_log_day, interval = "confidence")
fit_log_day <- as.data.frame(fit_log_day) %>% mutate(t = ph_log_day$x)

schiday <- ggplot(fit_ident_day, aes(x = t, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey", linewidth = .5, alpha = .5, col = "grey40", lty = 2) +
  geom_line(linewidth = 0.7) +
  geom_hline(data = ref_lines, aes(yintercept = yintercept, linetype = label), linewidth = 0.5) +
  scale_linetype_manual(values = c("No effect (hazard ratio = 1)" = "dashed", "Estimated effect for day of accusation" = "dotted")) +
  labs(title="", x="Analysis time", y="Smoothed scaled Schoenfeld residuals") +
  coord_cartesian(xlim = c(1, 20), ylim = c(-4, 4)) 

schlday <- ggplot(fit_log_day, aes(x = t, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey", linewidth = .5, alpha = .5, col = "grey40", lty = 2) +
  geom_line(linewidth = 0.7) +
  geom_hline(data = ref_lines, aes(yintercept = yintercept, linetype = label), linewidth = 0.5) +
  scale_linetype_manual(values = c("No effect (hazard ratio = 1)" = "dashed", "Estimated effect for day of accusation" = "dotted")) +
  labs(title="", x="Log analysis time", y="") +
  coord_cartesian(xlim = c(0, 3), ylim = c(-4, 4))

# Arrange all plots
sch <- ggarrange(schi, schl, common.legend = TRUE, labels = c('A','B'), legend = "bottom")
schday <- ggarrange(schiday, schlday, common.legend = TRUE, labels = c('C','D'), legend = "bottom")
schoen <- ggarrange(sch, schday, ncol = 1, heights = c(1, 1))

tiff(filename="Fig8.tiff",
     width=20, height=24,units="cm", 
     compression="lzw",bg="white",res=1000)
schoen
dev.off()
   # best to use woman * ident. time and day * log-time

################################################################################

# OTHER MODELS ----

# Models in manuscript:
m2 <- coxph(Surv(time0, time1, summons) ~ woman + day_acc_std + 
              denunc_std + kin_denunc_std + martinus + franciscus + host + 
              w_time + d_ln_time, data = d, ties = "efron")

m3 <- coxme(Surv(time0, time1, summons) ~ woman + day_acc_std + 
              denunc_std + kin_denunc_std + martinus + franciscus + host + 
              w_time + d_ln_time + (1 | day), data = d, ties = "efron")

m4 <- coxme(Surv(time0, time1, summons) ~ woman + day_acc_std + 
              denunc_std + kin_denunc_std + martinus + franciscus + host + 
              w_time + d_ln_time + (1 | day) + (1 | id), data = d, ties = "efron")

# Show side by side
sjPlot::tab_model(m1,m2,m3,m4,
                  transform = NULL,show.se=TRUE,show.ci=FALSE,digits=3,
                  dv.labels = paste('Model',1:4,sep=' '))

# Check multicollinearity
check_collinearity(m1) # low
check_collinearity(m2) # high for day_acc_std and d_ln_time
check_collinearity(m3) # weakly moderate
check_collinearity(m4) # weakly moderate

# Models not in manuscript:
# Include random slope for gender across days
m5 <- coxme(Surv(time0, time1, summons) ~ woman + day_acc_std + 
              denunc_std + kin_denunc_std + martinus + franciscus + host + 
              w_time + d_ln_time + (1 + woman | day) + (1 | id), data = d, ties = "efron")

# Anova tests
anova(m1, m2)
anova(m1, m3)
anova(m1, m4)
anova(m3, m4)
AIC(m1, m2, m3, m4, m5)
BIC(m1, m2, m3, m4, m5)

# Test the effect of gender against random effects
m5.1 <- coxme(Surv(time0, time1, summons) ~ day_acc_std + 
                denunc_std + kin_denunc_std + martinus + franciscus + host + 
                d_ln_time + (1 + woman | day) + (1 | id), data = d, ties = "efron")
m5.2 <- coxme(Surv(time0, time1, summons) ~ woman + day_acc_std + 
                denunc_std + kin_denunc_std + martinus + franciscus + host + 
                d_ln_time + (1 + woman | day) + (1 | id), data = d, ties = "efron")

sjPlot::tab_model(m5,m5.1,m5.2,
                  transform = NULL,show.se=TRUE,show.ci=FALSE,digits=3,
                  dv.labels = c('Model 5','Model 5.1','Model 5.2'))

anova(m5.1, m5.2, m5)
  # model 5 holds its own.  

ranef_m5 <- ranef(m5)$day
plot(ranef_m5[, "Intercept"], ranef_m5[, "woman"],
     xlab = "Random intercept (hazard)",
     ylab = "Random slope for var. woman")
lines(smooth.spline(ranef_m5[, "woman"] ~ ranef_m5[, "Intercept"], df = 3), col = "red")
  # On days when hazard is generally high, being a woman brings it down, 
    # and when it rises, days have generally low hazard, hence the -.83 corr.
sum(ranef_m5[, "Intercept"] < 0 & ranef_m5[, "woman"] > 0)
length(ranef_m5[, "Intercept"]) - sum(ranef_m5[, "Intercept"] < 0 & ranef_m5[, "woman"] > 0)
      # There are plenty cases where the slope is positive and hazard is comparatively low.
sum((ranef_m5[, "Intercept"] + ranef_m5[, "woman"]) > 0)
      # And there are plenty cases where being a woman brings the proportional hazard above 0.
      # These days are the first few days and the days from day 24 to the end:
(ranef_m5[, "Intercept"] + ranef_m5[, "woman"]) > 0
        # This pattern aligns with our findings that the hazard for women is not consistently
          # proportional, but rises with time and matches the Schoen. plots.

x_vals <- c(0, 1)
plot(0, xlim = c(0, 1), ylim = c(-4, 4), type = "n", xlab = "Woman", ylab = "Hazard")
for (i in 1:nrow(ranef_m5)) {
  intercept <- ranef_m5[i, 1]
  slope <- ranef_m5[i, 2]
  y_vals <- intercept + slope * x_vals
  lines(x_vals, y_vals, col = rgb(1, 0, 0, alpha = 0.5))
}

  # Likelihood ratio for m3 (against null model) with various variances for day
var <- seq(0.00001, 10, length = 100) 
l <- 0*var
for (i in 1:length(var)) {
  m <- coxme(Surv(time0, time1, summons) ~ woman + day_acc_std + denunc_std + 
                  kin_denunc_std + martinus + franciscus + host + w_time + d_ln_time +
                  (1 | day), 
                data = d, ties = "efron",
             vfixed = list(day = var[i]))  # Use vfixed instead of variance
  l[i] <- 2 * diff(m$loglik[1:2])
}
plot(l ~ var, xlab = "Variance", ylab = "Likelihood ratio", ylim = c(100, 150), type = "l")
points(l ~ var, col = "red")

# ----
ms6 <- stan_surv(Surv(time0, time1, summons) ~ woman + day_acc_std + denunc_std + 
                  kin_denunc_std + martinus + franciscus + host + w_time + d_ln_time + # avoid tvc() with whatever df, b-splines take an eternity
                  (1 + woman | day), 
                basehaz = "ms",
                basehaz_ops = list(df = 15),
                cores = parallel::detectCores(), chains = 4,
                prior = normal(0, 3),
                prior_intercept = normal(0, 3),
                iter = 4000, warmup = 1000,
                data = d,
                diagnostic_file = file.path(tempdir(), "df_ms6.csv"))
saveRDS(ms6, "ms6.rds") # save the model

# Check Bayesian model
summary(ms6)
plot(ms6) # hazard rate

# Extract posterior draws for fixed effects
fixed_effects <- as.data.frame(ms6)
fixed_effects <- fixed_effects[, grep("^woman$|^day_acc_std$|^denunc_std$|^kin_denunc_std$|^martinus$|^franciscus$|^host$|^w_time$|^d_ln_time$", names(fixed_effects))]

# Define new names
new_names <- c(
  "woman" = "Gender (woman)",
  "day_acc_std" = "Day of accusations",
  "denunc_std" = "Number of denunciations received",
  "kin_denunc_std" = "Number of kin accused",
  "martinus" = "Acquaintance with Martin",
  "franciscus" = "Acquaintance with Francis",
  "host" = "Host to congregation(x)",
  "w_time" = "Gender (woman) x time",
  "d_ln_time" = "Day of accusation x ln(time)"
)
# Reverse the order (last variable will appear at the top)
new_names_reversed <- rev(new_names)

# Show the 89% and 95% credible intervals
tiff(filename="Fig9.tiff",
     width=32, height=12,units="cm", 
     compression="lzw",bg="white",res=1000)
bayesplot::color_scheme_set("gray")  # Sets default colors to grey
bayesplot::mcmc_intervals(
  fixed_effects,
  prob = 0.89, # inner credible interval (89%)
  prob_outer = 0.95, # outer credible interval (95%)
  point_est = "mean" # Show mean instead of median (optional)
) +
  scale_y_discrete(labels = new_names_reversed) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(
    limits = names(new_names_reversed), # Reverse the order
    labels = new_names_reversed         # Apply custom labels
  ) +
  labs(x = "Posterior fixed effects (89% and 95% credible intervals)") +
  theme_paper
dev.off()

# For hypothesis testing, 89% CI intervals (recommended in Bayesian analyses)
hypothesis(ms6, "woman = 0", scope = "coef", alpha= 0.11)
hypothesis(ms6, "w_time = 0", scope = "coef", alpha = 0.11)
VarCorr(ms6)

mcmc_dens_overlay(ms6, pars = c("woman", "day_acc_std", "denunc_std", "kin_denunc_std", "martinus",
                                "franciscus", "host", "w_time", "d_ln_time", "Sigma[day:woman,(Intercept)]"))

results_b <- as.data.frame(summarise_draws(ms6, mean)) %>%
  filter(grepl("woman day", variable))
results_I <- as.data.frame(summarise_draws(ms6, mean)) %>%
  filter(grepl("\\(Intercept) day", variable))
results <- as.data.frame(cbind(results_I$mean, results_b$mean))
results$day <- 1:nrow(results)
results$hr <- results$V1 + results$V2
results

plot(hr ~ day, results)
lines(smooth.spline(results$hr ~ results$day, df = 4), type = "l")
  # m-spline replicates the initial period, then gets a bit chaotic, but tracks the general trend
    # of women's hazard rising later.

################################################################################