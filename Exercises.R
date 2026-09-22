#---- Packages ----

rm(list = ls())

# Core data / modeling / plotting
library(tidyverse)     # dplyr, tidyr, ggplot2, readr, etc.
library(broom)         # tidy() on model objects
library(modelsummary)  # compact model tables
library(sandwich)      # robust vcov
library(lmtest)        # coeftest()
library(Ecdat)         # HMDA data
library(ivreg)         # 2SLS
library(fixest)        # FE models (feols) + ggiplot
library(margins)       # AMEs for probit
library(wooldridge)    # injury data (DID)
library(knitr)         # kable() for simple tables
library(rddensity)     # McCrary density test for RDD
library(ggdag)         # DAGs

#---- Exercise 1: Regression ----

# Show the data in the Ecdat package
data("Hmda", package = "Ecdat")

"A data frame containing:
dir  debt payments to total income ratio
hir  housing expenses to income ratio
lvr  ratio of loan size to assessed property value
ccs  consumer credit score from 1 to 6 (lower = better)
mcs  mortgage credit score from 1 to 4 (lower = better)
pbcr public bad credit record?
dmi  denied mortgage insurance?
self self-employed?
single is the applicant single?
uria 1989 Massachusetts unemployment rate in the applicant’s industry
condominium is the unit a condominium?
black is the applicant Black?
deny mortgage application denied?

Source:
Federal Reserve Bank of Boston.

Munnell, Alicia H., Geoffrey M.B. Tootell, Lynne E. Browne, and James McEneaney (1996),
“Mortgage Lending in Boston: Interpreting HMDA Data,” American Economic Review, 25–53.
"

# Round the values of mcs to the nearest integer
Hmda$mcs <- round(Hmda$mcs)

"1. Plot the distribution of `dir`.
Describe what you see. Add the mean of `dir` to the plot. What does this value tell you?"

# Distribution plot
ggplot(Hmda, aes(x = dir)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "black") +
  theme_minimal() +
  xlab("Debt payments to total income ratio") +
  ylab("Number of observations")

"Answer: The distribution of dir is roughly bell-shaped but right-skewed due to some high values.
These values indicate that debt payments for some individuals are very large relative to their incomes."

# Add mean
ggplot(Hmda, aes(x = dir)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "black") +
  theme_minimal() +
  xlab("Debt payments to total income ratio") +
  ylab("Number of observations") +
  geom_vline(aes(xintercept = mean(dir)), color = "blue", linetype = "dashed") +
  geom_text(aes(x = mean(dir) + 0.2, y = 100, label = round(mean(dir), 2)), color = "blue")

"Answer: The mean of dir is about 0.33. On average, 33% of income is used to service debt."

"2. Next, assess the correlation between debt payments and the
relative size of the loan. Use a scatterplot to show the relation between `dir` and `lvr`
and indicate the correlation in the plot. What do you infer from this graph?"

ggplot(Hmda, aes(x = dir, y = lvr)) +
  geom_point() +
  theme_minimal() +
  xlab("Debt payments to total income ratio") +
  ylab("Loan-to-value ratio") +
  geom_text(aes(x = 1, y = 1.5, label = round(cor(Hmda$dir, Hmda$lvr, use = "complete.obs"), 2)),
            color = "blue")

"Answer: The correlation between dir and lvr is about 0.15, a weak positive relationship:
higher debt-to-income ratios are modestly associated with higher loan-to-value ratios."

"3. Add the regression line to the plot and explain how this line is obtained.
What does the slope tell you?"

ggplot(Hmda, aes(x = dir, y = lvr)) +
  geom_point() +
  theme_minimal() +
  xlab("Debt payments to total income ratio") +
  ylab("Loan-to-value ratio") +
  geom_smooth(method = "lm", se = FALSE, color = "red")

"Answer: The regression line minimizes the sum of squared residuals between observed and predicted lvr.
The slope shows the average change in lvr associated with a one-unit increase in dir."

"4. Check whether credit scores relate to debt ratios.
Take `mcs` and plot mean values and standard errors of `dir` across score groups.
Evaluate, taking into account the number of observations per group."

ggplot(Hmda, aes(x = mcs, y = dir)) +
  stat_summary(fun = mean, geom = "point", color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "red") +
  theme_minimal() +
  xlab("Mortgage credit score") +
  ylab("Debt payments to total income ratio")

ggplot(Hmda, aes(x = factor(mcs))) +
  geom_bar(fill = "red", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 2.5, color = "black") +
  theme_minimal() +
  xlab("Mortgage credit score") +
  ylab("Number of observations")

"Answer: Groups with worse mortgage credit scores (higher mcs) tend to have higher average dir, though precision varies.
Most observations are in categories 1 and 2; there are relatively few in categories 3 and 4 (about 62 total), so those means are noisier."

"5. Run a regression using `mcs` and `lvr` to explain variation in `dir`.
Write down the regression equation with subscripts and interpret both coefficients."

# dir_i = b0 + b1*lvr_i + b2*mcs_i + u_i

reg1ex1 <- lm(dir ~ lvr + mcs, data = Hmda)
summary(reg1ex1)

"Answer:
Interpretation of b1 (on lvr): A one-unit increase in loan-to-value is associated with about 0.09 higher dir,
holding mcs fixed. The coefficient is statistically significant at 5% (p < 0.05).

Interpretation of b2 (on mcs): A one-step increase in mortgage credit score (worse credit) is associated
with roughly 0.005 higher dir, holding lvr fixed. The coefficient is not statistically significant (p ≈ 0.243)."

"6. Use the 1989 Massachusetts industry unemployment rate (uria) and verify the regression anatomy
(Frisch–Waugh–Lovell) for the coefficient on `lvr`."

reg2ex1 <- lm(dir ~ lvr + mcs + uria, data = Hmda)
summary(reg2ex1)

reg3ex1 <- lm(lvr ~ mcs + uria, data = Hmda)
summary(reg3ex1)

Hmda$lvrhat <- reg3ex1$residuals

reg4ex1 <- lm(dir ~ lvrhat, data = Hmda)
summary(reg4ex1)
summary(reg2ex1)

"Answer: The coefficient on lvr from the multiple regression equals the coefficient from regressing dir on lvrhat.
FWL shows that the partial effect of lvr is identified from the component of lvr orthogonal to mcs and uria."

"7. You suspect heteroskedasticity. Use the previous regression and compute Huber–White (HC1)
robust standard errors. Does it make a difference?"

# Keep the model object; get robust test separately (don’t overwrite reg2ex1)
coeftest(reg2ex1, vcov = vcovHC(reg2ex1, type = "HC1"))
summary(reg2ex1)

"Answer: Robust SEs are larger, suggesting heteroskedasticity. For example, the SE on lvr rises by about 25.7%.
This does not change lvr’s statistical significance here, but uria’s significance may be affected."

"8. Add the variable `black`. Interpret this new coefficient."

reg5ex1 <- lm(dir ~ lvr + mcs + uria + black, data = Hmda)
coeftest(reg5ex1, vcov = vcovHC(reg5ex1, type = "HC1"))

"Answer: The coefficient on `black` is about 0.017: Black applicants have, on average, roughly 0.017 higher dir,
conditional on other controls. The estimate is statistically significant at 5% (p ≈ 0.0016)."

"9. Check whether the marginal effect of `lvr` differs between Black and non-Black applicants.
Run an interaction and interpret."

reg6ex1 <- lm(dir ~ lvr + mcs + uria + black + lvr*black, data = Hmda)
coeftest(reg6ex1, vcov = vcovHC(reg6ex1, type = "HC1"))

"Answer: The marginal effect of lvr for Black applicants is b1 + b5 (interaction term).
Here, b1 ≈ 0.09 and is significant; b5 ≈ −0.045 and is not significant. The implied total effect for Black applicants
is about 0.045, roughly half the non-Black effect, but the difference is not statistically significant."

"10. Which variables determine loan denials `deny`?
Estimate a linear probability model using dir, mcs, lvr, uria, and black. Interpret the coefficient on `dir`
and evaluate the effect of a one–standard deviation increase in `dir` (0.107)."

Hmda$deny <- as.numeric(Hmda$deny == "yes")
reg7ex1 <- lm(deny ~ dir + lvr + mcs + uria + black, data = Hmda)
coeftest(reg7ex1, vcov = vcovHC(reg7ex1, type = "HC1"))

"Answer: The coefficient on dir is about 0.502: higher DTI is associated with a higher denial probability,
holding other variables fixed. For a +1 SD in dir (0.107), the denial probability rises by about 0.054 (5.4 p.p.)."

"11. Using OLS with a binary dependent variable can be problematic. Make a scatter plot of deny vs dir and add
a regression line. Name two problems."

ggplot(Hmda, aes(x = dir, y = deny)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  xlab("Debt payments to total income ratio") +
  ylab("Loan denial")

"Answer:
1) The LPM can predict probabilities outside [0,1].
2) Errors are heteroskedastic, so usual OLS SEs are invalid."

"12. Run the previous regression with probit and interpret the coefficient on `dir` using the average marginal effect."

reg8ex1 <- glm(deny ~ dir + lvr + mcs + uria + black, data = Hmda, family = binomial(link = "probit"))
summary(reg8ex1)
margins(reg8ex1, variables = "dir")

"Answer: Probit coefficients reflect a latent index and are not marginal effects. The `margins` output provides
the AME: the average change in predicted probability for a one-unit increase in dir. Here, AME(dir) ≈ 0.463
vs. ≈ 0.502 in the LPM."

#---- Exercise 2: Fixed Effects ----

# Read the cornwell.csv file
cornwell <- read.csv("cornwell.csv")

# Load packages to draw DAGs in R
theme_set(theme_dag())

"1. You are interested in the direct effect of more police (relative to population) on crime rates.
You also acknowledge that a higher probability of being arrested relates to crime rates.
Draw a suitable DAG and write down the regression equation.
Mind the correct subscript since you observe counties (i) over several years (t)."

coord_dag <- list(
  x = c(Police = 2, ProbX = 4, Crime = 6),
  y = c(Police = 2, ProbX = 1, Crime = 2)
)

my_dag <- ggdag::dagify(
  Crime  ~ Police,
  ProbX  ~ Police,
  Crime  ~ ProbX,
  coords = coord_dag,
  exposure = "Police",
  outcome  = "Crime"
) %>%
  tidy_dagitty() %>%
  mutate(colour = ifelse(name == "FE", "Unobserved", "Observed"))

my_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = colour)) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag() +
  theme(legend.title = element_blank())

"Answer: More police (Police) raises the probability of arrest (ProbX), which should reduce crime (Crime).
Regression:
Crime_it = β0 + β1 Police_it + β2 ProbX_it + u_it.
Since ProbX is observed, controlling for it reduces omitted-variable bias."

"2. Name two (hard-to-measure) factors related to the policing–crime relationship."

"Answer: Examples: (i) Social norms (law-and-order preferences) that also affect police budgets;
(ii) Police force quality (training, morale, equipment); (iii) Judicial capacity; (iv) Drug-market shocks."

"3. If important controls are unobserved, why is this a problem? Update your DAG to include unobservables."

coord_dag <- list(
  x = c(Police = 2, FE = 4, ProbX = 4, Crime = 6),
  y = c(Police = 2, FE = 3, ProbX = 1, Crime = 2)
)

my_dag <- ggdag::dagify(
  Crime  ~ Police + FE,
  Police ~ FE,
  ProbX  ~ Police + FE,
  coords = coord_dag,
  exposure = "Police",
  outcome  = "Crime"
) %>%
  tidy_dagitty() %>%
  mutate(colour = ifelse(name == "FE", "Unobserved", "Observed"))

my_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = colour)) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag() +
  theme(legend.title = element_blank())

"Answer: If unobserved factors (FE) affect both Police/ProbX and Crime, OLS is biased and inconsistent because
Cov(X, u) ≠ 0. This is an endogeneity problem."

"4. Update the regression with county and year fixed effects."

"Answer:
Crime_it = β_i + τ_t + β1 Police_it + β2 ProbX_it + ε_it,
where β_i are county fixed effects (time-invariant county traits) and τ_t are year fixed effects
(common shocks each year)."

"5. Dig into the data. Produce a scatterplot between `polpc` and `crmrte`."

theme_set(theme_grey())

ggplot(cornwell, aes(x = polpc, y = crmrte)) +
  geom_point() +
  xlab("Police per capita") +
  ylab("Crime rate")

"Answer: The raw scatter suggests a positive association between police per capita and crime rates."

"6. Still worried about fixed effects? Use only counties with ID < 20 and color points by county.
Explain."

cornwell20 <- cornwell[cornwell$county <= 20, ]

ggplot(cornwell20, aes(x = polpc, y = crmrte, color = factor(county))) +
  geom_point() +
  xlab("Police per capita") +
  ylab("Crime rate")

ggplot(cornwell20, aes(x = polpc, y = crmrte, color = factor(county))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Police per capita") +
  ylab("Crime rate") +
  labs(color = "County")  # fixed legend title

"Answer: Within-county slopes vary; some are upward, others downward. Fixed effects use within-county variation
to remove time-invariant cross-county differences."

"7. Run the regression without fixed effects; then add county FE, year FE, and both (two-way FE).
Compare coefficients on `polpc` and `prbarr`. Use heteroskedasticity-robust SEs."

# (a) No FE (keep model; compute robust test separately)
reg1ex2 <- lm(crmrte ~ polpc + prbarr, data = cornwell)
ct_reg1ex2 <- coeftest(reg1ex2, vcov = vcovHC(reg1ex2, type = "HC1"))

# (b) County FE
reg2ex2 <- feols(crmrte ~ polpc + prbarr | county, data = cornwell, vcov = "HC1")
# (c) Year FE
reg3ex2 <- feols(crmrte ~ polpc + prbarr | year, data = cornwell, vcov = "HC1")
# (d) Two-way FE
reg4ex2 <- feols(crmrte ~ polpc + prbarr | county + year, data = cornwell, vcov = "HC1")

# Show table of models
modelsummary::msummary(
  list("No FE (OLS)" = reg1ex2,
       "County FE"   = reg2ex2,
       "Year FE"     = reg3ex2,
       "Two-way FE"  = reg4ex2),
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

mean(cornwell$polpc); sd(cornwell$polpc)
mean(cornwell$prbarr); sd(cornwell$prbarr)
mean(cornwell$crmrte); sd(cornwell$crmrte)

"Answer:
polpc: Without FE, β ≈ 2.092 (significant). With county FE, it falls and may lose significance.
With year FE only, it increases and stays significant. With two-way FE, β ≈ 1.815 (≈ 13.2% below no-FE)
and significant at 5%.

prbarr: Without FE, β ≈ −0.048 (significant). With county FE, it becomes small/insignificant.
With year FE only, it is ≈ −0.048 (as in no-FE). With two-way FE, it shrinks to ≈ −0.004 and is insignificant."

"8. With county and year FE, run three regressions clustering SEs at the county, year, and county×year levels.
Explain."

reg5ex2 <- feols(crmrte ~ polpc + prbarr | county + year, data = cornwell, cluster = ~ county)
reg6ex2 <- feols(crmrte ~ polpc + prbarr | county + year, data = cornwell, cluster = ~ year)
reg7ex2 <- feols(crmrte ~ polpc + prbarr | county + year, data = cornwell, cluster = ~ county + year)

modelsummary::msummary(
  list("Two-way FE (HC1)" = reg4ex2,
       "Cluster: county"  = reg5ex2,
       "Cluster: year"    = reg6ex2,
       "Cluster: county+year" = reg7ex2),
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

"Answer: Clustering adjusts SEs for within-cluster correlation. County clustering handles serial correlation
within counties; year clustering handles shared shocks within years; county+year allows two-way clustering.
SEs typically increase compared to plain HC1, which can affect significance."

"9. Even with FE, you find a positive relation between police and crime. Does that mean policing causes crime?
Explain."

"Answer: Not necessarily. Reverse causality (crime surges trigger more policing) and time-varying confounders can
bias the within estimates. FE remove time-invariant heterogeneity and common year shocks but not endogenous
policy responses. Stronger designs (IV, natural experiments, DiD with credible trends) are needed for causal claims."

#---- Exercise 3: DID ----

# Read the injury data from the wooldridge package
data("injury", package = "wooldridge")

# Rename variables and filter for KY
injury <- injury %>%
  rename(duration = durat, log_duration = ldurat, after_1980 = afchnge) %>%
  filter(ky == 1)

"1. Plot the mean and 95% CI of unemployment durations for low vs high earners
before and after 1980. What can you infer?"

ggplot(injury, aes(x = factor(highearn), y = log_duration)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(after_1980),
             labeller = labeller(after_1980 = c("0" = "Before 1980", "1" = "After 1980"))) +
  ylab("Log duration") + xlab("Low/High earners")

plot_data <- injury %>%
  mutate(highearn = factor(highearn, labels = c("Low earner", "High earner")),
         after_1980 = factor(after_1980, labels = c("Before 1980", "After 1980"))) %>%
  group_by(highearn, after_1980) %>%
  summarize(mean_duration = mean(log_duration),
            se_duration   = sd(log_duration) / sqrt(n()),
            upper = mean_duration + 1.96 * se_duration,
            lower = mean_duration - 1.96 * se_duration,
            .groups = "drop")

ggplot(plot_data, aes(x = after_1980, y = mean_duration, color = highearn)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = highearn)) +
  ylab("Mean log duration") + xlab("") +
  labs(color = "Earnings group")  # fixed legend title

"Answer: The classic DID pattern is visible: high earners appear to have longer unemployment spells after 1980."

"2. Explain how to get the 2x2 DID effect from the previous figure.
Tabulate and calculate by hand."

data_tab <- data.frame(
  Category = c("Low earners", "High earners", "Δ"),
  `Before 1980` = c("A", "C", "C − A"),
  `After 1980`  = c("B", "D", "D − B"),
  `Δ` = c("B − A", "D − C", "(D − C) − (B − A)")
)

kable(data_tab, format = "simple", col.names = c("", "Before 1980", "After 1980", "Δ"))

diffs <- injury %>%
  group_by(after_1980, highearn) %>%
  summarize(mean_duration = mean(log_duration), .groups = "drop")
diffs

before_treatment <- diffs %>% filter(after_1980 == 0, highearn == 1) %>% pull(mean_duration)
before_control   <- diffs %>% filter(after_1980 == 0, highearn == 0) %>% pull(mean_duration)
after_treatment  <- diffs %>% filter(after_1980 == 1, highearn == 1) %>% pull(mean_duration)
after_control    <- diffs %>% filter(after_1980 == 1, highearn == 0) %>% pull(mean_duration)

diff_treat   <- after_treatment - before_treatment
diff_control <- after_control   - before_control
diff_diff    <- diff_treat - diff_control
diff_treat; diff_control; diff_diff

ggplot(diffs, aes(x = as.factor(after_1980),
                  y = mean_duration,
                  color = as.factor(highearn))) +
  geom_point() +
  geom_line(aes(group = as.factor(highearn))) +
  annotate(geom = "segment", x = 0.9, xend = 1.1,
           y = before_treatment, yend = after_treatment - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = 1.0, xend = 1.0,
           y = after_treatment, yend = after_treatment - diff_diff,
           linetype = "dotted", color = "blue") +
  annotate(geom = "label", x = 1.05, y = after_treatment - diff_diff/2,
           label = "Program effect", size = 3) +
  ylab("Mean log duration") + xlab("") +
  labs(color = "Earnings group")

"Answer: The DID effect is (D − C) − (B − A). Here it’s about 0.19 (≈ 19 percentage points in log-points terms)."

"3. Run a DID regression and explain each coefficient."

reg1ex3 <- lm(log_duration ~ highearn + after_1980 + highearn:after_1980, data = injury)
reg1ex3_hc1 <- coeftest(reg1ex3, vcov = vcovHC(reg1ex3, type = "HC1"))
modelsummary::msummary(list(reg1ex3), stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer:
highearn: baseline difference (before 1980) between high and low earners.
after_1980: change over time for low earners (control group).
highearn × after_1980: DID estimate — extra change for high earners relative to low earners after 1980 (≈ 0.19), significant."

"4. Run a DID with individual and time fixed effects. Cluster SEs at ID level.
Why do single terms drop?"

data(base_did)  # example DID panel included in fixest
reg2ex3 <- feols(y ~ treat + period + treat:period | id + period, base_did, cluster = ~ id)
modelsummary::msummary(list(reg2ex3), stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer: With id FE, pure cross-sectional variation in treat is absorbed; with period FE, common time shocks
are absorbed. Only the interaction (treat × period) survives because it varies within id over time."

"5. Estimate period-specific treatment effects (event study) and plot."

reg3ex3 <- feols(y ~ i(period, treat, 5) | id + period, base_did)
modelsummary::msummary(list(reg3ex3), stars = c('*' = .1, '**' = .05, '***' = .01))

ggiplot(reg3ex3, ci_level = .95,
        xlab = "Event periods", ylab = "Estimate (95% CI)", main = "")

ggiplot(reg3ex3, ci_level = .95, aggr_eff = "post",
        aggr_eff.par = list(col = "orange"),
        xlab = "Event periods", ylab = "Estimate (95% CI)", main = "")

"Answer: Coefficients for i(period, treat, 5) are relative to period 5 (the omitted period).
Each shows the treatment–control difference at that event time relative to the reference."

"6. Parallel trends: discuss and use the event-study graph to assess."

# Simple schematic for PT (labels corrected)
data_pt <- data.frame(
  time = rep(c("Pre", "Post"), each = 4),
  state = rep(c("Observed Control", "Observed Treatment",
                "Counterfactual Treatment", "No PT"), times = 2),
  generated_y = c(10, 9, 6, 7,  12, 8, 8, 8)
)

ggplot(data_pt, aes(x = time, y = generated_y, group = state, color = state)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Observed Control" = "black",
                                "Observed Treatment" = "black",
                                "Counterfactual Treatment" = "gray",
                                "No PT" = "gray")) +
  geom_vline(xintercept = 1.5, color = "black", size = 1) +
  annotate("text", x = 2.05, y = 10, label = "Observed Control", hjust = 0, size = 4) +
  annotate("text", x = 2.05, y = 9,  label = "Observed Treatment", hjust = 0, size = 4) +
  annotate("text", x = 2.05, y = 6,  label = "Counterfactual Treatment", hjust = 0, size = 4, color = "blue") +
  annotate("text", x = 2.05, y = 7,  label = "No parallel trend", hjust = 0, size = 4, color = "red") +
  labs(x = "", y = "Y", title = "Parallel Trend Bias (schematic)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "none")

"Answer: The parallel trends assumption means that, absent treatment, treated and control would have moved in parallel.
We assess this by inspecting pre-trends in the event study. Flat, insignificant pre-event coefficients support the assumption."

"7. Heterogeneity by a pre-event covariate: triple-DiD with the individual’s pre-event mean of x1."

base_did <- base_did %>% group_by(id) %>% mutate(x1mean = mean(x1), .groups = "drop_last")
reg4ex3 <- feols(y ~ treat + period + x1mean + treat:period + treat:x1mean +
                   x1mean:period + treat:period:x1mean | id + period,
                 base_did, cluster = ~ id)
modelsummary::msummary(list(reg4ex3), stars = c('*' = .1, '**' = .05, '***' = .01))

# Demean x1mean so the 2x2 DID is at the average x1mean
x1allmean <- mean(base_did$x1mean)
base_did  <- base_did %>% mutate(x1demean = x1mean - x1allmean)

reg5ex3 <- feols(y ~ treat + period + x1demean + treat:period + treat:x1demean +
                   x1demean:period + treat:period:x1demean | id + period,
                 base_did, cluster = ~ id)
modelsummary::msummary(list(reg4ex3, reg5ex3), stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer: In reg4ex3, treat × period is the effect when x1mean = 0; the triple interaction adds the change per +1 in x1mean.
After demeaning, the treat × period coefficient represents the effect at the average x1mean."

#---- Exercise 4: IV ----
 
 # Read in the file angristandkrueger1991.csv
 # (if the 10MB CSV is not present in the repo working directory, fall back
 #  to the identical copy shipped in the `wooldridge` package as `ak91`)
 if (file.exists("angristandkrueger1991.csv")) {
   ak91 <- read.csv("angristandkrueger1991.csv")
 } else {
   ak91 <- wooldridge::ak91
 }

"1. Investigate whether years of schooling predict wages,
controlling for marital status and minority status.
Instrument education with quarter of birth (Qob).
Write first stage, second stage, and reduced form (with individual subscripts)."

"Answer:
Edu_i = a_0 + a_1 Qob_i + a_2 MA_i + a_3 MI_i + v_i   (First stage)
LogW_i = b_0 + b_1 ŜEdu_i + b_2 MA_i + b_3 MI_i + u_i   (Second stage)
LogW_i = g_0 + g_1 Qob_i + g_2 MA_i + g_3 MI_i + r_i   (Reduced form)
"

"2. State and explain the two central IV assumptions."

"Answer:
Relevance: Cov(Qob, Edu) ≠ 0 (a_1 ≠ 0 and significant in the first stage).
Exogeneity (exclusion): Qob affects LogW only through Edu, i.e., Cov(Qob, u_i) = 0."

"3. Why might OLS of LogW_i = l_0 + l_1 Edu_i + l_2 MA_i + l_3 MI_i + e_i be biased?"

"Answer: Omitted factors (e.g., ability) may correlate with both Edu and LogW, biasing l_1."

"4. Explain how Qob works as an instrument here and under which conditions."

# Schematic illustration (your original figure retained)
df <- data.frame(
  group = rep(c("Top", "Bottom"), each = 8),
  quarter = factor(rep(c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4"), 2)),
  xmin = rep(c(0:3, 5:8), 2),
  xmax = rep(c(1:4, 6:9), 2),
  ymin = c(rep(1.5, 8), rep(-0.5, 8)),
  ymax = c(rep(2.5, 8), rep(0.5, 8))
)

ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = "lightgrey", color = "black") +
  geom_vline(xintercept = 4.2, linetype = "dashed", color = "red", size = 1) +
  geom_segment(aes(x = 4, xend = 7.8, y = 1, yend = 1),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_segment(aes(x = 1, xend = 4.8, y = -1, yend = -1),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 3.5, y = 1,  label = "Birthday",     hjust = 0.5, size = 3) +
  annotate("text", x = 8.5, y = 1,  label = "Dropout Age",  hjust = 0.5, size = 3) +
  annotate("text", x = 0.5, y = -1, label = "Birthday",     hjust = 0.5, size = 3) +
  annotate("text", x = 5.5, y = -1, label = "Dropout Age",  hjust = 0.5, size = 3) +
  annotate("text", x = 4.6, y = 0.4, label = "Enrolled in\nSchool", angle = 90, color = "red", size = 3) +
  xlim(-1, 11) + ylim(-1, 3) + theme_void()

"Answer: Because of compulsory schooling laws (enter at ~6; compulsory until 16), quarter of birth shifts entry age
and thus completed education. Assuming date of birth is as-good-as-random relative to ability and affects wages
only via education, Qob is a valid instrument."

"5. Plot average years of schooling by quarter of birth and year of birth.
Use this to discuss instrument relevance."

ak91$Date <- as.Date(paste0(ak91$Yob, "-", ak91$Qob, "-01"))

ggplot(ak91, aes(x = factor(Yob), y = Edu, color = factor(Qob))) +
  stat_summary(fun = "mean", geom = "point", size = 3, position = position_dodge(width = 1)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1,
               position = position_dodge(width = 1)) +
  theme_minimal() +
  xlab("Year of birth") + ylab("Years of schooling") +
  labs(color = "Quarter of birth")

"Answer: A consistent seasonal pattern (e.g., Q4 > Q1) conditional on YOB supports instrument relevance."

"6. Estimate the reduced form and discuss the change from Q1 to Q4."

reg1ex4 <- lm(LogW ~ Qob + MA + MI, data = ak91)
reg1ex4_hc1 <- coeftest(reg1ex4, vcov = vcovHC(reg1ex4, type = "HC1"))
modelsummary::msummary(list(reg1ex4), stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer: Later birth quarters are associated with higher log wages. A Q1→Q4 change (three quarters)
implies ≈ 3×β_Qob increase in LogW (e.g., ≈ 1.5% if β_Qob ≈ 0.005)."

"7. Estimate the first stage and interpret the Qob coefficient. Is the instrument relevant?"

reg2ex4 <- lm(Edu ~ Qob + MA + MI, data = ak91)
reg2ex4_hc1 <- coeftest(reg2ex4, vcov = vcovHC(reg2ex4, type = "HC1"))
modelsummary::msummary(list(reg2ex4), stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer: The Qob coefficient is positive and significant (relevance). A one-quarter increase raises education by
≈ 0.052 years on average."

"8. Show that the 2SLS slope equals reduced form / first stage (ignore controls in the algebra). Compute it."

reg1auxex4 <- lm(LogW ~ Qob + MA + MI, data = ak91)
g1 <- coef(reg1auxex4)[["Qob"]]

reg2auxex4 <- lm(Edu ~ Qob + MA + MI, data = ak91)
a1 <- coef(reg2auxex4)[["Qob"]]

b1 <- g1 / a1
b1

"Answer: b1 = g1 / a1. Algebra: Cov(LogW, ŜEdu)/Var(ŜEdu) = [a1·Cov(LogW, Qob)]/[a1^2 Var(Qob)] = g1/a1."

"9. Run the IV regression (second stage) and interpret β on Edu."

reg3ex4 <- ivreg(LogW ~ Edu + MA + MI | Qob + MA + MI, data = ak91)
modelsummary::msummary(list(reg3ex4), metrics = "all", stars = c('*' = .1, '**' = .05, '***' = .01))

"Answer: β(Edu) > 0 and significant: each additional year of schooling increases wages by ≈ 9.1%."

"10. Briefly discuss the first-stage F-test in IV."

"Answer: The approximate 2SLS bias is inversely related to the first-stage F-statistic. A large F (rule of thumb > 10)
indicates a strong instrument and small finite-sample bias."

#---- Exercise 5: RDD ----

tut <- read.csv("tutoring_program.csv")

"1. Baseline question: are school tutoring programs effective?
Explain why the naive regression Exit_i = a + b·Tutoring_i + e_i is likely biased."

"Answer: Selection and omitted variables (ability, family background) correlate with both tutoring take-up and outcomes,
biasing b."

"2. Setup: students scoring ≤ 70 on an entrance exam are automatically enrolled in free tutoring during the year.
At year-end, students take an exit exam (max 100). Plot entrance scores vs tutoring participation.
Is this a sharp or fuzzy RDD?"

ggplot(tut, aes(x = entrance_exam, y = tutoring, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5, position = position_jitter(width = 0, height = 0.25, seed = 1234)) +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Participated in tutoring program") +
  guides(color = "none")

"Answer: The plot shows a near-perfect split at 70: this looks like a sharp RDD."

"3. Write a suitable RDD regression. What are the running and treatment variables?"

"Answer:
Exit_i = α + γ·Entrance_i + β·Tutoring_i + ε_i.
Running variable: entrance exam score; treatment: Tutoring (indicator for ≤ 70)."

"4. State the key RDD identifying assumption."

"Answer: Continuity: potential outcomes are smooth functions of the running variable at the cutoff,
so any discontinuity in Exit at 70 is attributable to treatment."

"5. Plot the distribution of entrance scores. Use this to argue for/against manipulation."

ggplot(tut, aes(x = entrance_exam, fill = tutoring)) +
  geom_histogram(binwidth = 2, color = "white", boundary = 70) +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Count", fill = "In program")

"Answer: No visible bunching right below/above the cutoff — consistent with no manipulation."

"6. Test for a density jump (McCrary test)."

test_density <- rddensity(tut$entrance_exam, c = 70)
summary(test_density)
rdplotdensity(rdd = test_density, X = tut$entrance_exam, type = "both")

"Answer: Confidence intervals overlap substantially; p-value ≈ 0.58 >> 0.05. No evidence of manipulation at 70."

"7. Plot entrance vs exit scores with local linear fits on each side. Explain the treatment effect."

ggplot(tut, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(data = filter(tut, entrance_exam <= 70), method = "lm") +
  geom_smooth(data = filter(tut, entrance_exam > 70),  method = "lm") +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")

"Answer: The RDD treatment effect is the vertical gap between the two fitted lines at the cutoff."

"8. Explain the smoothness assumption and the counterfactual via extrapolation."

"Answer: Absent treatment, both sides would continue smoothly through the cutoff. Extrapolating the control-side fit
to the treated side at 70 gives the counterfactual for treated students."

"9. Estimate the simple RDD regression and interpret β on Tutoring."

reg1ex5 <- lm(exit_exam ~ entrance_exam + tutoring, data = tut)
tidy(reg1ex5)

"Answer: β(Tutoring) is positive and significant: treated students score, on average, ≈ 10.8 points higher at the cutoff."

"10. Recenter the running variable: Entrance_centered = Entrance − 70. Interpret the constant."

tutoring_centered <- tut %>% mutate(entrance_centered = entrance_exam - 70)

reg2ex5 <- lm(exit_exam ~ entrance_centered + tutoring, data = tutoring_centered)
tidy(reg2ex5)

"Answer: The constant is the predicted exit score at the cutoff for non-treated students (≈ 59.4)."

"11. Estimate bandwidth-restricted regressions using ±10 and ±5 around the cutoff.
What is this called and why is it important? What is the trade-off?"

reg3ex5 <- lm(exit_exam ~ entrance_centered + tutoring,
              data = filter(tutoring_centered, entrance_centered >= -10 & entrance_centered <= 10))
tidy(reg3ex5)

reg4ex5 <- lm(exit_exam ~ entrance_centered + tutoring,
              data = filter(tutoring_centered, entrance_centered >=  -5 & entrance_centered <=   5))
tidy(reg4ex5)

modelsummary(list("Full data" = reg2ex5, "Bandwidth = 10" = reg3ex5, "Bandwidth = 5" = reg4ex5))

"Answer: This is bandwidth choice. Identification comes from observations near the cutoff; narrowing the bandwidth
improves internal validity but reduces precision (smaller n)."

"12. Estimate:
Exit_i = α + γ·Entrance_centered_i + β·Tutoring_i + λ·(Entrance_centered_i × Tutoring_i) + ε_i.
Explain and interpret β."

reg5ex5 <- lm(exit_exam ~ entrance_centered + tutoring + entrance_centered:tutoring, data = tutoring_centered)
tidy(reg5ex5)

"Answer: The interaction allows different slopes on either side. β is the discontinuity at the cutoff (the ATE at 70).
Here, β ≈ 10.8 points and is statistically significant."

"13. Add quadratic terms on each side. What’s the benefit and what pitfall does this address?"

reg6ex5 <- lm(exit_exam ~ entrance_centered + I(entrance_centered^2) + tutoring +
                entrance_centered:tutoring + I(entrance_centered^2):tutoring,
              data = tutoring_centered)
modelsummary(reg6ex5)

"Answer: Allowing curvature guards against spurious ‘jumps’ induced by nonlinear trends in the running variable.
If you omit curvature when present, the treatment effect can be biased. Here, conclusions are robust."