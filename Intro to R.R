############################################################
# R for Economists 
# Goals:
#   • RStudio workflow, objects, data frames
#   • Data wrangling (dplyr), quick EDA
#   • Plotting with ggplot2
#   • OLS + robust SEs (brief)
#   • Export results: PDF/Word/LaTeX/Excel/CSV
#   • Publication-ready plots: colorblind-safe & printer-friendly
############################################################

#### 0) First run: Install packages (uncomment ONCE if needed)
# - These commands install packages from CRAN.
# - Do this only the first time; afterwards keep them commented.
# install.packages(c("tidyverse","broom","sandwich","lmtest","margins","wooldridge",
#                    "modelsummary","writexl","rmarkdown","viridis","ggtext"))
# For Word table export via modelsummary:
# install.packages(c("flextable","officer"))
# For high-quality PDF plots via cairo_pdf on Windows:
# install.packages("Cairo")

#### 1) Load packages we use today
# - library() makes a package available in your current session.
# - If you get an error "there is no package called ...", go back to Section 0.
library(tidyverse)   # data wrangling (dplyr), ggplot2, readr, tibble, etc.
library(broom)       # tidy model outputs (glance/tidy/augment)
library(sandwich)    # heteroskedasticity-robust covariance matrices
library(lmtest)      # coeftest() to print models with robust SEs
library(margins)     # marginal effects (optional today)
suppressWarnings(suppressMessages(library(wooldridge))) # example datasets
library(modelsummary) # quick model tables + file export
library(writexl)     # write_xlsx() for Excel exports
library(rmarkdown)   # render() to knit R Markdown reports
library(viridis)     # colorblind-safe palettes for plots
library(ggtext)      # rich text in ggplot (optional for nicer captions)

# A fixed seed makes random examples reproducible across runs
set.seed(123)

cat("\n=== Welcome to R for Economists ===\n",
    "We’ll learn a minimal workflow for data, plots, OLS,\n",
    "and exporting outputs to PDF/Word/LaTeX/Excel.\n",
    "Run each section with Ctrl/Cmd + Enter.\n\n")

############################################################
# 2) RStudio orientation (read-only notes)
############################################################
# • Source pane: where your script (this file) lives.
# • Console: where code runs immediately.
# • Environment: shows objects you created (data, models, numbers).
# • Plots/Help/Files: view figures, get documentation, see files.
# • Run code: place cursor on a line or select lines, then Ctrl/Cmd + Enter.
# • Help for a function: ?function_name  (example: ?lm)

############################################################
# 3) R basics: objects, vectors, data frames (run this)
############################################################

# Create simple objects. The arrow <- assigns a value to a name.
x <- 10                 # a single number
y <- c(1, 4, 9)         # a numeric vector (c() combines values)
name <- "Econometrics"  # a string
flag <- TRUE            # a logical (TRUE/FALSE)

# Basic operations and checks:
x + 3            # arithmetic
mean(y)          # function call on a vector
length(y)        # vector length
class(name)      # object class/type
is.logical(flag) # test for logical

# Build a small table (a tibble is a modern data frame):
my_df <- tibble(
  id = 1:3,
  score = c(72,85,90),
  female = c(0,1,1)
)
my_df         # print the tibble
str(my_df)    # compact structure summary

############################################################
# 4) Data for today: load or create a usable dataset
############################################################
# We try to use wooldridge::wage1 (classic wage dataset).
# If not available, we fall back to a remapped mtcars for demonstration.

use_wage1 <- "wage1" %in% data(package = "wooldridge")$results[, "Item"]
if (use_wage1) {
  data("wage1", package = "wooldridge")
  d <- as_tibble(wage1)
  cat("\nData loaded: wooldridge::wage1 (", nrow(d), " rows)\n\n", sep = "")
} else {
  # Fallback: create variables with similar names so the script still runs.
  d <- as_tibble(mtcars) %>%
    mutate(wage = mpg, educ = cyl, exper = hp, female = as.integer(am == 0))
  cat("\nFallback data: mtcars remapped to wage/educ/exper/female.\n\n")
}

# Peek at the data: glimpse() shows types; summary() shows quick stats
glimpse(d)
summary(d)

############################################################
# 5) Wrangling with dplyr: select, rename, mutate, group_by (core skills)
############################################################
# We:
# 1) select relevant variables (if present),
# 2) normalize names to lower case,
# 3) drop missing values in key variables,
# 4) create log wage, a labeled gender factor, and exper^2,
# 5) drop rows where lwage ended as NA (e.g., non-positive wage).
d_clean <- d %>%
  select(any_of(c("wage","educ","exper","tenure","female"))) %>%
  rename_with(tolower) %>%
  drop_na(wage, educ, exper, female) %>%
  mutate(
    lwage = ifelse(wage > 0, log(wage), NA_real_),
    fem   = factor(female, levels = c(0,1), labels = c("male","female")),
    exp2  = exper^2
  ) %>%
  drop_na(lwage)

# Always re-check what you built:
glimpse(d_clean)

# Example: group summaries (average by gender)
desc_by_gender <- d_clean %>%
  group_by(fem) %>%
  summarise(
    n        = n(),
    mean_w   = mean(wage),
    mean_lw  = mean(lwage),
    mean_ed  = mean(educ),
    mean_exp = mean(exper),
    .groups = "drop"
  )
cat("\nDescriptives by gender:\n"); print(desc_by_gender)

############################################################
# 6) Plotting with ggplot2: three common plots
############################################################
# • ggplot(data, aes(...)) sets the input data and aesthetic mappings
# • + geom_* adds a geometry (bars, points, lines…)
# • + labs() adds labels; + theme_* controls overall look

# Histogram: distribution of wage
p_hist <- ggplot(d_clean, aes(x = wage)) +
  geom_histogram(bins = 30, color = "white", fill = "#3182bd") +
  labs(title = "Distribution of hourly wage", x = "Wage", y = "Count")
print(p_hist)

# Scatter + linear fit: wage vs education, colored by gender
p_scatter <- ggplot(d_clean, aes(x = educ, y = wage, color = fem)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +  # draws OLS line (per color) with CI band
  labs(title = "Wage vs Education by Gender",
       x = "Years of education", y = "Hourly wage", color = "Gender")
print(p_scatter)

# Boxplot: compare distributions across groups
p_box <- ggplot(d_clean, aes(x = fem, y = wage, fill = fem)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Wage distribution by gender", x = "Gender", y = "Hourly wage") +
  guides(fill = "none")   # legend is redundant for x-grouped boxplots
print(p_box)

############################################################
# 7) OLS regression + robust SEs (quick taste)
############################################################
# Fit a basic OLS: wage ~ educ
m1 <- lm(wage ~ educ, data = d_clean)
cat("\n=== OLS: wage ~ educ ===\n"); print(summary(m1))
cat("\nInterpretation hint: β_educ ≈ ",
    round(coef(m1)["educ"], 3),
    " means each extra year of education is associated with that many wage units on average.\n", sep = "")

# A richer model with exper and gender
m2 <- lm(wage ~ educ + exper + I(exper^2) + fem, data = d_clean)
cat("\n=== OLS: wage ~ educ + exper + exper^2 + fem ===\n"); print(summary(m2))

# Robust standard errors (HC1) can change inference if heteroskedasticity is present
cat("\n=== Robust SEs (HC1) for M2 ===\n")
print(coeftest(m2, vcov. = vcovHC(m2, type = "HC1")))

############################################################
# 8) EXPORTING RESULTS (PDF / Word / LaTeX / Excel / CSV)
#    — This is crucial for sharing your work outside R
############################################################

# 8a) Export CLEAN DATA you created
# • CSV is universal and small; Excel is familiar to collaborators.
write_csv(d_clean, "wage_clean.csv")
cat("\nSaved: wage_clean.csv (open in a spreadsheet or text editor)\n")

write_xlsx(d_clean, "wage_clean.xlsx")
cat("Saved: wage_clean.xlsx (Excel workbook)\n")

# 8b) Export PLOTS in high quality
# • PNG (raster) is good for slides/screens (use dpi >= 300).
# • PDF (vector) prints crisply at any size (great for journals).
ggsave("wage_hist.png", p_hist, width = 6, height = 4, dpi = 300)
# On Windows, cairo_pdf comes from Cairo; on macOS/Linux it's built-in.
ggsave("wage_hist.pdf", p_hist, width = 6, height = 4, device = cairo_pdf)
cat("Saved plots: wage_hist.png (raster) and wage_hist.pdf (vector)\n")

# 8c) Export REGRESSION TABLES with modelsummary
# • modelsummary detects output by file extension.
models <- list("(1) wage ~ educ" = m1, "(2) + exper + exper^2 + fem" = m2)

# HTML report (view in browser)
modelsummary(models, output = "wage_models.html")
cat("Saved: wage_models.html (open in your browser)\n")

# LaTeX table (.tex) — paste into your LaTeX paper
modelsummary(models, output = "wage_models.tex")
cat("Saved: wage_models.tex (compile in LaTeX if needed)\n")
