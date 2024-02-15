# Load necessary libraries
library(haven)   # For read_dta 
library(plm)     # For panel data models
library(lmtest)  # For robust standard errors
library(sandwich) # For robust standard errors
library(stargazer)# For table output
library(rmarkdown)

# Assuming 'data' is your dataframe with the same structure as your STATA data
datapath_clean <- "/Users/julianmarrero/Desktop/STA302 A2"
data <- read_dta(paste0(datapath_clean, "/state_score_data.dta"))

# Make sure the panel identifiers are factors
data$state <- as.factor(data$state)
data$district_id <- as.factor(data$district_id) 

# Calculate 'old_pass' 
data$temp1 <- ifelse(data$year < 2021, data$pass, NA)
data$old_pass <- ave(data$temp1, data$state, data$district_id, FUN = function(x) mean(x, na.rm = TRUE))

# Remove temporary variables
data$temp1 <- NULL

# Check for duplicate id-time combinations and remove them if necessary
data$year <- as.factor(data$year) 
duplicates <- duplicated(data[c("district_id", "year")])
if (any(duplicates)) {
  data <- data[!duplicates, ]
  warning("Duplicate id-time combinations were found and removed.")
}

variables <- c("old_pass", "share_black", "share_hisp", "share_lunch", "share_ELL", "case_rate", "trump_vote_share")

# Initialize a data frame to store the results
results <- data.frame(Variable = rep(variables, each = 3),
                      Model = rep(c("No Fixed Effects", "State Fixed Effects", "Commute Zone Fixed Effects"), times = length(variables)),
                      Estimate = numeric(length(variables) * 3),
                      StdError = numeric(length(variables) * 3),
                      stringsAsFactors = FALSE)


# Loop over the variables to run regressions and store results
for (var in variables) {
  # Model without fixed effects
  model_no_fe <- lm(as.formula(paste("share_inperson ~", var)), data = data, weights = data$EnrollmentTotal)
  no_fe_coefs <- coef(summary(model_no_fe))
  no_fe_robust_se <- sqrt(diag(vcovHC(model_no_fe, type = "HC1")))
  
  # Model with state fixed effects
  model_state_fe <- lm(as.formula(paste("share_inperson ~", var, "+ factor(state)")), data = data, weights = data$EnrollmentTotal)
  state_fe_coefs <- coef(summary(model_state_fe))
  state_fe_robust_se <- sqrt(diag(vcovHC(model_state_fe, type = "HC1")))
  
  # Model with commute zone fixed effects
  model_commzone_fe <- lm(as.formula(paste("share_inperson ~", var, "+ factor(commute_zone)")), data = data, weights = data$EnrollmentTotal)
  commzone_fe_coefs <- coef(summary(model_commzone_fe))
  commzone_fe_robust_se <- sqrt(diag(vcovHC(model_commzone_fe, type = "HC1")))
  
  # Fill in the results data frame
  results[results$Variable == var & results$Model == "No Fixed Effects", c("Estimate", "StdError")] <- c(no_fe_coefs[var, "Estimate"], no_fe_robust_se[var])
  results[results$Variable == var & results$Model == "State Fixed Effects", c("Estimate", "StdError")] <- c(state_fe_coefs[var, "Estimate"], state_fe_robust_se[var])
  results[results$Variable == var & results$Model == "Commute Zone Fixed Effects", c("Estimate", "StdError")] <- c(commzone_fe_coefs[var, "Estimate"], commzone_fe_robust_se[var])
}

# Use stargazer to create a formatted table in the R console
stargazer(results, type = "text", summary = FALSE, 
          title = "Table 2—Pairwise Correlations Between In-Person Learning on District Demographic and Pandemic Variables",
          digits = 3, 
          caption = "Each cell represents a separate regression, weighted by district enrollment. Robust standard errors are reported in parentheses.",
          float = FALSE)

# Create the LaTeX code using stargazer
stargazer(results, type = "latex", out = "regression_results.tex",
          title = "Table 2—Pairwise Correlations Between In-Person Learning on District Demographic and Pandemic Variables",
          header = TRUE, font.size = "footnotesize", digits = 3,
          align = TRUE, column.labels = c("Correlation (no fixed effects)", "Correlation (state fixed effects)", "Correlation (commute zone fixed effects)"),
          initial.zero = FALSE, summary = FALSE)

# Write the LaTeX code to a .tex file
# Save the LaTeX code to a .tex file
writeLines(stargazer_latex, "regression_table.tex")

# Create the R Markdown content as a string
rmd_content <- "
---
title: 'Regression Results'
output: pdf_document
---

```{r results='asis', echo=FALSE}
stargazer::stargazer(results, type = 'text', 
  summary = FALSE, 
  title = 'Table 2—Pairwise Correlations Between In-Person Learning on District Demographic and Pandemic Variables', 
  digits = 3, 
  caption = 'Each cell represents a separate regression, weighted by district enrollment. Robust standard errors are reported in parentheses.',
  float = FALSE)
"

rmd_file <- tempfile(fileext = ".Rmd")
writeLines(rmd_content, rmd_file)
rmarkdown::render(rmd_file, output_file = "/Users/julianmarrero/Desktop/STA302 A2/regression_results.pdf")

