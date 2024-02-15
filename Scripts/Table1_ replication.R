library(dplyr)
library(tidyr)
library(haven)
library(knitr) 
library(gt)

# Assuming you've set your working directory to where the data is located
datapath_clean <- "/Users/julianmarrero/Desktop/STA302 A2"
data <- read_dta(paste0(datapath_clean, "/state_score_data.dta"))

# Generate a grouped count by state and district_id
data_grouped <- data %>%
  count(state, district_id) %>%
  group_by(state) %>%
  summarise(Districts = n())

# Dropping duplicates and calculating counts again for Avg Years (assuming year is a variable)
data_years <- data %>%
  distinct(state, district_id, year, .keep_all = TRUE) %>%
  count(state, district_id) %>%
  group_by(state) %>%
  summarise(Avg_Years = mean(n))

# Calculate shares and adjust percentages
data <- data %>%
  mutate(share_blh = share_black + share_hisp,
         across(c(pass, share_inperson, share_virtual, share_hybrid, share_blh, share_lunch, share_ELL), ~ . * 100),
         pass21 = if_else(year == 2021, pass, NA_real_),
         pass19 = if_else(year == 2019, pass, NA_real_))

# Collapse to first non-missing values and calculate means weighted by EnrollmentTotal
data_summarized <- data %>%
  group_by(state, district_id) %>%
  summarise(share_inperson = first(na.omit(share_inperson)),
            share_hybrid = first(na.omit(share_hybrid)),
            share_virtual = first(na.omit(share_virtual)),
            share_blh = first(na.omit(share_blh)),
            share_lunch = first(na.omit(share_lunch)),
            share_ELL = first(na.omit(share_ELL)),
            EnrollmentTotal = first(na.omit(EnrollmentTotal)),
            .groups = 'drop') %>%
  group_by(state) %>%
  summarise(share_inperson = weighted.mean(share_inperson, w = EnrollmentTotal, na.rm = TRUE),
            share_hybrid = weighted.mean(share_hybrid, w = EnrollmentTotal, na.rm = TRUE),
            share_virtual = weighted.mean(share_virtual, w = EnrollmentTotal, na.rm = TRUE),
            share_blh = weighted.mean(share_blh, w = EnrollmentTotal, na.rm = TRUE),
            share_lunch = weighted.mean(share_lunch, w = EnrollmentTotal, na.rm = TRUE),
            share_ELL = weighted.mean(share_ELL, w = EnrollmentTotal, na.rm = TRUE))

summary_table <- data_grouped %>%
  left_join(data_years, by = "state") %>%
  left_join(data_summarized, by = "state")

# Print or view the table
print(summary_table)
kable(summary_table)

# Creating presentable table
nice_table <- summary_table %>%
  gt() %>%
  tab_header(title = "Summary of Statistics") %>%
  cols_label(
    state = "State",
    Districts = "Number of Districts",
    Avg_Years = "Average Years",
    share_inperson = "In-Person (%)",
    share_hybrid = "Hybrid (%)",
    share_virtual = "Virtual (%)",
    share_blh = "Black & Hispanic (%)",
    share_lunch = "Free/Reduced Lunch (%)",
    share_ELL = "English Language Learners (%)"
    # Removed EnrollmentTotal from here
  ) %>%
  fmt_number(
    columns = vars(share_inperson, share_hybrid, share_virtual, share_blh, share_lunch, share_ELL),
    decimals = 1
  )

print(nice_table)
kable(nice_table)
save_kable(pdf_table, file = "Table 1.pdf")