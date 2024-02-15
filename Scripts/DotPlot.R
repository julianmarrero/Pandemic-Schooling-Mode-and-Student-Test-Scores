library(dplyr)
library(ggplot2)
library(forcats)
library(haven)
library(tidyr)  # Ensure this package is loaded

# Load data
datapath_clean <- "/Users/julianmarrero/Desktop/STA302 A2"  # Update this path
data <- read_dta(paste0(datapath_clean, "/state_score_data.dta"))

# Calculate change in pass rates
data <- data %>%
  group_by(state, subject) %>%
  mutate(
    pass_change = pass - lag(pass, order_by = year)
  ) %>%
  filter(year %in% c(2021, 2016:2019)) %>%
  # Use summarise to calculate the average change for each state and subject
  summarise(
    change_2021 = mean(pass_change[year == 2021], na.rm = TRUE),
    change_2016_2019 = mean(pass_change[year < 2021], na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("change"),
    names_to = "year_group",
    values_to = "pass_change"
  ) %>%
  mutate(
    year_group = recode(year_group, 
                        change_2021 = "Spring 2021", 
                        change_2016_2019 = "Spring 2016–2019")
  )

# Create the dot plot
dotplot <- ggplot(data, aes(x = pass_change, y = fct_reorder(state, pass_change), color = year_group)) +
  geom_point() +
  scale_color_manual(values = c("Spring 2021" = "black", "Spring 2016–2019" = "grey")) +
  facet_wrap(~subject, scales = "free_y", ncol = 1) +
  labs(
    title = "Average Change in Pass Rates on State Standardized Assessments in Spring 2021 versus Spring 2016–2019",
    x = "Average change in pass rates (percentage points)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed")

# Print the plot
print(dotplot)

# Save the plot to a file
ggsave("pass_rate_comparisons_final.pdf", plot = dotplot, width = 10, height = 7)
