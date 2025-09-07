## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PulmoDataSets)
library(dplyr)
library(ggplot2)

## ----PulmoDataSets-datasets,echo = TRUE,message = FALSE,warning = FALSE,results = 'markup'----


view_datasets_PulmoDataSets()



## ----bronchitis-Cardiff-plot, fig.width=6, fig.height=4.5, out.width="90%"----

# Summary with .groups = "drop" to avoid the message (stored but not printed)
summary_stats <- bronchitis_Cardiff_df %>%
  group_by(r, rfac) %>%
  summarise(
    mean_cig = mean(cig, na.rm = TRUE),
    mean_poll = mean(poll, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Plot only
ggplot(bronchitis_Cardiff_df, aes(x = cig, y = poll, color = factor(r))) +
  geom_point() +
  labs(
    title = "Cigarette Consumption vs Pollution",
    x = "Cigarette Consumption",
    y = "Pollution Level",
    color = "Bronchitis"
  ) +
  theme_minimal()


## ----UK-smoking-plot, fig.width=6, fig.height=4.5, out.width="90%"------------

smoking_summary <- smoking_UK_tbl_df %>%
  group_by(gender, smoke) %>%
  summarise(avg_amt_weekends = mean(amt_weekends, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(avg_amt_weekends))

ggplot(smoking_summary, aes(x = gender, y = avg_amt_weekends, fill = smoke)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Weekend Smoking by Gender and Smoking Status",
    x = "Gender",
    y = "Average Cigarettes on Weekends",
    fill = "Smoke Status"
  ) +
  theme_minimal()


## ----nicotine-gum-plot, fig.width=6, fig.height=4.5, out.width="90%"----------


# Step 1: Calculate mean success rates (no extra packages)
nicotine_summary <- nicotine_gum_df %>%
  summarize(
    treatment = sum(qt) / sum(tt),  # Overall success rate (treatment)
    control = sum(qc) / sum(tc)      # Overall success rate (control)
  )

# Step 2: Plot (manually reshape data without tidyr)
ggplot(data.frame(
  group = c("Treatment", "Control"),
  success_rate = c(nicotine_summary$treatment, nicotine_summary$control)
), aes(group, success_rate, fill = group)) +
  geom_col(width = 0.5) +
  labs(
    title = "Nicotine Gum vs. Control (Overall Success Rate)",
    y = "Success Rate",
    x = ""
  ) +
  scale_fill_manual(values = c("Treatment" = "#1f77b4", "Control" = "#d62728")) +
  theme_minimal() +
  theme(legend.position = "none")


