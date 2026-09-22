# ==============================================================================
# Project: Efficiency of Public Spending on Education in the UK
# Description: Exploratory and Predictive analysis of UK Local Authority
#              spending and educational performance metrics
# ==============================================================================

# 1. Download and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, car, ggpubr, scales)

# 2. Download and read data
set.seed(42)
df <- read_csv("~/Downloads/planned-la-and-school-expenditure_2024-25/data/schools_othereducation_la_unrounded_data.csv")
summary(df)

# 3. Cleaning and Transforming
df_cleaned <- df %>% 
  mutate(across(where(is.character), stringr::str_trim)) %>% 
  mutate(
    time_identifier = as.factor(time_identifier),
    geographic_level = as.factor(geographic_level),
    country_name = as.factor(country_name),
    country_code = as.factor(country_code),
    region_name = as.factor(region_name),
    region_code = as.factor(region_code),
    main_category_planned_expenditure = as.factor(main_category_planned_expenditure),
    category_of_planned_expenditure = as.factor(category_of_planned_expenditure)
  ) %>%
  mutate(
    early_years_establishments = as.numeric(early_years_establishments),
    primary_schools = as.numeric(primary_schools),
    secondary_schools = as.numeric(secondary_schools),
    sen_and_special_schools = as.numeric(sen_and_special_schools),
    pupil_referral_units_and_alt_provision = as.numeric(pupil_referral_units_and_alt_provision),
    post_16 = as.numeric(post_16)
  ) %>%
  mutate(
    gross_planned_expenditure = as.numeric(gross_planned_expenditure),
    income = as.numeric(income),
    net_planned_expenditure = as.numeric(net_planned_expenditure),
    net_per_capita_planned_expenditure = as.numeric(net_per_capita_planned_expenditure)
  ) %>%
  distinct() %>% 
  select(
    -country_name, 
    -country_code, 
    -old_la_code, 
    -new_la_code, 
    -region_code, 
    -sen_and_special_schools, 
    -pupil_referral_units_and_alt_provision,
    -post_16
  )

# 4. Verification and Export
summary(df_cleaned)

df_cleaned %>% 
  distinct(region_name)

write_csv(df_cleaned, "raw_spending_data_cleaned.csv")
glimpse(df_cleaned)

# 5. Data Aggregation by Local Authority
df_summary <- df_cleaned %>% 
  filter(geographic_level == "Local authority") %>% 
  group_by(region_name, la_name) %>% 
  summarise(
    total_net_spend = sum(net_planned_expenditure, na.rm = TRUE),
    avg_per_capita_spend = mean(net_per_capita_planned_expenditure, na.rm = TRUE),
    primary_spend = sum(primary_schools, na.rm = TRUE),
    secondary_spend = sum(secondary_schools, na.rm = TRUE),
    .groups = "drop"
  )
summary(df_summary)

# 6. Regional differences with One-Way ANOVA
anova_spending <- aov(avg_per_capita_spend ~ region_name, data = df_summary)
summary(anova_spending)

# 7. Tukey Post-Hoc Test
tukey_results <- TukeyHSD(anova_spending)
print(tukey_results)

# 8. Pearson correlation between Primary and Secondary spending
cor.test(df_summary$primary_spend, df_summary$secondary_spend, method = "pearson")

# 9. Multiple Linear Regression Model
model_education <- lm(primary_spend ~ secondary_spend + avg_per_capita_spend, data = df_summary)
summary(model_education)

# 10. Multicollinearity Check
vif(model_education)

# 10.1. Preparation of diagnostic data for residuals plot
df_diag <- df_summary %>%
  mutate(
    fitted = fitted(model_education) / 1e9,
    residuals = residuals(model_education) / 1e6
  )

# ------------------------------------------------------------------------------
# Visualizations
# ------------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

# Visualization 1: Regional Per Capita Spending
p1 <- ggplot(df_summary, aes(x = reorder(region_name, avg_per_capita_spend, FUN = median),
                             y = avg_per_capita_spend,
                             fill = region_name == "Inner London")) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("FALSE" = "#2b5c8f", "TRUE" = "#d95f02"), guide = "none") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Per Capita Education Spending by Region in the UK",
    subtitle = "Inner London shows significantly higher per capita spending compared to all other regions (p < 0.001)",
    x = "Region",
    y = "Average Per Capita Spending (£)",
    caption = "Source: UK Department for Education | Black diamonds represent regional means"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p1)
ggsave("fig1_regional_per_capita_spending.png", plot = p1, width = 10, height = 6, dpi = 300)

# Visualization 2: Primary vs. Secondary School Expenditure 
custom_gbp_b <- function(x) {
  ifelse(x == 0, "", paste0("£", number(x, accuracy = 0.01), "b"))
}

p2 <- ggplot(df_summary, aes(x = secondary_spend / 1e9, y = primary_spend / 1e9)) +
  geom_point(aes(color = region_name), alpha = 0.8, size = 2.5) +                     
  geom_smooth(method = "lm", color = "#1e3d59", fill = "#1e3d59", alpha = 0.15) +
  scale_x_continuous(
    labels = custom_gbp_b,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_continuous(
    labels = custom_gbp_b,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(values = c(
    "East Midlands"            = "#4e79a7",
    "East of England"          = "#f28e2b",
    "Inner London"             = "#e15759",
    "North East"               = "#76b7b2",
    "North West"               = "#59a14f",
    "Outer London"             = "#edc949",
    "South East"               = "#af7aa1",
    "South West"               = "#ff9da7",
    "West Midlands"            = "#9c755f",
    "Yorkshire and The Humber" = "#bab0ac"
  )) +
  labs(
    title = "Relationship Between Primary and Secondary School Spending",
    subtitle = "Strong linear relationship (r = 0.99, R² = 0.982, p < 2.2e-16) across Local Authorities",
    x = "Secondary School Expenditure (£ Billions)",
    y = "Primary School Expenditure (£ Billions)",
    color = "Region",
    caption = "Source: UK Department for Education"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    axis.title = element_text(face = "bold", size = 11, color = "#333333"),
    axis.text = element_text(color = "#444444"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.4),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

print(p2)
ggsave("fig2_primary_vs_secondary_spending_updated.png", plot = p2, width = 10, height = 6, dpi = 300)

# Visualization 3: Model Diagnostics
fmt_gbp_millions <- function(x) {
  ifelse(x == 0, "", paste0("£", number(x, accuracy = 1), "M"))
}

fmt_gbp_billions <- function(x) {
  ifelse(x == 0, "", paste0("£", number(x, accuracy = 1), "B"))
}

p3 <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7f8c8d", linewidth = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 0.9, alpha = 0.7) +
  geom_point(alpha = 0.5, color = "#34495e", fill = "#2980b9", shape = 21, size = 2.5, stroke = 0.4) +
  scale_x_continuous(
    labels = fmt_gbp_billions,
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  scale_y_continuous(
    labels = fmt_gbp_millions,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title = "Model Residuals vs Fitted Values",
    subtitle = "Evaluation of homoscedasticity and linearity assumptions for education spending model",
    x = "Fitted Values (£ Billions)",
    y = "Residuals (£ Millions)",
    caption = "Source: UK Department for Education | Residuals randomly distributed around zero confirm model fit"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a252f", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = "#576574", margin = margin(b = 15)),
    plot.caption = element_text(size = 8.5, color = "#8395a7", margin = margin(t = 12), hjust = 0),
    axis.title.x = element_text(face = "bold", size = 10, color = "#2c3e50", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", size = 10, color = "#2c3e50", margin = margin(r = 8)),
    axis.text = element_text(size = 9, color = "#576574"),
    panel.grid.major = element_line(color = "#f1f2f6", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15)
  )

print(p3)
ggsave("fig3_model_residuals_publication.png", plot = p3, width = 8, height = 5.2, dpi = 300)
