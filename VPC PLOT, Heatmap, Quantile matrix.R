# ============================================================
# AIM 1 FINAL SCRIPT
# Multilevel Variance Partitioning of GDM-related Risk Factors
# ============================================================

# ------------------------------------------------------------
# PURPOSE OF THIS SCRIPT
# ------------------------------------------------------------
# This script performs Aim 1 of the study.
# It creates binary GDM-related risk variables and examines how much
# variation in each risk factor is attributable to:
#   1. State level
#   2. District level
#   3. Individual level
#
# The script produces four figures:
#   Figure 1: Overall VPC by state, district, and individual level
#   Figure 2: State-specific district-level VPC heatmap
#   Figure 3: State risk quintile count matrix
#   Figure 4: Highest-risk quintile matrix by state and correlate
#
# It also exports supporting CSV tables.
# ------------------------------------------------------------


# ============================================================
# 0. INSTALL AND LOAD REQUIRED PACKAGES
# ============================================================

# List of packages required for this analysis
packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "lme4", "broom.mixed",
  "janitor", "forcats", "stringr", "purrr", "writexl",
  "performance", "sjPlot", "survey", "patchwork", "scales"
)

# Install missing packages automatically
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages) > 0){
  install.packages(new_packages)
}

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(broom.mixed)
library(janitor)
library(forcats)
library(stringr)
library(purrr)
library(writexl)
library(performance)
library(sjPlot)
library(survey)
library(patchwork)
library(scales)


# ============================================================
# 1. IMPORT DATA
# ============================================================
gdm2 <- read_excel("D:\\ICMR _ GDM\\NFHS_GDM\\Gdm_Final.xlsx")

# ============================================================
# 2. BASIC FREQUENCY CHECKS FOR INPUT VARIABLES
# ============================================================

# This checks whether the coded variables have expected categories.
# It is important before creating binary risk variables.

input_vars <- c(
  "agegrp",         # age
  "Education",           # education
  "Caste_cat",           # caste
  "Religion_grp",     # religion
  "Wealth",           # wealth
  "Residence",      # residence
  "BMI_cat",        # bmi
  "Trimester",      # trimester
  "Parity",         # parity
  "Hypertension",   # hypertension
  "Thyroid2",       # thyroid (if exists)
  "Alcohol",        # alcohol (if exists)
  "Diet"          # diet

)
# Print frequency tables only for variables that exist in the dataset
existing_input_vars <- input_vars[input_vars %in% names(gdm2)]

lapply(gdm2[, existing_input_vars], table, useNA = "ifany")


# ============================================================
# 3. CREATE BINARY GDM-RELATED RISK VARIABLES
# ============================================================

# Each risk variable is coded as:
#   1 = risk category present
#   0 = risk category absent
#
# These categories should match the coding in your dataset.
# Please verify each original variable coding before final manuscript use.
  


gdm2 <- gdm2 %>%
  mutate(
    # 1. Age risk: age >= 25 years
    # Assumption: age categories 3, 4, and 5 represent age >= 25 years.
    age_risk = ifelse(agegrp %in% c(3, 4, 5), 1, 0),
    
    # 2. Education risk: no education
    # Assumption: education category 0 represents no education,
    # while categories 1, 2, and 3 represent primary, secondary, and higher education.
    education_risk = ifelse(Education == 0, 1, 0),
    
    # 3. Caste risk: SC/ST
    # Assumption: caste categories 1 and 2 represent SC/ST.
    caste_risk = ifelse(Caste_cat %in% c(1, 2), 1, 0),
    
    # 4. Religion risk: minority religion
    # Assumption: religion categories 2, 3, and 4 represent minority religions.
    religion_risk = ifelse(Religion_grp %in% c(2, 3, 4), 1, 0),
    
    # 5. Wealth risk: poorest/poorer
    # Assumption: wealth categories 1 and 2 represent poorest and poorer groups.
    wealth_risk = ifelse(Wealth %in% c(1, 2), 1, 0),
    
    # 6. Rural residence risk
    # Assumption: residence category 2 represents rural residence.
    rural_risk = ifelse(Residence == 2, 1, 0),
    
    # 7. BMI risk: overweight/obese
    # Assumption: bmi categories 3 and 4 represent overweight and obese.
    bmi_risk = ifelse(BMI_cat %in% c(3, 4), 1, 0),
    
    # 8. Parity risk: parity >= 2
    # Assumption: parity category 3 represents parity >= 2.
    parity_risk = ifelse(Parity == 3, 1, 0),
    
    # 9. Trimester risk: second or third trimester
    trimester_risk = ifelse(Trimester %in% c(2, 3), 1, 0),
    
    # 10. Hypertension risk
    hypertension_risk = ifelse(Hypertension == 1, 1, 0),
    
    # 11. Thyroid disorder risk
    thyroid_risk = ifelse(Thyroid2 == 1, 1, 0),
    
    # 12. Alcohol use risk
    alcohol_risk = ifelse(Alcohol == 1, 1, 0),
    
    # 13. High-fat diet risk
    diet_risk = ifelse(Diet == 1, 1, 0),
    
    State_Names,
    
    District_Names
    
    
    # Smoking was not included because of very low frequency in the dataset.
    # If required later, it can be added as:
    # smoking_risk = ifelse(smoking_status == 1, 1, 0)
  )


# ============================================================
# 4. DEFINE RISK VARIABLES AND LABELS
# ============================================================

# These are the final 13 risk variables used in Aim 1.
risk_vars <- c(
  "age_risk",
  "education_risk",
  "caste_risk",
  "religion_risk",
  "wealth_risk",
  "rural_risk",
  "bmi_risk",
  "parity_risk",
  "trimester_risk",
  "hypertension_risk",
  "thyroid_risk",
  "alcohol_risk",
  "diet_risk"
)

# Clean labels for plots and tables
var_labels <- c(
  age_risk = "Age 25+",
  education_risk = "No education",
  caste_risk = "SC/ST",
  religion_risk = "Non-hindu",
  wealth_risk = "Poorest/Poorer",
  rural_risk = "Rural",
  bmi_risk = "Overweight/Obese",
  parity_risk = "Parity 2+",
  trimester_risk = "2nd/3rd trimester",
  hypertension_risk = "Hypertension",
  thyroid_risk = "Thyroid disorder",
  alcohol_risk = "Alcohol",
  diet_risk = "High-fat diet"
)

# Domain classification for Figure 1
var_domains <- c(
  age_risk = "Sociodemographic factors",
  education_risk = "Sociodemographic factors",
  caste_risk = "Sociodemographic factors",
  religion_risk = "Sociodemographic factors",
  wealth_risk = "Sociodemographic factors",
  rural_risk = "Sociodemographic factors",
  bmi_risk = "Maternal and clinical factors",
  parity_risk = "Maternal and clinical factors",
  trimester_risk = "Maternal and clinical factors",
  hypertension_risk = "Maternal and clinical factors",
  thyroid_risk = "Maternal and clinical factors",
  alcohol_risk = "Lifestyle factors",
  diet_risk = "Lifestyle factors"
)

names(gdm2)
# ============================================================
# 5. FUNCTION TO ESTIMATE OVERALL VPC
# ============================================================

# This function fits a three-level empty logistic model for each binary risk factor.
# Model structure:
#   risk factor ~ 1 + random intercept for state + random intercept for district
#
# The VPC tells us the percentage of total unexplained variation located at each level.
# For logistic mixed models, individual-level variance is fixed as pi^2 / 3.

# Rename geography variables


get_vpc <- function(var){
  
  dat <- gdm2 %>%
    filter(
      !is.na(.data[[var]]),
      !is.na(State_Names),
      !is.na(District_Names)
    )
  
  # Skip variables without variation
  if(length(unique(dat[[var]])) < 2){
    return(NULL)
  }
  
  model_formula <- as.formula(
    paste0(var, " ~ 1 + (1 | State_Names) + (1 | District_Names)")
  )
  
  model <- tryCatch(
    glmer(
      model_formula,
      data = dat,
      family = binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    ),
    error = function(e) NULL
  )
  
  if(is.null(model)){
    return(NULL)
  }
  
  vc <- as.data.frame(VarCorr(model))
  
  state_var <- sum(vc$vcov[vc$grp == "State_Names"], na.rm = TRUE)
  district_var <- sum(vc$vcov[vc$grp == "District_Names"], na.rm = TRUE)
  individual_var <- (pi^2) / 3
  
  total_var <- state_var + district_var + individual_var
  
  data.frame(
    Variable = var,
    Variable_label = var_labels[var],
    Domain = var_domains[var],
    State_variance = state_var,
    District_variance = district_var,
    Individual_variance = individual_var,
    State_VPC = (state_var / total_var) * 100,
    District_VPC = (district_var / total_var) * 100,
    Individual_VPC = (individual_var / total_var) * 100
  )
}


# ============================================================
# 6. RUN OVERALL VPC MODELS
# ============================================================

vpc_results <- map_dfr(risk_vars, get_vpc)

print(vpc_results)

# Export the VPC results
write.csv(vpc_results, "Aim1_Overall_VPC_results.csv", row.names = FALSE)


# ============================================================
# 7. FIGURE 1: OVERALL VPC STACKED BAR PLOT (FINAL FIX)
# ============================================================

# ---------- clean values ----------
vpc_results_clean <- vpc_results %>%
  mutate(
    State_VPC = replace_na(State_VPC, 0),
    District_VPC = replace_na(District_VPC, 0),
    Individual_VPC = replace_na(Individual_VPC, 0)
  ) %>%
  rowwise() %>%
  mutate(
    Total = State_VPC + District_VPC + Individual_VPC,
    
    State_VPC = (State_VPC / Total) * 100,
    District_VPC = (District_VPC / Total) * 100,
    
    # force exact total
    Individual_VPC = 100 - State_VPC - District_VPC
  ) %>%
  ungroup() %>%
  select(-Total)


# ---------- long format ----------
vpc_plot <- vpc_results_clean %>%
  select(Domain, Variable_label,
         State_VPC,
         District_VPC,
         Individual_VPC) %>%
  pivot_longer(
    cols = c(State_VPC,
             District_VPC,
             Individual_VPC),
    names_to = "Level",
    values_to = "VPC"
  ) %>%
  mutate(
    Level = factor(
      Level,
      levels = c("Individual_VPC",
                 "District_VPC",
                 "State_VPC"),
      labels = c("Individual",
                 "District",
                 "State")
    ),
    
    Variable_label = factor(
      Variable_label,
      levels = var_labels
    ),
    
    Domain = factor(
      Domain,
      levels = c(
        "Lifestyle factors",
        "Maternal and clinical factors",
        "Sociodemographic factors"
      )
    )
  )


# ---------- plot ----------
figure1 <- ggplot(
  vpc_plot,
  aes(
    x = Variable_label,
    y = VPC,
    fill = Level
  )
) +
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        round(VPC,1) < 5,
        "",
        paste0(round(VPC,1), "%")
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 2.8,
    fontface = "bold",
    color = "black"
  ) +
  
  facet_grid(
    . ~ Domain,
    scales = "free_x",
    space = "free_x"
  ) +
  
  scale_fill_manual(
    values = c(
      "Individual" = "#E15759",
      "District"   = "#59A14F",
      "State"      = "#4E79A7"
    ),
    breaks = c(
      "Individual",
      "District",
      "State"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0,100,20),
    labels = function(x) paste0(x,"%"),
    expand = c(0,0)
  ) +
  
  labs(
    x = "",
    y = "Variance Partition Coefficient (%)",
    fill = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 10,
      color = "black"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(1.2, "lines"),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    strip.background = element_rect(
      fill = "#F2F2F2",
      color = NA
    ),
    legend.position = "bottom"
  )

figure1

ggsave(
  "Figure1_Aim1_Overall_VPC_domain_plot_FINAL.png",
  figure1,
  width = 14,
  height = 7,
  dpi = 300
)


# ============================================================
# 8. FUNCTION TO CALCULATE DISTRICT VPC WITHIN EACH STATE
# ============================================================

# This function fits a two-level empty logistic model separately within each state.
# Model structure:
#   risk factor ~ 1 + random intercept for district
#
# This gives district-level clustering within each state.

get_state_vpc <- function(state_name, var){
  
  dat <- gdm2 %>%
    filter(State_Names == state_name) %>%
    filter(
      !is.na(.data[[var]]),
      !is.na(District_Names)       
    )
  
  # If the state has fewer than two districts, district variance cannot be estimated.
  if(length(unique(dat$District_Names)) < 2){
    return(data.frame(
      State_Names = state_name,
      Variable = var,
      District_variance = 0,
      Individual_variance = (pi^2) / 3,
      District_VPC = 0
    ))
  }
  
  # If the risk factor has no variation within the state, model cannot be fitted.
  if(length(unique(dat[[var]])) < 2){
    return(data.frame(
      State_Names = state_name,
      Variable = var,
      District_variance = 0,
      Individual_variance = (pi^2) / 3,
      District_VPC = 0
    ))
  }
  
  model_formula <- as.formula(
    paste0(var, " ~ 1 + (1 | District_Names)")
  )
  
  model <- tryCatch(
    glmer(
      model_formula,
      data = dat,
      family = binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    ),
    error = function(e) NULL
  )
  
  # If model fails, assign district VPC as 0.
  if(is.null(model)){
    return(data.frame(
      State_Names = state_name,
      Variable = var,
      District_variance = 0,
      Individual_variance = (pi^2) / 3,
      District_VPC = 0
    ))
  }
  
  vc <- as.data.frame(VarCorr(model))
  
  district_var <- sum(vc$vcov[vc$grp == "District_Names"], na.rm = TRUE)
  
  # Safeguard for invalid values
  if(is.na(district_var) | is.infinite(district_var) | district_var < 0){
    district_var <- 0
  }
  
  individual_var <- (pi^2) / 3
  
  district_vpc <- (district_var / (district_var + individual_var)) * 100
  
  if(is.na(district_vpc) | is.infinite(district_vpc)){
    district_vpc <- 0
  }
  
  data.frame(
    State_Names = state_name,
    Variable = var,
    District_variance = district_var,
    Individual_variance = individual_var,
    District_VPC = district_vpc
  )
}


# ============================================================
# 9. RUN STATE-SPECIFIC DISTRICT VPC MODELS
# ============================================================

states <- sort(unique(gdm2$State_Names))

state_vpc_results <- map_dfr(
  states,
  function(st){
    map_dfr(
      risk_vars,
      function(v){
        get_state_vpc(st, v)
      }
    )
  }
)

state_vpc_results <- state_vpc_results %>%
  mutate(
    Variable_label = recode(Variable, !!!var_labels),
    District_VPC = replace_na(District_VPC, 0),
    VPC_label = paste0(round(District_VPC), "%"),
    text_colour = ifelse(District_VPC >= 50, "white", "black")
  )


# Export state-specific VPC results
write.csv(state_vpc_results, "Aim1_State_specific_District_VPC_results.csv", row.names = FALSE)


# ============================================================
# 10. FIGURE 2: STATE-SPECIFIC DISTRICT VPC HEATMAP
# ============================================================

figure2 <- ggplot(
  state_vpc_results,
  aes(x = Variable_label, y = State_Names, fill = District_VPC)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(
    aes(label = VPC_label, color = text_colour),
    size = 2.2
  ) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "#2C7BB6",
    high = "#D7191C",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    name = "District VPC (%)"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5)
  )

figure2

ggsave(
  "Figure2_State_specific_District_VPC_heatmap.png",
  plot = figure2,
  width = 13,
  height = 9,
  dpi = 300
)


# ============================================================
# 11. FUNCTION TO EXTRACT STATE-LEVEL RANDOM EFFECTS
# ============================================================

# This function fits the three-level model again and extracts state-level residuals.
# These residuals indicate whether a state has higher or lower risk than the national average
# for each specific GDM-related correlate.

get_state_residuals <- function(var){
  
  dat <- gdm2 %>%
    filter(
      !is.na(.data[[var]]),
      !is.na(State_Names),
      !is.na(District_Names)
    )
  
  if(length(unique(dat[[var]])) < 2){
    return(NULL)
  }
  
  model_formula <- as.formula(
    paste0(var, " ~ 1 + (1 | State_Names) + (1 | District_Names)")
  )
  
  model <- tryCatch(
    glmer(
      model_formula,
      data = dat,
      family = binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    ),
    error = function(e) NULL
  )
  
  if(is.null(model)){
    return(NULL)
  }
  
  state_re <- ranef(model)$State_Names %>%
    as.data.frame()
  
  state_re$State_Names <- rownames(state_re)
  
  state_re <- state_re %>%
    select(State_Names, everything())
  
  colnames(state_re)[2] <- "State_residual"
  
  state_re %>%
    mutate(
      Variable = var,
      Variable_label = var_labels[var]
    )
}


# ============================================================
# 12. CREATE STATE RESIDUAL QUINTILES
# ============================================================

state_residuals <- map_dfr(risk_vars, get_state_residuals)

# Quintiles are created separately for each risk factor.
# Quintile 1 = lowest state residual
# Quintile 5 = highest state residual

state_quintiles <- state_residuals %>%
  group_by(Variable) %>%
  mutate(
    Residual_quintile = ntile(State_residual, 5),
    Risk_quintile = case_when(
      Residual_quintile == 1 ~ "Lowest risk quintile",
      Residual_quintile == 2 ~ "Low risk quintile",
      Residual_quintile == 3 ~ "Moderate risk quintile",
      Residual_quintile == 4 ~ "High risk quintile",
      Residual_quintile == 5 ~ "Highest risk quintile"
    )
  ) %>%
  ungroup()

write.csv(state_quintiles, "State_residual_quintile_results.csv", row.names = FALSE)


# ============================================================
# 13. FIGURE 3: STATE RISK QUINTILE COUNT MATRIX
# ============================================================

# This figure counts how many factors each state appears in for each risk quintile.
# The count is grouped as:
#   0-2 factors
#   3-5 factors
#   6+ factors

figure3_counts <- state_quintiles %>%
  count(State_Names, Risk_quintile, name = "Quintile_count") %>%
  mutate(
    Count_group = case_when(
      Quintile_count <= 2 ~ "0-2",
      Quintile_count >= 3 & Quintile_count <= 5 ~ "3-5",
      Quintile_count >= 6 ~ "6+"
    )
  )

figure3_full <- expand_grid(
  Risk_quintile = c(
    "Highest risk quintile",
    "High risk quintile",
    "Moderate risk quintile",
    "Low risk quintile",
    "Lowest risk quintile"
  ),
  Count_group = c("0-2", "3-5", "6+")
) %>%
  left_join(
    figure3_counts %>%
      group_by(Risk_quintile, Count_group) %>%
      summarise(
        States = paste(sort(State_Names), collapse = ", "),
        .groups = "drop"
      ),
    by = c("Risk_quintile", "Count_group")
  ) %>%
  mutate(
    States = replace_na(States, ""),
    States = str_wrap(States, width = 34),
    Risk_quintile = factor(
      Risk_quintile,
      levels = c(
        "Lowest risk quintile",
        "Low risk quintile",
        "Moderate risk quintile",
        "High risk quintile",
        "Highest risk quintile"
      )
    ),
    Count_group = factor(Count_group, levels = c("0-2", "3-5", "6+"))
  )

figure3 <- ggplot(
  figure3_full,
  aes(x = Count_group, y = Risk_quintile, fill = Risk_quintile)
) +
  geom_tile(color = "black", linewidth = 0.4) +
  geom_text(
    aes(label = States),
    size = 2.1,
    lineheight = 0.78,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Highest risk quintile" = "#fb4d4d",
      "High risk quintile" = "#f46d43",
      "Moderate risk quintile" = "#bdbdbd",
      "Low risk quintile" = "#1fb6d9",
      "Lowest risk quintile" = "#1687b9"
    )
  ) +
  labs(
    title = "",
    subtitle = "State-level residuals were classified into quintiles separately for each correlate",
    x = "Number of factors in each quintile",
    y = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 9),
    plot.margin = margin(10, 20, 10, 20)
  )

figure3

ggsave(
  "Figure3_State_risk_quintile_count_matrix.png",
  plot = figure3,
  width = 13,
  height = 9,
  dpi = 300
)


# ============================================================
# 14. CREATE HIGHEST-RISK COUNT OBJECT FOR FIGURE 4
# ============================================================

# This object was missing in the earlier code.
# It counts how many times each state appears in the highest-risk quintile
# across all 13 GDM-related factors.

highest_risk_count <- state_quintiles %>%
  filter(Risk_quintile == "Highest risk quintile") %>%
  count(State_Names, name = "Highest_risk_count") %>%
  right_join(
    data.frame(State_Names = sort(unique(state_quintiles$State_Names))),
    by = "State_Names"
  ) %>%
  mutate(
    Highest_risk_count = replace_na(Highest_risk_count, 0),
    Risk_count_group = case_when(
      Highest_risk_count >= 6 ~ "6+",
      Highest_risk_count >= 3 & Highest_risk_count <= 5 ~ "3 5",
      Highest_risk_count <= 2 ~ "0 2"
    )
  )

write.csv(highest_risk_count, "Highest_risk_count_by_state.csv", row.names = FALSE)


# ============================================================
# 15. FIGURE 4: HIGHEST-RISK QUINTILE MATRIX
# ============================================================

# Recreate highest-risk count cleanly
highest_risk_count <- state_quintiles %>%
  filter(Risk_quintile == "Highest risk quintile") %>%
  count(State_Names, name = "Highest_risk_count") %>%
  right_join(
    data.frame(State_Names = sort(unique(state_quintiles$State_Names))),
    by = "State_Names"
  ) %>%
  mutate(
    Highest_risk_count = replace_na(Highest_risk_count, 0),
    
    Risk_count_group = case_when(
      Highest_risk_count >= 6 ~ "6+",
      Highest_risk_count >= 3 & Highest_risk_count <= 5 ~ "3-5",
      Highest_risk_count <= 2 ~ "0-2"
    )
  )

# Prepare Figure 4 data
figure4_data <- state_quintiles %>%
  mutate(
    Highest_risk = ifelse(
      Risk_quintile == "Highest risk quintile",
      "X",
      ""
    )
  ) %>%
  left_join(highest_risk_count, by = "State_Names") %>%
  mutate(
    Variable_label = factor(Variable_label, levels = var_labels),
    Risk_count_group = factor(
      Risk_count_group,
      levels = c("6+", "3-5", "0-2")
    )
  )

# Order states by number of highest-risk factors
state_order <- highest_risk_count %>%
  arrange(desc(Highest_risk_count), State_Names) %>%
  pull(State_Names)

figure4_data <- figure4_data %>%
  mutate(
    State_Names = factor(State_Names, levels = rev(state_order))
  )

# Plot Figure 4
figure4 <- ggplot(
  figure4_data,
  aes(x = Variable_label, y = State_Names, fill = Risk_count_group)
) +
  geom_tile(
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = Highest_risk),
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "6+" = "#e34a33",
      "3-5" = "#fdbb84",
      "0-2" = "#fee8c8"
    ),
    drop = FALSE,
    name = "Highest-risk\nfactors"
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 8,
      color = "black"
    ),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

figure4

ggsave(
  "Figure4_Highest_risk_quintile_matrix.png",
  plot = figure4,
  width = 11,
  height = 9,
  dpi = 300
)

write.csv(figure4_data, "Figure4_highest_risk_matrix_data.csv", row.names = FALSE)


# ============================================================
# 16. OPTIONAL: SAVE ALL MAIN RESULTS INTO ONE EXCEL FILE
# ============================================================

write_xlsx(
  list(
    Overall_VPC = vpc_results,
    State_District_VPC = state_vpc_results,
    State_Residual_Quintiles = state_quintiles,
    Highest_Risk_Count = highest_risk_count,
    Figure4_Data = figure4_data
  ),
  "Aim1_All_Results_Tables.xlsx"
)


