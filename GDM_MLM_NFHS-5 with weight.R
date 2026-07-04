library(haven)
library(dplyr)
library(survey)
library(lme4)
library(broom.mixed)
library(gt)
library(writexl)


gdm<- read_sav("D:\\ICMR _ GDM\\NFHS_GDM\\clean_data.sav")

# ============================================
# Select only the variables needed for analysis
# ============================================
gdm_analysis <- gdm %>%
  dplyr::select(
    # Outcome variable
    GDM_new,
    
    # Predictor variables
    
    agegrp,  #15-19; 20-24; 25-29; 30-34; >=35 
    
    Caste_cat, #Caste groups
    
    V025, #Place of residence (Urban; Rural)
    
    Religion_grp, # Hindus; Muslims; Christians; Others
    
    V190, # Wealth index
    
    BMI_cat, #BMI categories 
    
    V106,  #Education
    
    Hypertension, #No; Yes 
    
    Thyroid2, #Thyroid disorder
    
    Trimester, #First; Second; Third
    
    S720,  #Alcohol consumption
    
    Diet, # Low fat; high fat 
    
    Parity, # 0; 1; >=2
    
    SDIST, #District
    
    V024, #State
    
    V005, # Sample weight
    
    
    
  )





### Carle scaled weight

gdm_analysis <- gdm_analysis %>%
  mutate(weight = V005/1000000) %>%
  group_by(V024, SDIST) %>%   # State + District
  mutate(
    district_n = n(),
    district_weight_sum = sum(weight),
    wt_scaled = weight * (district_n / district_weight_sum)
  ) %>%
  ungroup()




## Match the sample size is same or not after applying weight
sum(gdm_analysis$wt_scaled)
nrow(gdm_analysis)

table(gdm_analysis$GDM_new)
# ============================================
# Convert variables to appropriate types
# ============================================

gdm_analysis <- gdm_analysis %>%
  mutate(
    # Outcome
    GDM_new = as.numeric(as.character(GDM_new)),  # Must be 0/1 numeric
    
    # Level identifiers
    State_id   = as.factor(V024),     # Level 3: State
    District_id = as.factor(SDIST),   # Level 2: District
    
    # Individual level predictors (Level 1)
    agegrp             = as.factor(agegrp),
    Caste              = as.factor(Caste_cat),
    Residence          = as.factor(V025),
    Religion           = as.factor(Religion_grp),
    Wealth             = as.factor(V190),
    BMI_cat            = as.factor(BMI_cat),
    Hypertension       = as.factor(Hypertension),
    Education          = as.factor(V106),
    Thyroid            = as.factor(Thyroid2),
    Trimester          = as.factor(Trimester),
    Alcohol            = as.factor(S720),
    Diet               = as.factor(Diet),
    Parity             = as.factor(Parity)
  )




library(lme4)
library(dplyr)
library(broom.mixed)
library(gt)
library(writexl)





# ============================================
# Set Reference Categories for all factors
# ============================================

gdm_analysis <- gdm_analysis %>%
  mutate(
    # Outcome (ensure 0/1)
    GDM_new = as.numeric(as.character(GDM_new)),
    
    # Age group: reference = 15-19 years
    agegrp = relevel(as.factor(agegrp), ref = "1"),  
    
    # Caste: reference = Others
    Caste = relevel(as.factor(Caste_cat), ref = "4"),     
    
    # Residence: reference = urban
    Residence = relevel(as.factor(V025), ref = "1"), 
    
    # Religion: reference = Hindu
    Religion = relevel(as.factor(Religion_grp), ref = "1"),
    
    # Wealth: reference = richest
    Wealth = relevel(as.factor(V190), ref = "5"),
    
    # BMI: reference = Normal
    BMI_cat = relevel(as.factor(BMI_cat), ref = "2"),  
    
    # Education: reference = higher education
    Education = relevel(as.factor(V106), ref = "3"),
    
    # Hypertension: reference = No
    Hypertension = relevel(as.factor(Hypertension), ref = "0"),
    
    # Thyroid: reference = No
    Thyroid = relevel(as.factor(Thyroid2), ref = "2"),
    
    # Trimester: reference = First
    Trimester = relevel(as.factor(Trimester), ref = "1"),
    
    # Alcohol: reference = No
    Alcohol = relevel(as.factor(S720), ref = "0"),
    
    # Diet: reference = Low fat
    Diet = relevel(as.factor(Diet), ref = "0"),        
    
    # Parity: reference = (0 child)
    Parity = relevel(as.factor(Parity), ref = "1"),
    
    # Level identifiers
    State_id    = as.factor(V024),
    District_id = as.factor(SDIST)
  )



## null model

model0 <- glmer(
  GDM_new ~ (1 | State_id/District_id),
  
  data = gdm_analysis,
  family = binomial,
  weights = wt_scaled,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model0)

## MODEL 1 — Socio-demographic

model1 <- glmer(
  GDM_new ~
    agegrp +
    Education +
    Wealth +
    Residence +
    Religion +
    Caste +
    
    (1 | State_id/District_id),
  
  data = gdm_analysis,
  family = binomial,
  weights = wt_scaled,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)
summary(model1)

## MODEL 2 — Socio-demographic + Maternal & Clinical

model2 <- glmer(
  
  GDM_new ~
    
    # Socio-demographic
    
    agegrp +
    Education +
    Wealth +
    Residence +
    Religion +
    Caste +
    
    # Maternal & Clinical
    
    Parity +
    BMI_cat +
    Hypertension +
    Thyroid +
    Trimester +
    
    (1 | State_id/District_id),
  
  data = gdm_analysis,
  family = binomial,
  weights = wt_scaled,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model2)

## MODEL 3 — Socio-demographic + Maternal & Clinical + lifestyle 

model3 <- glmer(
  
  GDM_new ~
    
    # Socio-demographic
    
    agegrp +
    Education +
    Wealth +
    Residence +
    Religion +
    Caste +
    
    # Maternal & Clinical
    
    Parity +
    BMI_cat +
    Hypertension +
    Thyroid +
    Trimester +
    
    ## Lifestyle  
    
    Alcohol +
    Diet +
    
    (1 | State_id/District_id),
  
  data = gdm_analysis,
  family = binomial,
  weights = wt_scaled,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model3)

## Extract AOR + 95% CI


extract_or <- function(model){
  
  broom.mixed::tidy(model,
                    effects="fixed",
                    conf.int=TRUE,
                    exponentiate=TRUE) %>%
    mutate(
      AOR_CI = sprintf("%.2f (%.2f - %.2f)",
                       estimate,
                       conf.low,
                       conf.high)
    ) %>%
    select(term, AOR_CI)
}

or1 <- extract_or(model1)
or2 <- extract_or(model2)
or3 <- extract_or(model3)


print(or1, n = Inf)
print(or2, n = Inf)
print(or3, n = Inf)

#############################################

library(lme4)
library(dplyr)

extract_random <- function(model){
  
  # Variance components
  vc <- as.data.frame(VarCorr(model))
  
  # District variance
  var_district <- vc$vcov[vc$grp == "District_id:State_id"]
  
  # State variance
  var_state <- vc$vcov[vc$grp == "State_id"]
  
  # Total random variance
  total_var <- var_state + var_district
  
  # Residual variance for logistic model
  resid_var <- (pi^2)/3
  
  # ----------------------------
  # VPC / ICC
  # ----------------------------
  vpc_state    <- var_state / (total_var + resid_var)
  vpc_district <- var_district / (total_var + resid_var)
  
  # ----------------------------
  # MOR
  # ----------------------------
  MOR_state    <- exp(0.6745 * sqrt(2 * var_state))
  MOR_district <- exp(0.6745 * sqrt(2 * var_district))
  
  # ----------------------------
  # Model fit
  # ----------------------------
  AIC_val  <- AIC(model)
  BIC_val  <- BIC(model)
  logLik_val <- as.numeric(logLik(model))
  
  out <- data.frame(
    State_Var = round(var_state, 3),
    District_Var = round(var_district, 3),
    
    State_VPC = round(vpc_state, 3),
    District_VPC = round(vpc_district, 3),
    
    State_MOR = round(MOR_state, 3),
    District_MOR = round(MOR_district, 3),
    
    AIC = round(AIC_val, 2),
    BIC = round(BIC_val, 2),
    LogLik = round(logLik_val, 2)
  )
  
  return(out)
}

rand0 <- extract_random(model0)
rand1 <- extract_random(model1)
rand2 <- extract_random(model2)
rand3 <- extract_random(model3)

random_table <- bind_rows(
  "Null Model" = rand0,
  "Model 1"    = rand1,
  "Model 2"    = rand2,
  "Model 3"    = rand3,
  .id = "Model"
)

print(random_table)



## Likelihood Ratio test
anova(model0, model1, model2, model3, test = "Chisq")
