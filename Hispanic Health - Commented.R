#loading in libraries
library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(gtsummary)
library(broom)
library(scales)
library(ggplot2)
library(tibble)
library(forcats)

#loading in dataset 
dataset <- read_sas("~/Downloads/chs2020_public.sas7bdat")

#Cleaning and labeling needed variables

data <- dataset %>%
  select(
    imputed_povertygroup, newrace, wt21_dual, strata,
    education, emp3, insuredgateway20, agegroup, birthsex,
    pcp20, didntgetcare20, fruitveg20, exercise20,
    drinker, usborn, maritalstatus20, bingenew ) %>%
  mutate(
    raceETH_ = factor(newrace, levels = 1:5,
                      labels = c("White, non-Hispanic",
                                 "Black, non-Hispanic",
                                 "Hispanic",
                                 "Asian/PI, non-Hispanic",
                                 "Other, non-Hispanic")),
    
    sex_ = factor(birthsex, levels = 1:2,
                  labels = c("Male", "Female")
                  ),
    
    edu_ = factor(education, levels = 1:4,
                  labels = c("Less than high school",
                             "High school graduate",
                             "Some college/technical school",
                             "College graduate")
                  ),
    
    empStatus_ = factor(emp3, levels = 1:3,
                        labels = c("Employed",
                                   "Unemployed",
                                   "Not in Labor Force")),
    
    insurStatus_ = factor(insuredgateway20, levels = 1:2,
                          labels = c("Yes", "No")),
    
    age_ = factor(agegroup, levels = 1:4,
                  labels = c("18-24", "25-44", "45-64", "65+")),
    
    didntgetcare12_ = factor(didntgetcare20, levels = 1:2,
                             labels = c("Yes", "No")),
    
    usborn_ = factor(usborn, levels = 1:2,
                     labels = c("US born", "Foreign born")),
    
    maritalStatus_ = factor(maritalstatus20, levels = 1:5,
                            labels = c("Married", "Divorced",
                                       "Widowed", "Separated",
                                       "Never married")),
    
    PovGroup_= factor(imputed_povertygroup, levels = 1:5,
                      labels = c("<100% poverty",
                                 "100%-<200% poverty",
                                 "200%-<400% poverty",
                                 "400%-<600% poverty",
                                 ">=600% poverty"))
  ) %>%
  drop_na()

#creating survey design
design_chs <- svydesign(
  id = ~1,
  strata = ~strata,
  weights = ~wt21_dual,
  data = data,
  nest = TRUE
)

#creating Figure 1: Poverty distribution
#calculating weighted poverty group proption. 
weighted_pov <- svyby(
  ~PovGroup_,
  ~raceETH_,
  design_chs,
  svymean,
  na.rm = TRUE
)

#converting results into long format for plotting 
weighted_pov_long <- weighted_pov %>%
  as.data.frame() %>%
  select(raceETH_, starts_with("PovGroup")) %>%
  pivot_longer(
    cols = starts_with("PovGroup"),
    names_to = "PovGroup",
    values_to = "proportion"
  ) %>%
  mutate(
    PovGroup = str_remove(PovGroup, "^PovGroup_"),
    PovGroup = factor(
      PovGroup,
      levels = c(
        "<100% poverty",
        "100%-<200% poverty",
        "200%-<400% poverty",
        "400%-<600% poverty",
        ">=600% poverty"
      )
    )
  )

#creating stacked proprotional bar chart 
ggplot(weighted_pov_long, aes(x = raceETH_, y = proportion, fill = PovGroup)) +
  geom_col(position = "fill") +
  labs(x = "Race/Ethnicity", y = "Proportion", fill = "Poverty Group") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(
    "<100% poverty"        = "#d9d9d9",
    "100%-<200% poverty"   = "#c6dbef",
    "200%-<400% poverty"   = "#9ecae1",
    "400%-<600% poverty"   = "#6baed6",
    ">=600% poverty"       = "#2171b5"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Creating figure 2: Delayed medical care due to cost by race/eth
delayed_race <- data %>%
  group_by(raceETH_) %>%
  summarise(
    percent = weighted.mean(didntgetcare12_ == "Yes", wt21_dual, na.rm = TRUE) * 100
  ) %>%
  mutate(label = paste0(round(percent, 1), "%"))

ggplot(delayed_race, aes(x = percent, y = reorder(raceETH_, percent))) +
  geom_segment(aes(x = 0, xend = percent, yend = reorder(raceETH_, percent)),
               color = "gray70", linewidth = 1) +
  geom_point(color = "red", size = 4) +
  geom_text(aes(label = label), hjust = -0.2, size = 4) +
  labs(
    title = "Delayed Medical Care Due to Cost by Race/Ethnicity",
    subtitle = "Percent reporting delayed medical care in past 12 months",
    x = "Percent",
    y = "",
    caption = "Source: 2020 NYC Community Health Survey"
  ) +
  theme_minimal()

#Poverty Status by race/ethnicity models
#creating binary poverty indicators 
data <- data %>%
  mutate(
    imp1 = as.numeric(imputed_povertygroup < 2),
    imp2 = as.numeric(imputed_povertygroup < 3),
    imp3 = as.numeric(imputed_povertygroup < 4),
    imp4 = as.numeric(imputed_povertygroup < 5)
  )

#updating survey design after added variables 
design_chs <- svydesign (
  id = ~1, 
  strata = ~strata, 
  weights = ~wt21_dual, 
  data = data, 
  nest = TRUE
)

#model 1: Below 100% FDL 
mod1 <- svyglm(imp1 ~ raceETH_ + sex_ + edu_ + empStatus_ +
                 age_ + maritalStatus_ + usborn_,
               design = design_chs,
               family = quasibinomial())

#model 2: Below 200% FDL 
mod2 <- svyglm(imp2 ~ raceETH_ + sex_ + edu_ + empStatus_ +
                 age_ + maritalStatus_ + usborn_,
               design = design_chs,
               family = quasibinomial())


#model 3: Below 400% FDL 
mod3 <- svyglm(imp3 ~ raceETH_ + sex_ + edu_ + empStatus_ +
                 age_ + maritalStatus_ + usborn_,
               design = design_chs,
               family = quasibinomial())


#model 4: Below 600% FDL 
mod4 <- svyglm(imp4 ~ raceETH_ + sex_ + edu_ + empStatus_ +
                 age_ + maritalStatus_ + usborn_,
               design = design_chs,
               family = quasibinomial())

#odds ratio and 95% CIs
exp(cbind(OR = coef(mod1), confint(mod1)))
exp(cbind(OR = coef(mod2), confint(mod2)))
exp(cbind(OR = coef(mod3), confint(mod3)))
exp(cbind(OR = coef(mod4), confint(mod4)))


#creating health care access model - outcome delayed medical care due to cost 
data_access <- dataset %>%
  select(
    didntgetcare20, newrace, agegroup, birthsex,
    education, emp3, insuredgateway20,
    maritalstatus20, usborn, drinker,
    imputed_povertygroup, wt21_dual, strata
  ) %>%
  filter(
    !is.na(didntgetcare20),
    !is.na(newrace),
    !is.na(agegroup),
    !is.na(birthsex),
    !is.na(education),
    !is.na(emp3),
    !is.na(insuredgateway20),
    !is.na(maritalstatus20),
    !is.na(usborn),
    !is.na(drinker),
    !is.na(imputed_povertygroup),
    !is.na(wt21_dual),
    !is.na(strata)
  ) %>%
  mutate(
    delayedcare = ifelse(didntgetcare20 == 1, 1, 0),
    
    raceETH_ = factor(newrace, levels = 1:5,
                      labels = c("White, non-Hispanic",
                                 "Black, non-Hispanic",
                                 "Hispanic",
                                 "Asian/PI, non-Hispanic",
                                 "Other, non-Hispanic")),
    
    age_ = factor(agegroup, levels = 1:4,
                  labels = c("18-24", "25-44", "45-64", "65+")),
    
    sex_ = factor(birthsex, levels = 1:2,
                  labels = c("Male", "Female")),
    
    edu_ = factor(education, levels = 1:4,
                  labels = c("Less than HS",
                             "HS Graduate",
                             "Some College",
                             "College Graduate")),
    empStatus_ = factor(emp3, levels = 1:3,
                        labels = c("Employed",
                                   "Unemployed",
                                   "Not in Labor Force")),
    
    insurStatus_ = factor(insuredgateway20, levels = 1:2,
                          labels = c("Insured", "Not Insured")),
    
    marital_ = factor(maritalstatus20, levels = 1:5,
                      labels = c("Married", "Divorced",
                                 "Widowed", "Separated",
                                 "Never Married")),
    
    usborn_ = factor(usborn, levels = 1:2,
                     labels = c("US Born", "Foreign Born")),
    
    drinker_ = factor(drinker, levels = 1:2,
                      labels = c("Drinker", "Non-drinker")),
    
    povGroup_ = factor(imputed_povertygroup, levels = 1:5,
                       labels = c("<100% poverty",
                                  "100%-<200% poverty",
                                  "200%-<400% poverty",
                                  "400%-<600% poverty",
                                  ">=600% poverty"))
)
#Creating survey design for health care access model 
design_access <- svydesign(
  ids = ~1,
  strata = ~strata,
  weights = ~wt21_dual,
  data = data_access,
  nest = TRUE
)

#survey weighted logistic regression model 
model_access <- svyglm(
  delayedcare ~ raceETH_ + age_ + sex_ + edu_ +
    empStatus_ + insurStatus_ + marital_ +
    usborn_ + drinker_ + povGroup_,
  design = design_access,
  family = quasibinomial()
)

#odds ratios and 95% CIs
access_results <- exp(cbind(
  OR = coef(model_access),
  confint(model_access)
))

access_results

#preparing odds ratio for plot  
plot_access <- as.data.frame(access_results) %>%
  rownames_to_column("predictor") %>%
  rename(
    lower = `2.5 %`,
    upper = `97.5 %`
  ) %>%
  filter(predictor != "(Intercept)") %>%
  mutate(
    significant = ifelse(lower > 1 | upper < 1,
                         "Significant (p < 0.05)",
                         "Not significant"),
    
    predictor_label = recode(
      predictor,
      "raceETH_Black, non-Hispanic" = "Black, non-Hispanic",
      "raceETH_Hispanic" = "Hispanic",
      "raceETH_Asian/PI, non-Hispanic" = "Asian/PI, non-Hispanic",
      "raceETH_Other, non-Hispanic" = "Other, non-Hispanic",
      
      "age_25-44" = "Age 25-44",
      "age_45-64" = "Age 45-64",
      "age_65+"  = "Age 65+",
      
      "sex_Female" = "Female",
      
      "edu_HS Graduate" = "HS Graduate",
      "edu_Some College" = "Some College",
      "edu_College Graduate"  = "College Graduate",
      
      "empStatus_Unemployed" = "Unemployed",
      "empStatus_Not in Labor Force" = "Not in Labor Force",
      
      "insurStatus_Not Insured" = "Not Insured",
      
      "marital_Divorced" = "Divorced",
      "marital_Widowed"  = "Widowed",
      "marital_Separated" = "Separated",
      "marital_Never Married" = "Never Married",
      
      "usborn_Foreign Born" = "Foreign Born",
      "drinker_Non-drinker" = "Non-drinker",
      
      "povGroup_100%-<200% poverty" = "100%-<200% poverty",
      "povGroup_200%-<400% poverty" = "200%-<400% poverty",
      "povGroup_400%-<600% poverty" = "400%-<600% poverty",
      "povGroup_>=600% poverty" = ">=600% poverty",
      
      .default = predictor
    )
  )

#plotting health care access model 
ggplot(plot_access,
       aes(x = fct_reorder(predictor_label, OR),
           y = OR,
           fill  = significant)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Significant (p < 0.05)" = "#243E8F",
    "Not significant" = "#B8CBEA"
  )) +
  labs(
    title = "Adjusted Delayed Medical Care Due to Cost",
    subtitle  = "Reference: White non-Hispanic, age 18-24, male,
less than HS, employed, insured, married, US-born, drinker, <100% Poverty",
    x = NULL,
    y = "Odds Ratio",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle  = element_text(size = 10),
    legend.position = "bottom",
    
    panel.grid.major  = element_blank(),
    panel.grid.minor = element_blank(),
    
  )

#Asthma Model - age of asthma diagnosis 
data_asthma <- dataset %>%
  select(
    everasthma, newrace, agegroup, birthsex,
    maritalstatus20, usborn, weight20in5,
    wt21_dual, strata, firsttoldasthma
  ) %>%
  filter(
    everasthma == 1,
    firsttoldasthma %in% c(1, 2, 3)
  ) %>%
  mutate(
    raceETH_ = factor(newrace, levels = 1:5,
                      labels = c("White, non-Hispanic",
                                 "Black, non-Hispanic",
                                 "Hispanic",
                                 "Asian/PI, non-Hispanic",
                                 "Other, non-Hispanic")
                      ),
    
    age_ = factor(agegroup, levels = 1:4,
                  labels = c("18-24", "25-44", "45-64", "65+")
                  ),
    
    sex_ = factor(birthsex, levels = 1:2,
                  labels = c("Male", "Female")),
    
    marital_ = factor(maritalstatus20, levels = 1:5,
                      labels = c("Married", "Divorced",
                                 "Widowed", "Separated",
                                 "Never married")),
    
    usborn_ = factor(usborn, levels = 1:2,
                     labels = c("US Born", "Not US Born")),
    
    weight_ = factor(weight20in5, levels = 1:5,
                     labels = c("Under weight", "Normal weight",
                                "Over weight", "Obese", "Very Obese")),
    
    asthmaAge_ = factor(firsttoldasthma, levels = c(1, 2, 3),
                        labels = c("<18", "18-40", ">40"),
                        ordered = TRUE)
  ) %>%
  drop_na()


# Survey design for asthma model
design_asthma <- svydesign(
  id = ~1,
  strata = ~strata,
  weights = ~wt21_dual,
  data = data_asthma,
  nest = TRUE
)


# Survey weighted ordinal logistic regression model
model_asthma <- svyolr(
  asthmaAge_ ~ raceETH_ + age_ + sex_ + marital_ + usborn_ + weight_,
  design = design_asthma
)


# Model results
summary(model_asthma)

