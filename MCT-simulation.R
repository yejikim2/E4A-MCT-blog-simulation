#### Simulation for E4A Methods Note on MCT ####

# load packages
library(lme4)
library(dplyr)

# Create dataset of n = 1000 ####
  # Set number of individuals within 4 'race' groups
  # Last group will be split to 65 and 35 => total of 5 race groups in 'race2'
  # The group n can be changed as needed to simulate desired assessment
  npergroup_uneq <-c(430, 320, 150, 100)
  
  # Create race groups 
  race <- rep(1:4, times = npergroup_uneq)
  
  # create dummy vars for 'race' with race == 1 as referent group
  dummyrace2 <- as.integer(race == 2)
  dummyrace3 <- as.integer(race == 3)
  dummyrace4 <- as.integer(race == 4)
  
  # combine into one dataframe
  data <- data.frame(dummyrace2,dummyrace3,dummyrace4,race)

# Simulate normally distributed outcome ####
  # For reproducibility
  set.seed(22) 

  # Set desired means and sd for race groups in order from 1 through 4
  # Note: The means and standard deviations can be changed as needed to simulate desired assessment
  means <- c(-1,-1,-1,-0.80)
  sd <- c(1, 1, 1,1)
  
  # Simulate normally dist outcome with different mean for race group 4
  y <- c()
  for(i in 1: length(race)) {
    y <- c(y, rnorm(1, 
                    mean = means[race[i]],
                    sd = sd[race[i]]))
  }

  # Dataframe to store outcome
  y_uneq <- data.frame(y)
  data_uneq <- cbind(y_uneq,data) 

# Create new race2 variable with 5 groups ####
  # 100 observations in race group 4 is split randomly into n = 65 and n = 35 into race group 4 and 5, respectively in new race2 variable
  # Note: You can change the '65' value in line 50 below to desired group split
  data_full <- data_uneq %>%
    mutate(race2 = case_when(
      race == 4 & row_number() %in% sample(which(race == 4), 65) ~ 4,
      race == 4 ~ 5,
      race == 3 ~ 3,
      race == 2 ~ 2,
      race == 1 ~ 1),
      # create dummy variables for race2
      dummyrace2_2 = as.integer(race2 == 2),
      dummyrace2_3 = as.integer(race2 == 3),
      dummyrace2_4 = as.integer(race2 == 4),
      dummyrace2_5 = as.integer(race2 == 5)
    )

  # Check correct grouping above 
  table(data_full$race, data_full$race2)

# Table 1: Summary statistics ####
  # By 'race' variable
  summarystats_full <- data_full %>% 
    group_by(race) %>% 
    summarise(
      n = n(),
      min = min(y),
      q1 = quantile(y, probs = 0.25),
      median = median(y),
      mean = mean(y),
      q3 = quantile(y, probs = 0.75),
      max = max(y),
      sd = sd(y),
      iqr = q3-q1)
  
  # By 'race2' variable
  summarystats2_full <- data_full %>% 
    group_by(race2) %>% 
    summarise(
      n = n(),
      min = min(y),
      q1 = quantile(y, probs = 0.25),
      median = median(y),
      mean = mean(y),
      q3 = quantile(y, probs = 0.75),
      max = max(y),
      sd = sd(y),
      iqr = q3-q1)

# Linear regressions and joint test ####
  # Model 1 - four race groups
  model1_uneq <- lm(y ~ dummyrace2 + dummyrace3 + dummyrace4, data = data_full)
  summary(model1_uneq) 
  
  # Model 2 - five race groups    
  model2_uneq <- lm(y ~ dummyrace2_2 + dummyrace2_3 + dummyrace2_4 + dummyrace2_5,
                    data = data_full)
  summary(model2_uneq) 
  
# Multiple Comparisons Adjustments
  # Model 1 - four race groups
  
    # Pull coefficients from model output
    coeff_m1_uneq <- summary(model1_uneq)
    
    # Extract p-values for dummyrace1, dummyrace2, and dummyrace3
    p_values_uneq <- coeff_m1_uneq$coefficients[, "Pr(>|t|)"][2:4]
    
    # Adjustment methods
    adjust_methods <- c("bonferroni", "BH", "fdr","hochberg",
                        "BY", "hommel", "holm")
    
    # Adjust p-values using different methods
    p_bonferroni <- p.adjust(p_values_uneq, method = "bonferroni")
    p_bh <- p.adjust(p_values_uneq, method = "BH")
    p_fdr <- p.adjust(p_values_uneq, method = "fdr")
    p_hochberg <- p.adjust(p_values_uneq, method = "hochberg")
    p_by <- p.adjust(p_values_uneq, method = "BY")
    p_hommel <- p.adjust(p_values_uneq, method = "hommel")
    p_holm <- p.adjust(p_values_uneq, method = "holm")
    
    # Create dataframe of original and adjusted p-values
    model1_pvalues_uneq <- data.frame(
          method = rep(adjust_methods, each = length(p_values_uneq)),
          dummyrace = rep(paste("dummyrace", 2:4, sep = ""), times = length(adjust_methods)),
          original_p = rep(p_values_uneq, times = length(adjust_methods)),
          adjusted_p = c(p_bonferroni, p_holm, p_fdr, p_hochberg,
                         p_by,p_hommel, p_bh)
        )

  # Model 2 - five race groups
    
    # Same steps as above but for Model 2
    coeff_m2_uneq <- summary(model2_uneq)
    p_values2_uneq <- coeff_m2_uneq$coefficients[, "Pr(>|t|)"][2:5]
    
    p_bonferroni2 <- p.adjust(p_values2_uneq, method = "bonferroni")
    p_bh2 <- p.adjust(p_values2_uneq, method = "BH")
    p_fdr2 <- p.adjust(p_values2_uneq, method = "fdr")
    p_hochberg2 <- p.adjust(p_values2_uneq, method = "hochberg")
    p_by2 <- p.adjust(p_values2_uneq, method = "BY")
    p_hommel2 <- p.adjust(p_values2_uneq, method = "hommel")
    p_holm2 <- p.adjust(p_values2_uneq, method = "holm")
    
    model2_pvalues_uneq <- data.frame(
      method = rep(adjust_methods, each = length(p_values2_uneq)),
      dummyrace = rep(paste("dummyrace", 2:5, sep = ""), times = length(adjust_methods)),
      original_p = rep(p_values2_uneq, times = length(adjust_methods)),
      adjusted_p = c(p_bonferroni2, p_holm2, p_fdr2, p_hochberg2,
                     p_by2,p_hommel2, p_bh2)
    )
