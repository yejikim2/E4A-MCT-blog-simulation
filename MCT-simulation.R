#### Simulation for E4A Methods Note on MCT ####

# load packages
library(lme4)
library(dplyr)

# set number of iterations
n_iterations  <- 1000

# creatingfunction to repeat through 1000 iterations
  main_fn <-  function(i){
    set.seed(i)
  
  # Create dataset of n = 1000 ####
  # Set number of individuals within 4 'race' groups
  # Last group will be split to 65 and 35 => total of 5 race groups in 'race2'
  # The group n can be changed as needed to simulate desired assessment
  npergroup <-c(430, 320, 150, 100)
  
  # Create race groups 
  race <- rep(1:4, times = npergroup)
    
    # create dummy vars for 'race' with race == 1 as referent group
    dummyrace2 <- as.integer(race == 2)
    dummyrace3 <- as.integer(race == 3)
    dummyrace4 <- as.integer(race == 4)
  
  # combine into one dataframe
  data <- data.frame(dummyrace2,dummyrace3,dummyrace4,race)
  
  # Simulate normally distributed outcome ####
  
    # Set desired means and sd for race groups in order from 1 through 4
    # Note: The means and standard deviations can be changed as needed to simulate desired assessment
    means <- c(-1,-1,-1,-0.635)
    sd <- c(1, 1, 1,1)
    
    # Simulate normally dist outcome with different mean for race group 4
    y <- c()
    for(i in 1: length(race)) {
      y <- c(y, rnorm(1, 
                      mean = means[race[i]],
                      sd = sd[race[i]]))
      }
  
    # Dataframe to store outcome
    y <- data.frame(y)
    data <- cbind(y,data) 
  
  # Create new race2 variable with 5 groups ####
    # 100 observations in race group 4 is split randomly into n = 65 and n = 35 into race group 4 and 5, respectively in new race2 variable
    # Note: You can change the '65' value in line 50 below to desired group split
    data_full <- data %>%
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
        dummyrace2_5 = as.integer(race2 == 5))
    
    # Check correct grouping above 
    # table(data_full$race, data_full$race2)
  
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
        iqr = q3-q1)%>% 
      as.data.frame()
  
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
        iqr = q3-q1) %>% 
      as.data.frame()
  
  # Linear regressions and joint test ####
    # Model 1 - four race groups
    model1 <- lm(y ~ dummyrace2 + dummyrace3 + dummyrace4, data = data_full)
    summary_model1 <- summary(model1) 
    coeff_m1 <- as.data.frame(summary_model1$coefficients) %>% 
      rename(estimate = Estimate,
             se = `Std. Error`,
             t = 't value',
             p = `Pr(>|t|)`) %>% 
      mutate(coeff = rownames(.))
    
    coeff_m1$Term <- rownames(coeff_m1)
  
    # Model 2 - five race groups    
    model2 <- lm(y ~ dummyrace2_2 + dummyrace2_3 + dummyrace2_4 + dummyrace2_5,
                 data = data_full)
    summary_model2 <- summary(model2) 
    coeff_m2 <- as.data.frame(summary_model2$coefficients) %>% 
      rename(estimate = Estimate,
             se = `Std. Error`,
             t = 't value',
             p = `Pr(>|t|)`)%>% 
      mutate(coeff = rownames(.))
  
  # Pulling joint test values into dataframe ####
  
    # MODEL 1
      # Pull values from model output
      f_statistic <- summary_model1$fstatistic
      f_value <- f_statistic[1]
      df1 <- f_statistic[2]
      df2 <- f_statistic[3]
      
      # Calculate the p-value for the F-statistic
      f_p_value <- pf(f_value, df1, df2, lower.tail = FALSE)
      
      # Create a dataframe for the F-statistic p-value
      f_p_value_df <- data.frame(
        f = f_value,
        p = f_p_value)
    
    # MODEL 2
      f_statistic_m2 <- summary_model2$fstatistic
      f_value_m2 <- f_statistic_m2[1]
      df1_m2 <- f_statistic_m2[2]
      df2_m2 <- f_statistic_m2[3]
      
      # Calculate the p-value for the F-statistic
      f_p_value_m2 <- pf(f_value_m2, df1_m2, df2_m2, lower.tail = FALSE)
      
      # Create a dataframe for the F-statistic p-value
      f_p_value_df_m2 <- data.frame(
        f = f_value_m2,
        p = f_p_value_m2)
  
  # Multiple Comparisons Adjustments ####
    # Model 1 - four race groups
      # Extract p-values for dummyrace1, dummyrace2, and dummyrace3
      p_values <- summary_model1$coefficients[, "Pr(>|t|)"][2:4]
      
      # Adjustment methods
      adjust_methods <- c("bonferroni", "BH", "fdr","hochberg",
                          "BY", "hommel", "holm")
      
      # Adjust p-values using different methods
      p_bonferroni <- p.adjust(p_values, method = "bonferroni")
      p_bh <- p.adjust(p_values, method = "BH")
      p_fdr <- p.adjust(p_values, method = "fdr")
      p_hochberg <- p.adjust(p_values, method = "hochberg")
      p_by <- p.adjust(p_values, method = "BY")
      p_hommel <- p.adjust(p_values, method = "hommel")
      p_holm <- p.adjust(p_values, method = "holm")
      
      # Create dataframe of original and adjusted p-values
      model1_pvalues <- data.frame(
        method = rep(adjust_methods, each = length(p_values)),
        dummyrace = rep(paste("dummyrace", 2:4, sep = ""), times = length(adjust_methods)),
        original_p = rep(p_values, times = length(adjust_methods)),
        adjusted_p = c(p_bonferroni, p_holm, p_fdr, p_hochberg,
                       p_by,p_hommel, p_bh))
  
    # Model 2 - five race groups
    
    # Same steps as above but for Model 2
    p_values2 <- summary_model2$coefficients[, "Pr(>|t|)"][2:5]
    
    p_bonferroni2 <- p.adjust(p_values2, method = "bonferroni")
    p_bh2 <- p.adjust(p_values2, method = "BH")
    p_fdr2 <- p.adjust(p_values2, method = "fdr")
    p_hochberg2 <- p.adjust(p_values2, method = "hochberg")
    p_by2 <- p.adjust(p_values2, method = "BY")
    p_hommel2 <- p.adjust(p_values2, method = "hommel")
    p_holm2 <- p.adjust(p_values2, method = "holm")
    
    model2_pvalues <- data.frame(
      method = rep(adjust_methods, each = length(p_values2)),
      dummyrace = rep(paste("dummyrace", 2:5, sep = ""), times = length(adjust_methods)),
      original_p = rep(p_values2, times = length(adjust_methods)),
      adjusted_p = c(p_bonferroni2, p_holm2, p_fdr2, p_hochberg2,
                     p_by2,p_hommel2, p_bh2))
  
  summarystats_full$i <- i
  summarystats2_full$i <- i
  coeff_m1$i <- i
  coeff_m2$i <- i
  f_p_value_df$i <- i
  f_p_value_df_m2$i <- i
  model1_pvalues$i <- i
  model2_pvalues$i <- i
  
  return(list(summarystats_full=summarystats_full,
              summarystats2_full=summarystats2_full,
              coeff_m1=coeff_m1,
              coeff_m2=coeff_m2,
              f_p_value_df=f_p_value_df,
              f_p_value_df_m2=f_p_value_df_m2,
              model1_pvalues=model1_pvalues,
              model2_pvalues=model2_pvalues))
}

# creating empty list to place data
  results <- main_fn(0) 

# binding together all iterations into list
  for(i in 1:n_iterations){
    results_i <- main_fn(i)
    results$summarystats_full <- rbind(results_i$summarystats_full,results$summarystats_full)
    results$summarystats2_full <- rbind(results_i$summarystats2_full,results$summarystats2_full)
    results$coeff_m1 <- rbind(results_i$coeff_m1,results$coeff_m1)
    results$coeff_m2 <- rbind(results_i$coeff_m2,results$coeff_m2)
    results$f_p_value_df <- rbind(results_i$f_p_value_df,results$f_p_value_df)
    results$f_p_value_df_m2 <- rbind(results_i$f_p_value_df_m2,results$f_p_value_df_m2)
    results$model1_pvalues <- rbind(results_i$model1_pvalues,results$model1_pvalues)
    results$model2_pvalues <- rbind(results_i$model2_pvalues,results$model2_pvalues)
  }

#### Summarizing iterations for output ####
  mean_summary_stats <- results$summarystats_full %>%
    group_by(i,race) %>% 
    summarise(
      mean_means = round(mean(mean),2),
      mean_sd = round(mean(sd),2),
      mean_median = round(mean(median),2),
      mean_q1 = round(mean(q1),2),
      mean_q3 = round(mean(q3),2),
      mean_min = round(mean(min),2),
      mean_max = round(mean(max),2),
      mean_iqr = round(mean(iqr),2))
  
  mean_summary_stats2 <- results$summarystats2_full %>%
    group_by(i,race2) %>% 
    summarise(
      mean_means = round(mean(mean),2),
      mean_sd = round(mean(sd),2),
      mean_median = round(mean(median),2),
      mean_q1 = round(mean(q1),2),
      mean_q3 = round(mean(q3),2),
      mean_min = round(mean(min),2),
      mean_max = round(mean(max),2),
      mean_iqr = round(mean(iqr),2))

  mean_m1_jointp <- results$f_p_value_df %>%
    group_by(i) %>% 
    summarise(
      mean_joint_f = round(mean(f),2),
      mean_joint_p = round(mean(p),3))
  
  mean_m2_jointp <- results$f_p_value_df_m2 %>%
    group_by(i) %>% 
    summarise(
      mean_joint_f = round(mean(f),2),
      mean_joint_p = round(mean(p),3))
  
  mean_m1_coeff <- results$coeff_m1 %>%
    group_by(i,coeff) %>% 
    summarise(
      mean_coeff = round(mean(estimate),2),
      mean_se = round(mean(se),2),
      mean_t = round(mean(t),2),
      mean_p = round(mean(p),3))
  
  mean_m2_coeff <- results$coeff_m2 %>%
    group_by(i,coeff) %>% 
    summarise(
      mean_coeff = round(mean(estimate),2),
      mean_se = round(mean(se),2),
      mean_t = round(mean(t),2),
      mean_p = round(mean(p),3))
  
  mean_m1_p <- results$model1_pvalues %>%
    group_by(i,dummyrace) %>% 
    summarise(
      mean_original_p = round(mean(original_p),3))
  
  mean_m1_adjp <- results$model1_pvalues %>%
    group_by(i,dummyrace,method) %>% 
    summarise(
      mean_adj_p = round(mean(adjusted_p),3))
  
  mean_m2_p <- results$model2_pvalues %>%
    group_by(i,dummyrace) %>% 
    summarise(
      mean_original_p = round(mean(original_p),3))
  
  mean_m2_adjp <- results$model2_pvalues %>%
    group_by(i,dummyrace,method) %>% 
    summarise(
      mean_adj_p = round(mean(adjusted_p),3))
