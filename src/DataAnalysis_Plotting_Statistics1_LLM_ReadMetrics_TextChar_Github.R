###########################################################################################################################
# Author: Rick Essers
# Lab: Cellular Genomic Medicine, Clinical Genetics, Maastricht University Medical Center (MUMC+)

# script purpose: Performing descriptive and statistical analysis on the readability metrics and textual characteristics.

# input: Readability metrics and textual characteristics dataset. 
#        colnames: "OR_LLM","ReadingGrade","ReadingDifficulty","AgeRange","NumberofWords","Hardwords","PassiveVoice",
#        "LongSentences","TotalSentences","FLESCH","GUNNING","FKGL","COLEMAN","SMOG","LINSER".

# output: Descriptive and statistical analysis (Students t-test, Wilcoxon rank sum test (Mann-Whitney test), and 
#         Kruskal-Wallis test, ANOVA)

_###########################################################################################################################

library(dplyr)
library(car)
library(dunn.test)

# Clear the workspace
rm(list=ls(all=T))

# Read the data
LLM_Metrics <- read.csv("/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Input/20241025_LLM_MetricScores.csv", sep = ";")
LLM_Metrics$OR_LLM <- gsub(" ","", LLM_Metrics$OR_LLM)

# Flesch Reading Ease Formula (FRE)
# Gunning Fog Index (GFI)
# Flesch–Kincaid Grade Level (FKGL)
# Coleman–Liau Index (CLI)
# Simplified Measure of Gobbledegook Index (SMOG)
# Linsear Write Formula (LWF)

LLM_Read_Metrics <- LLM_Metrics[, c("OR_LLM","FLESCH","GUNNING","FKGL","COLEMAN","SMOG","LINSER","ReadingGrade")]
LLM_Text_Characteristics <- LLM_Metrics[, c("OR_LLM","NumberofWords","Hardwords","LongSentences","TotalSentences","PassiveVoice")]


### Readability metrics 
{
###Descriptive statistics 
{
    # Replace commas in other columns with decimals and convert to numeric
    comma_cols <- c("FLESCH", "GUNNING", "FKGL", "COLEMAN", "SMOG", "LINSER")
    LLM_Read_Metrics[comma_cols] <- lapply(LLM_Read_Metrics[comma_cols], function(x) as.numeric(gsub(",", ".", x)))
    
    # Calculate mean and standard deviation for each unique value in OR_LLM
    LLM_Read_Metrics_Mean_SD <- LLM_Read_Metrics %>%
      group_by(OR_LLM) %>%
      summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), 
                                          sd = ~sd(.x, na.rm = TRUE))))
    
    # View the result
    print(LLM_Read_Metrics_Mean_SD)
    
    write.csv(LLM_Read_Metrics_Mean_SD, "/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Output/20241029_LLM_Read_Metrics_Mean_SD.csv", row.names = FALSE, quote = TRUE)
    
  }
  
###Statistics 
# Extract metrics per LLM
LLM_Read_Metrics_GPT_3_5 <- subset(LLM_Read_Metrics, OR_LLM == "GPT_3_5")
LLM_Read_Metrics_Copilot <- subset(LLM_Read_Metrics, OR_LLM == "Copilot")
LLM_Read_Metrics_Gemini <- subset(LLM_Read_Metrics, OR_LLM == "Gemini")
LLM_Read_Metrics_GPT_4_0 <- subset(LLM_Read_Metrics, OR_LLM == "GPT_4_0")
LLM_Read_Metrics_OT <- subset(LLM_Read_Metrics, OR_LLM == "Original_Text")
  
### single comparisons
{ # Define a function to perform both tests and collect results
  perform_tests <- function(var_name, df1, df2, model_name) {
    t_res <- t.test(df1[[var_name]], df2[[var_name]])
    wilcox_res <- wilcox.test(df1[[var_name]], df2[[var_name]], exact = FALSE)
    
    # Collect results into a list
    data.frame(
      Metric = var_name,
      Comparison = model_name,
      #t_mean_diff = t_res$estimate[1] - t_res$estimate[2],
      ttest_p_value = t_res$p.value,
      #wilcox_statistic = wilcox_res$statistic,
      wilcox_p_value = wilcox_res$p.value
    )
  }
  
  # Variables and models to loop over
  metrics <- c("FLESCH", "GUNNING", "FKGL", "COLEMAN", "SMOG", "LINSER", "ReadingGrade")
  models <- list(GPT_3_5 = LLM_Read_Metrics_GPT_3_5, 
                 GPT_4_0 = LLM_Read_Metrics_GPT_4_0, 
                 Copilot = LLM_Read_Metrics_Copilot, 
                 Gemini = LLM_Read_Metrics_Gemini)
  
  # Initialize an empty dataframe to store results
  results <- data.frame()
  
  # Loop through each metric and model, perform tests, and add results to the dataframe
  for (metric in metrics) {
    for (model_name in names(models)) {
      res <- perform_tests(metric, LLM_Read_Metrics_OT, models[[model_name]], model_name)
      results <- rbind(results, res)
    }
  }
  
  # View the results
  print(results)
  
  write.csv(results, "/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Output/20241029_LLM_Metrics_Individual_Comparisons_TTest_Wilcox.csv", row.names = FALSE, quote = TRUE)
  }
  
### Multiple comparisons
### FLESCH 
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_FLESCH <- shapiro.test(LLM_Read_Metrics_GPT_3_5$FLESCH)
    shapiro_copilot_FLESCH <- shapiro.test(LLM_Read_Metrics_Copilot$FLESCH)
    shapiro_gemini_FLESCH <- shapiro.test(LLM_Read_Metrics_Gemini$FLESCH)
    shapiro_gpt_4_0_FLESCH <- shapiro.test(LLM_Read_Metrics_GPT_4_0$FLESCH)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("FLESCH: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_FLESCH)
    print(shapiro_copilot_FLESCH)
    print(shapiro_gemini_FLESCH)
    print(shapiro_gpt_4_0_FLESCH)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_FLESCH$p.value < 0.05 | shapiro_copilot_FLESCH$p.value < 0.05 | 
        shapiro_gemini_FLESCH$p.value < 0.05 | shapiro_gpt_4_0_FLESCH$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_FLESCH <- kruskal.test(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_FLESCH)
      
      if (kruskal_test_FLESCH$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_FLESCH <- dunn.test(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_FLESCH)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_FLESCH <- kruskal.test(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_FLESCH)
        
        if (kruskal_test_FLESCH$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FLESCH <- dunn.test(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_FLESCH)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_FLESCH <- aov(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_FLESCH <- summary(anova_results_FLESCH)
        print("ANOVA Results:")
        print(anova_summary_FLESCH)
        
        if (anova_summary_FLESCH[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FLESCH <- TukeyHSD(anova_results)
          print(posthoc_results_FLESCH)
        }
      }
    }
  }
  
### GUNNING
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_GUNNING <- shapiro.test(LLM_Read_Metrics_GPT_3_5$GUNNING)
    shapiro_copilot_GUNNING <- shapiro.test(LLM_Read_Metrics_Copilot$GUNNING)
    shapiro_gemini_GUNNING <- shapiro.test(LLM_Read_Metrics_Gemini$GUNNING)
    shapiro_gpt_4_0_GUNNING <- shapiro.test(LLM_Read_Metrics_GPT_4_0$GUNNING)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("GUNNING: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_GUNNING)
    print(shapiro_copilot_GUNNING)
    print(shapiro_gemini_GUNNING)
    print(shapiro_gpt_4_0_GUNNING)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_GUNNING$p.value < 0.05 | shapiro_copilot_GUNNING$p.value < 0.05 | 
        shapiro_gemini_GUNNING$p.value < 0.05 | shapiro_gpt_4_0_GUNNING$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_GUNNING <- kruskal.test(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_GUNNING)
      
      if (kruskal_test_GUNNING$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_GUNNING <- dunn.test(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_GUNNING)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_GUNNING <- kruskal.test(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_GUNNING)
        
        if (kruskal_test_GUNNING$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_GUNNING <- dunn.test(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_GUNNING)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_GUNNING <- aov(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_GUNNING <- summary(anova_results_GUNNING)
        print("ANOVA Results:")
        print(anova_summary_GUNNING)
        
        if (anova_summary_GUNNING[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_GUNNING <- TukeyHSD(anova_results)
          print(posthoc_results_GUNNING)
        }
      }
    }
  }
  
### FKGL
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_FKGL <- shapiro.test(LLM_Read_Metrics_GPT_3_5$FKGL)
    shapiro_copilot_FKGL <- shapiro.test(LLM_Read_Metrics_Copilot$FKGL)
    shapiro_gemini_FKGL <- shapiro.test(LLM_Read_Metrics_Gemini$FKGL)
    shapiro_gpt_4_0_FKGL <- shapiro.test(LLM_Read_Metrics_GPT_4_0$FKGL)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("FKGL: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_FKGL)
    print(shapiro_copilot_FKGL)
    print(shapiro_gemini_FKGL)
    print(shapiro_gpt_4_0_FKGL)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_FKGL$p.value < 0.05 | shapiro_copilot_FKGL$p.value < 0.05 | 
        shapiro_gemini_FKGL$p.value < 0.05 | shapiro_gpt_4_0_FKGL$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_FKGL <- kruskal.test(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_FKGL)
      
      if (kruskal_test_FKGL$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_FKGL <- dunn.test(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_FKGL)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_FKGL <- kruskal.test(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_FKGL)
        
        if (kruskal_test_FKGL$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FKGL <- dunn.test(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_FKGL)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_FKGL <- aov(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_FKGL <- summary(anova_results_FKGL)
        print("ANOVA Results:")
        print(anova_summary_FKGL)
        
        if (anova_summary_FKGL[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FKGL <- TukeyHSD(anova_results)
          print(posthoc_results_FKGL)
        }
      }
    }
  }
  
### COLEMAN
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_COLEMAN <- shapiro.test(LLM_Read_Metrics_GPT_3_5$COLEMAN)
    shapiro_copilot_COLEMAN <- shapiro.test(LLM_Read_Metrics_Copilot$COLEMAN)
    shapiro_gemini_COLEMAN <- shapiro.test(LLM_Read_Metrics_Gemini$COLEMAN)
    shapiro_gpt_4_0_COLEMAN <- shapiro.test(LLM_Read_Metrics_GPT_4_0$COLEMAN)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("COLEMAN: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_COLEMAN)
    print(shapiro_copilot_COLEMAN)
    print(shapiro_gemini_COLEMAN)
    print(shapiro_gpt_4_0_COLEMAN)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_COLEMAN$p.value < 0.05 | shapiro_copilot_COLEMAN$p.value < 0.05 | 
        shapiro_gemini_COLEMAN$p.value < 0.05 | shapiro_gpt_4_0_COLEMAN$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_COLEMAN <- kruskal.test(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_COLEMAN)
      
      if (kruskal_test_COLEMAN$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_COLEMAN <- dunn.test(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_COLEMAN)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_COLEMAN <- kruskal.test(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_COLEMAN)
        
        if (kruskal_test_COLEMAN$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_COLEMAN <- dunn.test(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_COLEMAN)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_COLEMAN <- aov(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_COLEMAN <- summary(anova_results_COLEMAN)
        print("ANOVA Results:")
        print(anova_summary_COLEMAN)
        
        if (anova_summary_COLEMAN[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_COLEMAN <- TukeyHSD(anova_results)
          print(posthoc_results_COLEMAN)
        }
      }
    }
  }
  
### SMOG
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_SMOG <- shapiro.test(LLM_Read_Metrics_GPT_3_5$SMOG)
    shapiro_copilot_SMOG <- shapiro.test(LLM_Read_Metrics_Copilot$SMOG)
    shapiro_gemini_SMOG <- shapiro.test(LLM_Read_Metrics_Gemini$SMOG)
    shapiro_gpt_4_0_SMOG <- shapiro.test(LLM_Read_Metrics_GPT_4_0$SMOG)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("SMOG: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_SMOG)
    print(shapiro_copilot_SMOG)
    print(shapiro_gemini_SMOG)
    print(shapiro_gpt_4_0_SMOG)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_SMOG$p.value < 0.05 | shapiro_copilot_SMOG$p.value < 0.05 | 
        shapiro_gemini_SMOG$p.value < 0.05 | shapiro_gpt_4_0_SMOG$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_SMOG <- kruskal.test(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_SMOG)
      
      if (kruskal_test_SMOG$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_SMOG <- dunn.test(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_SMOG)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_SMOG <- kruskal.test(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_SMOG)
        
        if (kruskal_test_SMOG$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_SMOG <- dunn.test(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_SMOG)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_SMOG <- aov(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_SMOG <- summary(anova_results_SMOG)
        print("ANOVA Results:")
        print(anova_summary_SMOG)
        
        if (anova_summary_SMOG[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_SMOG <- TukeyHSD(anova_results)
          print(posthoc_results_SMOG)
        }
      }
    }
  }
  
### LINSER
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_LINSER <- shapiro.test(LLM_Read_Metrics_GPT_3_5$LINSER)
    shapiro_copilot_LINSER <- shapiro.test(LLM_Read_Metrics_Copilot$LINSER)
    shapiro_gemini_LINSER <- shapiro.test(LLM_Read_Metrics_Gemini$LINSER)
    shapiro_gpt_4_0_LINSER <- shapiro.test(LLM_Read_Metrics_GPT_4_0$LINSER)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("LINSER: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_LINSER)
    print(shapiro_copilot_LINSER)
    print(shapiro_gemini_LINSER)
    print(shapiro_gpt_4_0_LINSER)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_LINSER$p.value < 0.05 | shapiro_copilot_LINSER$p.value < 0.05 | 
        shapiro_gemini_LINSER$p.value < 0.05 | shapiro_gpt_4_0_LINSER$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_LINSER <- kruskal.test(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_LINSER)
      
      if (kruskal_test_LINSER$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_LINSER <- dunn.test(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_LINSER)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_LINSER <- kruskal.test(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_LINSER)
        
        if (kruskal_test_LINSER$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_LINSER <- dunn.test(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_LINSER)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_LINSER <- aov(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_LINSER <- summary(anova_results_LINSER)
        print("ANOVA Results:")
        print(anova_summary_LINSER)
        
        if (anova_summary_LINSER[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_LINSER <- TukeyHSD(anova_results)
          print(posthoc_results_LINSER)
        }
      }
    }
  }
  
### ReadingGrade
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_ReadingGrade <- shapiro.test(LLM_Read_Metrics_GPT_3_5$ReadingGrade)
    shapiro_copilot_ReadingGrade <- shapiro.test(LLM_Read_Metrics_Copilot$ReadingGrade)
    shapiro_gemini_ReadingGrade <- shapiro.test(LLM_Read_Metrics_Gemini$ReadingGrade)
    shapiro_gpt_4_0_ReadingGrade <- shapiro.test(LLM_Read_Metrics_GPT_4_0$ReadingGrade)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("ReadingGrade: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_ReadingGrade)
    print(shapiro_copilot_ReadingGrade)
    print(shapiro_gemini_ReadingGrade)
    print(shapiro_gpt_4_0_ReadingGrade)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_ReadingGrade$p.value < 0.05 | shapiro_copilot_ReadingGrade$p.value < 0.05 | 
        shapiro_gemini_ReadingGrade$p.value < 0.05 | shapiro_gpt_4_0_ReadingGrade$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_ReadingGrade <- kruskal.test(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_ReadingGrade)
      
      if (kruskal_test_ReadingGrade$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_ReadingGrade <- dunn.test(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_ReadingGrade)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_ReadingGrade <- kruskal.test(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_ReadingGrade)
        
        if (kruskal_test_ReadingGrade$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_ReadingGrade <- dunn.test(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_ReadingGrade)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_ReadingGrade <- aov(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_ReadingGrade <- summary(anova_results_ReadingGrade)
        print("ANOVA Results:")
        print(anova_summary_ReadingGrade)
        
        if (anova_summary_ReadingGrade[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_ReadingGrade <- TukeyHSD(anova_results)
          print(posthoc_results_ReadingGrade)
        }
      }
    }
  }
  
}


### Textual characteristics 
{
###Descriptive statistics 
{
    # Replace % in columns and convert to numeric
    comma_cols <- c("Hardwords", "LongSentences", "PassiveVoice")
    LLM_Text_Characteristics[comma_cols] <- lapply(LLM_Text_Characteristics[comma_cols], function(x) as.numeric(gsub("%", "", x)))
    
    # Calculate mean and standard deviation for each unique value in OR_LLM
    LLM_Text_Characteristics_Mean_SD <- LLM_Text_Characteristics %>%
      group_by(OR_LLM) %>%
      summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), 
                                          sd = ~sd(.x, na.rm = TRUE))))
    
    # View the result
    print(LLM_Text_Characteristics_Mean_SD)
    
    write.csv(LLM_Text_Characteristics_Mean_SD, "/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Output/20241029_LLM_Text_Characteristics_Mean_SD.csv", row.names = FALSE, quote = TRUE)
    
  }

###Statistics 
# Extract metrics per LLM
LLM_Text_Characteristics_GPT_3_5 <- subset(LLM_Text_Characteristics, OR_LLM == "GPT_3_5")
LLM_Text_Characteristics_Copilot <- subset(LLM_Text_Characteristics, OR_LLM == "Copilot")
LLM_Text_Characteristics_Gemini <- subset(LLM_Text_Characteristics, OR_LLM == "Gemini")
LLM_Text_Characteristics_GPT_4_0 <- subset(LLM_Text_Characteristics, OR_LLM == "GPT_4_0")
LLM_Text_Characteristics_OT <- subset(LLM_Text_Characteristics, OR_LLM == "Original_Text")
  
### single comparisons
{
# Define a function to perform both tests and collect results
perform_tests <- function(var_name, df1, df2, model_name) {
  t_res <- t.test(df1[[var_name]], df2[[var_name]])
  wilcox_res <- wilcox.test(df1[[var_name]], df2[[var_name]], exact = FALSE)
  
  # Collect results into a list
  data.frame(
    Metric = var_name,
    Comparison = model_name,
    #t_mean_diff = t_res$estimate[1] - t_res$estimate[2],
    ttest_p_value = t_res$p.value,
    #wilcox_statistic = wilcox_res$statistic,
    wilcox_p_value = wilcox_res$p.value
  )
}
  
# Variables and models to loop over
metrics <- c("NumberofWords","Hardwords","LongSentences","TotalSentences","PassiveVoice")
models <- list(GPT_3_5 = LLM_Text_Characteristics_GPT_3_5, 
               GPT_4_0 = LLM_Text_Characteristics_GPT_4_0, 
               Copilot = LLM_Text_Characteristics_Copilot, 
               Gemini = LLM_Text_Characteristics_Gemini)

# Initialize an empty dataframe to store results
results <- data.frame()

# Loop through each metric and model, perform tests, and add results to the dataframe
for (metric in metrics) {
    for (model_name in names(models)) {
      res <- perform_tests(metric, LLM_Text_Characteristics_OT, models[[model_name]], model_name)
      results <- rbind(results, res)
    }
  }

# View the results
print(results)

write.csv(results, "/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Output/20241029_LLM_Text_Characteristics_Individual_Comparisons_TTest_Wilcox.csv", row.names = FALSE, quote = TRUE)
}
  
### Multiple comparisons
### FLESCH 
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_FLESCH <- shapiro.test(LLM_Read_Metrics_GPT_3_5$FLESCH)
    shapiro_copilot_FLESCH <- shapiro.test(LLM_Read_Metrics_Copilot$FLESCH)
    shapiro_gemini_FLESCH <- shapiro.test(LLM_Read_Metrics_Gemini$FLESCH)
    shapiro_gpt_4_0_FLESCH <- shapiro.test(LLM_Read_Metrics_GPT_4_0$FLESCH)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("FLESCH: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_FLESCH)
    print(shapiro_copilot_FLESCH)
    print(shapiro_gemini_FLESCH)
    print(shapiro_gpt_4_0_FLESCH)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_FLESCH$p.value < 0.05 | shapiro_copilot_FLESCH$p.value < 0.05 | 
        shapiro_gemini_FLESCH$p.value < 0.05 | shapiro_gpt_4_0_FLESCH$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_FLESCH <- kruskal.test(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_FLESCH)
      
      if (kruskal_test_FLESCH$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_FLESCH <- dunn.test(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_FLESCH)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_FLESCH <- kruskal.test(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_FLESCH)
        
        if (kruskal_test_FLESCH$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FLESCH <- dunn.test(LLM_Read_Metrics$FLESCH, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_FLESCH)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_FLESCH <- aov(FLESCH ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_FLESCH <- summary(anova_results_FLESCH)
        print("ANOVA Results:")
        print(anova_summary_FLESCH)
        
        if (anova_summary_FLESCH[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FLESCH <- TukeyHSD(anova_results)
          print(posthoc_results_FLESCH)
        }
      }
    }
  }
  
### GUNNING
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_GUNNING <- shapiro.test(LLM_Read_Metrics_GPT_3_5$GUNNING)
    shapiro_copilot_GUNNING <- shapiro.test(LLM_Read_Metrics_Copilot$GUNNING)
    shapiro_gemini_GUNNING <- shapiro.test(LLM_Read_Metrics_Gemini$GUNNING)
    shapiro_gpt_4_0_GUNNING <- shapiro.test(LLM_Read_Metrics_GPT_4_0$GUNNING)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("GUNNING: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_GUNNING)
    print(shapiro_copilot_GUNNING)
    print(shapiro_gemini_GUNNING)
    print(shapiro_gpt_4_0_GUNNING)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_GUNNING$p.value < 0.05 | shapiro_copilot_GUNNING$p.value < 0.05 | 
        shapiro_gemini_GUNNING$p.value < 0.05 | shapiro_gpt_4_0_GUNNING$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_GUNNING <- kruskal.test(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_GUNNING)
      
      if (kruskal_test_GUNNING$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_GUNNING <- dunn.test(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_GUNNING)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_GUNNING <- kruskal.test(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_GUNNING)
        
        if (kruskal_test_GUNNING$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_GUNNING <- dunn.test(LLM_Read_Metrics$GUNNING, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_GUNNING)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_GUNNING <- aov(GUNNING ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_GUNNING <- summary(anova_results_GUNNING)
        print("ANOVA Results:")
        print(anova_summary_GUNNING)
        
        if (anova_summary_GUNNING[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_GUNNING <- TukeyHSD(anova_results)
          print(posthoc_results_GUNNING)
        }
      }
    }
  }
  
### FKGL
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_FKGL <- shapiro.test(LLM_Read_Metrics_GPT_3_5$FKGL)
    shapiro_copilot_FKGL <- shapiro.test(LLM_Read_Metrics_Copilot$FKGL)
    shapiro_gemini_FKGL <- shapiro.test(LLM_Read_Metrics_Gemini$FKGL)
    shapiro_gpt_4_0_FKGL <- shapiro.test(LLM_Read_Metrics_GPT_4_0$FKGL)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("FKGL: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_FKGL)
    print(shapiro_copilot_FKGL)
    print(shapiro_gemini_FKGL)
    print(shapiro_gpt_4_0_FKGL)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_FKGL$p.value < 0.05 | shapiro_copilot_FKGL$p.value < 0.05 | 
        shapiro_gemini_FKGL$p.value < 0.05 | shapiro_gpt_4_0_FKGL$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_FKGL <- kruskal.test(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_FKGL)
      
      if (kruskal_test_FKGL$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_FKGL <- dunn.test(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_FKGL)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_FKGL <- kruskal.test(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_FKGL)
        
        if (kruskal_test_FKGL$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FKGL <- dunn.test(LLM_Read_Metrics$FKGL, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_FKGL)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_FKGL <- aov(FKGL ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_FKGL <- summary(anova_results_FKGL)
        print("ANOVA Results:")
        print(anova_summary_FKGL)
        
        if (anova_summary_FKGL[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_FKGL <- TukeyHSD(anova_results)
          print(posthoc_results_FKGL)
        }
      }
    }
  }
  
### COLEMAN
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_COLEMAN <- shapiro.test(LLM_Read_Metrics_GPT_3_5$COLEMAN)
    shapiro_copilot_COLEMAN <- shapiro.test(LLM_Read_Metrics_Copilot$COLEMAN)
    shapiro_gemini_COLEMAN <- shapiro.test(LLM_Read_Metrics_Gemini$COLEMAN)
    shapiro_gpt_4_0_COLEMAN <- shapiro.test(LLM_Read_Metrics_GPT_4_0$COLEMAN)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("COLEMAN: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_COLEMAN)
    print(shapiro_copilot_COLEMAN)
    print(shapiro_gemini_COLEMAN)
    print(shapiro_gpt_4_0_COLEMAN)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_COLEMAN$p.value < 0.05 | shapiro_copilot_COLEMAN$p.value < 0.05 | 
        shapiro_gemini_COLEMAN$p.value < 0.05 | shapiro_gpt_4_0_COLEMAN$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_COLEMAN <- kruskal.test(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_COLEMAN)
      
      if (kruskal_test_COLEMAN$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_COLEMAN <- dunn.test(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_COLEMAN)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_COLEMAN <- kruskal.test(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_COLEMAN)
        
        if (kruskal_test_COLEMAN$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_COLEMAN <- dunn.test(LLM_Read_Metrics$COLEMAN, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_COLEMAN)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_COLEMAN <- aov(COLEMAN ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_COLEMAN <- summary(anova_results_COLEMAN)
        print("ANOVA Results:")
        print(anova_summary_COLEMAN)
        
        if (anova_summary_COLEMAN[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_COLEMAN <- TukeyHSD(anova_results)
          print(posthoc_results_COLEMAN)
        }
      }
    }
  }
  
### SMOG
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_SMOG <- shapiro.test(LLM_Read_Metrics_GPT_3_5$SMOG)
    shapiro_copilot_SMOG <- shapiro.test(LLM_Read_Metrics_Copilot$SMOG)
    shapiro_gemini_SMOG <- shapiro.test(LLM_Read_Metrics_Gemini$SMOG)
    shapiro_gpt_4_0_SMOG <- shapiro.test(LLM_Read_Metrics_GPT_4_0$SMOG)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("SMOG: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_SMOG)
    print(shapiro_copilot_SMOG)
    print(shapiro_gemini_SMOG)
    print(shapiro_gpt_4_0_SMOG)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_SMOG$p.value < 0.05 | shapiro_copilot_SMOG$p.value < 0.05 | 
        shapiro_gemini_SMOG$p.value < 0.05 | shapiro_gpt_4_0_SMOG$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_SMOG <- kruskal.test(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_SMOG)
      
      if (kruskal_test_SMOG$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_SMOG <- dunn.test(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_SMOG)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_SMOG <- kruskal.test(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_SMOG)
        
        if (kruskal_test_SMOG$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_SMOG <- dunn.test(LLM_Read_Metrics$SMOG, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_SMOG)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_SMOG <- aov(SMOG ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_SMOG <- summary(anova_results_SMOG)
        print("ANOVA Results:")
        print(anova_summary_SMOG)
        
        if (anova_summary_SMOG[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_SMOG <- TukeyHSD(anova_results)
          print(posthoc_results_SMOG)
        }
      }
    }
  }
  
### LINSER
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_LINSER <- shapiro.test(LLM_Read_Metrics_GPT_3_5$LINSER)
    shapiro_copilot_LINSER <- shapiro.test(LLM_Read_Metrics_Copilot$LINSER)
    shapiro_gemini_LINSER <- shapiro.test(LLM_Read_Metrics_Gemini$LINSER)
    shapiro_gpt_4_0_LINSER <- shapiro.test(LLM_Read_Metrics_GPT_4_0$LINSER)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("LINSER: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_LINSER)
    print(shapiro_copilot_LINSER)
    print(shapiro_gemini_LINSER)
    print(shapiro_gpt_4_0_LINSER)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_LINSER$p.value < 0.05 | shapiro_copilot_LINSER$p.value < 0.05 | 
        shapiro_gemini_LINSER$p.value < 0.05 | shapiro_gpt_4_0_LINSER$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_LINSER <- kruskal.test(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_LINSER)
      
      if (kruskal_test_LINSER$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_LINSER <- dunn.test(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_LINSER)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_LINSER <- kruskal.test(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_LINSER)
        
        if (kruskal_test_LINSER$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_LINSER <- dunn.test(LLM_Read_Metrics$LINSER, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_LINSER)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_LINSER <- aov(LINSER ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_LINSER <- summary(anova_results_LINSER)
        print("ANOVA Results:")
        print(anova_summary_LINSER)
        
        if (anova_summary_LINSER[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_LINSER <- TukeyHSD(anova_results)
          print(posthoc_results_LINSER)
        }
      }
    }
  }
  
### ReadingGrade
{
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_ReadingGrade <- shapiro.test(LLM_Read_Metrics_GPT_3_5$ReadingGrade)
    shapiro_copilot_ReadingGrade <- shapiro.test(LLM_Read_Metrics_Copilot$ReadingGrade)
    shapiro_gemini_ReadingGrade <- shapiro.test(LLM_Read_Metrics_Gemini$ReadingGrade)
    shapiro_gpt_4_0_ReadingGrade <- shapiro.test(LLM_Read_Metrics_GPT_4_0$ReadingGrade)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    LLM_Read_Metrics$OR_LLM <- as.factor(LLM_Read_Metrics$OR_LLM)
    levene_test <- leveneTest(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("ReadingGrade: Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_ReadingGrade)
    print(shapiro_copilot_ReadingGrade)
    print(shapiro_gemini_ReadingGrade)
    print(shapiro_gpt_4_0_ReadingGrade)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_ReadingGrade$p.value < 0.05 | shapiro_copilot_ReadingGrade$p.value < 0.05 | 
        shapiro_gemini_ReadingGrade$p.value < 0.05 | shapiro_gpt_4_0_ReadingGrade$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_ReadingGrade <- kruskal.test(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_ReadingGrade)
      
      if (kruskal_test_ReadingGrade$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_ReadingGrade <- dunn.test(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM, method="bonferroni")
        print(posthoc_results_ReadingGrade)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_ReadingGrade <- kruskal.test(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_ReadingGrade)
        
        if (kruskal_test_ReadingGrade$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_ReadingGrade <- dunn.test(LLM_Read_Metrics$ReadingGrade, LLM_Read_Metrics$OR_LLM, method="bonferroni")
          print(posthoc_results_ReadingGrade)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_ReadingGrade <- aov(ReadingGrade ~ OR_LLM, data = LLM_Read_Metrics)
        anova_summary_ReadingGrade <- summary(anova_results_ReadingGrade)
        print("ANOVA Results:")
        print(anova_summary_ReadingGrade)
        
        if (anova_summary_ReadingGrade[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_ReadingGrade <- TukeyHSD(anova_results)
          print(posthoc_results_ReadingGrade)
        }
      }
    }
  }
  
}


