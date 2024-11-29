###########################################################################################################################
# Author: Rick Essers
# Lab: Cellular Genomic Medicine, Clinical Genetics, Maastricht University Medical Center (MUMC+)

# script purpose: Creating barplot and performing statistical analysis on the expert evaluation data.

# input: Expert evaluation dataset colnames: "LLM","Topic","Expert","Question","Score".

# output: Barplot and statistical analysis (Students t-test, Wilcoxon rank sum test (Mann-Whitney test), and 
#         Kruskal-Wallis test, ANOVA)

###########################################################################################################################

library(car)
library(dunn.test)
library(dplyr)
library(ggplot2)

# Clear the workspace
rm(list=ls(all=T))

# Read the data
#LLM_Data <- read.csv("/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Input/20241101_LLM_Expert_Eval.csv", sep = ";")
LLM_Data <- read.csv("/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Input/20241126_Expert_Eval_Alt.csv", sep = ";")

### Discriptive statistics 
{
  # Function to calculate statistics
  calc_stats <- function(data, name, question) {
    mean_val <- mean(data$Score)
    median_val <- median(data$Score)
    sd_val <- sd(data$Score)
    
    return(data.frame(LLM = name, Question = as.character(question), Mean = mean_val, Median = median_val, SD = sd_val))
  }
  
  # Initialize an empty dataframe to store results
  results <- data.frame()
  
  # List of LLMs
  LLMs <- unique(LLM_Data$LLM)
  
  # Loop through each LLM and calculate statistics
  for (llm in LLMs) {
    LLM_Data_subset <- subset(LLM_Data, LLM == llm)
    
    # Overall stats for the LLM
    results <- rbind(results, calc_stats(LLM_Data_subset, llm, "Overall"))
    
    # Stats for each question
    for (q in unique(LLM_Data_subset$Question)) {
      LLM_Data_question <- subset(LLM_Data_subset, Question == q)
      results <- rbind(results, calc_stats(LLM_Data_question, llm, q))
    }
  }
  
  print(results)
}

### Plotting
{
  # Barplot 
  Col_Original_Text   <- "#CBABCB"
  Col_GPT_3_5         <- "#96CAC1"
  Col_GPT_4_0         <- "#8AAFC9" 
  Col_Copilot         <- "#EAB375"
  Col_Gemini          <- "#EA8E83"
  
  results$Question <- gsub("1","Accuracy", results$Question)
  results$Question <- gsub("2","Completeness", results$Question)
  results$Question <- gsub("3","Relevance of omission", results$Question)
  
  results$LLM <- gsub("GPT_4_0","GPT-4", results$LLM)
  results$LLM <- gsub("GPT_3_5","GPT-3.5", results$LLM)
  
  Order_Questions <- c("Accuracy","Completeness","Relevance of omission","Overall")
  results$LLM <- factor(results$LLM, levels = c("GPT-4","GPT-3.5","Copilot","Gemini"))
  
  Barplot_Expert_Eval <- ggplot(results, aes(x = factor(Question, levels = c(Order_Questions)), y = Mean, fill = LLM)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                  position = position_dodge(width = 0.8), width = 0.25, alpha = 0.5) +
    labs(x = "Evaluation metrics" ,y = "Score", title = "Scores by LLM for Each Question") +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
    scale_fill_manual(values = c("Copilot" = Col_Copilot, "Gemini" = Col_Gemini, "GPT-3.5" = Col_GPT_3_5, "GPT-4" = Col_GPT_4_0)) 
  Barplot_Expert_Eval 
  
  lapply(c(".pdf", ".png", ".svg"), function(ext) 
    ggsave(plot = Barplot_Expert_Eval, width = 12, height = 5,
           filename = paste0("/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Figures/20241126_Barplot_ExpertEval", ext))
  )
}


### Statistics 
# Extract overall scores for each LLM
LLM_Data_GPT_3_5_All <- subset(LLM_Data, LLM == "GPT_3_5")
LLM_Data_Copilot_All <- subset(LLM_Data, LLM == "Copilot")
LLM_Data_Gemini_All <- subset(LLM_Data, LLM == "Gemini")
LLM_Data_GPT_4_0_All <- subset(LLM_Data, LLM == "GPT_4_0")

# Statistics
{
  # Define a function to perform both tests and collect results
  {
    perform_tests <- function(df1, df2, question, model1, model2) {
      # Filter by question type if it's a specific question; otherwise, use all data
      if (question != "All") {
        df1 <- df1[df1$Question == question, ]
        df2 <- df2[df2$Question == question, ]
      }
      
      t_res <- t.test(df1$Score, df2$Score)
      wilcox_res <- wilcox.test(df1$Score, df2$Score, exact = FALSE)
      
      # Collect results into a data frame
      data.frame(
        Question = question,
        Model_Comparison = paste(model1, "vs", model2),
        #t_mean_diff = t_res$estimate[1] - t_res$estimate[2],
        t_p_value = t_res$p.value,
        #wilcox_statistic = wilcox_res$statistic,
        wilcox_p_value = wilcox_res$p.value
      )
    }
    
    # Questions, models, and datasets
    questions <- c("1", "2", "3", "All")  # Add "All" to indicate combined data comparison
    models <- list(GPT_3_5 = LLM_Data_GPT_3_5_All, 
                   GPT_4_0 = LLM_Data_GPT_4_0_All, 
                   Copilot = LLM_Data_Copilot_All, 
                   Gemini = LLM_Data_Gemini_All)
    
    # Initialize an empty dataframe to store results
    results <- data.frame()
    
    # Loop through each question and model pair to perform tests
    for (question in questions) {
      for (model1 in names(models)) {
        for (model2 in names(models)) {
          if (model1 != model2) {
            res <- perform_tests(models[[model1]], models[[model2]], question, model1, model2)
            results <- rbind(results, res)
          }
        }
      }
    }
    
    # Remove duplicate comparisons
    results <- results[!duplicated(results[, c("Question", "Model_Comparison")]), ]
    
    # Format p-values as text in scientific notation
    format_p_value <- function(p) {
      format(p, scientific = TRUE, digits = 4)
    }
    results <- results %>%
      mutate(
        t_p_value = sapply(t_p_value, format_p_value),
        wilcox_p_value = sapply(wilcox_p_value, format_p_value)
      )
    
    # View the results
    print(results)
    
    # Save the results as a CSV file
    #write.csv(results, "/Users/G10039937/Surfdrive/ClinicalGenetics/LLMs_Repro/Output/20241101_LLM_Expert_Eval_Individual_Comparisons_TTest_Wilcox.csv", row.names = FALSE, quote = TRUE)
  }  
  
  ### All
  {
    
    LLM_Data_GPT_3_5_All <- subset(LLM_Data, LLM == "GPT_3_5")
    LLM_Data_Copilot_All <- subset(LLM_Data, LLM == "Copilot")
    LLM_Data_Gemini_All <- subset(LLM_Data, LLM == "Gemini")
    LLM_Data_GPT_4_0_All <- subset(LLM_Data, LLM == "GPT_4_0")
    
    LLM_Data_GPT_3_5 <- LLM_Data_GPT_3_5_All
    LLM_Data_Copilot <- LLM_Data_Copilot_All
    LLM_Data_Gemini <- LLM_Data_Gemini_All
    LLM_Data_GPT_4_0 <- LLM_Data_GPT_4_0_All
    
    # Combine data into one dataframe for analysis
    overall_data <- data.frame(
      LLM = c(rep("GPT_3_5", nrow(LLM_Data_GPT_3_5)),
              rep("Copilot", nrow(LLM_Data_Copilot)),
              rep("Gemini", nrow(LLM_Data_Gemini)),
              rep("GPT_4_0", nrow(LLM_Data_GPT_4_0))),
      Score = c(LLM_Data_GPT_3_5$Score,
                LLM_Data_Copilot$Score,
                LLM_Data_Gemini$Score,
                LLM_Data_GPT_4_0$Score)
    )
    
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5 <- shapiro.test(LLM_Data_GPT_3_5$Score)
    shapiro_copilot <- shapiro.test(LLM_Data_Copilot$Score)
    shapiro_gemini <- shapiro.test(LLM_Data_Gemini$Score)
    shapiro_gpt_4_0 <- shapiro.test(LLM_Data_GPT_4_0$Score)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    overall_data$LLM <- as.factor(overall_data$LLM)
    levene_test <- leveneTest(overall_data$Score, overall_data$LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5)
    print(shapiro_copilot)
    print(shapiro_gemini)
    print(shapiro_gpt_4_0)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5$p.value < 0.05 | shapiro_copilot$p.value < 0.05 | 
        shapiro_gemini$p.value < 0.05 | shapiro_gpt_4_0$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test <- kruskal.test(Score ~ LLM, data = overall_data)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test)
      
      if (kruskal_test$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results <- dunn.test(overall_data$Score, overall_data$LLM, method="bonferroni")
        print(posthoc_results)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test)
      
      if (levene_test$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test <- kruskal.test(Score ~ LLM, data = overall_data)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test)
        
        if (kruskal_test$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          install.packages("dunn.test")
          posthoc_results <- dunn.test(overall_data$Score, overall_data$LLM, method="bonferroni")
          print(posthoc_results)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results <- aov(Score ~ LLM, data = overall_data)
        anova_summary <- summary(anova_results)
        print("ANOVA Results:")
        print(anova_summary)
        
        if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results <- TukeyHSD(anova_results)
          print(posthoc_results)
        }
      }
    }
  }
  
  ### Q1
  {
    
    LLM_Data_GPT_3_5_Q1 <- subset(LLM_Data_GPT_3_5, Question == "1")
    LLM_Data_Copilot_Q1 <- subset(LLM_Data_Copilot, Question == "1")
    LLM_Data_Gemini_Q1 <- subset(LLM_Data_Gemini, Question == "1")
    LLM_Data_GPT_4_0_Q1 <- subset(LLM_Data_GPT_4_0, Question == "1")
    
    # Combine data into one dataframe for analysis
    overall_data_Q1 <- data.frame(
      LLM = c(rep("GPT_3_5", nrow(LLM_Data_GPT_3_5_Q1)),
              rep("Copilot", nrow(LLM_Data_Copilot_Q1)),
              rep("Gemini", nrow(LLM_Data_Gemini_Q1)),
              rep("GPT_4_0", nrow(LLM_Data_GPT_4_0_Q1))),
      Score = c(LLM_Data_GPT_3_5_Q1$Score,
                LLM_Data_Copilot_Q1$Score,
                LLM_Data_Gemini_Q1$Score,
                LLM_Data_GPT_4_0_Q1$Score)
    )
    
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_Q1 <- shapiro.test(LLM_Data_GPT_3_5_Q1$Score)
    shapiro_copilot_Q1 <- shapiro.test(LLM_Data_Copilot_Q1$Score)
    shapiro_gemini_Q1 <- shapiro.test(LLM_Data_Gemini_Q1$Score)
    shapiro_gpt_4_0_Q1 <- shapiro.test(LLM_Data_GPT_4_0_Q1$Score)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    overall_data_Q1$LLM <- as.factor(overall_data_Q1$LLM)
    levene_test_Q1 <- leveneTest(overall_data_Q1$Score, overall_data_Q1$LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_Q1)
    print(shapiro_copilot_Q1)
    print(shapiro_gemini_Q1)
    print(shapiro_gpt_4_0_Q1)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_Q1$p.value < 0.05 | shapiro_copilot_Q1$p.value < 0.05 | 
        shapiro_gemini_Q1$p.value < 0.05 | shapiro_gpt_4_0_Q1$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_Q1 <- kruskal.test(Score ~ LLM, data = overall_data_Q1)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_Q1)
      
      if (kruskal_test_Q1$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_Q1 <- dunn.test(overall_data_Q1$Score, overall_data_Q1$LLM, method="bonferroni")
        print(posthoc_results_Q1)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test_Q1)
      
      if (levene_test_Q1$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_Q1 <- kruskal.test(Score ~ LLM, data = overall_data_Q1)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_Q1)
        
        if (kruskal_test_Q1$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          install.packages("dunn.test")
          posthoc_results_Q1 <- dunn.test(overall_data_Q1$Score, overall_data_Q1$LLM, method="bonferroni")
          print(posthoc_results_Q1)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_Q1 <- aov(Score ~ LLM, data = overall_data_Q1)
        anova_summary_Q1 <- summary(anova_results_Q1)
        print("ANOVA Results:")
        print(anova_summary_Q1)
        
        if (anova_summary_Q1[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_Q1 <- TukeyHSD(anova_results_Q1)
          print(posthoc_results_Q1)
        }
      }
    }
  }
  
  ### Q2
  {
    
    LLM_Data_GPT_3_5_Q2 <- subset(LLM_Data_GPT_3_5, Question == "2")
    LLM_Data_Copilot_Q2 <- subset(LLM_Data_Copilot, Question == "2")
    LLM_Data_Gemini_Q2 <- subset(LLM_Data_Gemini, Question == "2")
    LLM_Data_GPT_4_0_Q2 <- subset(LLM_Data_GPT_4_0, Question == "2")
    
    # Combine data into one dataframe for analysis
    overall_data_Q2 <- data.frame(
      LLM = c(rep("GPT_3_5", nrow(LLM_Data_GPT_3_5_Q2)),
              rep("Copilot", nrow(LLM_Data_Copilot_Q2)),
              rep("Gemini", nrow(LLM_Data_Gemini_Q2)),
              rep("GPT_4_0", nrow(LLM_Data_GPT_4_0_Q2))),
      Score = c(LLM_Data_GPT_3_5_Q2$Score,
                LLM_Data_Copilot_Q2$Score,
                LLM_Data_Gemini_Q2$Score,
                LLM_Data_GPT_4_0_Q2$Score)
    )
    
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_Q2 <- shapiro.test(LLM_Data_GPT_3_5_Q2$Score)
    shapiro_copilot_Q2 <- shapiro.test(LLM_Data_Copilot_Q2$Score)
    shapiro_gemini_Q2 <- shapiro.test(LLM_Data_Gemini_Q2$Score)
    shapiro_gpt_4_0_Q2 <- shapiro.test(LLM_Data_GPT_4_0_Q2$Score)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    overall_data_Q2$LLM <- as.factor(overall_data_Q2$LLM)
    levene_test_Q2 <- leveneTest(overall_data_Q2$Score, overall_data_Q2$LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_Q2)
    print(shapiro_copilot_Q2)
    print(shapiro_gemini_Q2)
    print(shapiro_gpt_4_0_Q2)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_Q2$p.value < 0.05 | shapiro_copilot_Q2$p.value < 0.05 | 
        shapiro_gemini_Q2$p.value < 0.05 | shapiro_gpt_4_0_Q2$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_Q2 <- kruskal.test(Score ~ LLM, data = overall_data_Q2)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_Q2)
      
      if (kruskal_test_Q2$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_Q2 <- dunn.test(overall_data_Q2$Score, overall_data_Q2$LLM, method="bonferroni")
        print(posthoc_results_Q2)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test_Q2)
      
      if (levene_test_Q2$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_Q2 <- kruskal.test(Score ~ LLM, data = overall_data_Q2)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_Q2)
        
        if (kruskal_test_Q2$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          install.packages("dunn.test")
          posthoc_results_Q2 <- dunn.test(overall_data_Q2$Score, overall_data_Q2$LLM, method="bonferroni")
          print(posthoc_results_Q2)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_Q2 <- aov(Score ~ LLM, data = overall_data_Q2)
        anova_summary_Q2 <- summary(anova_results_Q2)
        print("ANOVA Results:")
        print(anova_summary_Q2)
        
        if (anova_summary_Q2[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_Q2 <- TukeyHSD(anova_results_Q2)
          print(posthoc_results_Q2)
        }
      }
    }
  }
  
  ### Q3
  {
    
    LLM_Data_GPT_3_5_Q3 <- subset(LLM_Data_GPT_3_5, Question == "3")
    LLM_Data_Copilot_Q3 <- subset(LLM_Data_Copilot, Question == "3")
    LLM_Data_Gemini_Q3 <- subset(LLM_Data_Gemini, Question == "3")
    LLM_Data_GPT_4_0_Q3 <- subset(LLM_Data_GPT_4_0, Question == "3")
    
    # Combine data into one dataframe for analysis
    overall_data_Q3 <- data.frame(
      LLM = c(rep("GPT_3_5", nrow(LLM_Data_GPT_3_5_Q3)),
              rep("Copilot", nrow(LLM_Data_Copilot_Q3)),
              rep("Gemini", nrow(LLM_Data_Gemini_Q3)),
              rep("GPT_4_0", nrow(LLM_Data_GPT_4_0_Q3))),
      Score = c(LLM_Data_GPT_3_5_Q3$Score,
                LLM_Data_Copilot_Q3$Score,
                LLM_Data_Gemini_Q3$Score,
                LLM_Data_GPT_4_0_Q3$Score)
    )
    
    # Check assumptions for ANOVA
    # Shapiro-Wilk test for normality
    shapiro_gpt_3_5_Q3 <- shapiro.test(LLM_Data_GPT_3_5_Q3$Score)
    shapiro_copilot_Q3 <- shapiro.test(LLM_Data_Copilot_Q3$Score)
    shapiro_gemini_Q3 <- shapiro.test(LLM_Data_Gemini_Q3$Score)
    shapiro_gpt_4_0_Q3 <- shapiro.test(LLM_Data_GPT_4_0_Q3$Score)
    
    ###check for homogeneity 
    # Levene's test for homogeneity of variances using lawstat
    overall_data_Q3$LLM <- as.factor(overall_data_Q3$LLM)
    levene_test_Q3 <- leveneTest(overall_data_Q3$Score, overall_data_Q3$LLM)
    
    ###Check for normality 
    # Print Shapiro-Wilk test results
    print("Shapiro-Wilk Test for Normality:")
    print(shapiro_gpt_3_5_Q3)
    print(shapiro_copilot_Q3)
    print(shapiro_gemini_Q3)
    print(shapiro_gpt_4_0_Q3)
    
    ###Perform statistical analysis based on if normality and homogeneity assumptions are met. 
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_gpt_3_5_Q3$p.value < 0.05 | shapiro_copilot_Q3$p.value < 0.05 | 
        shapiro_gemini_Q3$p.value < 0.05 | shapiro_gpt_4_0_Q3$p.value < 0.05) {
      print("At least one group does not follow a normal distribution. Using Kruskal-Wallis test.")
      # Perform Kruskal-Wallis test if assumptions are not met
      kruskal_test_Q3 <- kruskal.test(Score ~ LLM, data = overall_data_Q3)
      print("Kruskal-Wallis Test Results:")
      print(kruskal_test_Q3)
      
      if (kruskal_test_Q3$p.value < 0.05) {
        print("Significant differences detected. Performing post-hoc analysis.")
        posthoc_results_Q3 <- dunn.test(overall_data_Q3$Score, overall_data_Q3$LLM, method="bonferroni")
        print(posthoc_results_Q3)
      }
    } else {
      print("All groups follow a normal distribution. Checking homogeneity of variances.")
      print("Levene's Test for Homogeneity of Variances:")
      print(levene_test_Q3)
      
      if (levene_test_Q3$p.value < 0.05) {
        print("Variances are not homogeneous. Using Kruskal-Wallis test.")
        # Perform Kruskal-Wallis test if variances are not homogeneous
        kruskal_test_Q3 <- kruskal.test(Score ~ LLM, data = overall_data_Q3)
        print("Kruskal-Wallis Test Results:")
        print(kruskal_test_Q3)
        
        if (kruskal_test_Q3$p.value < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          install.packages("dunn.test")
          posthoc_results_Q3 <- dunn.test(overall_data_Q3$Score, overall_data_Q3$LLM, method="bonferroni")
          print(posthoc_results_Q3)
        }
      } else {
        print("Variances are homogeneous. Using ANOVA.")
        # Perform ANOVA if assumptions are met
        anova_results_Q3 <- aov(Score ~ LLM, data = overall_data_Q3)
        anova_summary_Q3 <- summary(anova_results_Q3)
        print("ANOVA Results:")
        print(anova_summary_Q3)
        
        if (anova_summary_Q3[[1]][["Pr(>F)"]][1] < 0.05) {
          print("Significant differences detected. Performing post-hoc analysis.")
          posthoc_results_Q3 <- TukeyHSD(anova_results_Q3)
          print(posthoc_results_Q3)
        }
      }
    }
  }
}





