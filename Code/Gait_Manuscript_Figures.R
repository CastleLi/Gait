library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
InputFolder <- "../Data/"
OutputFolder <- "../Output/"
InputFile2 <- "Simulation_Results_NoSession20230711.xlsx"
InputFile1 <- "Simulation_Results_Session20230711.xlsx"

InputSheets <- getSheetNames(paste0(InputFolder, InputFile1))


for(File in c("No", "")){
  # Fig 1 Session effect ----------------------------------------------------
  Summary_Schwartz <- NULL
  Summary_Chia <- NULL
  Summary_Nested <- NULL
  Summary_XSectionalRand <- NULL
  Summary_XSectional <- NULL
  Figures <- list()
  
  for(InputSheet in InputSheets){
    #Load in Data
    InputData_Pre <- read.xlsx(paste0(InputFolder, gsub("Session", paste0(File, "Session"), InputFile1)), sheet = InputSheet)
    InputData_Index <- which(colnames(InputData_Pre) %in% c("STW", "CS", "Nest", "Cross_Random", "Cross_Fixed"))
    InputData_End <- c(InputData_Index[-1], ncol(InputData_Pre)+1) - 1
    InputData <- read.xlsx(paste0(InputFolder, gsub("Session", paste0(File, "Session"), InputFile1)), sheet = InputSheet, startRow = 2)
    
    # separate tables
    InputData_Schwartz <- InputData[,InputData_Index[1]:InputData_End[1]]
    InputData_Chia <- InputData[,InputData_Index[2]:InputData_End[2]]
    InputData_Nested <- InputData[,InputData_Index[3]:InputData_End[3]]
    InputData_XSectionalRand <- InputData[,InputData_Index[4]:InputData_End[4]]
    InputData_XSectional <- InputData[,InputData_Index[5]:InputData_End[5]]
    
    # summary
    Summary_Schwartz <- rbind(Summary_Schwartz, InputData_Schwartz[102,])
    Summary_Chia <- rbind(Summary_Chia, InputData_Chia[102,])
    Summary_Nested <- rbind(Summary_Nested, InputData_Nested[102,])
    Summary_XSectionalRand <- rbind(Summary_XSectionalRand, InputData_XSectionalRand[102,])
    Summary_XSectional <- rbind(Summary_XSectional, InputData_XSectional[102,])
  }
  
  Summary_All <- merge(as.data.frame(t(colSums(Summary_Schwartz>1))),
                       as.data.frame(t(colSums(Summary_Chia>1))), all = TRUE, sort = FALSE)
  Summary_All <- merge(Summary_All, 
                       as.data.frame(t(colSums(Summary_Nested>1))), all = TRUE, sort = FALSE)
  Summary_All <- merge(Summary_All, 
                       as.data.frame(t(colSums(Summary_XSectionalRand>1))), all = TRUE, sort = FALSE)
  Summary_All <- merge(Summary_All, 
                       as.data.frame(t(colSums(Summary_XSectional>1))), all = TRUE, sort = FALSE)
  rownames(Summary_All) <- c("STW", "CS", "Nested ANOVA", "Cross-sectional ANOVA (Random)", "Cross-sectional ANOVA (Fixed)")
  Summary_All[is.na(Summary_All)] <- 0
  Summary_All <- Summary_All[, colnames(Summary_XSectional)]
  Summary_All <- t(Summary_All)
  
  Summary_All <- as.data.frame(Summary_All)

  
  Summary_All$Variance <- rownames(Summary_All)
  # exclude trial related error
  Summary_All <- Summary_All[1:7,]
  
  # reshape
  Summary_All <- reshape2::melt(Summary_All, id.vars = "Variance")
  colnames(Summary_All) <- c("Variance", "Method", "Number of Identified Outcomes")
  Summary_All$Variance <- as.character(Summary_All$Variance)
  Summary_All$Variance <- gsub(":", "\n", Summary_All$Variance)
  levels(Summary_All$Variance) <- unique(Summary_All$Variance)
  
  
  # quantification
  Summary_Ratio <- NULL
  Summary_Schwartz_Long <- reshape2::melt(Summary_Schwartz[,1:2])
  Summary_Schwartz_Long$Method <- "STW"
  Summary_Ratio <- rbind(Summary_Ratio, Summary_Schwartz_Long)
  Summary_Chia_Long <- reshape2::melt(Summary_Chia[,1:4])
  Summary_Chia_Long$Method <- "CS"
  Summary_Ratio <- rbind(Summary_Ratio, Summary_Chia_Long)
  Summary_Nested_Long <- reshape2::melt(Summary_Nested[,1:3])
  Summary_Nested_Long$Method <- "Nested ANOVA"
  Summary_Ratio <- rbind(Summary_Ratio, Summary_Nested_Long)
  Summary_XSectionalRand_Long <- reshape2::melt(Summary_XSectionalRand[,1:7])
  Summary_XSectionalRand_Long$Method <- "Cross-sectional ANOVA (Random)"
  Summary_Ratio <- rbind(Summary_Ratio, Summary_XSectionalRand_Long)
  Summary_XSectional_Long <- reshape2::melt(Summary_XSectional[,1:7])
  Summary_XSectional_Long$Method <- "Cross-sectional ANOVA (Fixed)"
  Summary_Ratio <- rbind(Summary_Ratio, Summary_XSectional_Long)
  colnames(Summary_Ratio) <- c("Variance", "Value", "Method")
  Summary_Ratio$Variance <- gsub(":", "\n", Summary_Ratio$Variance)
  levels(Summary_Ratio$Variance) <- gsub(":", "\n", levels(Summary_XSectional_Long$variable))
  
  write.csv(Summary_All, row.names = FALSE,
            paste0(OutputFolder, "Fig1_", File, "Session.csv"))
  
  P_values <- sapply(unique(Summary_Ratio$Method), function(Method){
    Summary_Ratio_Sub <- Summary_Ratio[which(Summary_Ratio$Method==Method),]
    Summary_Ratio_Sub$Variance <- as.numeric(as.factor(Summary_Ratio_Sub$Variance))
    KW_fml <- as.formula("Value~Variance")
    return(c(Method,
             round(kruskal.test(KW_fml, Summary_Ratio_Sub)$p.value, 4)))
  })
  P_values <- as.data.frame(P_values)
  
  write.csv(P_values, row.names = FALSE,col.names = FALSE,
            paste0(OutputFolder, "Fig1_Pvalues_", File, "Session.csv"))
  
  
  label_data <- Summary_Ratio %>%
    group_by(Variance, Method) %>%
    summarize(count_above_1 = sum(Value > 1.0))
  all_combinations <- expand.grid(Variance = unique(Summary_Ratio$Variance), 
                                  Method = unique(Summary_Ratio$Method))
  label_data_full <- all_combinations %>%
    left_join(label_data, by = c("Variance", "Method")) %>%
    mutate(count_above_1 = ifelse(is.na(count_above_1), 0, count_above_1))
  label_data_full <- as.data.frame(label_data_full)
  label_data_full$count_above_1 <- paste0("N=", label_data_full$count_above_1)
  label_data_full$Variance <- factor(label_data_full$Variance, levels = levels(Summary_Ratio$Variance))
  
  png(paste0(OutputFolder, "Fig1_", File, "Session.png"),
      res = 600, width = 12, height = 8, units = "in")
  Fig2 <- ggplot(Summary_Ratio, aes(x=factor(Variance, levels = levels(Variance)), 
                                    y=Value)) + 
    geom_boxplot() +
    facet_wrap(~factor(Method, levels = c("STW", "CS", "Nested ANOVA", "Cross-sectional ANOVA (Random)", "Cross-sectional ANOVA (Fixed)")),
               scales="free_y") +
    theme(legend.position="none") +
    scale_fill_discrete(name = "Model specification")+
    xlab("Source of Variance") +
    ylab("Averaged Ratio") + 
    geom_text(data = label_data_full, aes(x = factor(Variance, levels = levels(Variance)), 
                                          y = -1,
                                          label = count_above_1), 
              vjust = 1.5, size = 3)
  Figures[[paste0(File, 2)]] <- Fig2
  print(Figures[[paste0(File, 2)]])
  
  dev.off()
  
  
  rm(list = setdiff(ls(),
                    c("InputFolder", "OutputFolder", "InputFile1", "InputFile2", "InputSheets", "File", "Figures")))
  
  
  
}



# Fig 2 Session effect ----------------------------------------------------

for(File in c("No", "")){
  Error_Var <- NULL
  for(InputSheet in InputSheets){
    #Load in Data
    InputData_Pre <- read.xlsx(paste0(InputFolder, gsub("Session", paste0(File, "Session"), InputFile1)), sheet = InputSheet)
    InputData_Index <- which(colnames(InputData_Pre) %in% c("STW", "CS", "Nest", "Cross_Random", "Cross_Fixed"))
    InputData_End <- c(InputData_Index[-1], ncol(InputData_Pre)+1) - 1
    InputData <- read.xlsx(paste0(InputFolder, gsub("Session", paste0(File, "Session"), InputFile1)), sheet = InputSheet, startRow = 2)
    
    # separate tables
    InputData_Schwartz <- InputData[,InputData_Index[1]:InputData_End[1]]
    InputData_Chia <- InputData[,InputData_Index[2]:InputData_End[2]]
    InputData_Nested <- InputData[,InputData_Index[3]:InputData_End[3]]
    InputData_XSectionalRand <- InputData[,InputData_Index[4]:InputData_End[4]]
    InputData_XSectional <- InputData[,InputData_Index[5]:InputData_End[5]]
    
    # summary
    Error_Var_Single <- cbind(InputData_Schwartz[-102,ncol(InputData_Schwartz)], 
                              InputData_Chia[-102,ncol(InputData_Chia)], 
                              InputData_Nested[-102,ncol(InputData_Nested)], 
                              InputData_XSectionalRand[-102,ncol(InputData_XSectionalRand)],
                              InputData_XSectional[-102,ncol(InputData_XSectional)])
    colnames(Error_Var_Single) <- c("STW", "CS", "Nested ANOVA", "Cross-sectional ANOVA (Random)", "Cross-sectional ANOVA (Fixed)")
    Error_Var_Single <- reshape2::melt(Error_Var_Single[,1:5])
    Error_Var_Single$Outcome <- gsub("_", "\n", InputSheet)
    Error_Var <- rbind(Error_Var, 
                       Error_Var_Single[,-1])
    
  }
  Error_Var <- as.data.frame(Error_Var)
  colnames(Error_Var) <- c("Method", "Error", "Outcome")
  

  Averaged_Error <- aggregate(Error_Var$Error, list(Error_Var$Method, Error_Var$Outcome), mean)
  colnames(Averaged_Error) <- c("Method", "Outcomes", "Error")
  Fig3_Pvalues <- as.data.frame(pairwise.wilcox.test(Averaged_Error$Error, Averaged_Error$Method,
                                                     p.adjust.method = "BH")$p.value)
  
  
  Averaged_Error_Names <- Averaged_Error
  Averaged_Error_Names$Method <- make.names(Averaged_Error_Names$Method)
  method_pairs <- combn(unique(Averaged_Error$Method), 2, simplify = FALSE)
  method_pairs_main <- combn(unique(Averaged_Error$Method)[c(1:2, 5)], 2, simplify = FALSE)
  method_pairs_supp <- setdiff(method_pairs, method_pairs_main)
  
  plots <- list()
  Supp_plots <- list()
  
  for (pair in method_pairs_main) {
    bivariate_data <- Averaged_Error_Names %>%
      filter(Method %in% make.names(pair)) %>%
      pivot_wider(names_from = Method, values_from = Error, names_prefix = "Error_")
    
    bivariate_data <- as.data.frame(bivariate_data)
    
    overall_limits <- range(bivariate_data[,-1])
    
    plot <- ggplot(bivariate_data, aes_string(x = paste("Error_", make.names(pair)[1], sep = ""), 
                                              y = paste("Error_", make.names(pair)[2], sep = ""))) +
      geom_point() +
      geom_abline(slope=1.0, intercept=0) + 
      labs(x = pair[1],
           y = pair[2]) +
      xlim(overall_limits) +  
      ylim(overall_limits) +  
      annotate("text", x = overall_limits[1], y = overall_limits[2], 
               label = paste("adjusted p-value =", format(round(Fig3_Pvalues[which(levels(Averaged_Error$Method)==pair[2]) - 1, 
                                                                             which(levels(Averaged_Error$Method)==pair[1])], 4), 
                                                          nsmall = 2)), 
               hjust = 0, vjust = 1, size = 4, color = "blue") +
      theme_minimal()
    
    plots[[paste(make.names(pair)[1], make.names(pair)[2], sep = "_vs_")]] <- plot
  }
  
  for (pair in method_pairs_supp) {
    bivariate_data <- Averaged_Error_Names %>%
      filter(Method %in% make.names(pair)) %>%
      pivot_wider(names_from = Method, values_from = Error, names_prefix = "Error_")
    
    bivariate_data <- as.data.frame(bivariate_data)
    
    overall_limits <- range(bivariate_data[,-1])
    
    plot <- ggplot(bivariate_data, aes_string(y = paste("Error_", make.names(pair)[1], sep = ""), 
                                              x = paste("Error_", make.names(pair)[2], sep = ""))) +
      geom_point() +
      geom_abline(slope=1.0, intercept=0) + 
      labs(y = pair[1],
           x = pair[2]) +
      xlim(overall_limits) +  
      ylim(overall_limits) +  
      annotate("text", x = overall_limits[1], y = overall_limits[2], 
               label = paste("adjusted p-value =", format(round(Fig3_Pvalues[which(levels(Averaged_Error$Method)==pair[2]) - 1, 
                                                                             which(levels(Averaged_Error$Method)==pair[1])], 4), 
                                                          nsmall = 2)), 
               hjust = 0, vjust = 1, size = 4, color = "blue") +
      theme_minimal()
    
    Supp_plots[[paste(make.names(pair)[1], make.names(pair)[2], sep = "_vs_")]] <- plot
  }
  
  
  write.csv(Averaged_Error, row.names = FALSE,
            paste0(OutputFolder, "Fig2_", File, "Session.csv"))
  
  png(paste0(OutputFolder, "Fig2_", File, "Session.png"),
      res = 600, width = 9, height = 3, units = "in")
  do.call(gridExtra::grid.arrange, c(plots, ncol = 3))
  dev.off()
  
  png(paste0(OutputFolder, "Supp_Fig1_", File, "Session.png"),
      res = 600, width = 12, height = 6, units = "in")
  do.call(gridExtra::grid.arrange, c(Supp_plots, ncol = 4))
  dev.off()
  

  rm(list = setdiff(ls(),
                    c("InputFolder", "OutputFolder", "InputFile1", "InputFile2", "InputSheets", "File", "Figures")))
  
  
}




