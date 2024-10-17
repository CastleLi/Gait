library(openxlsx)
library(ggplot2)
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
  
  write.csv(Summary_All, row.names = TRUE,
            paste0(OutputFolder,"Fig1_", File, "Session.csv"))
  
  Summary_All$Variance <- rownames(Summary_All)
  # exclude trial related error
  Summary_All <- Summary_All[1:7,]
  
  # reshape
  Summary_All <- reshape2::melt(Summary_All, id.vars = "Variance")
  colnames(Summary_All) <- c("Variance", "Method", "Number of Identified Outcomes")
  Summary_All$Variance <- as.character(Summary_All$Variance)
  Summary_All$Variance <- gsub(":", "\n", Summary_All$Variance)
  levels(Summary_All$Variance) <- unique(Summary_All$Variance)
  
  
  # plot
  png(paste0(OutputFolder, "Fig1_", File, "Session.png"),
      res = 600, width = 9, height = 9, units = "in")
  Fig1 <- ggplot(Summary_All, aes(x=factor(Variance, levels = levels(Variance)), 
                          y=`Number of Identified Outcomes`,fill = Method)) +
    geom_bar(stat="identity", position = "dodge", width=.5)+ 
    theme(legend.position = "top")+
    scale_fill_discrete(name = "Model specification")+
    xlab("Source of Variance")
  Figures[[paste0(File, 1)]] <- Fig1
  print(Figures[[paste0(File, 1)]])
  
  dev.off()
  
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
            paste0(OutputFolder, "Fig2_", File, "Session.csv"))
  
  P_values <- sapply(unique(Summary_Ratio$Method), function(Method){
    Summary_Ratio_Sub <- Summary_Ratio[which(Summary_Ratio$Method==Method),]
    Summary_Ratio_Sub$Variance <- as.numeric(as.factor(Summary_Ratio_Sub$Variance))
    KW_fml <- as.formula("Value~Variance")
    return(c(Method,
             round(kruskal.test(KW_fml, Summary_Ratio_Sub)$p.value, 4)))
  })
  P_values <- as.data.frame(P_values)
  
  write.csv(P_values, row.names = FALSE,col.names = FALSE,
            paste0(OutputFolder, "Fig2_Pvalues_", File, "Session.csv"))
  
  png(paste0(OutputFolder, "Fig2_", File, "Session.png"),
      res = 600, width = 12, height = 8, units = "in")
  Fig2 <- ggplot(Summary_Ratio, aes(x=factor(Variance, levels = levels(Variance)), 
                            y=Value, fill = factor(Variance, levels = levels(Variance)))) + 
    geom_boxplot() +
    facet_wrap(~factor(Method, levels = c("STW", "CS", "Nested ANOVA", "Cross-sectional ANOVA (Random)", "Cross-sectional ANOVA (Fixed)")),
               scales="free_y") +
    theme(legend.position="none") +
    scale_fill_discrete(name = "Model specification")+
    xlab("Source of Variance") +
    ylab("Averaged Ratio")
  Figures[[paste0(File, 2)]] <- Fig2
  print(Figures[[paste0(File, 2)]])

  dev.off()
  
  
  rm(list = setdiff(ls(),
                    c("InputFolder", "OutputFolder", "InputFile1", "InputFile2", "InputSheets", "File", "Figures")))
  
  
  
}



# Fig 3 Session effect ----------------------------------------------------

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
  
  write.csv(Error_Var, row.names = FALSE,
            paste0(OutputFolder, "Fig3_", File, "Session.csv"))
  
  png(paste0(OutputFolder, "Fig3_", File, "Session.png"),
      res = 600, width = 12, height = 8, units = "in")
  
  Fig3 <- ggplot(Error_Var, aes(x=Outcome, 
                        y=Error, 
                        fill = factor(Method, levels = c("STW", "CS", "Nested ANOVA", "Cross-sectional ANOVA (Random)", "Cross-sectional ANOVA (Fixed)")))) + 
    geom_boxplot() + 
    theme(legend.position = "top") + 
    scale_fill_discrete(name = "Model specification") +
    xlab("Outcomes")
  Figures[[paste0(File, 3)]] <- Fig3
  print(Figures[[paste0(File, 3)]])
  dev.off()
  
  Averaged_Error <- aggregate(Error_Var$Error, list(Error_Var$Method, Error_Var$Outcome), mean)
  colnames(Averaged_Error) <- c("Method", "Outcomes", "Error")
  print(  pairwise.wilcox.test(Averaged_Error$Error, Averaged_Error$Method,
                             p.adjust.method = "BH"))
  Fig3_Pvalues <- as.data.frame(pairwise.wilcox.test(Averaged_Error$Error, Averaged_Error$Method,
                                                     p.adjust.method = "BH")$p.value)
  
  write.csv(Fig3_Pvalues, row.names = TRUE,
            paste0(OutputFolder, "Fig3_Pvalues_", File, "Session.csv"))
  
  rm(list = setdiff(ls(),
                    c("InputFolder", "OutputFolder", "InputFile1", "InputFile2", "InputSheets", "File", "Figures")))
  
  
}

