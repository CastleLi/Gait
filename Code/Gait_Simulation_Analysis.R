library(openxlsx)
gaitanalysis <- function(Sim_Data, Output_File){
  N_timept <- ncol(Sim_Data) - 4
  Sim_Data$Subject <- as.factor(Sim_Data$Subject)
  Sim_Data$Therapist <- as.factor(Sim_Data$Therapist)
  Sim_Data$Session <- as.factor(Sim_Data$Session)
  Sim_Data$Trial <- as.factor(Sim_Data$Trial)
  
  # Schwartz ----------------------------------------------------------------
  
  Output_Schwartz <- sapply(1:N_timept, function(Time_pt_t){
    ## data at point t
    Outcome <- paste0("Time_pt_", Time_pt_t)
    ## Inter-subject
    Subject_Vars <- aggregate(Sim_Data[,Outcome], 
                              by = list(Sim_Data$Subject), 
                              var)
    Subject_Sigma2 <- sum(Subject_Vars[,2])*(nrow(Sim_Data)/length(Subject_Vars[,2])-1)/(nrow(Sim_Data)-1)
    
    ## Inter-Therapists
    Side_Vars <- aggregate(Sim_Data[,Outcome], 
                           by = list(Sim_Data$Subject, Sim_Data$Therapist), 
                           var)
    Side_Sigma2 <- sum(Side_Vars[,3])*(nrow(Sim_Data)/length(Side_Vars[,3])-1)/(nrow(Sim_Data)-1)
    
    ## Inter-trials
    Session_Vars <- aggregate(Sim_Data[,Outcome], 
                              by = list(Sim_Data$Subject, Sim_Data$Therapist, Sim_Data$Session), 
                              var)
    Session_Sigma2 <- sum(Session_Vars[,4])*(nrow(Sim_Data)/length(Session_Vars[,4])-1)/(nrow(Sim_Data)-1)
    
    return(c(Subject_Sigma2, Side_Sigma2, Session_Sigma2))
  })
  rownames(Output_Schwartz) <- c("Therapist", "Session","Trial")
  Schwartz_ANOVA_var_ratio <- sapply(1:N_timept, function(Time_pt_t){
    Ratio_unit <- Output_Schwartz[,Time_pt_t] / Output_Schwartz[length(Output_Schwartz[,Time_pt_t]),Time_pt_t] 
  })
  Averaged_Schwartz_var_ratio <- round(rowMeans(Schwartz_ANOVA_var_ratio), 2)
  Averaged_Schwartz_var_ratio[Averaged_Schwartz_var_ratio<0] <- 0
  Averaged_Schwartz_var_ratio <- sqrt(Averaged_Schwartz_var_ratio)

  
  # Chia --------------------------------------------------------------------
  
  # ANOVA (Table 2)
  Output_Chia_ANOVA <- sapply(1:N_timept, function(Time_pt_t){
    ## data at point t
    Outcome <- paste0("Time_pt_", Time_pt_t)

    Nested_int_fml <- as.formula(paste0(Outcome, "~ Subject*Therapist/Session"))
    Nested_int_AOV <- VCA::anovaVCA(Nested_int_fml, Sim_Data)
    
    return(as.numeric(as.data.frame(Nested_int_AOV$aov.tab)$VC[-1]))
  })
  rownames(Output_Chia_ANOVA) <- c("Subject", "Therapist", "Subject:Therapist", "Subject:Therapist:Session", "Trial")
  
  Chia_ANOVA_var_ratio <- sapply(1:N_timept, function(Time_pt_t){
    Ratio_unit <- Output_Chia_ANOVA[,Time_pt_t] / Output_Chia_ANOVA[length(Output_Chia_ANOVA[,Time_pt_t]),Time_pt_t] 
  })
  Averaged_Chia_var_ratio <- round(rowMeans(Chia_ANOVA_var_ratio), 2)
  Averaged_Chia_var_ratio[Averaged_Chia_var_ratio<0] <- 0
  
  
  
  # Random Nested ANOVA ------------------------------------------------------------
  
  Output_Nested_ANOVA <- sapply(1:N_timept, function(Time_pt_t){
    ## data at point t
    Outcome <- paste0("Time_pt_", Time_pt_t)
    
    Nested_fml <- as.formula(paste0(Outcome, "~ Subject/Therapist/Session"))
    Nested_AOV <- VCA::anovaVCA(Nested_fml, Sim_Data)
    
    return(as.numeric(as.data.frame(Nested_AOV$aov.tab)$VC[-1]))
  })
  rownames(Output_Nested_ANOVA) <- c("Subject", "Subject:Therapist", "Subject:Therapist:Session", "Trial")
  
  
  Nested_ANOVA_var_ratio <- sapply(1:N_timept, function(Time_pt_t){
    Ratio_unit <- Output_Nested_ANOVA[,Time_pt_t] / Output_Nested_ANOVA[length(Output_Nested_ANOVA[,Time_pt_t]),Time_pt_t] 
  })
  Averaged_Nested_var_ratio <- round(rowMeans(Nested_ANOVA_var_ratio), 2)
  Averaged_Nested_var_ratio[Averaged_Nested_var_ratio<0] <- 0
  Averaged_Nested_var_ratio <- sqrt(Averaged_Nested_var_ratio)
  
  # Random cross-sectional ANOVA ------------------------------------------------------------
  
  Output_Random_Cross_ANOVA <- sapply(1:N_timept, function(Time_pt_t){
    ## data at point t
    Outcome <- paste0("Time_pt_", Time_pt_t)
    
    ANOVA_FML <- as.formula(paste0(Outcome, " ~ Subject + Therapist + Session + Trial"))
    aov_model <- EMSaov::EMSanova(ANOVA_FML, Sim_Data, type = c("R", "R", "R", "R"))
    
    # document the name of variance components
    Var_Comp_Names <- rownames(aov_model)
    Var_Comp_Names[15] <- "Error"
    # calculate the mapping matrix between variance components and mean squares
    Var_Output <- data.frame(matrix(NA, ncol = 15))
    colnames(Var_Output) <- Var_Comp_Names
    for(i in aov_model$EMS){
      Var_Output_Unit <- data.frame(matrix(0, ncol = 15))
      colnames(Var_Output_Unit) <- Var_Comp_Names
      Var_Comp <- strsplit(i, "\\+")[[1]]
      Var_Comp_Num <- gsub("[^0-9.-]", "", Var_Comp)
      Var_Comp_Num[Var_Comp_Num==""] <- 1
      Var_Comp <- gsub('[[:digit:]]+', '',  Var_Comp)
      Var_Output_Unit[Var_Comp] <- Var_Comp_Num
      Var_Output <- rbind(Var_Output, Var_Output_Unit)
    }
    Var_Output <- Var_Output[-1,]
    Var_Output <- as.matrix(sapply(Var_Output, as.numeric))  
    # solve the equation and calculate the variance components
    Var_Comp_Output <- solve(Var_Output, aov_model$MS)
    
    return(Var_Comp_Output)
  })
  
  Random_Cross_ANOVA_var_ratio <- sapply(1:N_timept, function(Time_pt_t){
    Ratio_unit <- Output_Random_Cross_ANOVA[,Time_pt_t] / Output_Random_Cross_ANOVA[length(Output_Random_Cross_ANOVA[,Time_pt_t]),Time_pt_t] 
  })
  Averaged_Random_Cross_var_ratio <- round(rowMeans(Random_Cross_ANOVA_var_ratio), 2)
  Averaged_Random_Cross_var_ratio[Averaged_Random_Cross_var_ratio<0] <- 0
  Averaged_Random_Cross_var_ratio <- sqrt(Averaged_Random_Cross_var_ratio)
  
  
  # Fixed cross-sectional ANOVA ------------------------------------------------------------
  
  Output_Fixed_Cross_ANOVA <- sapply(1:N_timept, function(Time_pt_t){
    ## data at point t
    Outcome <- paste0("Time_pt_", Time_pt_t)
    
    ANOVA_FML <- as.formula(paste0(Outcome, " ~ Subject + Therapist + Session + Trial"))
    aov_model <- EMSaov::EMSanova(ANOVA_FML, Sim_Data, type = c("F", "F", "F", "R"))
    
    # document the name of variance components
    Var_Comp_Names <- rownames(aov_model)
    Var_Comp_Names[15] <- "Error"
    # calculate the mapping matrix between variance components and mean squares
    Var_Output <- data.frame(matrix(NA, ncol = 15))
    colnames(Var_Output) <- Var_Comp_Names
    for(i in aov_model$EMS){
      Var_Output_Unit <- data.frame(matrix(0, ncol = 15))
      colnames(Var_Output_Unit) <- Var_Comp_Names
      Var_Comp <- strsplit(i, "\\+")[[1]]
      Var_Comp_Num <- gsub("[^0-9.-]", "", Var_Comp)
      Var_Comp_Num[Var_Comp_Num==""] <- 1
      Var_Comp <- gsub('[[:digit:]]+', '',  Var_Comp)
      Var_Output_Unit[Var_Comp] <- Var_Comp_Num
      Var_Output <- rbind(Var_Output, Var_Output_Unit)
    }
    Var_Output <- Var_Output[-1,]
    Var_Output <- as.matrix(sapply(Var_Output, as.numeric))  
    # solve the equation and calculate the variance components
    Var_Comp_Output <- solve(Var_Output, aov_model$MS)
    
    return(Var_Comp_Output)
  })
  
  Fixed_Cross_ANOVA_var_ratio <- sapply(1:N_timept, function(Time_pt_t){
    Ratio_unit <- Output_Fixed_Cross_ANOVA[,Time_pt_t] / Output_Fixed_Cross_ANOVA[length(Output_Fixed_Cross_ANOVA[,Time_pt_t]),Time_pt_t] 
  })
  Averaged_Fixed_Cross_var_ratio <- round(rowMeans(Fixed_Cross_ANOVA_var_ratio), 2)
  Averaged_Fixed_Cross_var_ratio[Averaged_Fixed_Cross_var_ratio<0] <- 0
  Averaged_Fixed_Cross_var_ratio <- sqrt(Averaged_Fixed_Cross_var_ratio)
  
  
  
  # Aggregate the results -----------------------------------------------------
  Output_Schwartz <- as.data.frame(t(cbind(Output_Schwartz, Averaged_Schwartz_var_ratio)))
  rownames(Output_Schwartz) <- c(paste0("Time_pt_", 1:N_timept), "Averaged_Ratio")
  
  Output_Chia_ANOVA <- as.data.frame(t(cbind(Output_Chia_ANOVA, Averaged_Chia_var_ratio)))
  rownames(Output_Chia_ANOVA) <- c(paste0("Time_pt_", 1:N_timept), "Averaged_Ratio")
  
  Output_Nested_ANOVA <- as.data.frame(t(cbind(Output_Nested_ANOVA, Averaged_Nested_var_ratio)))
  rownames(Output_Nested_ANOVA) <- c(paste0("Time_pt_", 1:N_timept), "Averaged_Ratio")
  
  Output_Random_Cross_ANOVA <- as.data.frame(t(cbind(Output_Random_Cross_ANOVA, Averaged_Random_Cross_var_ratio)))
  rownames(Output_Random_Cross_ANOVA) <- c(paste0("Time_pt_", 1:N_timept), "Averaged_Ratio")
  
  Output_Fixed_Cross_ANOVA <- as.data.frame(t(cbind(Output_Fixed_Cross_ANOVA, Averaged_Fixed_Cross_var_ratio)))
  rownames(Output_Fixed_Cross_ANOVA) <- c(paste0("Time_pt_", 1:N_timept), "Averaged_Ratio")
  
  Output_Results <- list(STW = Output_Schwartz,
                         CS = Output_Chia_ANOVA,
                         Nest = Output_Nested_ANOVA,
                         Cross_Random = Output_Random_Cross_ANOVA,
                         Cross_Fixed = Output_Fixed_Cross_ANOVA)
  return(Output_Results)
}


