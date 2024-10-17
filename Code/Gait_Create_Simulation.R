

simdata <- function(InputData, Outcome, Session_Effect = TRUE,
                    Subject = "Subject", Therapist = "Therapist", Session = "Session", Trial = "Trial", Time = "Time_pt",
                    Sim_Subject = 2, Sim_Therapist = 4, Sim_Session = 3, Sim_Trial = 5){

  InputData_Raw <- InputData[,c(Subject, Therapist, Session, Trial, Time, Outcome)]
  colnames(InputData_Raw) <- c("Subject", "Therapist", "Session", "Trial", "Time_pt", Outcome)
  # check input data
  if(Sim_Subject > 1){
    if(length(unique(InputData_Raw$Subject)) < Sim_Subject){
      stop("No enough subject.")
    }
  }
  if(Sim_Therapist > 1){
    if(length(unique(InputData_Raw$Therapist)) == 1){
      stop("No enough therapist.")
    }
  }
  if(Sim_Session > 1){
    if(length(unique(InputData_Raw$Session)) < Sim_Session){
      stop("No enough session.")
    }
  }
  
  # Data preparation  
  Index_Data <- InputData_Raw[!duplicated(InputData_Raw[c("Subject", "Therapist", "Session", "Trial")]), c("Subject", "Therapist", "Session", "Trial")]
  Dev_Sub <- mapply(function(Therapist_sub, Subject_sub, Session_sub, Trial_sub){
    InputData_Sub <- subset(InputData_Raw, 
                            Therapist == Therapist_sub & Subject == Subject_sub & Session == Session_sub & Trial == Trial_sub)
    InputData_Sub <- InputData_Sub[order(InputData_Sub$Time_pt),]
    Dev1_Sub <- InputData_Sub[,Outcome]
    return(c(Outcome, Therapist_sub, Subject_sub, Session_sub, Trial_sub, Dev1_Sub[1], diff(Dev1_Sub)))
  },
  Index_Data$Therapist, Index_Data$Subject, Index_Data$Session, Index_Data$Trial
  )
  Dev_Sub <- as.data.frame(t(Dev_Sub))
  Num_TimePt <- ncol(Dev_Sub) - 5
  colnames(Dev_Sub)[1:6] <- c("Outcome", "Therapist", "Subject", "Session", "Trial", "Starting_Point")
  for(i in 1:Num_TimePt + 5){
    Dev_Sub[,i] <- as.numeric(Dev_Sub[,i])
  }
  
  # add additional therapist
  if(length(unique(InputData_Raw$Therapist)) < Sim_Therapist){
    Additional_Therapist <- ceiling(Sim_Therapist/length(unique(InputData_Raw$Therapist))) - 1
    for(Additional_Therapist_index in 1:Additional_Therapist){
      # shift the curve on x-axis
      Dev_Sub_jitter <- mapply(function(Therapist_sub, Subject_sub, Session_sub, Trial_sub){
        InputData_Sub <- subset(InputData_Raw, 
                                Therapist == Therapist_sub & Subject == Subject_sub & Session == Session_sub & Trial == Trial_sub)
        InputData_Sub <- InputData_Sub[order(InputData_Sub$Time_pt),]
        Dev1_Sub <- InputData_Sub[,Outcome]
        Fitted_spline <- predict(smooth.spline(Dev1_Sub),  c(-5:(Num_TimePt + 5)))$y
        Dev1_Sub <- Fitted_spline[floor(runif(1, 0, 11)) + 1:Num_TimePt]
        
        return(c(Outcome, Therapist_sub, Subject_sub, Session_sub, Trial_sub, Dev1_Sub[1], diff(Dev1_Sub)))
      },
      Index_Data$Therapist, Index_Data$Subject, Index_Data$Session, Index_Data$Trial
      )
      
      Dev_Sub_jitter <- as.data.frame(t(Dev_Sub_jitter))
      colnames(Dev_Sub_jitter)[1:6] <- c("Outcome", "Therapist", "Subject", "Session", "Trial", "Starting_Point")
      for(i in 1:Num_TimePt + 5){
        Dev_Sub_jitter[,i] <- as.numeric(Dev_Sub_jitter[,i])
      }
      # add the therapist effect and shift the curve on y-axis
      Averaged_starting <- as.numeric(aggregate(Dev_Sub_jitter$Starting_Point, list(Dev_Sub_jitter$Therapist), mean)[,2])
      Averaged_diff <- Averaged_starting[1] - Averaged_starting[2]
      Starting_addon <- rnorm(nrow(Dev_Sub_jitter), Averaged_diff * Additional_Therapist_index, abs(Averaged_diff / 2))
      Dev_Sub_jitter$Starting_Point <- Dev_Sub_jitter$Starting_Point + Starting_addon
      Dev_Sub_jitter$Therapist <- as.character(as.numeric(Dev_Sub_jitter$Therapist ) + 2 * Additional_Therapist_index)
      
      Dev_Sub <- rbind(Dev_Sub, Dev_Sub_jitter)
      
    }
  }
  Dev_Sub_Therapist <- Dev_Sub[which(Dev_Sub$Therapist%in%c(1:Sim_Therapist)),]
  Dev_Sub_Therapist <- Dev_Sub_Therapist[which(Dev_Sub_Therapist$Subject%in%(unique(Dev_Sub_Therapist$Subject))[c(1:Sim_Subject)]),]
  Dev_Sub_Therapist <- Dev_Sub_Therapist[which(Dev_Sub_Therapist$Session%in%c(1:Sim_Session)),]
  
  colnames(Dev_Sub_Therapist)[1:6] <- c("Outcome", "Therapist", "Subject", "Session", "Trial", "Starting_Point")

  
  # Generate simulation
  Sim_N <- 10000
  Sim_samplesize <- 100
  set.seed(1)
  ## Assuming Therapist, subject and session effect
  Total_Sample <- nrow(Dev_Sub_Therapist)/Sim_Subject/Sim_Therapist/Sim_Session
  Sim_TherapistSubjectSession_effect <- sapply(1:Sim_N, function(i){
    SubData_Therapist <- sample(unique(Dev_Sub_Therapist$Therapist), 1)
    SubData_Subject <- sample(unique(Dev_Sub_Therapist$Subject), 1)
    SubData_Session <- sample(unique(Dev_Sub_Therapist$Session), 1)
    if(Session_Effect){
      SubData <- subset(Dev_Sub_Therapist, 
                        Therapist ==  SubData_Therapist&
                          Subject == SubData_Subject&
                          Session == SubData_Session)
    }
    else{
      SubData <- subset(Dev_Sub_Therapist, 
                        Therapist ==  SubData_Therapist&
                          Subject == SubData_Subject)
    }
    SelectedData <- SubData[sample(Total_Sample, Sim_samplesize, replace = TRUE), 1:Num_TimePt + 5]
    SelectedDev <- apply(SelectedData, 2, mean)
    SelectedValues <- cumsum(SelectedDev)
    SelectedValues <- c(SubData_Subject, SubData_Therapist, SubData_Session, SelectedValues)
    return(SelectedValues)
  })
  Sim_TherapistSubjectSession_effect <- t(t(Sim_TherapistSubjectSession_effect)[!duplicated(t(Sim_TherapistSubjectSession_effect)),])
  Sim_TherapistSubjectSession_effect <- as.data.frame(t(Sim_TherapistSubjectSession_effect))
  colnames(Sim_TherapistSubjectSession_effect) <- c("Subject", "Therapist", "Session", paste0("Time_pt_", 1:Num_TimePt))

  
  Sim_Data <- NULL
  for(SubData_Subject in unique(Sim_TherapistSubjectSession_effect$Subject)){
    for(SubData_Therapist in unique(Sim_TherapistSubjectSession_effect$Therapist)){
      for(SubData_Session in unique(Sim_TherapistSubjectSession_effect$Session)){
        SubData <- subset(Sim_TherapistSubjectSession_effect, 
                          Therapist ==  SubData_Therapist&
                            Subject == SubData_Subject& 
                            Session == SubData_Session)
        SubData <- SubData[1:Sim_Trial,]
        SubData$Trial <- 1:Sim_Trial
        Sim_Data <- rbind(Sim_Data, SubData)
      }
    }
  }
  Sim_Data$Subject <- as.factor(Sim_Data$Subject)
  levels(Sim_Data$Subject) <- paste0("Subject_", 1:Sim_Subject)
  Sim_Data$Therapist <- as.factor(Sim_Data$Therapist)
  levels(Sim_Data$Therapist) <- paste0("Therapist_", 1:Sim_Therapist)
  Sim_Data$Session <- as.factor(Sim_Data$Session)
  levels(Sim_Data$Session) <- paste0("Session", 1:Sim_Session)
  for(Time_pt_t in 1:Num_TimePt){
    Outcomes <- paste0("Time_pt_", Time_pt_t)
    Sim_Data[,Outcomes] <- as.numeric(Sim_Data[,Outcomes])
  }
  
  return(Sim_Data)
}



#Simulated_Data <- simdata(InputData = Final_Outputs, Outcome = "Head_Flexion", Session_Effect = TRUE,
#                          Subject = "Subject", Therapist = "Leg", Session = "Session", Trial = "Trial", Time = "Time_pt",
#                          Sim_Subject = 2, Sim_Therapist = 4, Sim_Session = 3, Sim_Trial = 5)


#Simulated_Data_NoSession <- simdata(InputData = Final_Outputs, Outcome = "Head_Flexion", Session_Effect = FALSE,
#                          Subject = "Subject", Therapist = "Leg", Session = "Session", Trial = "Trial", Time = "Time_pt",
#                          Sim_Subject = 2, Sim_Therapist = 4, Sim_Session = 3, Sim_Trial = 5)



