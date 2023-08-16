source("GE_Data_Modification.r")


GE_Data_Normalization <- function(GE.data){
  
  for(i in 1:ncol(GE.data)){
    Xavg <- mean(GE.data[,i])
    Xsd <- sd(GE.data[,i])
    
    for(j in 1:nrow(GE.data)){
      GE.data[j,i] <- (GE.data[j, i] - Xavg) / Xsd
    }
    
  }
  
  print(GE.data)
  
}

GE_Data_Normalization(GE_Data_Modification())