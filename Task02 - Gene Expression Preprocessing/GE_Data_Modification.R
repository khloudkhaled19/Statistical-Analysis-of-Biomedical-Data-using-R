install.packages("tidyverse")
library(tidyverse) 


GE_Data_Modification <- function(){
  path.name <- "D:/Work/NU/Master/Visualization/A2/Assignment_02_Data.csv"
  GE.data <- read.table(path.name, sep = ',', header=T, row.names=1)
  GE.data %>% 
    mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
  
  }


New_GE_data = GE_Data_Normalization(GE_Data_Modification())  

write.table(New_GE_data, file = "D:/Work/NU/Master/Visualization/A2/GE.txt", sep = "\t", row.names = FALSE)