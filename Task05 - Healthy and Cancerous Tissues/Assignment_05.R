#Reading data and split it into 2 subsets
path.name <- "D:/Work/NU/Master/Visualization/A5/Assignment_05_GE_Data.txt"
GE_data <- read.table(file = path.name, sep = "", header=TRUE)
healthy <- GE_data$G_Healthy
cancer <- GE_data$G_Cancerous

#T-test and Wilcoxon implementation

healthy_normality = shapiro.test(healthy)
cancer_normality = shapiro.test(cancer)

healthy_p = healthy_normality$p.value
cancer_p = cancer_normality$p.value

if(healthy_p>=0.05 && cancer_p>=0.05)
{
  if(var.test(healthy, cancer)$p.value>=0.05)
  {
    test = t.test(healthy, cancer, paired = FALSE, alternative = "g", var.equal = TRUE)
    
  }else{
    test = t.test(healthy, cancer, paired = FALSE, alternative = "g", var.equal = FALSE)
    
  }
}else{
  test = wilcox.test(healthy, cancer, alternative = 'g', paired = FALSE)
}

#print(test)

file_path <- "D:/Work/NU/Master/Visualization/A5/A5_output.txt"
write("p_value of wilcoxon test as the data of the cancerous is not normally distributed ", file = file_path, append = TRUE)
write("p_value of wilcoxon test", file = file_path, append = TRUE)

write(test$p.value, file = file_path, append = TRUE)


#Permutation Implementation
#calc the average of both sets
T_healthy <- mean(healthy)
T_cancer <- mean(cancer)

# The observed test statistic:
T_obs = T_cancer - T_healthy
Permutations =  1000000# The number of data permutations.
T_perm = vector(mode = "double", length = Permutations) # A vector to store the T_perm's.
Y = c(healthy, cancer) # Gather all the yield data in one array.
L = length(Y)

for (i in 1:Permutations) 
{
  Ind_healthy = sample(L, L/2) # Pick up random (L/2) samples and consider them normal.
  healthy_perm = Y[Ind_healthy] # The normal samples.
  cancer_perm = Y[-Ind_healthy] # The modified samples.
  
  # The metric in the normal case:
  T_healthy_perm = mean(healthy_perm)
  
  # The metric in the modified case:
  T_cancer_perm = mean(cancer_perm)
  
  # The permuted test statistic at iteration i:
  T_perm[i] = T_cancer_perm - T_healthy_perm
}

p_value = length(which(T_perm >= T_obs)) / Permutations

write("p_value of permutation ", file = file_path, append = TRUE)
write(p_value, file = file_path, append = TRUE)

