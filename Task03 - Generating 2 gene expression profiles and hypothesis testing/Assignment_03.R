#Khloud Khaled - 212002354

#generating samples
set.seed(50)
G1_control <- rnorm(100)
G1_condition <- rnorm(100)
G2_control <- rnorm(1000,2,0.5)
G2_condition <- rnorm(1000,4,0.5)
G3_control <- rnorm(1000,2,0.5)
G3_condition <- rnorm(1000,0,1.5)
G4_control <- rnorm(1000)
G4_condition <- runif(1000,0,1)

#For G1, check whether its GE level is the same under the normal state 
#and the conditioned state assuming samples are paired.

G1_difference = G1_condition - G1_control
G1_normality_test = shapiro.test(difference)
G1_p_value = G1_normality_test$p.value
if(G1_p_value >= 0.05){
  test = t.test(G1_condition, G1_control, alternative = "two.sided")
  }else{
  test = wilcox.test(G1_condition, G1_control, alternative = 'two.sided', paired = TRUE)
  }
test_result = capture.output(print(test))
write(test_result, file = ("D:/Work/NU/Master/Visualization/output.txt"), append=TRUE)


#For G2, check whether its GE level is greater under the conditioned state 
#assuming samples are independent.

G2_condition_normality = shapiro.test(G2_condition)
G2_control_normality = shapiro.test(G2_control)
G2_condition_p = G2_condition_normality$p.value
G2_control_p = G2_control_normality$p.value
if(G2_control_p >= 0.05 && G2_condition_p >= 0.05){
 if(var.test(G2_control, G2_condition)$p.value>= 0.05){
   test = t.test(G2_condition, G2_control, paired = FALSE, alternative = "g", var.equal = TRUE)
 }else{
   test = t.test(G2_condition, G2_control, paired = FALSE, alternative = "g", var.equal = FALSE)
 }
  
}else{
  test = wilcox.test(G2_condition, G2_control, alternative = 'g', paired = FALSE)
  
}
test_result = capture.output(print(test))
write(test_result, file = ("D:/Work/NU/Master/Visualization/output.txt"), append=TRUE)

#For G3, check whether its GE level is greater under the conditioned state 
#assuming samples are independent.

G3_condition_normality = shapiro.test(G3_condition)
G3_control_normality = shapiro.test(G3_control)
G3_condition_p = G3_condition_normality$p.value
G3_control_p = G3_control_normality$p.value
if(G3_control_p >= 0.05 && G3_condition_p >= 0.05){
  if(var.test(G3_control, G3_condition)$p.value>= 0.05){
    test = t.test(G3_condition, G3_control, paired = FALSE, alternative = "g", var.equal = TRUE)
  }else{
    test = t.test(G3_condition, G3_control, paired = FALSE, alternative = "g", var.equal = FALSE)
  }
  
}else{
  test = wilcox.test(G3_condition, G3_control, alternative = 'g', paired = FALSE)
  
}

test_result = capture.output(print(test))
write(test_result, file = ("D:/Work/NU/Master/Visualization/output.txt"), append=TRUE)


#For G4, check whether its GE level is the same under the normal state 
#and the conditioned state assuming samples are independent.
G4_condition_normality = shapiro.test(G4_condition)
G4_control_normality = shapiro.test(G4_control)
G4_condition_p = G4_condition_normality$p.value
G4_control_p = G4_control_normality$p.value
if(G4_control_p >= 0.05 && G4_condition_p >= 0.05){
  if(var.test(G4_control, G4_condition)$p.value>= 0.05){
    test = t.test(G4_condition, G4_control, paired = FALSE, alternative = "two.sided", var.equal = TRUE)
  }else{
    test = t.test(G4_condition, G4_control, paired = FALSE, alternative = "two.sided", var.equal = FALSE)
  }
  
}else{
  test = wilcox.test(G4_condition, G4_control, alternative = 'two.sided', paired = FALSE)
  
}
test_result = capture.output(print(test))
write(test_result, file = ("D:/Work/NU/Master/Visualization/output.txt"), append=TRUE)

#For G4, check if the GE level of 0.2 is significantly different 
#from any of its control values.
G4_normality_test = shapiro.test(G4_control)
G4_p_value = G4_normality_test$p.value
if(G4_p_value >= 0.05){
  test = t.test(G4_control, mu = 0.2, alternative = "two.sided")
}else{
  test = wilcox.test(G4_control, mu = 0.2, alternative = 'two.sided', paired = FALSE)
  
}
test_result = capture.output(print(test))
write(test_result, file = ("D:/Work/NU/Master/Visualization/output.txt"), append=TRUE)
