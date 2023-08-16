path.name <- "D:/Work/NU/Master/Visualization/A4/Assignment_04_P_values.txt"
p_values <- read.table(path.name, sep = '\t', header=T, row.names=1)

pvec = scan(path.name, character(), quote = "")
print(pvec)

counter = 0

#function to count elements of a vector
auto_counter <- function(dataset,countervar){
  countervar = 0
  for(value in dataset)
  {
    countervar = countervar+1
  }
  print(countervar)
  
}

pval_num = auto_counter(pvec, counter)

#writing data into txt file
file_path <- "D:/Work/NU/Master/Visualization/A4/A4_output.txt"
write("Number of p_values before any correlation ", file = file_path, append = TRUE)
write(pval_num, file = file_path, append = TRUE)

#print(pval_num)

gene.names = paste('G', seq(1:pval_num), sep = "") # The names of genes.

significant_values = gene.names[which(pvec <= 0.05)]
before_corr = auto_counter(significant_values, counter)
#print(before_corr)

write("Number of significant p_values before any correlation ", file = file_path, append = TRUE)
write(before_corr, file = file_path, append = TRUE)


# Apply the Bonferroni correction.
q_bonf = p.adjust(pvec, method = 'bonferroni') 


significant_values = gene.names[which(q_bonf <= 0.05)]
after_bonf = auto_counter(significant_values, counter)
#print(after_bonf)

write("Number of significant p_values after applying the Bonferroni method ", file = file_path, append = TRUE)
write(after_bonf, file = file_path, append = TRUE)

# Apply the fdr correction.
q_fdr = p.adjust(pvec, method = 'fdr') 


significant_values = gene.names[which(q_fdr <= 0.05)]
after_fdr = auto_counter(significant_values, counter)
#print(after_fdr)

write("Number of the significant p_values after applying the FDR method ", file = file_path, append = TRUE)
write(after_fdr, file = file_path, append = TRUE)
