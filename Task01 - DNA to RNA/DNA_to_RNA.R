DNA_to_RNA <- function(DNA){
  count = nchar(DNA)
  DNA_splitted <- strsplit(DNA, '')[[1]]
  for (i in 1:count)
    if(DNA_splitted[i]=='T')
      DNA_splitted[i] = 'U'
  RNA <- paste(DNA_splitted, collapse = "")
  print(RNA)
  
}

