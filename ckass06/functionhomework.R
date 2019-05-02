library(bio3d)

analyze_protein <- function(x){
  s<-read.pdb(x)
  s.chainA <-trim.pdb(s, chain="A", elety="CA")
  s.b <- s.chainA$atom$b
  plotb3(s.b, sse=s.chainA, typ="l", ylab="Bfactor")
  
}

a_proteins <- function(x){
  browser()
  for (i in length(x)){
    analyze_protein(x[i])
  }
}

protein_list <- c("4AKE", "1AKE", "1E4Y")
a_proteins(protein_list)
