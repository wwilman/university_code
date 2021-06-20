RelMatrixA = function(pedigree)
{
  
  n = nrow(pedigree)
  N = n + 1
  A = matrix(0, ncol=N, nrow=N)
  
  sire = (pedigree[,2] == 0)*N + pedigree[,2]
  dam = (pedigree[,3] == 0)*N + pedigree[,3]
  
  for(i in 1:n)
  {
    
    A[i,i] = 1 + A[sire[i], dam[i]]/2
    
    for(j in (i+1):n)
    {
      if (j > n) break
      A[i,j] = ( A[i, sire[j]] + A[i,dam[j]] )/2
      A[j,i] = A[i,j] 	
    }			
  }
  
  write.table(A[1:n, 1:n],"A.txt",row.names = F,col.names = F)
  return(A[1:n, 1:n])
  
}