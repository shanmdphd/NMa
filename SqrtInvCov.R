SqrtInvCov <- function(M)
{
  EigenResult <- eigen(as.matrix(M))
  EigenVector <- EigenResult$vectors
  EigenValues <- abs(EigenResult$values)
  return(EigenVector %*% diag(1/sqrt(EigenValues)) %*% t(EigenVector))
}

SqrtCov <- function(M)
{
  EigenResult <- eigen(as.matrix(M))
  EigenVector <- EigenResult$vectors
  EigenValues <- abs(EigenResult$values)
  return(EigenVector %*% diag(sqrt(EigenValues)) %*% t(EigenVector))
}


MatPow <- function(M, Pow)
{
  EigenResult <- eigen(as.matrix(M))
  EigenVector <- EigenResult$vectors
  if (Pow < 0) EigenValues <- abs(EigenResult$values)
  return(EigenVector %*% diag(power(EigenValues, Pow)) %*% t(EigenVector))
}
