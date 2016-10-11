RemoveNA = function(RawData)
{
  nRow = nrow(RawData)
  Index = vector(length=nRow)
  for (i in 1:nRow) {
    Index[i] = !any(is.na(x.raw[i,]))
  }
  return(RawData[Index,])
}

