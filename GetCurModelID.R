GetCurModelID = function()
{
  vPath = strsplit(getwd(), "/")[[1]]
  n = length(vPath)
  return(strsplit(vPath[n],"(\\.)")[[1]][1])
}

GetCurFolder = function()
{
  vPath = strsplit(getwd(), "/")[[1]]
  n = length(vPath)
  return(strsplit(vPath[n],"(\\.)")[[1]][1])
}