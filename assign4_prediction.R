### ONLY past your predict.y function here
predict.y <- function(x)
{
  load("fit_params.Rdata")
  f.x <- x %*% beta.final
  return(f.x)
}
