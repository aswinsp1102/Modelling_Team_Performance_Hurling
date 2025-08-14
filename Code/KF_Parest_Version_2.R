KFparest_updated <- function( data, m , cov_info , initial_values )
{
  start <- Sys.time()
  
  inits <- initial_values
  
  # use optim to find optimal parameters
  oppars <- optim(inits,
                  KFfit_updated,
                  data=data,
                  cov_info = cov_info,
                  m = m,
                  optim=TRUE,
                  control=list(trace=100, maxit = 700),
                  hessian = TRUE)
  
  end <- Sys.time()
  cat("\n")
  cat("Time Taken",difftime(end,start,units="mins"),"\n")
  cat("\n")
  
  return(oppars)
}