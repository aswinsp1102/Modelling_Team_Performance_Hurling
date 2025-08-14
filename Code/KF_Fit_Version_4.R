KFfit_updated <- function(  param,              # model parameters
                            data,               # a matrix containing the data ie Y
                            cov_info, m,               # delete this line and include other arguments to be passed
                            #     to the function here
                            optim=FALSE,        # set this to FALSE if not estimating parameters, otherwise,
                            #     if parameters have already been estimated, then set to TRUE
                            history.means=FALSE,# whether to save a matrix of filtered E(Theta_t)
                            history.vars=FALSE, # whether to save a list of filtered V(Theta_t)
                            prior.mean=NULL,    # optional prior mean. column vector.    # Normally set
                            prior.var=NULL,     # optional prior mean. matrix.           # these inside the code
                            fit=FALSE,          # whether to return a matrix of fitted values
                            se.fit=FALSE,       # whether to return the standard error of the fitted values
                            se.predict=FALSE,   # whether to return the prediction standard error =
                            #     se(fitted values) + observation variance V_t
                            noisy=TRUE,         # whether to print a progress bar, useful.
                            na.rm=FALSE){       # whether to use NA handling. set to TRUE if any Y is NA
  
  start <- Sys.time()
  
  if(se.predict & !se.fit){
    stop("Must have se.fit=TRUE in order to compute se.predict") # leads to a computational saving
  }
  
  m <- as.numeric(m)
  if(m <= 0) stop("m must be positive")
  
  phi <- exp(param[1]) / (1 + (exp(param[1])))
  sigma.w.sqr <- exp(param[2]) ^ 2
  h <- param[3]
  sigma.v.sqr <- exp(param[4]) ^ 2
  gamma <- exp(param[5]) / (1 + (exp(param[5])))
  
  # Initializing the momentum_lookup table
  momentum_lookup<- matrix(cbind(seq(1,m),0,0,0,0,0),nrow = m , ncol = 6 )
  colnames(momentum_lookup) <- c("team_id" , "mo_1" , "mo_2" , "mo_3" , "mo_4" , "mo_5")
  
  T <- dim(data)[1]
  
  if(is.null(prior.mean)){
    Xpost <- matrix(0, nrow = m , ncol = 1)
  }
  else{
    Xpost <- prior.mean
  }
  
  if(is.null(prior.var)){
    Vpost <- diag(1000, nrow = m)
  }
  else{
    Vpost <- prior.var
  }
  
  if (history.means){
    Xrec <- Xpost
  }
  if(history.vars){
    Vrec <- list()
    Vrec[[1]] <- Vpost
  }
  
  A <- diag(phi, nrow = m)
  W <- diag(sigma.w.sqr, nrow = m)
  B <- matrix(0, nrow = m, ncol = 1)
  # C <- diag(m) # C bit in this version is been moved to the 
  #                loop where it is changing with the delta_t 
  #               (time difference) between the games played 
  #                is taken into consideration
  # F <- diag(m)
  V <- diag(sigma.v.sqr, nrow = 1)
  
  loglik <- 0
  fitmat <- c()
  sefitmat <- c()
  sepredictmat <- c()
  
  if(noisy){
    pb <- txtProgressBar(min=1,max=T,style=3)
  }
  
  
  for(t in 1:T){
    
    D <- matrix(0, nrow = 1, ncol = m)
    D[1, cov_info$i_value[t]] <- 1
    D[1, cov_info$j_value[t]] <- -1
    
    C <- diag(cov_info$time_period[t],nrow = m)
    
    # E <- matrix(0, nrow = m, ncol = 1)
    # E[cov_info$i_value[t], 1] <- ifelse(cov_info$h_value[t] == 1, h, 0)
    E <- matrix(ifelse(cov_info$h_value[t] == 1, h, 0), 1, 1)
    
    F <-  matrix(1, nrow=1, ncol=1) # this too?
    
    # Calculating the summarized momentum value of i and j to be sent to the KF advance function
    M <- matrix(0,1,1)
    sum_gamma <- 0
    # for (n in 1:5){
    #   sum_gamma <- sum_gamma + (gamma ** n)
    # }
    for (k in 1:5){
      temp_m <- momentum_lookup[cov_info$i_value[t] , k+1] - momentum_lookup[cov_info$j_value[t] , k+1]
      # M <- M + matrix(temp_m, 1 ,1)
      # M <- M + matrix(temp_m * ((gamma ** k) / sum_gamma),1,1) # trying to normalise the gamma to sum of 1
      # M <- M + matrix(temp_m * (gamma / k),1,1) # tried using a linear approach towards interpreting gamma
      M <- M + matrix(temp_m * (gamma ** k)) # quadratic approach
    }
    # M_hat <- matrix(gamma * M,1,1)
    
    
    # this bit calls KF advance
    new <- kfadvance_udpated(obs=data[t,],oldmean=Xpost,oldvar=Vpost,A=A,B=B,C=C,D=D,E=E,F=F,P=M,W=W,V=V,marglik=TRUE,log=TRUE,na.rm=na.rm)
    
    Xpost <- new$mean
    Vpost <- new$var
    
    # This bit post the momentum calculated from the advance function to the lookup table
    momentum_i <- new$momentum_i
    momentum_j <- new$momentum_j
    
    if(t==1){ # used when this function is called iteratively one step at a time
      running.mean <- Xpost
      running.var <- Vpost
    }
    
    
    # this bit calculates the new rt values and updating the momentum_lookup for i values
    momentum_lookup[cov_info$i_value[t] , 6] <- momentum_lookup[cov_info$i_value[t] , 5]
    momentum_lookup[cov_info$i_value[t] , 5] <- momentum_lookup[cov_info$i_value[t] , 4]
    momentum_lookup[cov_info$i_value[t] , 4] <- momentum_lookup[cov_info$i_value[t] , 3]
    momentum_lookup[cov_info$i_value[t] , 3] <- momentum_lookup[cov_info$i_value[t] , 2]
    momentum_lookup[cov_info$i_value[t] , 2] <- momentum_i
    # updating the momentum_lookup for j values
    momentum_lookup[cov_info$j_value[t] , 6] <- momentum_lookup[cov_info$j_value[t] , 5]
    momentum_lookup[cov_info$j_value[t] , 5] <- momentum_lookup[cov_info$j_value[t] , 4]
    momentum_lookup[cov_info$j_value[t] , 4] <- momentum_lookup[cov_info$j_value[t] , 3]
    momentum_lookup[cov_info$j_value[t] , 3] <- momentum_lookup[cov_info$j_value[t] , 2]
    momentum_lookup[cov_info$j_value[t] , 2] <- momentum_j
    
    # if(t==100){
    # browser()
    # }
    
    loglik <- loglik + new$mlik
    
    
    if (history.means){
      Xrec <- cbind(Xrec,Xpost)
    }
    if(history.vars){
      Vrec[[t+1]] <- Vpost # since first entry is the prior
    }
    
    if(fit){
      fitmat <- cbind(fitmat,D%*%Xpost + E)
    }
    if(se.fit){
      sefitmat <- cbind(sefitmat,sqrt(diag(D%*%Vpost%*%t(D))))
    }
    if(se.predict){
      sepredictmat <- cbind(sepredictmat,sqrt((sefitmat[,ncol(sefitmat)])^2+diag(F%*%V%*%t(F))))
    }
    
    if(noisy){
      setTxtProgressBar(pb,t)
    }
  }
  
  if(noisy){
    close(pb)
  }
  end <- Sys.time()
  
  if(noisy){
    cat("Time taken:",difftime(end,start,units="secs")," seconds.\n")
  }
  
  if(optim){
    return(-loglik) # just return the -log likelihood if in parameter estimation mode
  }
  else{
    retlist <- list(mean=Xpost,var=Vpost,mlik=loglik,data=data,running.mean=running.mean,running.var=running.var)
    if(history.means){
      retlist$history.means <- Xrec
    }
    if(history.vars){
      retlist$history.vars <- Vrec
    }
    if(fit){
      retlist$fit <- fitmat
    }
    if(se.fit){
      retlist$se.fit <- sefitmat
    }
    if(se.predict){
      retlist$se.predict <- sepredictmat
    }
    return(retlist)
  }
}