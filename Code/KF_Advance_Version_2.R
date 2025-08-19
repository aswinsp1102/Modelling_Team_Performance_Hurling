kfadvance_udpated <- function (obs, oldmean, oldvar, A, B, C, D, E, F,P, W, V, marglik = FALSE,log = TRUE, na.rm = FALSE){
  if (na.rm) {
    if (any(is.na(obs))) {
      if (all(is.na(obs))) {
        if (log) {
          return(list(mean = A %*% oldmean + B, var = A %*%
                        oldvar %*% t(A) + C %*% W %*% t(C), mlik = 0))
        }
        else {
          return(list(mean = A %*% oldmean + B, var = A %*%
                        oldvar %*% t(A) + C %*% W %*% t(C), mlik = 1))
        }
      }
      else {
        M <- diag(length(obs))
        M <- M[-which(is.na(obs)), ]
        obs <- obs[which(!is.na(obs))]
        D <- M %*% D
        E <- M %*% E
        F <- M %*% F
        P <- M %*% P
      }
    }
  }
  T <- A %*% oldmean + B
  S <- A %*% oldvar %*% t(A) + C %*% W %*% t(C)
  thing1 <- D %*% S
  tD <- t(D)
  K <- thing1 %*% tD + F %*% V %*% t(F)
  margmean <- D %*% T + E + P
  resid <- obs - margmean

  
  # This bit is capturing the momentum information
  
  expect_i <- pnorm(0,margmean , sqrt(as.numeric(K)) , lower.tail = FALSE)
  # expect_j <- 1 - expect_i
  obs_value <- ifelse(obs == 0 , 0.5 , ifelse(obs > 0 , 1 , 0))
  momentum_i <- (obs_value - expect_i) / expect_i
  momentum_j <- - (obs_value - expect_i) / (1 - expect_i)
  
  
  if (marglik == TRUE) {
    if (all(dim(K) == 1)) {
      thing2 <- S %*% tD
      newmean <- T + as.numeric(1/K) * thing2 %*% resid
      newvar <- S - as.numeric(1/K) * thing2 %*% thing1
      marginal <- dnorm(obs, as.numeric(margmean), sqrt(as.numeric(K)),
                        log = log)
    }
    else {
      Kchol <- chol(K)
      Kcholinv <- solve(Kchol)
      logdetK <- 2*sum(log(diag(Kchol)))
      Kinv <- Kcholinv%*%t(Kcholinv)
      #Kinv <- solve(K)
      thing3 <- tD %*% Kinv
      thing4 <- S %*% thing3
      newmean <- T + thing4 %*% resid
      newvar <- S - thing4 %*% thing1
      #marginal <- -(1/2)*determinant(K)$modulus + (-1/2) * t(resid) %*% Kinv %*% resid
      marginal <- -(1/2)*logdetK + (-1/2) * t(resid) %*% Kinv %*% resid
      #marginal <- dmvnorm(as.vector(obs),as.vector(margmean),K,log=TRUE)
      if (!log) {
        marginal <- exp(marginal)
      }
    }
    return(list(mean = newmean, var = newvar, mlik = marginal,momentum_i = momentum_i , momentum_j = momentum_j))
  }
  else {
    if (all(dim(K) == 1)) {
      thing2 <- S %*% tD
      newmean <- T + as.numeric(1/K) * thing2 %*% resid
      newvar <- S - as.numeric(1/K) * thing2 %*% thing1
    }
    else {
      #Kinv <- solve(K)
      Kchol <- chol(K)
      Kcholinv <- solve(Kchol)
      #logdetK <- 2*sum(log(diag(Kchol)))
      Kinv <- Kcholinv%*%t(Kcholinv)
      thing3 <- tD %*% Kinv
      thing4 <- S %*% thing3
      newmean <- T + thing4 %*% resid
      newvar <- S - thing4 %*% thing1
    }
    return(list(mean = newmean, var = newvar,momentum_i = momentum_i , momentum_j = momentum_j))
  }
}