## function for constructing a classic (& rather general) lifetable
lifetable <- function(x, Nx, Dx, sex="Male"){
  m <- length(x)
  mx  <- Dx/Nx
  n <- c(diff(x), NA)
  ax <- rep(0,m)
  ax <- n/2
  ax[m] <- 1 / mx[m]
  
  if(sex=="Female"){
    ax[1] <- ifelse(mx[1]>=0.107, 0.35, 0.053+2.8*mx[1])
    ax[2] <- ifelse(mx[1]>=0.107, 1.361, 1.522-1.518*mx[1])
    #if(mx[1]>=0.107){
    #  ax[1] <- 0.350
    #}else{
    #  ax[1] <- 0.053 + 2.800*mx[1]
    #}
  }
  if(sex=="Male"){
    ax[1] <- ifelse(mx[1]>=0.107, 0.33, 0.045+2.684*mx[1])
    ax[2] <- ifelse(mx[1]>=0.107, 1.352, 1.651-2.816*mx[1])
    #if(mx[1]>=0.107){
    #  ax[1] <- 0.330
    #}else{
    #  ax[1] <- 0.045 + 2.684*mx[1]
    #}
  }
  ax[-1] <- n[-1]/2
  ax[m] <- 1 / mx[m]
  
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  sd  <- ex*0
  sd[1] <-  sqrt(sum(dx*(x+ax-ex[1L])^2))
  return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex, sd)
  return(return.df)
}

