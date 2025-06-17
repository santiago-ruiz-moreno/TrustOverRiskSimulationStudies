#############################################################################################################
# Program Name  si.test, power.si.test                                                                      #
# Purpose   nonparametric test for stochastic inequality                                                    #
#           based on two independent samples (Schlag, 2008)                                                 #
#           computes p value - theta is fixed at optimal level for delta=0 and conf.level=0.05              #
#           H0: P(X_2>X_1)-P(X2<X1)<=delta against H1:P(X_2>X_1)-P(X2<X1)>d ("greater")                     #
#           H0: P(X_2>X_1)-P(X2<X1)>=delta against H1:P(X_2>X_1)-P(X2<X1)<d ("less")                        #
#           H0: P(X_2>X_1)-P(X2<X1)=delta against H1:P(X_2>X_1)-P(X2<X1)<>d ("two.sided")                   #
#           Difference in differences:                                                                      #
#           H0: P(X_2-X_4>X_1-X_3)-P(X_2-X_4<X_1-X_3)<=delta against H1:P(X_2>X_1)-P(X2<X1)>d ("greater")   #
#           H0: P(X_2-X_4>X_1-X_3)-P(X_2-X_4<X_1-X_3)>=delta against H1:P(X_2>X_1)-P(X2<X1)<d ("less")      #
#           H0: P(X_2-X_4>X_1-X_3)-P(X_2-X_4<X_1-X_3)=delta against H1:P(X_2>X_1)-P(X2<X1)<>d ("two.sided") #
#                                                                                                           #
# Authors       Karl Schlag, Natalia Shestakova, and James Tremewan                                         #
#                                                                                                           #
# Based on program "CITE" co-written by Peter Saffert, Oliver Reiter                                        #
# Date          6.01.2022 (this version)                                                                    #
#                                                                                                           #
#           (Schlag and Tremewan 2023, The Stochastic Inequality Test - a Test for a                        #
#           Directional Treatment Effect)                                                                   #
#                                                                                                           #
#############################################################################################################


## x1, x2 ... two data vectors, x1 independent samples from X1, x2 independent sample from X2
## conf.level ... level of the test
## delta ... threshold for testing the stochastic difference P(X_2>X_1)-P(X2<X1)
## ci = TRUE  computes 100*(1-conf.level)% confidence interval
## digits. ... number of decimal places for p value and CI
## epsilon ,,, error tolerance of Monte Carlo loop
## max.iterations ... maximal number of iterations in the Monte Carlo loop
## conf.level defines both size of confidence interval and optimal theta!!!


#### STOCHASTIC INEQUALITY TEST ####
si.test <- function(x1, x2, x3=NULL, x4=NULL,
                              did = FALSE,
                              ci = FALSE,
                              delta = 0,
                              alternative = "two.sided",
                              digits = 2, conf.level = 0.05,
                              epsilon = 1e-06, ignoreNA = FALSE,
                              max.iterations = 1e+05)
{
  d <- delta
  # probability of success for given delta
  p <- max((1 + d) / 2, epsilon)
  
  prec = 10^{-1*digits}
  
  if(!ci) cint <- NULL
  
  if(ignoreNA == TRUE)
  {
    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
    x3 <- x3[!is.na(x3)]
    x4 <- x4[!is.na(x4)]
  }  else if(any(is.na(c(x1, x2, x3, x4))) == TRUE)
  {
    stop("The data contains NA's!")
  }
  
  if(!missing(conf.level) &&
     (length(conf.level) != 1 || !is.finite(conf.level) ||
      conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (!(alternative %in% c("two.sided","greater","less")))
    stop("Please a value of alternative in {less, greater, two.sided}.")
  
  if (d > 1 | d < -1)
    stop("Please supply a  value for delta in [-1,1].")
  
  if(!did){
  method <- "Stochastic Inequality Test"
  names.x1 <- deparse(substitute(x1))
  names.x2 <- deparse(substitute(x2))
  DNAME <- paste(names.x1, "and",
                 names.x2)
  } else{
  method <- "Stochastic Inequality Test for differences in differences"
  names.x1 <- deparse(substitute(x1))
  names.x2 <- deparse(substitute(x2))
  names.x3 <- deparse(substitute(x3))
  names.x4 <- deparse(substitute(x4))
  DNAME <- paste(names.x1, names.x2, names.x3, "and",
                 names.x4) 
  }
## Sample estimate of stochastic difference
    if(!did){
      group1 <- x1
      group2 <- x2
    } else{
      # creates vectors of all differences between treated and untreated observations by group
      group1 <- as.vector(vapply(
        x3, 
        function(x) x - x1, 
        FUN.VALUE = vector(mode = "numeric", length = length(x1))))
      group2 <- as.vector(vapply(
        x4, 
        function(x) x - x2,
        FUN.VALUE = vector(mode = "numeric", length = length(x2))))
    }
  
  count.group1 <- sum(unlist(lapply(group1, function(x) as.numeric(x > group2))))
  count.group2 <- sum(unlist(lapply(group2, function(x) as.numeric(x > group1))))
  sample.est <- (count.group2 - count.group1)/(length(group1) * length(group2))
  
## Perform matchings and return number of signed differences and ties
  # size of smallest (sub-)group
  if(!did){
    min.length <- min(length(x1), 
                      length(x2))
  }else{
    min.length <- min(length(x1),
                      length(x2),
                      length(x3),
                      length(x4))
  }
  
  if (min.length < 5)
    stop("There must be at least 5 observations in smallest group")
    
    diff.tie <- as.data.frame(t(replicate(max.iterations,
                                          sample.diffs(larger.under.null = x2,
                                                       smaller.under.null = x1,
                                                       larger.under.null.untreated = x4,
                                                       smaller.under.null.untreated = x3,
                                                       sample.size = min.length,
                                                       did = did))))

## Find optimal type II error and corresponding theta
    
  optimaltypeII <- uniroot(
    minTypeIIErrorWrapper,
    c(0, 1),
    p = p,
    N = min.length,
    alpha = conf.level - epsilon
  )
  
  theta_list <- minTypeIIError(
    p.alt = optimaltypeII[[1]],
    p = p,
    N = min.length,
    alpha = conf.level - epsilon
  )
  
  theta <- theta_list$theta

## Find minimum possible p-value given data
    
  psearch <- function(rejMatrix){
  
  L <- prec
  rejpL <- significance(min.length, L, theta, rejMatrix, epsilon)[1]
  U <- 1
  rejpU <- significance(min.length, U, theta, rejMatrix, epsilon)[1]
  if(rejpL == TRUE) {
    pval <- prec
  } else if(rejpU == FALSE) {
    pval <- 1
  } else {
    p1 <- ceiling((L+U)/2/prec)*prec
    p2 <- max(L, p1-prec)
    
    rejp1 <- significance(min.length, p1, theta, rejMatrix, epsilon)[1]
    rejp2 <- significance(min.length, p2, theta, rejMatrix, epsilon)[1]
    
    while(rejp2 >= rejp1 & p2 != L) {
      if(rejp1 == 0 & rejp2 == 0) {
        L = p1
        p1 = ceiling((L+U)/2/prec)*prec
        p2 = max(L, p1-prec)
        rejp1 <- significance(min.length, p1, theta, rejMatrix, epsilon)[1]
        rejp2 <- significance(min.length, p2, theta, rejMatrix, epsilon)[1]
      }
      else {
        U = p2
        p1 = ceiling((L+U)/2/prec)*prec
        p2 = max(L, p1-prec)
        rejp1 <- significance(min.length, p1, theta, rejMatrix, epsilon)[1]
        rejp2 <- significance(min.length, p2, theta, rejMatrix, epsilon)[1]
      }
    }
    pval <- min(p1,1)
  }
  return(pval)
} ## end of psearch routine

if(alternative!="greater"){
  rejMatrix_ls <- SampleBinomTest(diff.tie$s1temp, diff.tie$s2temp, diff.tie$ties,1-p)
  pval <- pval.less <- psearch(rejMatrix_ls)
}
if(alternative!="less"){
  rejMatrix_gr <- SampleBinomTest(diff.tie$s2temp, diff.tie$s1temp, diff.tie$ties,p)
  pval <- pval.greater <- psearch(rejMatrix_gr)
}
if(alternative=="two.sided"){
    if(pval.less < pval.greater){
    pval_temp <- min(1,2*pval.less)
    rejMatrix <- rejMatrix_ls
    }else
  {
    pval_temp <- min(1,2*pval.greater)
    rejMatrix <- rejMatrix_gr
  }
    pval <- ifelse(significance(min.length, pval_temp-prec, theta, rejMatrix, epsilon)[1],pval_temp-prec, pval_temp)
    
}

## Find confidence intervals
if(ci){
CIsearch <- function(lower.lim = TRUE, alternative = "two.sided"){
  alpha <- ifelse(alternative=="two.sided",conf.level/2,conf.level)
  
  if(lower.lim){
    z1 <-  diff.tie$s2temp
    z2 <-  diff.tie$s1temp
    U <- floor(sample.est/prec)*prec
  }else{
    z1 <-  diff.tie$s1temp
    z2 <-  diff.tie$s2temp
    U <- -ceiling(sample.est/prec)*prec
  }
  L <- -1
  
  rejMatrix <- SampleBinomTest(z1, z2, diff.tie$ties,0)
  if(significance(min.length, alpha, theta, rejMatrix, epsilon)[1]==FALSE){
  lim_temp <- -1
  }else{
    d1 <- ceiling((L+U)/2/prec)*prec
    d2 <- max(L, d1-prec)
    p1 <- max((1 + d1) / 2, epsilon)
    p2 <- max((1 + d2) / 2, epsilon)
    rejMatrixU <- SampleBinomTest(z1, z2, diff.tie$ties,p1)
    rejMatrixL <- SampleBinomTest(z1, z2, diff.tie$ties,p2)
    rejd1 <- significance(min.length, alpha, theta, rejMatrixU, epsilon)[1]
    rejd2 <- significance(min.length, alpha, theta, rejMatrixL, epsilon)[1]
    
    while(rejd1 >= rejd2) {
      if(rejd1 == 1 & rejd2 == 1) {
        L <- d2
        d1 <- ceiling((L+U)/2/prec)*prec
        d2 <- max(L, d1-prec)
        p1 <- max((1 + d1) / 2, epsilon)
        p2 <- max((1 + d2) / 2, epsilon)
        rejMatrixU <- SampleBinomTest(z1, z2, diff.tie$ties,p1)
        rejMatrixL <- SampleBinomTest(z1, z2, diff.tie$ties,p2)
        rejd1 <- significance(min.length, alpha, theta, rejMatrixU, epsilon)[1]
        rejd2 <- significance(min.length, alpha, theta, rejMatrixL, epsilon)[1]
      }
      else {
        U <- d1
        d1 <- ceiling((L+U)/2/prec)*prec
        d2 <- max(L, d1-prec)
        p1 <- max((1 + d1) / 2, epsilon)
        p2 <- max((1 + d2) / 2, epsilon)
        rejMatrixU <- SampleBinomTest(z1, z2, diff.tie$ties,p1)
        rejMatrixL <- SampleBinomTest(z1, z2, diff.tie$ties,p2)
        rejd1 <- significance(min.length, alpha, theta, rejMatrixU, epsilon)[1]
        rejd2 <- significance(min.length, alpha, theta, rejMatrixL, epsilon)[1]
      }
    }
    lim_temp <- d1
  }
  lim <- ifelse(lower.lim,lim_temp,-lim_temp)
  return(lim)
} ## end of CIsearch routine

if (alternative == "less") {
  cint <- c(-1, CIsearch(lower.lim = FALSE,alternative="less"))
}
else if (alternative == "greater") {
  cint <- c(CIsearch(lower.lim = TRUE,alternative = "greater"),1)
}
else {
  cint <- c(CIsearch(lower.lim = TRUE,alternative = "two.sided"),CIsearch(lower.lim = FALSE,alternative="two.sided"))
}
attr(cint,"conf.level") <- conf.level
}
  ## Output
  names(sample.est) <- if(!did) paste("P(", names.x2, ">", names.x1, ") - P(",
                                      names.x2, "<", names.x1, ")") else
                                        paste("P(", names.x4, "-", names.x2, ">", names.x3,"-", names.x1, ") - P(",
                                              names.x4, "-", names.x2, "<", names.x3,"-", names.x1, ")")
                             
  names(d) <- if(!did) "stochastic difference" else
                        "stochastic difference in differences"

  names(theta) = "theta"
  rval <- list(statistic = NULL,
               parameter = theta,
               p.value = pval,
               conf.int = cint, estimate = sample.est, null.value = d,
               alternative = alternative,
               method = method, data.name = DNAME)
  class(rval) <- "htest"
  return(rval)

}
#### END OF si.test FUNCTION ####

#### POWER CALCULATIONS ####
power.si.test <- function(n = NULL, delta = NULL, sig.level = NULL, power = NULL,
                          alternative = "two.sided", digits = 2, epsilon = 1e-06)
{
  
  if (sum(vapply(list(n, delta, sd, power, sig.level), is.null, 
                 NA)) != 1) 
    stop("exactly one of 'n', 'delta', 'power', and 'sig.level' must be NULL")
  
  if (!(alternative %in% c("two.sided","one.sided")))
    stop("Please a value of alternative in {one.sided, two.sided}.")
  if(!is.null(delta)){
    delta <- abs(delta)
    p = (1 + delta) / 2}
  
  if(is.null(power))
  {
    if(alternative == "two.sided"){sig.level <-  sig.level/2}
    optimaltypeII <- uniroot(minTypeIIErrorWrapper,
                             c(0, 1), p = 0.5, N = n,
                             alpha = 0.05 - epsilon)
    theta <- as.numeric(minTypeIIError(optimaltypeII[[1]],
                                       p = 0.5, N = n, alpha = 0.05 - epsilon)[1])
    power <- max(1 - (1- g2((1 + delta) / 2, n, 1/2, sig.level*theta))/(1-theta),0)
  }
  else if(is.null(n))
  {
    if(alternative == "two.sided"){sig.level <-  sig.level/2}
    n <- 5
    while(is.null(minTypeIIError(p, 1/2, n, sig.level, alternative))){
      n <- n + 1
    }
    optimaltypeII <- uniroot(minTypeIIErrorWrapper,
                             c(0, 1), p = 0.5, N = n,
                             alpha = 0.05 - epsilon)
    theta <- as.numeric(minTypeIIError(optimaltypeII[[1]],
                                       p = 0.5, N = n, alpha = 0.05 - epsilon)[1])
    ptemp <- 1 - (1- g2(p, n, 1/2, sig.level*theta))/(1-theta)
    while(ptemp < power)
    {
      n <- n + 1
      # find theta
      optimaltypeII <- uniroot(minTypeIIErrorWrapper,
                               c(0, 1), p = 0.5, N = n,
                               alpha = 0.05 - epsilon)
      theta <- as.numeric(minTypeIIError(optimaltypeII[[1]],
                                         p = 0.5, N = n, alpha = 0.05 - epsilon)[1])
      ptemp <- 1 - (1- g2(p, n, 1/2, sig.level*theta))/(1-theta)
    }
  }
  else if(is.null(delta)){
    if(alternative == "two.sided"){sig.level <-  sig.level/2}
    optimaltypeII <- uniroot(minTypeIIErrorWrapper,
                             c(0, 1), p = 0.5, N = n,
                             alpha = 0.05 - epsilon)
    theta <- as.numeric(minTypeIIError(optimaltypeII[[1]],
                                       p = 0.5, N = n, alpha = 0.05 - epsilon)[1])
    
    d <- 1
    p <- (1 + d) / 2
    ptemp <- 1 - (1- g2(p, n, 1/2, sig.level*theta))/(1-theta)
    if(ptemp < power){
      stop("no stochastic difference can be rejected with this n, power, and sig.level")
    } else {
      while(ptemp > power)
      {
        d <- d - 10^(-digits)
        p <- (1 + d) / 2
        ptemp <- 1 - (1- g2(p, n, 1/2, sig.level*theta))/(1-theta)
      }
    }
    delta <- d + 10^(-digits)
  }
  
  else if(is.null(sig.level)){
    
    optimaltypeII <- uniroot(minTypeIIErrorWrapper,
                             c(0, 1), 0.5, N = n,
                             alpha = 0.05 - epsilon)
    theta <- as.numeric(minTypeIIError(optimaltypeII[[1]],
                                       p = 0.5, N = n, alpha = 0.05 - epsilon)[1])
    sigtemp <- prec <- 10^{-digits} 
    ptemp <- 1 - (1- g2(p, n, 1/2, sigtemp *theta))/(1-theta)
    while(ptemp < power&sigtemp < 1)
    {
      sigtemp <- sigtemp + prec
      ptemp <- 1 - (1- g2(p, n, 1/2, sigtemp *theta))/(1-theta)
    }
  }
  
  if(alternative == "two.sided"&!is.null(sig.level)){sig.level <- min(1,2*sig.level)}
  if(alternative == "two.sided"&is.null(sig.level)){
    if(1 - (1- g2(p, n, 1/2, (2*sigtemp-prec) *theta))/(1-theta) > power){
      sig.level <- 2*sigtemp-prec
    }else{
      sig.level <- 2*sigtemp
    }
    }
  if(alternative == "one-sided"){
    sig.level <- ifelse(is.null(sig.level),sigtemp,sig.level)
  }
  
  NOTE <- "n is number in *smallest* group"
  METHOD <- "Stochastic inequality test (lower bound) power calculation"
  structure(list(n = n, delta = delta, theta=theta, sig.level = sig.level, 
                 power = power, alternative = alternative, note = NOTE, 
                 method = METHOD), class = "power.htest")
  
}

#### END OF POWER CALCULATIONS ####



###--------------------------------------------------
### Generate sample data and return number of times a value from each group (or difference) is bigger than the other
###--------------------------------------------------
sample.diffs <- function(larger.under.null,
                            smaller.under.null,
                            larger.under.null.untreated = NULL,
                            smaller.under.null.untreated = NULL,
                            sample.size,
                            did = FALSE) {
  if(did) {
    # treatment effect in group 1
    y1 <- sample(larger.under.null, sample.size) - 
      sample(larger.under.null.untreated, sample.size)
    # treatment effect in group 2
    y2 <- sample(smaller.under.null, sample.size) - 
      sample(smaller.under.null.untreated, sample.size)
  } else {
    y1 <- larger.under.null
    y2 <- smaller.under.null
  }
  
  c1 <- sample(y1, sample.size)
  c2 <- sample(y2, sample.size)
  
  s1temp <- sum(c1 > c2) #counts how often c1 > c2
  s2temp <- sum(c1 < c2) #counts how often c1 < c2
  ties <- sample.size - s1temp - s2temp
  return(c(s1temp = s1temp, 
           s2temp = s2temp,
           ties = ties))
  }


###--------------------------------------------------
### Sample Binomial Test
###--------------------------------------------------
SampleBinomTest <- function(s1temp,
                              s2,
                              ties,
                              prob.success) {
  
  s1 <- s1temp + sapply(ties, function(x) rbinom(1, x, abs(2 * prob.success - 1) / (1 + abs(2 * prob.success - 1))))
  prob <- mapply(function(xx,yy) sum(dbinom(yy:(xx + yy), (xx + yy), prob.success)), s1,s2)
  h2 <- (prob.success ^ s2) * ((1 - prob.success) ^ s1) * choose(s1 + s2, s2)
  
  return(data.frame(prob, h2))
  }

###--------------------------------------------------
### Significance: wrapper function
###--------------------------------------------------
## tests whether given combination of d and alpha leads to rejection

significance <- function(min.length, alpha, theta, rejMatrix, epsilon) {
  
  pseudoalpha <- (alpha-epsilon) * theta
  
  # evaluate results of sample binomial tests given alpha and optimal theta 
  res <- ifelse(rejMatrix$prob <= pseudoalpha, 1,
                ifelse(
                  rejMatrix$prob <= (pseudoalpha + rejMatrix$h2),
                  (pseudoalpha - rejMatrix$prob + rejMatrix$h2) / rejMatrix$h2,
                  0))
  
  rej <- mean(res)
  
  # error <- exp(-2 * nrow(rejMatrix) * (threshold) ^ 2)
  threshold <- sqrt(-log(epsilon) / (2 * nrow(rejMatrix)))
  rejection <- ifelse(rej >= theta + threshold, TRUE, FALSE)
  
  return(rejection)
} ## end of significance

###--------------------------------------------------
### Functions to derive the optimal theta
###--------------------------------------------------


## w
## helper function, to ease the reading of the code

w <- function(x)
{
  as.numeric(x >= 0)
}

g1 <- function(k, N, z, alpha)
## g1
## helper function
{
  term2 <- pbinom(k, N, z, lower.tail = FALSE)
  ifelse(alpha < term2, 0,
         ifelse(alpha > pbinom(k - 1, N, z, lower.tail = FALSE),
                1,
                (alpha - term2)/dbinom(k, N, z)))
}

## g2
## helper function
g2 <- function(mu, N, z, alpha)
{
  k <- 0:(N - 1)
  
  term1 <- ifelse(alpha >= z^N, 1, alpha/z^N)
  
  res <- sum(dbinom(k, N, mu) * g1(k, N, z, alpha)) + mu^N*term1
  res
}

## possibleTheta
## Calculates possible values of theta, which are in interval (0,1)
possibleTheta <- function(N, p, alpha)
{
  k <- 0:N

  theta <- (1/alpha) * pbinom(k - 1, N, p,
                              lower.tail = FALSE)
  sensibletheta <- theta < 1 & theta > 0
  res <- theta[sensibletheta]
  res <- rbind(k[sensibletheta], res)
  
  row.names(res) <- c("k","theta")
  res
  ## print(identical(round(res[2], 5), round(res1[2], 5)))
  ## list(res, res1)
}


minTypeIIError <- function(p.alt, p, N, alpha, alternative)
{
  ## Calculates minimum value, for given difference d
  ## uses possibleTheta, g2
  theta <- possibleTheta(N, p, alpha)
  
  f <- function(x)
  {
    (1 - g2(p.alt, N, p, alpha*x))/(1 - x)
  }
  typeIIerrors <- sapply(theta[2,], f)
  
  if(!is.numeric(typeIIerrors))
  {
    return(NULL)   
  } else {
    mintypeII <- min(typeIIerrors, 1)
    
    righttheta <- theta[2, which(typeIIerrors == mintypeII)]
    righttheta <- ifelse(length(righttheta) == 0, NA, righttheta)
    
    list(theta = righttheta,
         typeII = mintypeII)
  }
}

minTypeIIErrorWrapper <- function(p.alt, p, N, alpha,
                                  typeIIgoal = .5)
{
  minTypeIIError(p.alt, p, N, alpha)$typeII - typeIIgoal
}

## uniroot(minTypeIIErrorWrapper, c(0, 1), p = .1, N = 50, alpha = .05)

## typeII error computation for d.alt > d
typeIIerr <- function(N, alpha, theta, d, d.alt)
{
  p.alt <- (1 + d.alt) / 2
  p <- (1 + d) / 2
  res <- min((1 - g2(p.alt, N, p, alpha*theta))/(1 - theta),1)
  return(res)
}

## computes value of d.alt that gives type II error equal to target typeIIgoal
invtypeIIerr <- function(N, alpha, theta, d, typeIIgoal)
{
  f <- function(d.alt) {res <- typeIIerr(N, alpha, theta, d, d.alt)-typeIIgoal}
  res <- uniroot(f, c(d, 1))$root
  return(res)
}

