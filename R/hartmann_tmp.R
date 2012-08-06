# helper functions for Slater distribution simulation
#
generate_quasi <- function(nc=5, ne=10, r=1:5, prob= rep(1, length(r))) 
{
  matrix(sample(r, size=nc*ne, replace=T, prob=prob), ncol=ne)
}


generate_quasis <- function(n, nc=5, ne=10, r=1:5, prob= rep(1, length(r)))
{
  replicate(n, generate_quasi(nc=nc, ne=ne, r=r, prob=prob), simplify=FALSE)
}


get_upper_triangle <- function(x)
{
  x[upper.tri(x, diag=FALSE)]
}


quasiDistributionDistanceSlater <- function(reps, nc, ne, range, 
                                            prob=NULL, progress=TRUE)
{
  q <- generate_quasis(reps, nc=nc, ne=ne, r=range, prob= NULL)
  fun <- lapply
  if (progress)
    fun <- lapply_pb 
  dist.sl <- fun(q, slaterStandardization)
  dist.sl <- lapply(dist.sl, get_upper_triangle)
  unlist(dist.sl)
}


# quasiDistributionDistanceSlater <- function(rep, nc, ne, range, prob=NULL, progress=TRUE)
# {
#   quasis <- randomGrids(rep, nc=nc, ne=ne, range=range, prob=prob, options=0)
#   if (progress)                 # whether to show progress bar
#     lapply_fun <- lapply_pb else
#       lapply_fun <- lapply
#   quasis.sd <- lapply_fun(quasis, function(x){
#     ds <- distanceSlater(x)
#     ds[lower.tri(ds, diag=FALSE)]
#   })
#   unlist(quasis.sd)         # return as vector
# }



# Return a list with the mean and sd as indicated by Hartmann
# 
getSlaterPaperPars <- function(nc)
{
  # hartmann only provides values for a number of constructs between 7 and 21.
  # Smaller and bigger grids use the parameters with the next best number of
  # constructs from Hartmann
  if (nc < 7)
    nc <- 7
  if (nc > 21)
    nc <- 21
  
  ## constants ##
  # parameters for Slater distance distributions used to calculated Hartmann
  # distances as supplied by Hartmann, 1992,  p. 51. (only defined for 7 to 21
  # constructs)
  #
  hartmann.pars <- data.frame(
    constructs=7:21,
    mean=c(.97596, .97902, 0.98236, .98322, .98470, 
           .98643, .98749, .98811, .98908, .98972, 
           .99034, .99092, .99135, .99193, .99228),
    sd=c(.21910, .20376, .19211, .18240, .17396, .16416, .15860, .15374, .14700, 
         .14303, .13832, .13444, .13082, .12676, .12365))
  
  # "Therefore the means of all Z-transformed percentiles [avering three scale
  # range, MH] are suggested to be used as cutoffs for distance interpretation.
  # The use of the 5th and the 95th percentiles is recommended (see Table 5)"
  # (Hartmann, 1992, p.52).
  # 
  hartmann.cutoffs <- c(p01=2.492, p05=1.777, p10=1.387, 
                        p90=-1.186, p95=- 1.519, p99=- 2.129)
  slater.mean <- unlist(subset(hartmann.pars, constructs == nc, mean))
  slater.sd <- unlist(subset(hartmann.pars, constructs == nc, sd))
  list(mean=slater.mean, sd=slater.sd)  
}


simulateSlaterAndHartmannDistribution <- function(reps=1000, nc, ne, range, 
                                                  prob=NULL,
                                                  progress=TRUE)
{
  slater.vals <- quasiDistributionDistanceSlater(reps=reps, nc=nc, 
                                                 ne=ne, range=range,
                                                 prob=prob,                                      
                                                 progress=progress)  
  # mean and sd of Slater distribution
  mean.slater <- mean(slater.vals, na.rm=TRUE)   
  sd.slater <- sd(slater.vals, na.rm=TRUE)
  
  # conversion to Hartmann values
  hartmann.vals <- -1 * (slater.vals - mean.slater) / sd.slater  
  list(slater = slater.vals, 
       hartmann = hartmann.vals)
}


coverageProbability <- function(x, cutoffs)
{   
  l  <- length(x)
  oneCoverProb <- function(cutoff)
    sum(x < cutoff) / l
  sapply(cutoffs, oneCoverProb)
}


getDistributionParameters <- function(x, probs=c(.01, .025, .05, .1, .9, .95, .975, .99), 
                                      na.rm=TRUE)
{
  pars <- describe(x)
  qs <- quantile(x, probs = probs, na.rm = na.rm)
  #cover.probs <- coverageProbability(x, cutoffs)       # get coverage probabalities for cutoffs
  list(pars=pars, quantiles=qs)
}


# Hartmann  distance function version using the supplied values from
# the Hartmann 1992 paper
#
# distributions:  return Slater and Hartmann simulated distribution values if requested
#
distanceHartmann <- function(x, trim=10, reps=10000, type="sim", 
                             prob=NULL, progress=TRUE, distibutions=FALSE)
{
  if (!inherits(x, "repgrid")) 
    stop("Object must be of class 'repgrid'")
  ps <- seq(0, 1, .001)   # probabilty for quantiles to return
  
  # select parameter derivation for transformation
  type <- match.arg(type, c("paper", "simulate", "new"))

  ## get grid parameters ## 
  range <- getScale(x)            # get min and max scale value
  nc <- getNoOfConstructs(x)
  ne <- getNoOfElements(x)
   
  # derive parameters mean and sd by simulation of Slater distance distributions
  # for given grid size
  if (type == "simulate") {
    v <- simulateSlaterAndHartmannDistribution(reps=reps, nc=nc, ne=ne, range=range, 
                                               prob=NULL, progress=progress)
    sl <- getDistributionParameters(v$slater)
    hm <- getDistributionParameters(v$hartmann)    
  }
  
  # use parameters mean and sd from Hartmann paper or simulated 
  # TODO: use my own simulated parameters with bigger sample size
  # for more accuracy than Hartmann
  notes <- NULL
  if (type == "paper") {
    p <- getSlaterPaperPars(nc) 
    notes <- c("\nFor calculation the parameters from Hartmann (1992) were used.",
               "Use 'type=new' or type='sim' for a more accurate version.\n")
  } else 
  if (type == "simulate")
    p <- list(mean=sl$pars$mean, sd=sl$pars$sd) else
  if (type == "new")
    stop("type 'new' hast not yet been implemented", call.=FALSE) else
  stop("'type' must be 'paper', 'simulate' or 'new'", call.=FALSE)    
    
  # linear tranformation to derive Hartmann distance from Slater distances (1992, p. 49)
  D <- distanceSlater(x)
  H <- -1 * (D - p$mean) / p$sd
  #diag(H) <- 0      # replace diagonal as zeros (Hmmm?)
  quantiles.slater <- quantile(v$slater, probs=ps)
  quantiles.hartmann <- quantile(v$hartmann, probs=ps)
  attr(H, "quantiles") <- list(slater=quantiles.slater,
                               hartmann=quantiles.hartmann)
  attr(H, "arguments") <- list(along=2, dmethod="Hartmann (standardized Slater distances)", p=2,
                               notes=notes,
                               parameters=p)
  # return Slater and Hartmann simulated distribution values if requested
  # caution: objects get quite big ~ 6mb with 10000 reps
  if (distibutions)
    attr(H, "distibutions") <- v       
  H
}

