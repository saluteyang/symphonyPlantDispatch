library(Rsymphony)
library(Matrix)
library(parallel)
library(doParallel)
library(foreach)

## matlab example ####
rm(list=ls())
gc()

system.time({

# constants
gen <- c(61, 152)
fuel <- c(61*9, 152*7.5)

nPeriods <- 48
# nGens <- 2
nHeatrates <- 2

totalfuel <- 3.95e4
fuelPrice <- 3

startCost <- 10000
shutdownCost <- 1000
minTimeUp <- 4
minTimeDn <- 4

nPaths <- 10 * 4

set.seed(123)
poolPrice <- matrix(rnorm(nPeriods*nPaths, mean = 60, sd = 30), 
                    nrow = nPeriods, ncol = nPaths) 

# the bounds are not set explicitly as Rsymphony_solve_LP uses "B" types param
# to indicate binary variables
lby <- array(0, c(nPeriods, nHeatrates))
lbz <- array(0, c(nPeriods))
lbo <- array(0, c(nPeriods))
lb <- c(c(lby), c(lbz), c(lbo))
ub <- rep(1, length(lb))

cleary <- array(0, c(nPeriods, nHeatrates))
clearz <- array(0, c(nPeriods))
clearo <- array(0, c(nPeriods))

## constraint where no more than one state is on per generator ####
A <- matrix(NA, nrow = nPeriods, ncol = length(lb))
idx <- 1
for (ii in 1:nPeriods){
    tempy <- cleary
    tempy[ii, ] <- 1
    addrow <- c(c(tempy), c(clearz), c(clearo))
    A[idx, ] <- addrow
    idx <- idx + 1
}
b <- rep(1, nPeriods)

## fuel total constraint ####
yFuel <- lby
for (i in 1:nHeatrates){
  yFuel[, i] <- fuel[i]
}

addrow <- c(c(yFuel), c(clearz), c(clearo))
A <- rbind(A, addrow)
b <- c(b, totalfuel)

## constraint to minimize number of starts ####
tempA <- matrix(NA, nrow = nPeriods, ncol = length(lb))
idx <- 1
for (ii in 1:nPeriods){
    tempy <- cleary
    tempz <- clearz
    tempo <- clearo
    for (jj in 1:nHeatrates){
      tempy[ii, jj] <- -1
      if(ii < nPeriods){
        tempy[ii + 1, jj] <- 1
      } else {
        tempy[1, jj] <- 1
      }
    }
    tempz[ii] <- -1
    tempA[idx, ] <- c(c(tempy), c(tempz), c(tempo))
    idx <- idx + 1
}

A <- rbind(A, tempA)
b <- c(b, rep(0, nPeriods))

## constraint to minimize number of stops ####
tempA <- matrix(NA, nrow = nPeriods, ncol = length(lb))
idx <- 1
for (ii in 1:nPeriods){
  tempy <- cleary
  tempz <- clearz
  tempo <- clearo
  for (jj in 1:nHeatrates){
    tempy[ii, jj] <- 1
    if(ii < nPeriods){
      tempy[ii + 1, jj] <- -1
    } else {
      tempy[1, jj] <- -1
    }
  }
  tempo[ii] <- -1
  tempA[idx, ] <- c(c(tempy), c(tempz), c(tempo))
  idx <- idx + 1
}

A <- rbind(A, tempA)
b <- c(b, rep(0, nPeriods))

## minimum time up constraint ####
tempA <- matrix(0, nrow = nPeriods, ncol = length(lb))
idx <- 1
for (ii in 1:nPeriods){
    tempy <- cleary
    tempz <- clearz
    tempo <- clearo
    tempo[ii] <- 1
    shiftMinTimeUp <- 1
    while (shiftMinTimeUp <= minTimeUp - 1){
      if(ii < (nPeriods + shiftMinTimeUp) & ii > shiftMinTimeUp){
        tempz[ii - shiftMinTimeUp] <- 1
      }
      shiftMinTimeUp <- shiftMinTimeUp + 1
    }
    tempA[idx, ] <- c(c(tempy), c(tempz), c(tempo))
    idx <- idx + 1
}

A <- rbind(A, tempA)
b <- c(b, rep(1, nPeriods))

## minimum time down constraint ####
tempA <- matrix(0, nrow = nPeriods, ncol = length(lb))
idx <- 1
for (ii in 1:nPeriods){
    tempy <- cleary
    tempz <- clearz
    tempo <- clearo
    tempz[ii] <- 1
    shiftMinTimeDn <- 1
    while (shiftMinTimeDn <= minTimeDn){
      if(ii < (nPeriods + shiftMinTimeDn) & ii > shiftMinTimeDn){
        tempo[ii - shiftMinTimeDn] <- 1
      }
      shiftMinTimeDn <- shiftMinTimeDn + 1
    }
    tempA[idx, ] <- c(c(tempy), c(tempz), c(tempo))
    idx <- idx + 1
}

A <- rbind(A, tempA)
b <- c(b, rep(1, nPeriods))

## construct objective ####
generatorlevel <- lby
for (i in 1:nHeatrates){
  generatorlevel[, i] <- gen[i]
}

# change dimension of revenue to account for nPaths
revenue <- array(0, c(nPeriods, nHeatrates, nPaths))
for (jj in 1:nPaths){
  for (ii in 1:nPeriods){
    revenue[ii, ,jj] <- poolPrice[ii, jj]*generatorlevel[ii, ]
  }
}


fuelCost <- yFuel*fuelPrice

starts <- (clearz + 1)*startCost
starts <- c(starts)

shuts <- (clearo + 1)*shutdownCost
shuts <- c(shuts)

# change f into funciton that takes nPaths idx
f <- function(i){
  c(c(revenue[, , i]) - c(fuelCost), -starts, -shuts)
}

# obj <- f
mat <- A
rhs <- b
dir <- rep("<=", length(b))

## comment out next three lines unless testing ####
# resultSympth <- foreach(i = 1:nPaths) %dopar%
#   Rsymphony_solve_LP(f(i), mat, dir, rhs, types = "B", max = TRUE)
# }) # run to here when testing time spent with sequential foreach

## replace sequential foreach with registered cluster ####
cl <- makeCluster(4) # make cluster appropriate to machine core count
registerDoParallel(cl) # register the number of cores in the cluster
clusterExport(cl, c('f', 'mat', 'dir', 'rhs')) # make objects visible to cluster
clusterEvalQ(cl, library(Rsymphony)) # make the solver visible to cluster
resultSymph <- foreach(i = 1:nPaths) %dopar%
  Rsymphony::Rsymphony_solve_LP(f(i), mat, dir, rhs, types = "B", max = TRUE)
registerDoSEQ() # 'unregister' the cluster

}) # run to here when testing time spent with parrallel

# resultSymph$objval

## remap the results ####
i <- 1 # index of nPaths
ysolution <- resultSymph[[i]]$solution[1:(nPeriods*nHeatrates)]
zsolution <- resultSymph[[i]]$solution[(nPeriods*nHeatrates + 1):length(resultSymph[[i]]$solution)]
ysolution <- array(ysolution, c(nPeriods, nHeatrates))
zsolution <- array(zsolution, c(nPeriods))
ysolution
zsolution

# check minup/down constraints are adhered to
for (i in 1:nHeatrates){
  ycheck <- apply(ysolution, MARGIN = 1, FUN = sum)
}
ycheck <- rle(ycheck)
ycheck <- data.frame(val = ycheck$values, runs = ycheck$lengths)
ycheck$idx <- cumsum(ycheck$runs)
View(ycheck)
