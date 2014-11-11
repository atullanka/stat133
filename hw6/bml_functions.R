#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  par(ask = FALSE)
  m <- matrix(sample(0:2, r*c, p = c(1-p, p/2, p/2), replace = TRUE), ncol = r)
 return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

move.east <- function(m){
  if(ncol(m) ==1) return(m)
blocked = m[, c(2:ncol(m),1)] != 0
red.car = m*(m==1)
m*(m==2) + red.car*blocked + (red.car*!blocked)[,c(ncol(m), 1:ncol(m)-1)]
}

move.north <- function(m){
  if(nrow(m) ==1) return(m)
  blocked = m[c(nrow(m),1:nrow(m)-1),] != 0
  blue.car = m*(m==2)
  m*(m==1) + blue.car*blocked + (blue.car*!blocked)[c(2:nrow(m),1),]
 }     
    
bml.step <- function(m){
    orig.pos = m
    new.pos = move.north(move.east(m))
  m = new.pos
grid.new = !(identical(new.pos, orig.pos))  
return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
m  = bml.init(r, c, p)
image(t(apply(m,2,rev)), col = c("white", "red", "blue"))
steps = 0
for(i in 1:50000) {
   temp <- bml.step(m)
   m <- temp[[1]]
  logic.value <- temp[[2]] 
  if(logic.value == TRUE) steps = steps + 1
  if(logic.value == FALSE) break 
  if(steps == 50000) break
 }
image(t(apply(m,2,rev)), col = c("white", "red", "blue"))
return(steps) 
}


