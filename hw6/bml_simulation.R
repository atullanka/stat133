#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

source("bml_functions.R")

#Case 1: Given a 100x100 grid with cars, how does 'p' impact the number of steps
# it takes to hit gridlock

q = seq(0.25,0.95,0.05)
table <- matrix(nrow = length(q), ncol = 10)
for(a in 1:length(q)){
  table[a,] = replicate(10, bml.sim(100,100,q[a]))
}
rownames(table) = seq(0.25,0.95,0.05)

#Table for data reference:
summary.table <- matrix(summary(t(table)), byrow = TRUE, nrow = 15, ncol = 6)

#Boxplot to show the trends (to show clarity in the graph, some values too large were omitted):
boxplot(t(table[7:15,]), main = "Steps vs p", ylab = "Steps", xlab = "p")
axis(1, at=1:9, labels = seq(0.55,0.95,0.05))

#This simulation will be repeated for a 10x10 matrix now
w = seq(0.25,0.95,0.05)
table.1 <- matrix(nrow = length(w), ncol = 10)
for(a in 1:length(w)){
  table.1[a,] = replicate(10, bml.sim(10,10,w[a]))
}
rownames(table.1) = seq(0.25,0.95,0.05)

#Table for data reference:
summary.table.1 <- matrix(summary(t(table.1)), byrow = TRUE, nrow = 15, ncol = 6)

#Case 2: Given p = 0.5, how would different dimensions for the grid affect the number of steps 
#taken to reach gridlock

x = c(10,50,100)
table.2 <- matrix(nrow = length(x), ncol = 10)
for(a in 1:length(x)){
  table.2[a,] = replicate(10, bml.sim(x[a],x[a],0.5))
}
rownames(table.2) = c(10,50,100)

#Case 3: Does the orientation(rectangle vs square) of the grid impact the steps, given
#that the 'p' is kept constant at 0.7

table.3 <- replicate(10,bml.sim(10,10,0.7))
table.3.1 <- replicate(10,bml.sim(20,5,0.7))
table.3.2 <- replicate(10,bml.sim(25,4,0.7))
table.3.compiled = t(rbind(t(table.3), t(table.3.1), t(table.3.2)))
colnames(table.3.compiled) = c("10x10", "20x5", "25x4")

#Case 4: Does the flipping the dimensions have an affect on the steps, keeping the 'p'
#value constant at 0.6

table.4 <- replicate(10,bml.sim(10,50,0.7))
table.4.1 <- replicate(10,bml.sim(50,10,0.7))
table.4.compiled = t(rbind(t(table.4), t(table.4.1)))
colnames(table.4.compiled) = c("10x50", "50x10")