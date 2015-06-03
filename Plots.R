#Plot 1
#Ajai Sharma
frs.lambdas = as.vector(unique(frs[1]))
kappa.locations = list()

for (i in 1:23){
  kappa.locations[[i]] = which(frs[1]==frs.lambdas[i,])
  
}

#frs = final sorted results
#frs.lambdas = values of unique lambdas
#kappa.locations = row# for each particular value of lambda

plot(frs[kappa.locations[[1]],2],frs[kappa.locations[[1]],6],
     col = "blue", xlim = c(1,1.4), ylim = c(0, 7100),
     xlab = "Kappa", ylab = "Number of Offspring", main = "Number of Offspring and Kappa",
     sub = "(Each line represents one Lambda value)")

legend.cols1 = character()
legend.lambdas = numeric()

for (i in 11:23){
  
  xxx = frs[kappa.locations[[i]],2]
  yyy = frs[kappa.locations[[i]],6]
  curcol = cols[i-10]
  legend.lambdas[i] = frs.lambdas[i,]
  legend.cols1[i] = curcol
  points(xxx, yyy, col = curcol)
  lines(xxx,yyy,col = curcol, lwd = 4)
    
}

legend.cols1 = legend.cols1[11:23]
legend.lambdas = legend.lambdas[11:23]

legend("topright", legend = legend.lambdas[1:10], col = legend.cols1[1:10], lwd = 4, title = "Lambda Values", ncol = 2)

#Plot 2

plot(frs[kappa.locations[[1]],2],frs[kappa.locations[[1]],7],
     col = "blue", xlim = c(1,2), ylim = c(1, 8),
     xlab = "Kappa", ylab = "Number of Generations", main = "Number of Generations and Kappa",
     sub = "(Each line represents one Lambda value)")

legend.cols1 = character()
legend.lambdas = numeric()

for (i in c(5,10,15,20:23)){
  
  xxx = frs[kappa.locations[[i]],2]
  yyy = frs[kappa.locations[[i]],7]
  curcol = cols[i-4]
  legend.lambdas[i] = frs.lambdas[i,]
  legend.cols1[i] = curcol
  points(xxx, yyy, col = curcol)
  lines(xxx,yyy,col = curcol, lwd = 4)
  
}

legend.cols1 = legend.cols1[is.na(legend.cols1)==0]
legend.lambdas = legend.lambdas[is.na(legend.lambdas)==0]

legend("topright", legend = legend.lambdas[1:4], col = legend.cols1[1:4], lwd = 4, title = "Lambda Values", ncol = 2)

# Plots were exported as PNG using Export tool in Plot window.
