#Ajai Sharma
exptOne = function(l, k, mG, mT){
  
  ex1 = familyTree(l,k,mG,mT)
  
  # The following logic is used to determine
  # if no children were produced
  # If no children return a 1 else 0 -- basically an indicator function.
  # This will help us to count the number of families.
  
  if (ex1[[length(ex1)]][1,4]==0){
    ng = length(ex1)-1
    ncg = integer()
    
    for (i in 1:(length(ex1)-1)){
      ncg[i] = nrow(ex1[[i]])
    }
    
    nc = sum(ncg)
    sumstat = c(ng,nc,1)
    return(sumstat)
    
  }else{
    ng = length(ex1)
    ncg = integer()
    
    for (i in 1:length(ex1)){
      ncg[i] = nrow(ex1[[i]])
    }
    
    nc = sum(ncg)
    sumstat = c(ng,nc,0)
    return(sumstat)
    
  }
  
}

MCBA = function(params, repeats = 100){
  final.stats = list()
  
  for (i in 1:nrow(params)){
    ngen = integer()
    nchil = integer()
    I.dieout = integer() # An indicator for dieouts from return(exptOne[3]).
    
    for (j in 1:repeats){
      t.stat = exptOne(params[i,1], params[i,2], 7, 20) # Here you can change maxGen and maxBirthtime parameters
      ngen[j] = t.stat[1]
      nchil[j] = t.stat[2]
      I.dieout[j] = t.stat[3]
      
    }
    
    numEnd = sum(I.dieout)
    os.med = median(nchil, na.rm = TRUE)
    os.quant = quantile(nchil,c(.25,.75))
    gen.quant = quantile(ngen, c(.25,.75))
    gen.med =  median(ngen, na.rm = TRUE)
    cstats = unname(c(numEnd,os.med,os.quant,gen.med,gen.quant))
    final.stats[[i]] = data.frame(cstats)
    #print(final.stats) (useful for 'long' simulations)
  }
  
  ## Let's make the dataframe
  
  f.df = data.frame(final.stats) 
  
  f.df = cbind(as.vector(params[,1]),as.vector(params[,2]),data.frame(t(f.df)))
  
  rownames(f.df) <- c()
  
  colnames(f.df) <- c("lambda", "kappa", "numEnd", "OS50", 
                      "OS25", "OS75", "Gen50", "Gen25", "Gen75")
  
  
  print("The simulation is complete.")
  return(f.df)
  
}