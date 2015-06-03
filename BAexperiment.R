#Ajai Sharma
genKids = function(id, bTime, aTime, lambda = 1, kappa = 1) {
  
  ncp = integer() # number of children
  btc = list() # birth times of children
  atc = list() # assassination times
  
  for (i in 1:length(id)){
    
    #NUMBER OF CHILDREN 

    ncpi = rpois(1,lambda * (aTime[i] - bTime[i])) 
    
    if (ncpi>=1){
      ncp[i] = ncpi
    }else{
      ncp[i] = 0
    }
    
    #BIRTHTIMES
    
    if (ncp[i]!=0){
      btc[[i]] = runif(ncp[i], bTime[i], aTime[i])
      
    }
    
    #ASSASSINATION TIMES
    
    if (ncp[i]!=0){
      atc[[i]] = aTime[i] + rexp(ncp[i], kappa)
      
    }
    
  }
  
  #DATAFRAME
  
  if (sum(ncp)>0){
    
    cid = 1:sum(ncp)
    pid = rep(id,ncp)
    vbtc = unlist(btc)
    vatc = unlist(atc)
    
    fdf = data.frame(cid, pid, vbtc, vatc)
    colnames(fdf) <- c("id", "parentID", "birth", "death")
    return(fdf)
    
  }else{
    return(data.frame(id = 0, parentID = 0, birth = 0, death =0))    
  }
  
}


familyTree = function(lambda = 1, kappa = 1, maxGen = 10, maxTime = 5*lambda) {
  
  allGens = list()
  
  #1st Gen
  allGens[[1]] = data.frame(id=1, parentID=0, bTime = 0, aTime = rexp(1,1))
  cgbt = as.vector(allGens[[1]][,3])
  
  while (length(allGens) < maxGen & 
    all(cgbt < maxTime)){
    
    clag = length(allGens)
    allGens[[clag+1]] = genKids(as.vector(allGens[[clag]][,1]), 
                                as.vector(allGens[[clag]][,3]), 
                                as.vector(allGens[[clag]][,4]), 
                                lambda, kappa)
    
    if (as.vector(allGens[[length(allGens)]][1,4])==0){
      return(allGens)
    }else{
      cgbt = as.vector(allGens[[length(allGens)]][,3]) # vector of latest birthtimes
      
    }
  }
  
  return(allGens)
  
}
