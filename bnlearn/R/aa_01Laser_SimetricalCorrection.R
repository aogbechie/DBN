sim.correction.static <- function(data, nodes){
  present<-data
  if(! is.null(present)){
    nbr_corrected <- present
    
    for(node.i in nodes){
      nodes.i<-nbr_corrected[[node.i]][[2]]
      backtracking <-unlist(sapply(nbr_corrected, function(x){ node.i %in% x$nbr }))
      known.bad = names(backtracking[!backtracking])
      nbr_corrected[[node.i]][[2]]  = nodes.i[! nodes.i %in% known.bad]
    }
    #Reduce list to just nbr
    nbr_corrected <- sapply(nbr_corrected, function(x){x$nbr})
  
  } else {
    print ("Please, specify the nbr for X[t] (npcr_PC_t_xt || npcr_CC_t_xt)")
    nbr_corrected -> NULL
  }
  return(nbr_corrected)
}
 

sim.correction.dynamic <- function(data, nodes){
 
  past<-data[[1]]
  future <-data[[2]]
  
  if(! (is.null(past) || is.null(future))){

    past <- sapply(past, function(x){x$nbr[! x$nbr %in% nodes]})
    past <- sapply(past, function(x){gsub("past", "", x)})
    future <- sapply(future, function(x){x$nbr[! x$nbr %in% nodes]})
    future <- sapply(future, function(x){gsub("next", "", x)})
    
    nbr_corrected <- past
    
    for(node.i in nodes){
      nodes.i<-nbr_corrected[[node.i]]
      backtracking <-unlist(sapply(future, function(x){ node.i %in% x }))
      known.bad = names(backtracking[!backtracking])
      nbr_corrected[[node.i]]  = nodes.i[! nodes.i %in% known.bad]

    }
  } else {
    print ("Please, specify the nbr for X[t] (npcr_PC_t_xt || npcr_CC_t_xt)")
    nbr_corrected -> NULL
  }
  
  return(nbr_corrected)
}