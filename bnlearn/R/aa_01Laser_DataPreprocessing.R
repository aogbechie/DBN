readTimeSerie <- function(filepaths,pixels) {
  #Function that returns a list of vectors. Where each vector is the time serie of a pixel 
  #for a certain operation sequence.
  
  num_files<-length(filepaths)
  num_pixels<-length(pixels)
  
  #If we do not have any filepath we exit the function, else we read each file
  if(num_files==0 || num_pixels==0){
    print("Please, specify the paths to read or the pixels")
    return (0)
  } else {
    dataset=list() #Initialize the dataset
    #choose only the selected pixels
    pixel_select<-rep("NULL", 1025)
    for(pixel in pixels){
      pixel_select[pixel]<-"integer"
    }
    #read the files
    for(file in filepaths){
      mydata <- read.table(file, header=FALSE, sep=";", colClasses=pixel_select)
      mydata<-list(mydata)
      dataset<-c(dataset,mydata)
    }
    return(dataset)
  }
  
}#END readTimeSerie

equalLengthDiscr<- function(dataset, breaks, levels_factor) {
  #Function to discretize each dataset in a fix number of bins. 
  #For this implementation the range is from 0 to 1024
  init <- 0
  end <- 1024
  edges <- seq(from = init, by = (end/breaks), length.out = (breaks +1))
  label <- levels_factor
  n_seqs <- length(dataset)
  n_vars<-length(dataset[[1]])
  
  for (i in 1:n_seqs){
    df<-dataset[[i]]
    df_numeric<-sapply(df,as.numeric)
    for (j in 1:n_vars ){
      v <- df_numeric[,j]
      v <- cut(v,breaks=edges,labels = label, include.lowest=TRUE)
      #v <- cut(v,breaks=edges,labels = FALSE, include.lowest=TRUE)
      df_numeric[,j] <- v
    }
    df <- data.frame(df_numeric)
    df <- sapply(df,as.integer)
    df <- data.frame(df)
    dataset[[i]] <- df
  }
  return(dataset)
}#END equalLengthDiscr

separateDatasetDBN <- function(dataset,criteria, breaks) {
  #Function to separate the dataset in CPC_0, CPC_t, PC_t, CC_t
  n_seqs<-length(dataset)
  n_vars<-length(dataset[[1]])
  #Obtain CPC_0
  switch(criteria, 
         CPC_0={
           # case 'CPC_0' here...
           cpc_0<-dataset[[1]][1,]
           for (i in 1:n_seqs) {
             cpc_0<-rbind(cpc_0,dataset[[i]][1,])
           }
           cpc_0=cpc_0[-1,]
           names(cpc_0)<-paste(names(dataset[[1]]),"_0",sep="")
           cpc_0<-data.frame(sapply(cpc_0,as.numeric)) #Must be in numeric & dataframe for the algorithm
           return(cpc_0)
         },
         CPC_t={
           # case 'CPC_t' here...
           cpc_t<-dataset[[1]][1,]
           for (i in 1:n_seqs) {
             cpc_t<-rbind(cpc_t,dataset[[i]][-1,])
           }
           cpc_t=cpc_t[-1,]
           names(cpc_t)<-paste(names(dataset[[1]]),"_t",sep="")
           if(is.null(breaks)){
             cpc_t<-data.frame(sapply(cpc_t,as.numeric)) 
           }else{
             ###assure that we have num_factors= breaks in the correct order
             cpc_t <- int2factor_breaks(cpc_t,breaks)
           }
           return(cpc_t)    
         },
         PC_t={
           # case 'PC_t' here...
           pc_t<-cbind(dataset[[1]][1,],dataset[[1]][1,])
           for (i in 1:n_seqs) {
             n_slices<-length(dataset[[i]][,1])
             pc_int_t=dataset[[i]][-1,]
             pc_int_tpast=dataset[[i]][-n_slices,]
             pc_int<-cbind(pc_int_t,pc_int_tpast)
             pc_t<-rbind(pc_t,pc_int)
           }
           pc_t=pc_t[-1,]
           names(pc_t)<-c(paste(names(dataset[[1]]),"_t",sep=""),paste(names(dataset[[1]]),"_tpast",sep=""))
           if(is.null(breaks)){
             pc_t<-data.frame(sapply(pc_t,as.numeric)) 
           }else{
             ###assure that we have num_factors= breaks in the correct order
             pc_t <- int2factor_breaks(pc_t,breaks)
           }
           return(pc_t)         
         },
         CC_t={
           # case 'CC_t' here...
           cc_t<-cbind(dataset[[1]][1,],dataset[[1]][1,])
           for (i in 1:n_seqs) {
             n_slices<-length(dataset[[i]][,1])
             cc_int_t=dataset[[i]][-n_slices,]
             cc_int_tnext=dataset[[i]][-1,]
             cc_int<-cbind(cc_int_t,cc_int_tnext)
             cc_t<-rbind(cc_t,cc_int)
           }
           cc_t=cc_t[-1,]
           names(cc_t)<-c(paste(names(dataset[[1]]),"_t",sep=""),paste(names(dataset[[1]]),"_tnext",sep=""))
           if(is.null(breaks)){
             cc_t<-data.frame(sapply(cc_t,as.numeric)) 
           }else{
             ###assure that we have num_factors= breaks in the correct order
             cc_t <- int2factor_breaks(cc_t,breaks)
           }
           return(cc_t)     
         },
         {
           print('Please state valid criteria')
         }
  )
}#END separateDatasetDBN

int2factor_breaks <- function(dataset, breaks){
  ###assure that we have num_factors= breaks in the correct order
  n_vars<-length(dataset)
  correct_factor <- dataset[1,]
  for (i in 1:breaks){
    factor_in <- rep(i,n_vars) 
    correct_factor <- rbind(correct_factor,factor_in)
  }
  correct_factor<-correct_factor[-1,]
  dataset <- rbind(correct_factor, dataset)
  ###
  dataset_factor<-as.data.frame(matrix(NA, nrow = nrow(dataset), ncol = ncol(dataset)))
  for(i in 1:ncol(dataset)){
    dataset_factor[,i] <- factor(dataset[,i], levels = 1:breaks, labels = 1:breaks, ordered = TRUE)
  }
  names(dataset_factor)<-names(dataset)
  ###
  dataset_factor <- dataset_factor[-(1:breaks),]
  
  return(dataset_factor)
}


whitelist_CPC <- function(cpc, nodes_t) {
  white <- data.frame()
  for (i in 1:length(nodes_t)){
    node_name <-names(cpc[i])
    node_nbr <- cpc[[i]][2]$nbr
    
    node_nbr <- node_nbr[node_nbr %in% nodes_t]
    
    for (neighbour_node in node_nbr){
      white <- rbind(white, data.frame(from = c(node_name, neighbour_node), to = c(neighbour_node, node_name)))
    }
  }
  return(white)
}

in.t <- function(cpc, nodes_t) {
  
  for (i in 1:length(nodes_t)){
    node_name <-names(cpc[i])
    node_nbr <- cpc[[i]][2]$nbr
    node_nbr <- node_nbr[node_nbr %in% nodes_t]
    cpc[[i]][2]$nbr <- node_nbr
  }
  return(cpc)
}

blacklist_notCPC <- function(cpc, nodes_t) {
  black <- data.frame()
  for (i in 1:length(nodes_t)){
    node_name <-names(cpc[i])
    node_nbr <- cpc[[i]][2]$nbr
    node_not_nbr <- nodes_t[! nodes_t %in% node_nbr]
    
    for (not_neighbour_node in node_not_nbr){
      black <- rbind(black, data.frame(from = c(node_name, not_neighbour_node), to = c(not_neighbour_node, node_name)))
    }
  }
  return(black)
}

blacklistDMMPC <- function(data, nodes) {
  cpc<- data[[1]]
  pc <- data[[2]]
  nodes_t <- names(cpc)
  nodes_past <- nodes[! nodes %in% nodes_t]
  
  blacklist <- data.frame()
 
  
  #1. blacklist CPC
  for (i in 1:length(nodes_t)){
    node_name <-names(cpc[i])
    node_nbr <- cpc[[i]]
    node_not_nbr <- nodes_t[! nodes_t %in% node_nbr]
    
    for (not_neighbour_node in node_not_nbr){
      blacklist <- rbind(blacklist, data.frame(from = c(node_name, not_neighbour_node), to = c(not_neighbour_node, node_name)))
    }
  }
  
  #2. blacklist from present to past
  for (i in 1:length(nodes_t)){
    node_name_present <-names(cpc[i])
    for (j in 1:length(nodes_past)) {
      node_name_past <- nodes_past[j]
      blacklist <- rbind(blacklist, data.frame(from = node_name_present, to = node_name_past))
    }
  }
  
  #3. blacklist inter past-present
  for (i in 1:length(nodes_t)){
    node_name <-names(cpc[i])
    node_nbr <- pc[[i]]
    node_not_nbr <- nodes_past[! nodes_past %in% node_nbr]
    
    for (not_neighbour_node in node_not_nbr){
      blacklist <- rbind(blacklist, data.frame(from = not_neighbour_node, to = node_name))
    }
  }
  
  #4. blacklist intra past
  node_iter <- nodes_past
  for (i in 1:length(nodes_past)) {
    node_name <-nodes_past[i]
    for (j in 1:length(node_iter)) {
      node_name_j <-node_iter[j]
      blacklist <-rbind(blacklist, data.frame(from = c(node_name,node_name_j), to = c(node_name_j,node_name)))
    }
    node_iter <- node_iter[-1]
  }
  
  blacklist<-blacklist[!duplicated(blacklist),]
  return(blacklist)
}

bic_dbn <- function(data_past, complete_score, score) {
  nodes_past <- names(data_past)
  node_iter <- nodes_past
  blacklist <- data.frame()
  for (i in 1:length(nodes_past)) {
    node_name <-nodes_past[i]
    for (j in 1:length(node_iter)) {
      node_name_j <-node_iter[j]
      blacklist <-rbind(blacklist, data.frame(from = c(node_name,node_name_j), to = c(node_name_j,node_name)))
    }
    node_iter <- node_iter[-1]
  }
  blacklist<-blacklist[!duplicated(blacklist),]
  bn_past=hc(data_past,blacklist = blacklist, score = score)
  score_past=score(bn_past,data_past, type = score)
  score_dbn=complete_score-score_past
  return(score_dbn) 
}

loglik.dbn <- function(data_past, complete_score, score) {
  nodes_past <- names(data_past)
  node_iter <- nodes_past
  blacklist <- data.frame()
  for (i in 1:length(nodes_past)) {
    node_name <-nodes_past[i]
    for (j in 1:length(node_iter)) {
      node_name_j <-node_iter[j]
      blacklist <-rbind(blacklist, data.frame(from = c(node_name,node_name_j), to = c(node_name_j,node_name)))
    }
    node_iter <- node_iter[-1]
  }
  blacklist<-blacklist[!duplicated(blacklist),]
  bn_past=hc(data_past,blacklist = blacklist, score = score)
  score_past=score(bn_past,data_past, type = score)
  score_dbn=complete_score-score_past
  return(score_dbn) 
}

two_tbn.plot <- function(bn_trans, n_nodes){
  nodes_t<-names(bn_trans[[2]])[1:n_nodes]
  nodes_tpast<-names(bn_trans[[2]])[-(1:n_nodes)]
  nodes1 <-nodes_tpast
  nodes2 <-nodes_t  
  ft <- bn_trans$arcs
  ft <- ft[!duplicated(apply(ft, 1, paste, collapse="")),]
  back1<-sapply(nodes1,function(x){ x %in% ft})
  nodes1 <- subset(nodes1,back1)
  back2<-sapply(nodes2,function(x){ x %in% ft})
  nodes2 <- subset(nodes2,back2)
  g <- ftM2graphNEL(ft, edgemode="directed")
  twocolors <- c("transparent", "#E0F3F8")
  nodeType <- 1 + (nodes(g) %in% nodes1)
  nA = makeNodeAttrs(g, fillcolor=twocolors[nodeType])
  sg1 = subGraph(nodes1, g)
  sgL = list(list(graph=sg1, cluster = FALSE, attrs = c(rank="sink")))
  att = list(graph = list(rankdir = "RL", rank = ""))
  #plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)
  
  #nodes not used
  nodes_used <- c(ft[,1],ft[,2])
  black_t <- sapply(nodes_t, function(x){x %in% nodes_used})
  innecessary_t <-names(black_t[! black_t])
  print("Innecesary variables in t")
  print(innecessary_t)
  black_tpast <- sapply(nodes_tpast, function(x){x %in% nodes_used})
  innecessary_tpast <-names(black_tpast[! black_tpast])
  print("Innecesary variables in t-1")
  print(innecessary_tpast)
  
  return(list(g, att, nA, sgL))
  
}
  