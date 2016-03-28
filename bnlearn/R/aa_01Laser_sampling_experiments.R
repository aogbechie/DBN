
sample_dbn <- function(bn_trans.fit_gRain,num_seqs_sim,num_samples){
  n_nodes <- length(nodeNames(bn_trans.fit_gRain))/2
  nodes_tpast <- nodeNames(bn_trans.fit_gRain)[-(1:n_nodes)]
  nodes_t <- nodeNames(bn_trans.fit_gRain)[(1:n_nodes)]
  # samples <- list()
  samples <- mclapply(1:num_seqs_sim, function(i){
  # for (i in 1:num_seqs_sim ) {

    #data_simul <- c() 
    #initialize simulation by setting as evidence t_past = '1'
    # data_simul <- rep(1, times = n_nodes*2)
    data_simul <- as.data.frame( matrix(NA, nrow = num_samples, ncol = n_nodes*2) )
    data_simul[1,] <- 1
    names(data_simul) <- nodeNames(bn_trans.fit_gRain)
    bn_ev <-setEvidence(bn_trans.fit_gRain, nodes = nodes_tpast, states =  rep("1", times = n_nodes))
    
    #Simulate t
    ts_data <-simulate.grain(bn_ev,1)
    # Add time slice t simulated to time serie
    data_simul[2,] <- ts_data
    # data_simul <- rbind(data_simul, ts_data)
    t <- 3
    if(num_samples>2){
      for (j in 1:(num_samples-1)){
      #while(num_samples >= t ){
        bn_ev <-setEvidence(bn_trans.fit_gRain, 
                            nodes = nodes_tpast, 
                            states =  as.character(data.frame(lapply(ts_data[1:n_nodes], as.character), 
                                                              stringsAsFactors=FALSE)))
        ts_data <-simulate.grain(bn_ev,1)
        #data_simul[2+j,] <- ts_data
        data_simul[t,] <- ts_data
        #t <- t+1
        #data_simul <- rbind(data_simul, ts_data)
      }
  }
    data_simul <- data_simul[-1,]
    data_simul <- data.frame(lapply(data.frame(lapply(data_simul, as.character), stringsAsFactors=FALSE), as.integer))
    #samples <- c(samples, list(data_simul))
    return(data_simul)
  })
  
  return(samples)
  
}

logLik_dbn_sample <- function(bn_trans.fit, sample_PC_t){
  #Initialize parameters
  num_nodes <- length(bn_trans.fit)
  name_nodes <- names(bn_trans.fit)
  nodes_i <- list()
  sizes_t <- list()
  log_CPD <- list()
  N_freq <- list()
  for (i in 1: (num_nodes/2)){
    nodes_i <- c(nodes_i, list(names(dimnames(bn_trans.fit[[i]]$prob))))
    sizes_t <- c(sizes_t, list(dim(bn_trans.fit[[i]]$prob)[1]))
    log_CPD <- c(log_CPD, list(log(bn_trans.fit[[i]]$prob)))
    N_freq <- c(N_freq, list(array(data = 0, dim = dim(bn_trans.fit[[i]]$prob))))
  }
  
  T <- dim(sample_PC_t)[1]
  #Compute frequencies of observations
  for (t in 1:T){
    for (i in 1:(num_nodes/2)){
      dim_N_freq <- cumprod(dim(N_freq[[i]]))
      dim_N_freq <- Lag(dim_N_freq,1)
      dim_N_freq[1] <- 0
      names(dim_N_freq) <- nodes_i[[i]]
      pos <- rep(0, length(dim(N_freq[[i]])))
      names(pos) <- nodes_i[[i]]
      elem <-0
      for (node in nodes_i[[i]]){
        pos[node] <- as.integer(sample_PC_t[t,node])
        if(name_nodes[i]==node){
          elem <- elem + pos[node]
        } else {
          elem <- elem +(pos[node]-1)*dim_N_freq[node]
        }
      }
      N_freq[[i]][elem] <- N_freq[[i]][elem] + 1 
    }
  }
  # Calculate loglikelihood
  loglik <- 0
  for (i in 1:(num_nodes/2)){
    loglik <- loglik + sum(N_freq[[i]]*log_CPD[[i]])
  }
}
