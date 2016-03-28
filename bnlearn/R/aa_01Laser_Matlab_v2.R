as.bnet <- function(bn_trans.fit, arcs, samp){
  
  Matlab$startServer()
  matlab <- Matlab()
  print(matlab)
  isOpen <- open(matlab)
  if (!isOpen)
    throw("MATLAB server is not running: waited 30 seconds.")
  print(matlab)
  #Define paths for functions bnet
  evaluate(matlab, "addpath('/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing1_3','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing1_4','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing1_5','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing1_6','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing1_7','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing2_3','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing2_4','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing2_5','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing2_6','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing2_7','/home/aogbechie/Desktop/Datasets/PHM2012/Data/Test_set/Bearing3_3','/home/aogbechie/Desktop/Datasets/PHM2012/Matlab','/home/aogbechie/Desktop/bnt','/home/aogbechie/Desktop/bnt/BNT','/home/aogbechie/Desktop/bnt/BNT/CPDs','/home/aogbechie/Desktop/bnt/BNT/CPDs/CVS','/home/aogbechie/Desktop/bnt/BNT/CPDs/Old','/home/aogbechie/Desktop/bnt/BNT/CPDs/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/CVS','/home/aogbechie/Desktop/bnt/BNT/examples','/home/aogbechie/Desktop/bnt/BNT/examples/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Map','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Map/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Map/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Map/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Mgram','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Mgram/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Mgram/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Mgram/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Motif','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Motif/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Square','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Square/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Square/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/HHMM/Square/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/SLAM','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/SLAM/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/SLAM/Old','/home/aogbechie/Desktop/bnt/BNT/examples/dynamic/SLAM/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/limids','/home/aogbechie/Desktop/bnt/BNT/examples/limids/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static','/home/aogbechie/Desktop/bnt/BNT/examples/static/Belprop','/home/aogbechie/Desktop/bnt/BNT/examples/static/Belprop/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/Brutti','/home/aogbechie/Desktop/bnt/BNT/examples/static/Brutti/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/HME','/home/aogbechie/Desktop/bnt/BNT/examples/static/HME/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/Misc','/home/aogbechie/Desktop/bnt/BNT/examples/static/Misc/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/Models','/home/aogbechie/Desktop/bnt/BNT/examples/static/Models/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/Models/Old','/home/aogbechie/Desktop/bnt/BNT/examples/static/Models/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/SCG','/home/aogbechie/Desktop/bnt/BNT/examples/static/SCG/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/StructLearn','/home/aogbechie/Desktop/bnt/BNT/examples/static/StructLearn/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/Zoubin')")
  evaluate(matlab, "addpath('/home/aogbechie/Desktop/bnt/BNT/examples/static/Zoubin/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/dtree','/home/aogbechie/Desktop/bnt/BNT/examples/static/dtree/CVS','/home/aogbechie/Desktop/bnt/BNT/examples/static/fgraph','/home/aogbechie/Desktop/bnt/BNT/examples/static/fgraph/CVS','/home/aogbechie/Desktop/bnt/BNT/general','/home/aogbechie/Desktop/bnt/BNT/general/CVS','/home/aogbechie/Desktop/bnt/BNT/general/Old','/home/aogbechie/Desktop/bnt/BNT/general/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/inference','/home/aogbechie/Desktop/bnt/BNT/inference/CVS','/home/aogbechie/Desktop/bnt/BNT/inference/dynamic','/home/aogbechie/Desktop/bnt/BNT/inference/dynamic/CVS','/home/aogbechie/Desktop/bnt/BNT/inference/online','/home/aogbechie/Desktop/bnt/BNT/inference/online/CVS','/home/aogbechie/Desktop/bnt/BNT/inference/static','/home/aogbechie/Desktop/bnt/BNT/inference/static/CVS','/home/aogbechie/Desktop/bnt/BNT/learning','/home/aogbechie/Desktop/bnt/BNT/learning/CVS','/home/aogbechie/Desktop/bnt/BNT/potentials','/home/aogbechie/Desktop/bnt/BNT/potentials/CVS','/home/aogbechie/Desktop/bnt/BNT/potentials/Old','/home/aogbechie/Desktop/bnt/BNT/potentials/Old/CVS','/home/aogbechie/Desktop/bnt/BNT/potentials/Tables','/home/aogbechie/Desktop/bnt/BNT/potentials/Tables/CVS','/home/aogbechie/Desktop/bnt/CVS','/home/aogbechie/Desktop/bnt/GraphViz','/home/aogbechie/Desktop/bnt/GraphViz/CVS','/home/aogbechie/Desktop/bnt/GraphViz/Old','/home/aogbechie/Desktop/bnt/GraphViz/Old/CVS','/home/aogbechie/Desktop/bnt/HMM','/home/aogbechie/Desktop/bnt/HMM/CVS','/home/aogbechie/Desktop/bnt/KPMstats','/home/aogbechie/Desktop/bnt/KPMstats/CVS','/home/aogbechie/Desktop/bnt/KPMtools','/home/aogbechie/Desktop/bnt/KPMtools/CVS','/home/aogbechie/Desktop/bnt/Kalman','/home/aogbechie/Desktop/bnt/Kalman/CVS','/home/aogbechie/Desktop/bnt/docs','/home/aogbechie/Desktop/bnt/docs/Eqns','/home/aogbechie/Desktop/bnt/docs/Figures','/home/aogbechie/Desktop/bnt/docs/Talks','/home/aogbechie/Desktop/bnt/graph','/home/aogbechie/Desktop/bnt/graph/CVS','/home/aogbechie/Desktop/bnt/graph/Old','/home/aogbechie/Desktop/bnt/graph/Old/CVS','/home/aogbechie/Desktop/bnt/nethelp3.3','/home/aogbechie/Desktop/bnt/nethelp3.3/CVS','/home/aogbechie/Desktop/bnt/netlab3.3','/home/aogbechie/Desktop/bnt/netlab3.3/CVS','/home/aogbechie/Desktop/bnt/netlabKPM','/home/aogbechie/Desktop/bnt/netlabKPM/CVS','/home/aogbechie/Desktop/Datasets/Laser/MATLAB','/home/aogbechie/Desktop/Datasets/Laser/Prueba_26_Ene/4272/')")
  
  num_nodes<-length(names(bn_trans.fit))
  names_nodes <- names(bn_trans.fit)[1:(num_nodes/2)]
  names_nodes_past <-names(bn_trans.fit)[(num_nodes/2+1):num_nodes]
  
  intrac <-arcs[arcs[,1] %in% names(bn_trans.fit)[1:(num_nodes/2)],]
  interc <-arcs[arcs[,1] %in% names(bn_trans.fit)[(num_nodes/2+1):num_nodes],]
  #Leave only node_t instead of node_tpast
  for (i in 1:nrow(interc)){
    interc[i,1] <- names_nodes[match(interc[i,1],names_nodes_past)]
  }
  setVariable(matlab, names=names_nodes)
  evaluate(matlab, "names=names';")
  setVariable(matlab, intrac=intrac)
  evaluate(matlab, "[intra, names] = mk_adj_mat(intrac, names, 1);")
  setVariable(matlab, interc=interc)
  evaluate(matlab, "inter = mk_adj_mat(interc, names, 0);")
  names_bnet<- unlist(getVariable(matlab, "names"))
  
  dnodes <- 1:(num_nodes/2)
  ns <- rep(1,(num_nodes/2))
  for(i in 1:(length(names(bn_trans.fit))/2)){
    ns[i] <- dim(bn_trans.fit[[names_bnet[i]]]$prob)[1]
  }
  eclass1<- 1:(num_nodes/2)
  eclass2<- (num_nodes/2 +1):num_nodes
  
  
  setVariable(matlab, ns=ns)
  setVariable(matlab, dnodes=dnodes)
  setVariable(matlab, onodes=dnodes)
  setVariable(matlab, num_nodes=num_nodes)
  setVariable(matlab, eclass1=eclass1)
  setVariable(matlab, eclass2=eclass2)
  #Necesary to traspose vectors later on
  evaluate(matlab, "ns=ns';, dnodes=dnodes';, eclass1=eclass1';, eclass2=eclass2';, onodes=onodes';")
  evaluate(matlab, "intra,inter,ns,dnodes,onodes,num_nodes,names,eclass1,eclass2")
  
  #Cretate bnet object
  evaluate(matlab, "bnet = mk_dbn(intra, inter, ns, 'discrete', dnodes, 'observed', onodes, 'eclass1', eclass1, 'eclass2', eclass2, 'names',names);")
  #Set CPDs as tabular
  evaluate(matlab, "for i=1:num_nodes,bnet.CPD{i} = tabular_CPD(bnet, i);,end")
  evaluate(matlab, "bnet")
  #CPDs parents allways starting at 1 in this case
  #evaluate(matlab, "CPDs = cell(1,num_nodes);")
  for (i in 1:(num_nodes/2)){ #slice t0 --Todo igual
    dimension_node <- dim(bn_trans.fit[[names_bnet[i]]]$prob)[1]
    dimension_parents <- dim(bn_trans.fit[[names_bnet[i]]]$prob)[-1]
    parents_node <- bn_trans.fit[[names_bnet[i]]]$parents
    dimension_parents <- dimension_parents[parents_node %in% names(bn_trans.fit)[1:(num_nodes/2)]]
    if(length(dimension_parents)>0){
      CPD_parents <- 1
      for (j in 1:(length(dimension_parents))){
        CPD_parents <- rep(CPD_parents, dimension[j])
      }
      CPD_parents <- c(CPD_parents, rep(rep(0,length(CPD_parents)),dimension_node-1))
    } else {
      CPD_parents <- c(1,rep(0,dimension_node-1))
    }
    setVariable(matlab, prior0=CPD_parents)
    evaluate(matlab, "prior0=prior0';")
    setVariable(matlab, i=i)
    #evaluate(matlab, "CPDs{i} = prior0;")
    evaluate(matlab, "bnet.CPD{i} = tabular_CPD(bnet, i, prior0);")
  }
  order_ancestral <- c(paste(names_bnet,"past",sep = ""), paste(names_bnet,"",sep = ""))
  for (i in 1:(num_nodes/2)){ #slice t1
    order_bnt <-c()
    parents_node <- bn_trans.fit[[names_bnet[i]]]$parents
    order_bnt <- c()
    for(j in 1:length(order_ancestral)){
      if(order_ancestral[j] %in% parents_node){
        order_bnt <- c(order_bnt,which(parents_node==order_ancestral[j]))
      }
    }
    order_bnt <-c(order_bnt+1,1) #In BNT the last index if for de node
    CPD_t <-aperm(bn_trans.fit[[names_bnet[i]]]$prob, order_bnt)
    CPD_t <- as.vector(CPD_t)
    setVariable(matlab, CPD_t=CPD_t)
    evaluate(matlab, "CPD_t=CPD_t';")
    setVariable(matlab, i=i+num_nodes/2)
    #evaluate(matlab, "CPDs{i} = CPD_t;")
    evaluate(matlab, "bnet.CPD{i} = tabular_CPD(bnet, i, CPD_t);")
  }
  ######################
  #Hasta aqui para guardar todas las vars que necesita la red. basta con ejecutar en matlab
  #######################
  
  
  #Create sample of length T
  samp <-c(21500, 10000)
  T <- samp[1] #pass through argument to the function
  num_samples<- samp[2]
  setVariable(matlab, T=T)
  ll_samples<-c()
  for(i in 1:num_samples){
    print(i)
    system.time(evaluate(matlab, "ll = logLik_transition(bnet,sample_dbn(bnet, T));"))
    ll<-unlist(getVariable(matlab,"ll"))
    ll_samples<-c(ll_samples,ll)
  }
  names(ll_samples)<-c()
  close(matlab)
  #prueba loglikelihood
  #eval(matlab, )
  #evaluate(matlab, "onodes=[931,933,934,953,967,981,983,985,987];") #Necesito orden ancestral
  #nodes_test <- as.integer(gsub("\\D+","",order_ancestral[1:(num_nodes/2)]))
  #setVariable(matlab, onodes=nodes_test)
  #evaluate(matlab, "onodes=onodes'")
  #evaluate(matlab, "edges=0:102.4:1024")
  #evaluate(matlab, "files_sequence={'4272_p1.csv';'4272_p2.csv';'4272_p3.csv';'4272_p4.csv'};")
  #evaluate(matlab, "T=21500;")
  #evaluate(matlab, "startRow=1;")
  #evaluate(matlab, "onodes,names,edges,files_sequence,T,startRow")
  #evaluate(matlab, "cases=obtener_cases(onodes,files_sequence,T,startRow,edges);")
  #evaluate(matlab, "logLik_transition(bnet,cases{1})")
  return(ll_samples)
  
  close(matlab)
}



