
# Grow-Shrink frontend.
gs = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B, debug = debug,
    optimized = optimized, strict = strict, undirected = undirected)

}#GS

# Incremental Association frontend.
iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B, method = "iamb",
    debug = debug, optimized = optimized, strict = strict,
    undirected = undirected)

}#IAMB

# Fast-IAMB frontend.
fast.iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "fast.iamb", debug = debug, optimized = optimized,
    strict = strict, undirected = undirected)

}#FAST.IAMB

# Inter-IAMB frontend.
inter.iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "inter.iamb", debug = debug, optimized = optimized,
    strict = strict, undirected =  undirected)

}#INTER.IAMB

# MMPC frontend.
#--------------------
# AO modification: 
# (1) New boolean argument called "simetry". If simetry = FALSE (defult=TRUE) then 
#     no simetrical correction is done by the mmpc algorithm, just the forward and 
#     backward phases. 
# (2) New list argument that delivers the undirected CPC_t graph learned until now from previous steps
#--------------------
#mmpc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
#    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
#    strict = FALSE) {
mmpc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
       test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
       strict = FALSE, simetry = TRUE, cpc_t = NULL) {  
  

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "mmpc", debug = debug, optimized = optimized,
    strict = strict, undirected = TRUE, simetry = simetry, cpc_t = cpc_t)

}#MMPC

# Semi-Interleaved HITON-PC.
si.hiton.pc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "si.hiton.pc", debug = debug, optimized = optimized,
    strict = strict, undirected = TRUE)

}#HITON-PC

# ARACNE frontend.
aracne = function(x, whitelist = NULL, blacklist = NULL, mi = NULL,
    debug = FALSE) {

  mi.matrix(x = x, whitelist = whitelist, blacklist = blacklist,
    method = "aracne", mi = mi, debug = debug)

}#ARACNE

# Chow-Liu frontend.
chow.liu  = function(x, whitelist = NULL, blacklist = NULL, mi = NULL,
    debug = FALSE) {

  mi.matrix(x = x, whitelist = whitelist, blacklist = blacklist,
    method = "chow.liu", mi = mi, debug = debug)

}#CHOW.LIU

# Hill Climbing greedy search frontend.
hc = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, ..., debug = FALSE, restart = 0, perturb = 1,
    max.iter = Inf, maxp = Inf, optimized = TRUE) {

  greedy.search(x = x, start = start, whitelist = whitelist,
    blacklist = blacklist, score = score, heuristic = "hc", debug = debug,
    expand = c(list(...), restart = restart, perturb = perturb,
    max.iter = max.iter, maxp = maxp), optimized = optimized)

}#HC

# TABU list greedy search frontend.
tabu = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, ..., debug = FALSE, tabu = 10, max.tabu = tabu,
    max.iter = Inf, maxp = Inf, optimized = TRUE) {

  greedy.search(x = x, start = start, whitelist = whitelist,
    blacklist = blacklist, score = score, heuristic = "tabu", debug = debug,
    expand = c(list(...), max.iter = max.iter, tabu = tabu, max.tabu = max.tabu,
    maxp = maxp), optimized = optimized)

}#TABU

# Generic Restricted Maximization frontend.
rsmax2 = function(x, whitelist = NULL, blacklist = NULL, restrict = "gs",
    maximize = "hc", test = NULL, score = NULL, alpha = 0.05, B = NULL,
    ..., maximize.args = list(), optimized = TRUE, strict = FALSE,
    debug = FALSE) {

  restrict.args = list(test = test, alpha = alpha, B = B, strict = strict)
  maximize.args = c(list(...), maximize.args)

  hybrid.search(x, whitelist = whitelist, blacklist = blacklist,
    restrict = restrict, maximize = maximize, score = score,
    restrict.args = restrict.args, maximize.args = maximize.args,
    optimized = optimized, debug = debug)

}#RSHC

# MMHC frontend.
mmhc = function(x, whitelist = NULL, blacklist = NULL, test = NULL,
    score = NULL, alpha = 0.05, B = NULL, ..., restart = 0, perturb = 1,
    max.iter = Inf, optimized = TRUE, strict = FALSE, debug = FALSE) {

  restrict.args = list(test = test, alpha = alpha, B = B, strict = strict)
  maximize.args = c(list(...), restart = restart,
                   perturb = perturb, max.iter = max.iter)

  hybrid.search(x, whitelist = whitelist, blacklist = blacklist,
    restrict = "mmpc", maximize = "hc", restrict.args = restrict.args,
    maximize.args = maximize.args, score = score, optimized = optimized,
    debug = debug)

}#MMHC

# Frontend for the Markov blanket learning algorithms.
learn.mb = function(x, node, method, whitelist = NULL, blacklist = NULL,
    start = NULL, test = NULL, alpha = 0.05, B = NULL, debug = FALSE) {

  mb.backend(x, target = node, method = method, whitelist = whitelist,
    blacklist = blacklist, start = start, test = test, alpha = alpha,
    B = B, debug = debug)

}#LEARN.MB

# Frontend for causal discovery learning algorithms.
learn.nbr = function(x, node, method, whitelist = NULL, blacklist = NULL,
    start = NULL, test = NULL, alpha = 0.05, B = NULL, debug = FALSE) {

  nbr.backend(x, target = node, method = method, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B, debug = debug)

}#LEARN.NBR

# naive Bayes frontend.
naive.bayes = function(x, training, explanatory) {

  bayesian.classifier(x, training = training, explanatory = explanatory,
    method = "naive.bayes", whitelist = NULL, blacklist = NULL, expand = list(),
    debug = FALSE)

}#NAIVE.BAYES

# tree-augmented naive Bayes frontend.
tree.bayes = function(x, training, explanatory, whitelist = NULL, blacklist = NULL,
    mi = NULL, root = NULL, debug = FALSE) {

  bayesian.classifier(x, training = training, explanatory = explanatory,
    method = "tree.bayes", whitelist = whitelist, blacklist = blacklist,
    expand = list(estimator = mi, root = root), debug = debug)
}#TAN

# DMMPC (o simmetry correction) frontend.
#--------------------
# AO creation: 
#--------------------
#mmpc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
#    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
#    strict = FALSE) {
dmmhc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
                test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
                strict = FALSE, simetry = FALSE, score = NULL) {  

        #Obtain the different datasets for th DBN structure learning
        dataset_CPC_0 <- x[[1]]
        dataset_CPC_t <- x[[2]]
        dataset_PC_t <- x[[3]]
        dataset_CC_t <- x[[4]]
        
        nodes_t=names(dataset_CPC_t)
        
        
        ###################################
        # 1. t=0 Model BN_0
        ####################################
        
        #Calculate t=0 Bayesian network that is simply applaying the static MHHC proposed in [Tsamardinos et.al, 2006]
        bn0 <- mmhc(dataset_CPC_0)
        #nbr_CPC_0 <- mb(bn0)
        
        
        #################################################
        # 2. DMMPC_optimised without simmetry check
        #################################################
      
        #----------------------------
        # 2.1 Search CPC_t: Just do classical mmpc to CPC_t
        #----------------------------
        nbr_CPC_t <- dyn_nbr(dataset_CPC_t, simetry = FALSE, test = test, alpha = 0.05) #without simetry because it is not a BN, just the mb/nbr
        blacklist <-blacklist_notCPC(nbr_CPC_t, nodes_t)
        #----------------------------
        # 2.2 Search CP_t-1: initiate cpc of PC_t whith the result form mmpc(CPC_t). Then, look only for
        #                    parents in list(PC_t). Backtracking can remove both list(CPC_t) and 
        #                    list(PC_t)
        #----------------------------
        nbr_PC_t <- dyn_nbr(dataset_PC_t, cpc_t = nbr_CPC_t, simetry = FALSE, blacklist = blacklist, test = test, alpha = 0.05)
        
        #----------------------------
        # 2.3 Search CP_t-1: whitelist cpc of CC_t whith the result form (PC_t but just in X[t]). Then,
        # look only for children in list(CC_t). Backtracking can only remove list(CC_t). 
        #----------------------------
        whitelist <-whitelist_CPC(nbr_PC_t, nodes_t)
        blacklist <-blacklist_notCPC(nbr_CPC_t, nodes_t)
        nbr_PC_t_xt <-in.t(nbr_PC_t, nodes_t)
        nbr_CC_t <- dyn_nbr(dataset_CC_t, cpc_t = nbr_PC_t_xt, whitelist = whitelist, simetry = FALSE, 
                            blacklist = blacklist, test = test, alpha = 0.05)
  
        
        #################################################
        # 3. Simetrical Correction for DBNs
        #################################################
        
        #----------------------------
        # 3.1 Simetrical correction for CPC_t as in static case
        #----------------------------
        CPC_t <- sim.correction.static(nbr_PC_t_xt,nodes_t)
        PC_t <- sim.correction.dynamic(list(nbr_PC_t,nbr_CC_t),nodes_t)
        PC_t <-sapply(PC_t,function(x){if(length(x)>0) paste(x,"past",sep = "")}) #Add "past" to the PC of each node
        #################################################
        # 4. Greedy Search (hill climbing or tabu)
        #################################################
        blacklist_Search<-blacklistDMMPC(list(CPC_t,PC_t),names(dataset_PC_t))
        #data_dyn <-data.frame(sapply(dataset_PC_t,as.factor)) #It is a factor already
        
        dbn<-hc(dataset_PC_t, blacklist = blacklist_Search, score = score, restart = 15, perturb = 15)
        #dbn<-tabu(dataset_PC_t, blacklist = blacklist_Search, score = "bic", tabu = 100)
        #graphviz.plot(dbn)
        #print(score(dbn, dataset_PC_t, type="bic"))
        
        #dbn2<-tabu(dataset_PC_t, blacklist = blacklist_Search, score = "bic", tabu = 20)
        #graphviz.plot(dbn2)
        #score(dbn2, dataset_PC_t, type="bic")
        
#Hasta ahora discreto, pero ahora hay que recalcular        
        score_complete<-score(dbn, dataset_PC_t, type=score)
        score_dbn<-bic_dbn(dataset_PC_t[,-(1:length(nodes_t))], score_complete, score = score)
        print("Score")
        print(score_dbn)
        #browser()
        #2-TBN plot
        #nodes1 <-names(dataset_PC_t)[! names(dataset_PC_t) %in% nodes_t]
        #nodes2 <-nodes_t  
        #ft <- dbn$arcs
        #ft <- ft[!duplicated(apply(ft, 1, paste, collapse="")),]
        #back1<-sapply(nodes1,function(x){ x %in% ft})
        #nodes1 <- subset(nodes1,back1)
        #back2<-sapply(nodes2,function(x){ x %in% ft})
        #nodes2 <- subset(nodes2,back2)
        #g <- ftM2graphNEL(ft, edgemode="directed")
        #twocolors <- c("transparent", "#E0F3F8")
        #nodeType <- 1 + (nodes(g) %in% nodes1)
        #nA = makeNodeAttrs(g, fillcolor=twocolors[nodeType])
        #sg1 = subGraph(nodes1, g)
        #sgL = list(list(graph=sg1, cluster = FALSE, attrs = c(rank="sink")))
        #att = list(graph = list(rankdir = "RL", rank = ""))
        #plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)
        
        return(list(bn0, dbn, score_dbn))
}#DMMPC