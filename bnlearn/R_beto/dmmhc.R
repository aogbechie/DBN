setwd("~/sources/bnlearn")
library (bnlearn)
library (Rgraphviz)
library (devtools)
library(gRain)
library (parallel)
library(R.matlab)
library(Hmisc)

###################################
    # 0. Data Preprocessing
####################################


  #define the paths
root="/home/aogbechie/Desktop/Datasets/Laser/dataset/"
#filepaths <- c("4272/4272_p1.csv","4272/4272_p2.csv","4272/4272_p3.csv","4272/4272_p4.csv","4276/4276_p1.csv","4276/4276_p2.csv","4276/4276_p3.csv","4276/4276_p4.csv","4275/4275_p1.csv","4275/4275_p2.csv","4275/4275_p3.csv","4275/4275_p4.csv","4274/4274_p1.csv","4274/4274_p2.csv","4274/4274_p3.csv","4274/4274_p4.csv","4273/4273_p1.csv","4273/4273_p2.csv","4273/4273_p3.csv","4273/4273_p4.csv","4271/4271_p1.csv","4271/4271_p2.csv","4271/4271_p3.csv","4271/4271_p4.csv","4270/4270_p1.csv","4270/4270_p2.csv","4270/4270_p3.csv","4270/4270_p4.csv")
#filepaths <- c("4272/4272_p1.csv","4272/4272_p2.csv","4272/4272_p3.csv","4272/4272_p4.csv","4276/4276_p1.csv","4276/4276_p2.csv","4276/4276_p3.csv","4276/4276_p4.csv","4275/4275_p1.csv","4275/4275_p2.csv","4275/4275_p3.csv","4275/4275_p4.csv")
filepaths <- c("4272/4272_p1.csv","4272/4272_p2.csv","4272/4272_p3.csv","4272/4272_p4.csv")
#define the pixels to useP
pixels <- sort(c(931,933,934,953,967,981,983,985,987))
#pixels <- sort(c(321,385, 417, 418,449,450,451, 454, 481, 485, 513, 514, 545, 546, 547, 548, 579, 580, 610, 611, 612, 613, 614, 615, 642, 643, 644, 645, 646, 647)) #30 pixels area izq 321-647
#pixels <- sort(c(174,321,336,366,385,400,417,418,427,449,450,451,454,456,462,481,483,484,485,495,506,513,514,525,545,546,547,548,560,579,580,592,610,611,612,613,614,615,623,625,626,628,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,661,663,664,665,666,684,692,693,695,696,697,698,717,720,721,778,787,788,790,791,792,815,819,820,821,822,823,825,846,851,852,853,856,880,886,887,901,902,903,904,935,936,941))
breaks <- 10 #if continuous set NULL
#breaks <- NULL #if continuous set NULL


filepaths <- paste(root,filepaths,sep="")
n_seqs <- length(filepaths)
n_nodes <- length(pixels)

  #Read the files
print("Inicio de lectura de ficheros")
system.time(dataset<-readTimeSerie(filepaths,pixels))

T_length <- length(dataset[[1]][,1])
  
  #Discretize the datasets (o no)
if(! is.null(breaks)){
  print("Inicio de discretizacion de ficheros")
  #levels_factor <- as.character(1:breaks)
  levels_factor <- 1:breaks
  system.time(dataset<- equalLengthDiscr(dataset, breaks, levels_factor))
}
  #Separate the datasets in CPC_0, CPC_t, PC_t and CC_T
print("Inicio de separación de ficheros")
system.time({dataset_CPC_0 <- separateDatasetDBN(dataset,"CPC_0", breaks);
dataset_CPC_t <- separateDatasetDBN(dataset,"CPC_t", breaks);
dataset_PC_t <- separateDatasetDBN(dataset,"PC_t", breaks);
dataset_CC_t <- separateDatasetDBN(dataset,"CC_t", breaks)})

###################################
    # 1. DBN Structure Learning
####################################  
  #Perform DMMHC optimized in the dataset with dbn[[1]]= Bn0 and dbn[[2]]= dbn (transition model)

print("Inicio de aprendizaje de estructura")
if(is.null(breaks)){
  system.time(dbn_complete <- dmmhc(list(dataset_CPC_0, dataset_CPC_t, dataset_PC_t, dataset_CC_t), test = "mi-g", score = "bic-g"))
} else {
  system.time(dbn_complete <- dmmhc(list(dataset_CPC_0, dataset_CPC_t, dataset_PC_t, dataset_CC_t), test = "mi", score = "bic"))
}

bn0 <- dbn_complete[[1]]
bn_trans <- dbn_complete[[2]]
graphviz.plot(bn_trans)

#2-TBN plot
plot_parameters <- two_tbn.plot(bn_trans, n_nodes)
plot(plot_parameters[[1]], attrs = plot_parameters[[2]], nodeAttrs=plot_parameters[[3]], subGList = plot_parameters[[4]])


###################################
    # 2. DBN Parameters Learning
####################################  

bn0.fit <- bn.fit(bn0,dataset_CPC_0, method = "mle")
bn_trans.fit <- bn.fit(bn_trans, dataset_PC_t, method = "bayes") #No problem with missing states in instances
#Convert to gRain
bn_trans.fit_gRain <- as.grain(bn_trans.fit)
write.net("dbn.net", bn_trans.fit) #writes the hugin file

###################################
    # 3. Sampling generation
####################################  

  #Obtain the loglikelihoods of the used sequencies.
logliks_samples <- c()
dataset_sample_PC_t <- list()
for(i in 1:length(dataset)){
  sample_loglik <- 0
  dataset_sample <- dataset[[i]]
  dataset_sample_PC_t[[i]] <- separateDatasetDBN(list(dataset_sample),"PC_t", breaks)
  sample_loglik <- logLik(bn_trans.fit, dataset_sample_PC_t[[i]], names(dataset_CPC_t))
  print(sample_loglik)
  logliks_samples <- c(logliks_samples, sample_loglik)
}
  #Obtain the likelihood of the samples

#Generate Samples
#options(mc.cores = 1) #Select the number of cores to use if parallel generation
num_samples <- T_length 
num_seqs_sim <- 1
print("Inicio de sampling")
#system.time(samples <- sample_dbn(bn_trans.fit_gRain,num_seqs_sim,num_samples))
samp<- c(num_samples,num_seqs_sim)
dbnet <- as.bnet(bn_trans.fit, bn_trans$arcs)

#Compute Loglikelihood
for(i in 1:length(samples)){
  dataset_sample<- samples[[i]]
  dataset_sample_PC_t <- int2factor_breaks(dataset_sample, breaks)
  complete_score <- score(bn_trans,dataset_sample_PC_t, type = "loglik")
  sample_loglik <- loglik.dbn(dataset_sample_PC_t[,-(1:n_nodes)], complete_score, score = "loglik")
  logliks_samples <- c(logliks_samples, sample_loglik)
}
###################################
    # 4. Experiment for each sequence
####################################  

#Alpha test
alpha <- 5 #in percentege
index <- round(length(logliks_samples)*alpha/100)
if(index < 1){
  index <-1
}
threshold <- sort(logliks_samples)[index]


#Sequence preparation
exp_filepath <- c("4274/4274_p2.csv")
exp_filepath <- paste(root,exp_filepath,sep="")
exp_dataset <- readTimeSerie(exp_filepath,pixels)
exp_dataset <- equalLengthDiscr(exp_dataset,breaks, levels_factor)
exp_dataset_PC_t <- separateDatasetDBN(exp_dataset,"PC_t", breaks)
#Loglikelihood calculation of the unseen sequence
system.time(
  complete_score <- score(bn_trans,exp_dataset_PC_t, type = "loglik"),
  exp_loglik <- loglik.dbn(exp_dataset_PC_t[,-(1:n_nodes)], complete_score, score = "loglik"))
# Check if sequence is normal or not
if(exp_loglik < threshold){
  print("The sequence contains anomallies")
} else {
  print("The sequence is normal")
}



