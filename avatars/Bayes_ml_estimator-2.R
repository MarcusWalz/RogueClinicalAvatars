#ML ESTIMATOR FOR A BAYESIAN NETWORK OF DISCRETE VALUES################################################################
require(ggm)
args <- commandArgs(T)
bayes_network_model=read.delim(args[1],header=T)
Training_data<-read.delim(args[2],header=T)
file_name=(args[3])
if(length(args)>3){
  scarcity_cutoff<-as.numeric(args[4])
  missing_identifier=args[5]
}
else{
  scarcity_cutoff=30
  missing_identifier="*"
}
rownames(bayes_network_model)=colnames(bayes_network_model)
dummy_dag=bayes_network_model
rownames(dummy_dag)=colnames(dummy_dag)
dummy_dag[dummy_dag>0]=0
dummy_dag[dummy_dag<0]=1
bayes_network_model=bayes_network_model[topOrder(dummy_dag),topOrder(dummy_dag)]
Table_1=Bayes_ml_estimator(Training_data,bayes_network_model,scarcity_cutoff)
Final_table=final_prob_table(Table_1,Training_data,scarcity_cutoff,missing_identifier,file_name)












Bayes_ml_estimator<-function(data,bayes_network_model,scarcity_cutoff=30){
 if(!is.data.frame(data)){
   cat('Ay-Caramba!!~!')
 }
  if(ncol(data)!=ncol(bayes_network_model)){
    cat("Something is horribly wrong, the number of variables of data and BNM don't agree")
   
 }
 ###top. sort begin###
 rownames(bayes_network_model)=colnames(bayes_network_model)
 hotdog2=bayes_network_model
 hotdog2[hotdog2>0]=0
 hotdog2[hotdog2<0]=1
 bayes_network_model=bayes_network_model[topOrder(hotdog2),topOrder(hotdog2)]
 #topological sort of BNM done
 table=list()
  for(i in colnames(bayes_network_model)){
    conditional_indices=which(bayes_network_model[,i]<0)
    cNames=sort(-1*bayes_network_model[conditional_indices,i])
    cNames=names(cNames)
    table[[i]]=WRAP(data,i,cNames,scarcity_cutoff)
  }
 return(table)
}




recursive_conditioning<-function(vector,conditional_columns,shit_so_far,conditions,possible_values,condition_values=c(),scarcity_cut_off,Data_scarcity){
  if(is.na(conditional_columns)|is.null(condition_values)){#Tail condition
    prob=c()
    for(j in possible_values){#calculate probabilities
      prob=c(prob,length(which(vector==j)))
    }
    shit_so_far=rbind(shit_so_far,c(conditions,prob,length(vector),Data_scarcity))
    return(shit_so_far)#bind them then return them
  }else{
    if(is.data.frame(conditional_columns)){#if there are multiple conditional variables left
      for(k in unlist(condition_values[1])){
        conditioned_vector=vector[conditional_columns[,1]==k]
        if(length(conditioned_vector)<=scarcity_cut_off){#switches Data scarcity value to T if there
          Data_scarcity=TRUE
        }
        if(Data_scarcity){#If there is data scarcity, keep passing less conditions, but stop conditioning on them
          remaining_conditions=conditional_columns[,2:ncol(conditional_columns)]
          remaining_values=condition_values[2:length(condition_values)]
          shit_so_far=recursive_conditioning(vector,remaining_conditions,shit_so_far,c(conditions,k),possible_values,remaining_values,scarcity_cut_off,Data_scarcity=TRUE)
        }
        else{
        remaining_conditions=conditional_columns[conditional_columns[,1]==k , 2:ncol(conditional_columns)]
        remaining_values=condition_values[2:length(condition_values)]
        shit_so_far = recursive_conditioning(conditioned_vector,remaining_conditions,shit_so_far,c(conditions,k),possible_values,remaining_values,scarcity_cut_off,Data_scarcity=FALSE)
        }
      }
    }else{
      for(k in unlist(condition_values[1])){
        conditioned_vector=vector[conditional_columns==k]
        if(length(conditioned_vector)<=scarcity_cut_off|Data_scarcity){
          shit_so_far = recursive_conditioning(vector,NA,shit_so_far,c(conditions,k),possible_values,scarcity_cut_off=scarcity_cut_off,Data_scarcity=TRUE)
         }else{
        shit_so_far = recursive_conditioning(conditioned_vector,NA,shit_so_far,c(conditions,k),possible_values,scarcity_cut_off=scarcity_cut_off,Data_scarcity=FALSE)  
        }
      }
    
    }
  }
  return(shit_so_far)
  #
}
WRAP<-function(data,index_of_interest,cNames,scarcity_cut_off){
  vector=data[,index_of_interest]
  possible_values=levels(as.factor(vector))
  if(!is.null(cNames)){
    conditions=data[,cNames]
    condition_values=list()
    if(is.factor(conditions)){
      condition_values[[1]]=levels(conditions)
      number_of_conditions=1
    }else{
      for(i in 1:ncol(conditions)){
        condition_values[[i]]=levels(as.factor(conditions[,i]))
      }
      number_of_conditions=ncol(conditions)
    }
  } else{
    conditions=NA
    condition_values=c()
    number_of_conditions=0
}##END IF CONDITIONS EXIST IF/ELSE

  shit_so_far=recursive_conditioning(vector,conditions,c(),c(),possible_values,condition_values,scarcity_cut_off,Data_scarcity=F)
  if(number_of_conditions!=0){
    condition_set=as.data.frame(shit_so_far[,1:number_of_conditions],stringsAsFactors=F)
    colnames(condition_set)=cNames
  } else{
    condition_set=data.frame()
  }
                                 
  probs=as.data.frame((shit_so_far[,(number_of_conditions+1):(ncol(shit_so_far)-2)]))
  if(number_of_conditions==0){
    probs=t(probs)
    
  }
  colnames(probs)=possible_values
  return(list("Conditions"=condition_set
              ,"Probabilities"=as.data.frame(probs,stringsAsFactors=F)
              ,"Sample Size"=shit_so_far[,ncol(shit_so_far)-1]
              ,"Data Scarcity"=shit_so_far[,ncol(shit_so_far)]
              ))
}



Missing_data_estimator=function(data,project_site,missing_prob_table){
  conditions=(missing_prob_table$Conditions)
  prob1=missing_prob_table$Probabilities#seperating probabilities and conditions
  for(m in 1:ncol(prob1)){
    prob1[,m]=as.numeric(as.character(prob1[,m]))
  }
  relatives=Project_site_map(project_site,data,.1)#calls function to get names of racially close project sites
  subset_c=c()
  subset_p=c()
  for(r in relatives){
    subset_c=rbind(subset_c,conditions[which(conditions$PRO_SITE==r),])
    subset_p=rbind(subset_p,prob1[which(conditions$PRO_SITE==r),])
    
  }
  number_of_conditions=length(which(conditions$PRO_SITE==relatives[1]))
  pro_index=which(colnames(subset_c)=="PRO_SITE")#getting column index of Project Site
  ordered_subset=as.matrix(subset_c[1:number_of_conditions,-pro_index])
  
  project_site_name=levels(as.factor(data[,"PRO_SITE"]))[project_site]#Getting Project site name
  missing_data=which(conditions[,"PRO_SITE"]==project_site_name)#gnabbing indices of data needing imputation
  probs_total=c()
  if(nrow(ordered_subset)!=0){
    for(k in 1:nrow(ordered_subset)){#looping through the csets, starts at 2 because of some dumb data type stuff I had to do before
      probs=c()
      indices=which(sapply(subset_c[,-pro_index]==ordered_subset[k,],sum)==ncol(ordered_subset))
      #finding the probabilities of subset c conditioned on cset.
      for(l in 1:ncol(subset_p)){
        probs=c(probs,mean(as.numeric(as.character(unlist(subset_p[indices,l])))))
        #doing the column means of the probabilities and creating a vector with them
      }
      probs_total=rbind(probs_total,probs)#jam em together for posterity
      index=which(sapply(ordered_subset[k]==conditions[missing_data,-pro_index],sum)==ncol(ordered_subset))
      #get the stuff with matching csets. Might not be necessary, depends on data scarcity thing. 
      for(i in 1:length(probs)){
        prob1[missing_data,][index,i]=floor(probs[i])#replace probs with average
      }
    }
  }
  
  #table=cbind(ordered_subset,t(probs))
  missing_prob_table$Probabilities=prob1
  return(missing_prob_table)  
}



Project_site_map=function(project_site,data,threshold){
  probabilities=(WRAP(data,"RACE","PRO_SITE",0)$Probabilities)
  #get the racial population break down for each Project Site
  probabilities2=c()
  for(i in 1:ncol(probabilities)){
    probabilities2=cbind(probabilities2,as.numeric(levels(probabilities[,i])[probabilities[,i]]))
  }
  totals=rowSums(probabilities2)
  probabilities2=probabilities2/totals
  #Just converting some garbage from factors to vectors
  thing=probabilities2[project_site,]#select project site pop. break down
  probabilities2=as.matrix(probabilities2)[-project_site,]#select everything else
  differences=c()
  for(j in 1:nrow(probabilities2)){
    differences=c(differences,sum((thing-probabilities2[j,])^2))#compare pop. break down for each 
    #other proj site.
  }
  names=levels(as.factor(data[,"PRO_SITE"]))#get names of different project site
  names=as.matrix(names)[-project_site]#exclude name of project site in question
  return(names[which(differences<=threshold)])#return vector of names of project sites whose 
  #square difference in racial proportions is less than a threshold. 
}

Missing_data_selecter=function(data,prob_table,scarcity_cutoff=15,missing_identifier="*",file_name){
  probs=prob_table$Probabilities
  conditions=prob_table$Conditions
  project_sites=levels(as.factor(data$PRO_SITE))
  if(is.data.frame(probs)){
    for(i in 1:ncol(probs)){
      probs[,i]=as.numeric(as.character(probs[,i]))
    }
    combined=c()
    for(l in project_sites){
      combined=rbind(combined,probs[which(conditions$PRO_SITE==l),])
    }
    not_missing=which(colnames(probs)!=missing_identifier)
    
    for(k in 1:length(project_sites))
      Project_sites_with_insufficient_non_missing=which(rowSums(combined[,not_missing])<scarcity_cutoff)
    for(j in Project_sites_with_insufficient_non_missing){
      pro_site= which(project_sites==conditions[j,"PRO_SITE"])
      if(project_sites[pro_site]!="K"){
        prob_table=Missing_data_estimator(data,pro_site,prob_table)
      }
    }
  }
  return(prob_table)
}


final_prob_table=function(prob_list,data,scarcity_cutoff=30,missing_identifier="*",filename="Conditional_prob_table.RData"){
  for(i in 1:length(prob_list)){
    prob_list[i]=Missing_data_selecter(data,prob_list[i],scarcity_cutoff,missing_identifier)
  }
  save(prob_list,file=filename)
}