#ML ESTIMATOR FOR A BAYESIAN NETWORK OF DISCRETE VALUES################################################################
require(ggm)

test=TRUE

if(!test) {
args <- commandArgs(T)
bayes_network_model=as.matrix(read.delim(args[1],header=T))

Training_data<-as.data.frame(read.delim(args[2],header=T))

file_name=(args[3])
if(length(args)>3){
  scarcity_cutoff<-as.numeric(args[4])
  missing_identifier=args[5]
} else{
  scarcity_cutoff=30
  missing_identifier="*"
}
rownames(bayes_network_model)=colnames(bayes_network_model)
dummy_dag=bayes_network_model
rownames(dummy_dag)=colnames(dummy_dag)
dummy_dag[dummy_dag>0]=0
dummy_dag[dummy_dag<0]=1


}









Bayes_ml_estimator<-
  function(data,bayes_network_model,scarcity_cutoff=30){
    if(!is.data.frame(data)){
      cat('Ay-Caramba!!~!')
    }
    if(ncol(data)!=ncol(bayes_network_model)){
      cat("Something is horribly wrong, the number of variables of data and BNM don't agree")
    }

# topsort matrix
    rownames(bayes_network_model)=colnames(bayes_network_model)
    adj_matrix=bayes_network_model
    adj_matrix[adj_matrix>0]=0
    adj_matrix[adj_matrix<0]=1

    bayes_network_model=bayes_network_model[topOrder(adj_matrix),topOrder(adj_matrix)]
    table=list()
    for(i in colnames(bayes_network_model)){
      conditional_indices=which(bayes_network_model[,i]<0)
      cNames=sort(-1*bayes_network_model[conditional_indices,i])
      cNames=names(cNames)
      table[[i]]=WRAP(data,i,cNames,scarcity_cutoff)
}
 return(table)
}

# scarcity calculators: RETURN true if cutoff is reached

# cutoff when values are less than
cutoff_by_matches = function(n) { 
  function(matches) {
    length(matches) < n
  }
}

# cutoff 
cutoff_ignore_missing = function(n, missing_symbol="*") {
  function(matches) {
    sum(matches != missing_symbol) < n
  }

}


cartesian_condition = function( df      # data frame
                              , on_col  # column to compute
                              , c_names # parent columns from most to least important
                              , scarcity_cutoff # scarcity cutoff. Either a number i
                                                # or derived from a function above.
                              ) {

  # set the scarcity cutoff if scarcity_cutoff is numeric
  if(is.numeric(scarcity_cutoff)) {
    n = scarcity_cutoff
    scarcity_cutoff = cutoff_by_matches(n)
  }

  on = df[,on_col]
  on_values = levels(as.factor(on))

  df = as.data.frame(df[,c_names])
  colnames(df) <- c_names

  values = list()
  for(colname in c_names) {
    values[[colname]] <- levels(as.factor(df[,colname]))
  }

  lookup_table = expand.grid(values)

  print(lookup_table)

  # compute the prob table
  find_prob = function(row) {
    last_matches = 1:length(on) 
    is_scarce = FALSE
    # subset column-by-column until data scarcity is reached
    for(colname in c_names) {
      matches = which(df[,colname] == row[colname])

      x = intersect(matches, last_matches)
      # check for scarcity
      if(length(x) == 0 || scarcity_cutoff(on[x])) {
        is_scarce = TRUE
        break
      } else {
        last_matches = x
      }
    }
    list( p_table =
           sapply(on_values, function (value)
           { sum(on[last_matches] == value) }
        )
        , scarcity = is_scarce
        )
  }
  
  if(length(c_names) == 0) {
    out=find_prob(NA) # ROW only used in inner loop
    p_table  = t(out$p_table)
    scarcity = out$scarcity 
  } else {
    out=apply(lookup_table, 1, find_prob)
    p_table  = t(sapply(out, function(x) x$p_table))
    scarcity = sapply(out, function(x) x$scarcity)
  }
  colnames(p_table) <- on_values

  list(field = on_col, conditions = lookup_table, prob_table=p_table, scarcity = scarcity)
}


WRAP = cartesian_condition

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
        #doing the column means of the probabilities and creating a column with them
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
  #Just converting some garbage from factors to columns
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
  print("what?")
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

# if(!test) {
# final_prob_table=function(prob_list,data,scarcity_cutoff=30,missing_identifier="*",filename="Conditional_prob_table.RData"){
#  for(i in 1:length(prob_list)){
#    prob_list[i]=Missing_data_selecter(data,prob_list[i],scarcity_cutoff,missing_identifier)
#  }
#  save(prob_list,file=filename)
#}
