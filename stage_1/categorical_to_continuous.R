######################################  NOTES FOR DEVELOPING   ###################################################################
#
#Converting our categorical data back to continuous data.
#The general idea is to look at each category and determine an approximate distribution for it.
#Then we will randomly sample from that distribution to re-assign values to the categorical data.
#I'll write a function which takes as arguments, the original data, cut points, and data to be converted
#Things I have to decide::: Should I use some function to approximate a distribution, or just
#use a trick like splitting each category into smaller categories, and look at the prob of falling
# into each, and then use a uniform distribution on each smaller category. ##I PICKED THE LATTER##
##########################################################################################################

#function takes as arguments,
#training_data: this is the vector which will be used to determine the categorical distributions
#cut_points:    this is a vector with the max value for each category. ie, if you have categories
#               30-50,50-70,70-80,80+:cut_points would be 50,70,80
#categorical_data: this is the vector you would like to convert to continuous. this vector should
#               take values of 1,2,3...(total #of categories). so each value should refer to which
#               category the data point belongs to. 1 would refer to the first category. 
#subs:          this is the number of subcategories the each category will be split into

categorical_to_continuous<-function(training_data,categorical_data,cut_points,subs=5){
  number_of_categories<-length(cut_points)+1
  categorical_data=as.numeric(as.factor(categorical_data))
  training_data=as.numeric(as.character(training_data))
  total<-length(training_data)
  probabilities<-array(0,dim=c(number_of_categories,subs,3))
  sample_sizes<-array(0,number_of_categories)                    
    for(i in 1:number_of_categories){
      randoms=c()
       category<-if(i>1&i<number_of_categories){
         training_data[training_data<cut_points[i]&training_data>=cut_points[i-1]]
       }
       else if(i==1){
         training_data[training_data<cut_points[i]]
       }else{
         training_data[training_data>=cut_points[i-1]]
       }
       if(length(category!=0)){
       max<-max(as.numeric(category))
       min<-min(as.numeric(category))
       sample_sizes[i]<-length(category)
       interval<-(max-min)/subs
       for(j in 1:subs){
          probabilities[i,j,2]<-min+(j-1)*interval #subcategory min
          probabilities[i,j,3]<-min+(j)*interval   #subcategory max
          for(m in 2:number_of_categories){#cheap way to make sure there are no gaps
            probabilities[m,1,2]<-probabilities[(m-1),subs,3]
          }
          probabilities[i,j,1]<-length(training_data[training_data>probabilities[i,j,2]#(min+(j-1)*interval)#subcategory probability
                                      &training_data<=probabilities[i,j,3]])/length(category)  
          number_in_sub<-round(probabilities[i,j,1]*length(categorical_data[categorical_data==i]),0)
          randoms<-c(randoms,runif(number_in_sub,(probabilities[i,j,2]),(probabilities[i,j,3])))
       }
      randoms<-c(randoms,runif(max(0,length(categorical_data[categorical_data==i])-length(randoms)),(min),(max)))
      indices<-sample(1:length(randoms),length(categorical_data[categorical_data==i]),replace=F)
      categorical_data[categorical_data==i]<-randoms[indices]
     
       }
    }
  x<-list(round(categorical_data,1),sample_sizes,randoms,probabilities[,,])
  return(unlist(x[1]))
}
