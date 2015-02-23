args <- commandArgs(trailingOnly = TRUE)
load(args[1])

tables = table

# loop over the tables
cat("prob_tables =[")
for(table_num in 1:length(tables)) {
  cat("[")
  table_name = names(tables)[table_num]
  table = tables[[table_num]]

  cond = colnames(table$Conditions)

  values = colnames(table$Probabilities)

  for(table_row_num in 1:nrow(table$Conditions) ) {


  # for field  
  cat(paste("{ \"for\" => \"", table_name, "\"", sep=""))

  # on_fields


  cat(", \"on_fields\" => {")  
  if(cond != "X0") {
    for(i in 1:length(cond)) {
      if(as.character(table$Condition[table_row_num, i]) == "Data Scarcity") {
        break
      }

      cat("\"", cond[i], "\" => \"", as.character(table$Conditions[table_row_num,i]), "\" ,", sep="")
    }
  }
  cat("}")

  
  #probs
  cat(", \"probs\" => {")   
  for(i in 1:length(values)) {
    cat("\"", values[i], "\" => ", as.numeric(as.character((table$Probabilities[table_row_num,i]))), " ,", sep="")
  }
  cat("}")

  
  cat( "}," )
  }
  cat("],")
}
cat("]\n")

