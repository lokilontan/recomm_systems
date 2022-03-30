library(rectools)
library(regtools)
library(qeML)

#preprocess datasets
#pef
data(pef)
pef_d <- factorsToDummies(pef,omitLast = T)
pef_df <- as.data.frame(pef_d)
#print(head(pef_df))

#mlb
data(mlb)
mlb <- mlb[,c(3,4,5,6)]
mlb_d <- factorsToDummies(mlb, omitLast = T)
mlb_df <- as.data.frame(mlb_d)
#print(head(mlb_df))

lmAlpha <- function(data,yName,nReplic,holdout) {
  #This function fits features with the smallest p-values incrementally
  
  #Args:
    #data: data frame, numerical columns
    #yName: name of the column to be predicted (Y is a continuous, not categorical)
    #nReplic: number of holdout sets
    #holdout: size of the holdout sets, NULL if none
  
  #Returns: R list, where ith element is a tuple of i-sized feature set and MAPE from qeLin() on these features
  
  #call qeLin(), fitting on the entire dataset
  qeout <- qeLin(data, yName, holdout = NULL)
  p_values <- summary(qeout)$coefficients[,4]
  
  #(Intercept) is not needed
  p_values <- p_values[-1]
  
  #sort in ascending order
  p_values <- sort(p_values)
  
  #make yName to look like 'yName'
  yName_string <- paste("'", yName, "'", collapse="")
  #remove whitespaces(artifacts from previous step)
  yName_string <- gsub(' ', '', yName_string, fixed = TRUE)
  
  #vectors for the output
  mape <- vector(length = length(p_values))
  vars <- vector(length = length(p_values))
  
  #for each set of features
  for(i in 1:length(p_values)) {
    
    #concatenate all column names in ith range adding quotes to both sides
    cols_string <- paste("'", names(p_values[1:i]),"'" ,collapse=", ")
    #remove whitespaces(artifacts from previous step)
    cols_string <- gsub(' ', '', cols_string, fixed = TRUE)
    
    #concatenate columns with yName
    features_string <- paste(cols_string, yName_string, sep=", ")
    
    #build a string qeLin command
    qeLin_command = paste("qeLin(data[, c(", features_string ,")], \'",yName, "\', holdout = ", holdout,")$testAcc", sep="")
    
    #finally, pass it to replicMeans()
    mape[i] <- list(replicMeans(nReplic, qeLin_command))
    vars[i] <- list(names(p_values[1:i]))
    
  }
  
  #combine everything in an apropriate format
  res <- lapply(1:length(mape), function(i){
    list(vars = unlist(vars[i]), testAcc = mape[[i]])
  })

  return (res)
  
}

#lmAlpha(mlb_df, "Weight", 50, 250)
#lmAlpha(pef_df, "wageinc", 50, 250)