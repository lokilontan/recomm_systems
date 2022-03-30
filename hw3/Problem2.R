library(rectools)
library(regtools)
library(qeML)

setwd("/Users/lokilontan/Documents/College/ECS - 172/ecs-172-repos/ECS-172/HW3")
load("Hwk3_1.RData")
collect_data <- function() {
  #GET the data tables
  library(data.table)
  users_dt <- fread("BX-CSV-Dump/BX-Users.csv", sep=";")
  colnames(users_dt) <- c('userID','location','age')
  books_dt <- fread("BX-CSV-Dump/BX-Books.csv", sep=";")
  colnames(books_dt) <- c('itemID','title','author', 'year', 'publisher', 'urlS', 'urlM', 'urlL')
  ratings_dt <- fread("BX-CSV-Dump/BX-Book-Ratings.csv", sep=";")
  colnames(ratings_dt) <- c('userID','itemID','ratings')
  
  #MERGE them by itemID then by userID, convert to DF
  book_crossing <- merge(ratings_dt, books_dt, by = "itemID")
  book_crossing <- merge(book_crossing,users_dt, by = "userID")
  book_crossing <- setDF(book_crossing)
  return (book_crossing)
}

#book_crossing <- collect_data()

#PREPROCESS data tables

#drop title, urlS, urlM, urlL (lenghty, redundant)
#book_cross <- subset(book_crossing, select=-c(title, urlS, urlM, urlL)) 

add_extra_user_item_columns <- function(inputDF) {
  #compute userMean and userRatingCount (Takes long time)
  uniqueID <- unique(inputDF[,1])
  resUser <- rep(0,nrow(inputDF))
  resUserCount <- rep(0,nrow(inputDF))
  ratingsCol <- inputDF[,3]
  counter <- 1
  uniqueLen <- length(uniqueID)
  for (id in uniqueID){
    boolIdx <- inputDF[,1] == id
    sidAvgRating <- mean(ratingsCol[boolIdx])
    resUser[boolIdx] <- sidAvgRating 
    resUserCount[boolIdx] <- length(ratingsCol[boolIdx])
    cat(sprintf("Done: ID - %s; %s/%s\n", id, counter, uniqueLen))
    counter <- counter + 1
  }

  print("userMean and userRatingCount done")

  #compute itemMean and itemRatingCount (Takes long time)
  uniqueID <- unique(inputDF[,2])
  resItem <- rep(0,nrow(inputDF))
  resItemCount <- rep(0,nrow(inputDF))
  ratingsCol <- inputDF[,3]
  counter <- 1
  uniqueLen <- length(uniqueID)
  for (id in uniqueID){
    boolIdx <- inputDF[,2] == id
    sidAvgRating <- mean(ratingsCol[boolIdx])
    resItem[boolIdx] <- sidAvgRating
    resItemCount[boolIdx] <- length(ratingsCol[boolIdx])
    cat(sprintf("Done: ID - %s; %s/%s\n", id, counter, uniqueLen))
    counter <- counter + 1
  }

  print("itemMean and itemRatingCount done")

  #combine new columns
  inputDF$userMean <- resUser
  inputDF$itemMean <- resItem
  inputDF$userRatingCount <- resUserCount
  inputDF$itemRatingCount <- resItemCount

  return (inputDF)
}

#book_cross <- add_extra_user_item_columns(book_cross) 

modify_id_s <- function(inputDF) {
  #This function should modifu userID and itemID in such a way so 
  #both columns have a unique ID starting from 1 and are integer
  #USERID
  uniqueID <- unique(inputDF[,1])
  counter <- as.integer(1)
  uniqueLen <- length(uniqueID)
  for (id in uniqueID){
    boolIdx <- inputDF[,1] == id
    inputDF[boolIdx, 1] <- rep(counter, length(inputDF[boolIdx, 1]))
    cat(sprintf("Done: ID - %s; %s/%s\n", id, counter, uniqueLen))
    counter <- counter + 1
  }
  #ITEMID
  uniqueID <- unique(inputDF[,2])
  counter <- as.integer(1)
  uniqueLen <- length(uniqueID)
  for (id in uniqueID){
    boolIdx <- inputDF[,2] == id
    inputDF[boolIdx, 2] <- rep(counter, length(inputDF[boolIdx, 2]))
    cat(sprintf("Done: ID - %s; %s/%s\n", id, counter, uniqueLen))
    counter <- counter + 1
  }
  
  return (inputDF)
}

#book_cross <- modify_id_s(book_cross)

#get a subset sample of size N from book_cross
#N = 500000
bc_sample <- book_cross #[sample(nrow(book_cross), N), ]

#clean ds, generate NAs
to_na <- function(entry){
  if (entry == "NULL" || is.null(entry) || entry == "NA" ) {
    return(NA)
  }
  else {
    return (entry)
  }
}

process_columns <- function(columns_list){
  for (i in 1:length(columns_list)){
    new_col <- lapply(bc_sample[,columns_list[i]] , to_na)
    bc_sample[,columns_list[i]] <<- unlist(new_col)
  }
}

#process_columns(c(3, 4, 5, 6, 7, 8))

#drop location
bc_sample <- bc_sample[, -7]

to_factors <- function(inputDF) {
  #convert needed columns to factors
  inputDF$userID <- as.factor(inputDF$userID)
  inputDF$itemID <- as.factor(inputDF$itemID)
  inputDF$ratings <- as.factor(inputDF$ratings)
  inputDF$author <- as.factor(inputDF$author)
  inputDF$publisher <- as.factor(inputDF$publisher)
}
#bc_sample <- to_factors(bc_sample)

#convert factors to dummies
#bc_sample_final <- data.frame(bc_sample[, c(3,8,9)])
#names(bc_sample_final) <- c('ratings', 'userMean', 'itemMean')
#bc_dummies <- factorsToDummies(bc_sample[, -c(3,8,9)], omitLast = T) 
#Error: vector memory exhausted (limit reached?)
#save(bc_sample_final, bc_dummies, file="Hwk3.RData")

#I couldn't manage to convert to dummies all of my columns
#I will run my linear model using userMean, itemMean, userRatingCount and itemRatingCount
#bc_qeout <- qeLin(bc_sample[,c(3,8,9,10,11)], "ratings") #testAcc ~ 2.17
print(paste(bc_qeout$testAcc))
bc_predict <- predict(bc_qeout, bc_sample[,c(8,9,10,11)])
bc_sample["lm_predicted"] <- as.data.frame(bc_predict)

#get residuals (actual-predicted)
bc_residuals <- bc_sample[,3] - as.data.frame(bc_predict)
bc_sample["residuals"] <- bc_residuals

#split the bc_sample into training(80%) and testing(20%) subsets
bc_sample_train <- bc_sample[1:floor(nrow(bc_sample)*0.8),]
bc_sample_test <- bc_sample[(nrow(bc_sample_train)+1):nrow(bc_sample), ]
if (nrow(bc_sample_train) + nrow(bc_sample_test) == nrow(bc_sample)) {
  print("The dataframe is split correctly")
} else {
  print(paste("Train set nrow: ", nrow(bc_sample_train)))
  print(paste("Test set nrow: ", nrow(bc_sample_test)))
  print(paste("Total set nrow: ", nrow(bc_sample)))
}

#save(book_crossing, resUser, resUserCount, resItem, resItemCount, book_cross, bc_qeout, file="Hwk3_1.RData")

#RECOSYSTEM
library(recosystem)

reco_experiment <- function(trainDF, testDF, tune=FALSE, tuning_params=list()){
  r <- Reco()
  
  train.dm <- data_memory(trainDF[, 1],
                          trainDF[, 2],
                          trainDF[, 3],
                          index1 = TRUE)
  
  test.dm <- data_memory(testDF[, 1],
                         testDF[, 2],
                         testDF[, 3], #this is ignored by RECO
                         index1 = TRUE)
  
  r_tune_min <- tuning_params
  
  if (tune) {
    r_tune <- r$tune(train.dm, opts = list( dim = c(20:40), 
                                   nthread = 6,
                                   progress = TRUE,
                                   verbose = TRUE))
    r_tune_min <- r_tune$min
  }
  
  print(r_tune_min) #dim = 40, costp_l1 = 0.1, costp_l2 = 0.01, costq_l1 = 0.1, costq_l2 = 0.1, lrate = 0.01, loss_fun = 2.796951
  
  r$train(train.dm, 
          out_model = file.path(getwd(), "reco_model.txt"),
          opts = c(r_tune_min, nthread = 6, niter = 100))
  
  r_pred_out = r$predict(test.dm, out_memory())

  return (r_pred_out)
}

# using covariates effect
#r_pred <- reco_experiment(bc_sample_train[, c(1,2,12)], bc_sample_test[, c(1,2,12)], TRUE)
#save(book_crossing, resUser, resUserCount, resItem, resItemCount, book_cross, bc_qeout, r_pred, file="Hwk3_1.RData")

#add predicted from covariates back
bc_pred_test <- as.data.frame(bc_predict)[(nrow(bc_sample_train)+1):nrow(bc_sample), ]
r_pred_plus_residuals <- r_pred + bc_pred_test

#error calculation
rmse <- sqrt(mean((bc_sample_test[, "ratings"] - r_pred_plus_residuals) ^ 2))
print(paste("Matrix factorization on residuals RMSE: ", rmse)) #2.74

# without covariates effect
tune_opts_mins <- list(dim = 40, costp_l1 = 0.1, costp_l2 = 0.01, costq_l1 = 0.1, costq_l2 = 0.1, lrate = 0.01)
#r_pred_2 <- reco_experiment(bc_sample_train[, c(1,2,3)], bc_sample_test[, c(1,2,3)], FALSE, tune_opts_mins)
#save(book_crossing, resUser, resUserCount, resItem, resItemCount, book_cross, bc_qeout, r_pred, r_pred_2, file="Hwk3_1.RData")
rmse_2 <- sqrt(mean((bc_sample_test[, "ratings"] - r_pred_2) ^ 2))
print(paste("Matrix factorization on ratings RMSE: ", rmse_2)) #3.79

#SOFTIMPUTE
library(softImpute)

#build matrix
bc_sample_train[,2] <- as.numeric(bc_sample_train[,2])
bcm <- rectools::buildMatrix(bc_sample_train[c(1:10000), c(1,2,12)], NAval=NA)

#run
z <- softImpute(bcm, rank.max=40)
A <- complete(bcm, z, unscale = FALSE)
print(A)


