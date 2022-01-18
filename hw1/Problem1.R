getML100K <- function(needDownload=FALSE)
{
   if (needDownload) {
      # 5 Mb
      download.file(
         'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
         'ml-100k.zip')
      unzip('ml-100k.zip')
   }
   currdir <- getwd()  # leave a trail of bread crumbs
   datadir <- 'ml-100k'  # easier to hard code
   setwd(datadir)
   on.exit(setwd(currdir))
   
   # make matrices ud, uu and ui, for the main ratings data, user info
   # an item info

   ud <- read.table('u.data',header=F,sep='\t')
   colnames(ud) <- c('user','item','rating','timestamp')
   ud <- as.data.frame(ud)
   
   uu <- read.table('u.user',header=F,sep='|',stringsAsFactors=TRUE)
   ur <- split(ud[,3],ud[,1])  # ratings by user
   uu <- cbind(uu,sapply(ur,mean))
   uu <- cbind(uu,sapply(ur,length))
   colnames(uu) <- c('user','age','gender','occ','zip','userMean','Nuser')

   # reading u.item is tricky, with some problematic records etc.;
   # fortunately, we only need the last 19 fields
   z <- readLines('u.item')
   zs <- strsplit(z,'|',fixed=TRUE)  # splits to single characters
   zgl <- lapply(zs,function(charvec) charvec[6:24])  # get the genre dummies
   zgls <- t(sapply(zgl,as.integer))  # create matrix of genres
   ui <- cbind(1:nrow(zgls),zgls)
   ir <- split(ud[,3],ud[,2])  # ratings by item
   ui <- cbind(ui,sapply(ir,mean))
   ui <- cbind(ui,sapply(ir,length))
   colnames(ui) <- c('item',paste('G',1:19,sep=''),'itemMean','Nitem')

   setwd(currdir) # follow the trail back 
   uduu <- merge(ud,uu)
   uduuui <- merge(uduu,ui)
   # this ends up in (item,user) order, whereas we need the opposite
   outdf <- uduuui[,c(2,1,3:ncol(uduuui))]
   attr(outdf,'useritemCovs') <- c(4,4)
   attr(outdf,'userCovs') <- c(5,10)
   attr(outdf,'itemCovs') <- c(11,31)
   outdf
}

df <- getML100K(FALSE)
df <- data.frame(df$user, df$timestamp)
colnames(df) <- c("user","timestamp")

waitTimes <- function(rawData) {
    
    #rawData can have duplicate timestamps
    #prof says "either time is discretized"
    #rawData <- rawData[!duplicated(rawData[,2]), ]

    #split by user
    ss <- split.data.frame(rawData,rawData[,1])

    #create a list of vectors
    listOfVecs <- lapply(ss, "[[", colnames(rawData)[2])
    
    #sort each vector in the listOfVecs
    listOfVecs <- lapply(listOfVecs, function(list) sort(list))

    w_i <- lapply(listOfVecs, difference)
    print(w_i)
    #mergedVecs <- mergeEm(listOfVecs)

    print(mergedVecs)
}

mergeEm <- function(listOfVecs) {
    #INITIALIZATION

    #create pointer list - will point to the current(smallest) element of each vector
    current_el <- unlist(list(rep(1, length(listOfVecs))))

    #put all current(first) elements inside a dataframe
    #first column is the index of the vector, 
    id_s <- c(1:length(listOfVecs))

    #second column is the value of the current smalest element from this index
    elements <- unlist(lapply(listOfVecs, function(list) list[1]))

    df <- data.frame(id_s, elements)

    #sort by elements. do only one time O(klogk)
    df <- df[order(elements),]

    #need the total number of elements
    counter <- 0
    for (i in 1:length(listOfVecs)) {
        counter = counter + length(listOfVecs[[i]])
    }

    #resultant vector
    res <- unlist(c(1:counter))

    #PROCESSING
    #O(n*k*k)
    for (i in 1:counter) {
        #pop the top
        res[i] <- df[1,2]
        min_ind <- df[1,1]
        df <- df[-c(1), ]

        #check if listOfVecs[[min_ind]] still has elements
        if (current_el[min_ind] < length(listOfVecs[[min_ind]])) {
            #increment counter at current_el[min_ind]
            current_el[min_ind] <- current_el[min_ind] + 1
            #get next element from listOfVecs[[min_ind]][current_el[min_ind]]
            el_to_add <- listOfVecs[[min_ind]][current_el[min_ind]]
          
            #insert el_to_add in such a way that the smallest ellement stays at the top
            #df will have maximum of K elements, so O(k)
            new_row <- c(min_ind, el_to_add) 
            if (nrow(df) == 0) {
                df[seq(1,1),] <- df[seq(1,0),] 
                df[j,] <- new_row  
            }
            else {
            for (j in 1:nrow(df)){
                if (el_to_add <= df[j,2]) {
                    df[seq(j+1,nrow(df)+1),] <- df[seq(j,nrow(df)),] 
                    df[j,] <- new_row  
                    break
                } else if (j == nrow(df)) {
                    df[seq(nrow(df)+1,nrow(df)+1),] <- df[seq(nrow(df),nrow(df)),] 
                    df[nrow(df),] <- new_row 
                }    
            }
            }
        }
    }
    return(res)
}

difference <- function(vec) {
    if (length(vec) < 2){
        return(vec)
    }
    else{
        res <- vec[2:length(vec)] - vec[1:length(vec)-1]
        return(res)
    }
}

waitTimes(df)


