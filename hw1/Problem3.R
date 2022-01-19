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

df <- getML100K()
library(regtools)
library(ggplot2)


encode_genres <- function(df) {
    genres <- df[,c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10",
            "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G19")]

    which(is.na(genres)) # 0 NA or null values

    #takes so much time on a full dataframe
    lapply(1:nrow(genres), function(i) {
        # get a vector of a positive indicator (1)
        row <- genres[i, ]
        sample_out_of <- which(row %in% 1)
        # if inside the loop, then more than one genre
        if (length(sample_out_of) > 1) {
            # assign to all a zero'
            row[sample_out_of] <- 0
            # randomly select a genre
            row[[sample(sample_out_of, 1)]] <- 1
            genres[i, ] <<- row
        }
    } )
    encoded <- dummiesToFactor(genres)
    df$genres <- encoded
    return(df)
} 

count_means <- function(inputDF) {
    df <- inputDF[, c("item", "rating")]
    df <- aggregate(rating ~ item, df, mean)
    lapply(1:nrow(inputDF), function(row) {
        inputDF$mean_rating[row] <<- df[inputDF$item[row],2]
    })
    print(inputDF)
}

df <- encode_genres(head(df, n = 2000))

plotDensities <- function(inputDF, xName, grpName) {
    grouped <- split(inputDF[,xName],inputDF[,grpName])

    max_y <- 0
    max_x_id <- 1
    for (i in 1:length(grouped)) {
        max_i_y <- max(density(grouped[[i]])$y)
        max_y <- max(max_y, max_i_y)
        if ( length(density(grouped[[max_x_id]])) < length(density(grouped[[i]])) ) {
            max_x_id <- i
        }
    }
    cl <- rainbow(19)

    x <- density(grouped[[max_x_id]])$x
    y <- density(grouped[[1]])$y

    plot(density(grouped[[1]]), main = "Density vs. Genre", col = cl[i], ylim = c(0, max_y))

    for (i in 2:length(grouped)) {
        lines(density(grouped[[i]]), col = cl[i])
    }
    legend("topleft",legend = paste("G", 1:length(grouped)), col = cl[1:length(grouped)],pch = 19, bty = "n")
}

plotDensities(df, "itemMean", "genres")