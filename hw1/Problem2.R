currdir <- getwd()  # leave a trail of bread crumbs
datadir <- "ml-100k/"  # easier to hard code
setwd(datadir)
on.exit(setwd(currdir))

findLengths <- function(vector) {
    res <- c(1:length(vector))
    for (i in 1:length(vector)) {
        res[i] = nchar(vector[i])
    }
    return(res)
}

df <- read.table("u.item", TRUE, sep = "|", quote = "\"")
names <- as.list(df[, 2])
dates <- as.list(df[, 3])
dates <- lapply(dates, function(date) substr(date, 8, 11))
movies_df <- data.frame(unlist(dates), unlist(names))
ss <- split.data.frame(movies_df, movies_df[, 1])
ss <- ss[-1]
listOfVecs <- lapply(ss, "[[", "unlist.names.")
listOfVecs <- lapply(listOfVecs, findLengths)
meanLengths <- lapply(listOfVecs, mean)
meanLengths <- unlist(lapply(1:length(meanLengths), function(i) meanLengths[[i]]))

years <- sort(unique(unlist(dates)))
years <- years[-1]

plot(data.frame(years, meanLengths))
