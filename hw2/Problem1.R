#   Class virtualRatings

#   Params: 
#       inputDF (matrix): 3-column matrix in the usual 
#                         (userID, itemID, rating) format
virtualRatings <- setRefClass("virtualRatings",
    fields = list(inputDF = "matrix"),
    methods = list(
        initialize = function(extDF, ...) {
            inputDF <<- extDF
            callSuper(...)
        }
))

#   Method [ for virtualRatings (overloaded)

#   Looks for ratings given by the ith user to jth movie.
#   In case there are no such ratings, NA is displayed
#   In case there are more than one rating, only the first displayed

#   Params: 
#       x (virtualRatings): virtualRatings object
#       i (numeric): user number
#       j (numeric): item(movie) number

#   Warnings:
#       More than one rating found: muliple instances found
`[.virtualRatings` <- function(x,i,j){
        df <- as.data.frame(x$inputDF)
        res <- df[df$V1 == i & df$V2 == j, ]       
        if (nrow(res) == 0) {
            return(NA)
        }
        if (nrow(res) > 1) {
            warning("muliple instances found")
        }
        return(res[1,3])
    }

iDF <- rbind(c(2, 5, 1), c(3, 5, 4), c(2, 5, 2), c(6, 1, 5))
ex <- virtualRatings(iDF)

#   Test cases
#ex[3, 5]
#ex[2, 5]
#ex[6, 1]
#ex[7, 1]
#ex[6, 2]
#ex[7, 2]