#' This function generate all cut points for a passed in variable.
#' @param x the column to generate the cut points from
#' @param minbucket minimum amount of data a node should hold
#' @param nsplit specify integer to sample proposed cut points, NULL gives all cut points
#' @return a vector of cut points
#' @author Peter Calhoun
findCutpts <- function(x, minbucket, nsplit) {
    nX <- length(x)
    #Due to ties, it's possible minbucket cannot be satisfied
    if (sort(x)[minbucket] == sort(x, decreasing = TRUE)[minbucket]) {
        cutpts = NULL
    } else {
        #Cutpoints considered must satisfy minbucket
        cutpts <- unique(sort(x)[minbucket:(nX - minbucket + 1)])
        #Take average of distinct points to determine cutoff (like rpart)
        if (length(cutpts) == 1) {
            stop(paste0("Only 1 cutpt??? ", cutpts, x))
        }
        cutpts <-
            (cutpts[1:(length(cutpts) - 1)] + cutpts[2:length(cutpts)]) / 2
    }

    if (!is.null(nsplit)) {
        cutpts <- sample(cutpts, nsplit, replace = TRUE)
    }

    return(cutpts)
}
