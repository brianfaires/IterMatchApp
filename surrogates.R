#' This function takes in the current node data and builds possible surrogate splits in conjunction
#' to the primary split.
#' @param surrogateargs Defines the options of the surrogate split
#' @param data Data at that current node used for splitting
#' @param primeVar list defining the prime split, used to recreate the prime partykit::partysplit()
#' @param surrogateVars A vector of the surrogate indices to grab from data
#' @param minbucket Smallest amount of data used to try to create a surrogate split with
#' @param nsplit For randomly grabbing from the available subset of vars (extremely random forest)
#' @param search What algorithm to use, default is greedy
#' @return list of surrogate splits, or partykit::partysplit() objects
#' @export makeSurrogates
#' @author Tristan Hills
makeSurrogates <- function(surrogateargs,
                           data,
                           varGlobalOrd,
                           primary_split,
                           surrogateVars,
                           minbucket,
                           nsplit,
                           search)
{

    splits <- list()
    bestAgreements <- c()
    i <- 1
    # for loop the potential surrogates
    for (sur in surrogateVars) {

        # ordinalize
        if (search == "greedy" &
            !is.null(nsplit)) {
            xTemp <- ordinalize(x = data[, sur], data$y, varGlobalOrd[[sur]], sortCat = FALSE)
        } else {
            xTemp <- ordinalize(x = data[, sur], data$y, varGlobalOrd[[sur]])
        }

        if ( !(abs(max(xTemp$x, na.rm = T) - min(xTemp$x, na.rm = T)) > 1e-8) ) {
            next
        }

        # equalize data across the vars. Primary split has NAs and surrogate has NAs
        missing_mask <- ( !is.na(data[, primary_split$varid]) & !is.na( data[, sur]) )
        complete_data <- data[missing_mask, ]
        x_complete <- xTemp$x[missing_mask]

        # check that we have more data than min bucket
        if (nrow(complete_data) <= minbucket)
            next

        # get primary split of kidids
        primary_kidids <- as.factor( kidids_split(primary_split, data = complete_data) )

        # generate all cut points for the current surrogate
        cutpts <- findCutpts(x_complete, minbucket, nsplit)
        if (is.null(cutpts))
            next

        # make a temp split based on the current potential split, run "kidids_node" to find how the split
        # would occur, use "table" to compute the confusion matrix, and finally calculate agreement
        bestSplit <- surrogateAgreement(complete_data, sur, cutpts,
                                        primary_kidids, cutToLvl = xTemp$cutToLvl)


        # make the surrogate split. Make sure to give the split value, agreement, and adjusted R
        # associated with the surrogate split
        if ( length(bestSplit$cut) > 1 ) {
            surrogate_sp <- partysplit( varid = sur, index = bestSplit$cut, info = bestSplit )
        } else {
            surrogate_sp <- partysplit( varid = sur, breaks = bestSplit$cut, info = bestSplit )
        }

        # compute the adjusted R for the surrogate split (-) is bad and should throw away, (+) is good and
        # should be kept
        # we continue if the adjR is less than 0
        proportions <- c()
        pLevels <- c(1,2) # levels are always binary
        for (level in pLevels) {
            proportions <- c( proportions, mean( primary_kidids == level, na.rm = T ) )
        }

        # compute adjusted R
        adjR <- (min(proportions) - (1-bestSplit$agree)) / min(proportions)
        bestSplit$adjR <- adjR

        if (is.na(adjR))

        if (bestSplit$adjR <= 0)
            next

        # apply surrogate split convention after determining it is good
        convention <- checkTreeConvention(complete_data[,ncol(complete_data)],
                                          kidids_split(surrogate_sp,  data = complete_data),
                                          surrogate_sp$index)
        if (convention$reorder) {
            surrogate_sp <- partysplit(varid = surrogate_sp$varid, breaks = surrogate_sp$breaks,
                                       info = surrogate_sp$info, index = convention$index)
        }

        # collect the best agreements
        bestAgreements <- append(bestAgreements, bestSplit$agree)

        # Change the surrogate split to reflect the column name
        surrogate_sp$varid <- as.integer( colnames(data)[sur] )

        splits[[i]] <- surrogate_sp

        i <- i + 1
    }

    if ( length(splits) == 0 ) {
        return(NULL)
    }

    splits <- splits[ order(bestAgreements, decreasing = TRUE) ]

    # reduce list to size of "maxsurrogates" in surrogateargs
    if ( length(splits) > surrogateargs$maxsurrogate )
        splits <- splits[ 1:surrogateargs$maxsurrogate ]

    # return list of surrogate splits
    splits
}

