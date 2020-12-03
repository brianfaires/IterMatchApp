#' Enforces the CART standard of the "less risky", or smaller proportion of trues being on the left node.
#' Conversely, the larger proportion is sent to the right.
#' @param response vector of the binary response
#' @param kidids vector generated from partykit::kidids_splitI()
#' @return returns a vector, or reoriented kidids vector
#' @export checkTreeConvention
#' @author Tristan Hills
checkTreeConvention <- function(response, kidids, index)
{
    # Agnostic to the number of splits contained within kidids, i.e. binary or more.
    ## This function will check that "left" is "less risky" and that right is "more risky".
    ## If not it will "flip" the kidids


    if (!is.factor(kidids)) {
        kidids <- as.factor(kidids)
    }

    true_class <- as.integer( levels(response)[2] ) # get the second level, or "1" out of "0" and "1"
    count_of_trues <- sum( response == true_class )
    kidid_levels <- as.integer( levels(kidids) )

    # compute the proportions of ones in each split
    ps <- c()
    for (i in 1:length(kidid_levels)) {
        kid <- response[ kidids == kidid_levels[i] ]
        ps <- c( ps, sum( kid == true_class )/count_of_trues )
    }

    # Our 1 kid id contains the "more risky" proportions need to flip kidids
    min_idx <- sort(ps, index.return = TRUE)[["ix"]]
    if (any(min_idx != 1:length(min_idx)) ) {
        temp_kids <- rep(NA, length(kidids))
        for (i in 1:length(ps)) {
            temp_kids[ kidids == kidid_levels[min_idx[i]] ] <- as.integer(i)
        }

        kidids <- as.factor( temp_kids )
        if (is.null(index)) {
            return( list(reorder = T, index = min_idx, new_kidids = kidids) )
        } else {
            # indices are non zero and we need to reverse them
            #new_index <- index
            new_index <- sapply(index, function(i) {if (is.na(i)) { i; } else if (i == 1) { 2; } else {1; }} )

            #print(index)
            #print(new_index)
            return( list(reorder = T, index = as.integer(new_index), new_kidids = kidids) )
        }

    } else {
        return( list(reorder = F, index = NULL, new_kidids = NULL) )
    }
}
