#' @title getFactorSplitIndex
#' @param data Dataframe of subjects and variables
#' @param idx Unique subject ID
#' @param cutToLvl
#' @param cutpt cutpoints of a variable
#' @return
#' @export getFactorSplitIndex
#' @author Tristan Hills
getFactorSplitIndex <- function(data, idx, cutToLvl, cutpt)
{

    # getter function for breakLeft
    breakLeft <- rep( NA, length(levels(data[, idx])) )
    breakLeft[levels(data[, idx]) %in% names(cutToLvl)[cutToLvl <= cutpt]] <- 1L
    breakLeft[levels(data[, idx]) %in% names(cutToLvl)[cutToLvl > cutpt]] <- 2L

    if ( all(is.na(breakLeft)) | length(unique(breakLeft)) <= 1 ) {
        stop("Did not find correct cutpoints")
    }

    breakLeft
}
