#' This function maximizes the agreement to the primary split.
#' @param complete_data data used create the surrogate split
#' @param sur index for the current surrogate data
#' @param cutpts the candidate cut points to consider for the surrogate split
#' @param primary_kidids node directions on primary split, e.g. left or right
#' @param cutToLvl used for factored data, will be NULL if continuous
#' @return list of the best agreement and the cut to use when making the partysplit object
#' @author Tristan Hills
surrogateAgreement <- function(complete_data, sur, cutpts, primary_kidids, cutToLvl)
{
  # make sure cutpts are a vector
  cutpts <- as.vector(cutpts)

  ## set the kid node ids to be mappted to the left and right convention
  #primary_kidids <- orientKidids( complete_data[, ncol(complete_data)], primary_kidids )
  #primary_kidids <- kidids_split(psplit, data = complete_data)

  agreements <- sapply(as.vector(cutpts), computeAgreement, complete_data, sur, cutToLvl,
                       primary_kidids)

  # return the max agreement with also the value of the split, agreement, and adjusted R
  if ( !is.null(cutToLvl) ) {
    breakLeft <- getFactorSplitIndex(complete_data, sur, cutToLvl, cutpts[which.max(agreements)])
    return ( list("agree" = max(agreements), "cut" = breakLeft) )
  } else {
    return ( list("agree" = max(agreements), "cut" = cutpts[which.max(agreements)]) )
  }
}