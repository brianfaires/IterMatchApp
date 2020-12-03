#' This function will compute a candidate agreement, i.e. does not maximize agreement
#' @param cutpt A single cut point to split on
#' @param completedata data used for making the surrogate split
#' @param idx index for the column in completedata to split on
#' @param cutToLvl used for factored data
#' @param pkidids how the primary splits sends the data
#' @return we return an agreement value for the passed in cutpoint
#' @author Tristan Hills
computeAgreement <- function(cutpt, completedata, idx, cutToLvl, pkidids)
{

  if ( !is.null(cutToLvl) ) {
    breakLeft <- NULL
    # Entered when we calculate surrogate agreement during adjusted R
    if(  length(cutpt) > 1 ) {
      breakLeft <- cutpt
    } else {
      breakLeft <- getFactorSplitIndex(completedata, idx, cutToLvl, cutpt)
    }
    temp_sp <- partysplit(varid = idx, index = breakLeft)
  } else {
    temp_sp <- partysplit(varid = idx, breaks = cutpt)
  }

  temp_kidids <- kidids_split(temp_sp, data = completedata)
  # check for convention
  convention <- checkTreeConvention(completedata[, ncol(completedata)], temp_kidids, temp_sp$index)
  if (convention$reorder) {
    # assign flipped kidids
    temp_kidids <- convention$new_kidids
  }


  confusion_mat <- as.matrix(table(pkidids, temp_kidids))
  agreement <- sum(diag(confusion_mat)) / sum(confusion_mat)

  agreement
}
