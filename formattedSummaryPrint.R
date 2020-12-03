#' This function generate a formatted summary statistics for all matched variables
#' @param VarName  The matching variable name
#' @param SMD Statndardized Mean Difference
#' @param Pvalue P-value correponding to either two-sample t-test or Chi-square
#' @return a vector of cut points
#' @export formattedSummaryPrint
#' @author Tristan Hills
formattedSummaryPrint <- function(VarName, SMD, Pvalue)
{

    for (i in 1:length(VarName)) {

        current_s <- sprintf("VarName: %s\nP-value: %s\n", VarName[i], Pvalue[i])

        temp_s <- NULL
        if (length(SMD[[i]]) > 1) {
            temp_s <- sprintf("SMD:\n\t%s: %s\n", names(SMD[[i]])[1], (SMD[[i]])[1])
            for (j in 2:length(SMD[[i]])) {
                temp_s <- sprintf("%s\t%s: %s\n", temp_s, names(SMD[[i]])[j], (SMD[[i]])[j])
            }
        } else {
            temp_s <- sprintf("SMD: %s", SMD[[i]])
        }

        current_s <- sprintf("%s%s\n", current_s, temp_s)
        writeLines(current_s)
        #string <- cat()
    }


}
