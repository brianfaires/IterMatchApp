#' @title Summarize the result of matching
#' @description Calculate mean, variance, max, min, number of non-missing values
#'              for each group and provide P-value and, standardized mean
#'              difference (SMD) of the variable for matched pairs with missing
#' @param DATA The data frame
#' @param GROUP The binary variable
#' @param Var The matching variable
#' @return A list contains of p-values and SMD for the variable and summary statistics
#'         by group for the variable
#' @export summaryMatch
#' @examples
#' mtcars$am=as.factor(mtcars$am)
#' summaryMatch(mtcars,"mpg","am")
#' @author Afrooz Jahedi
#'
summaryMatch <- function(DATA, Var, GROUP) {
    if (!is.factor(DATA[, Var])) {
        pvalue <-  t.test(DATA[, Var] ~ DATA[, GROUP])$p.value
        summary <-
            aggregate(DATA[, Var] ~ DATA[, GROUP], DATA, function(x)
                c(
                    mean = mean(x) ,
                    var = var(x),
                    n = sum(!is.na(x)),
                    min = min(x),
                    max = max(x)
                ))
        smd <- smd(DATA, Var, GROUP)
    } else{

        summary <- table(DATA[, GROUP], DATA[, Var])
        pvalue <- chisq.test(summary)$p.value
        smd <- smd(DATA, Var, GROUP)

    }
    return((
        list(
            varname = paste0(Var),
            name = cat("========", Var, "========", "\n"),
            print(c(GROUP, table(DATA[, GROUP]))),
            cat("***", paste0("Pval"), "***", "\n"),
            pval = print(pvalue),
            cat("***", paste0("SMD"), "***", "\n"),
            smd = print(smd),
            cat("*** Mean & Var & n & range ***", "\n"),
            print(summary)
        )
    ))
}
