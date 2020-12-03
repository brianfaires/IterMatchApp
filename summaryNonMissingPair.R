#' @title Summarize the result of matched sample for matched pairs without missing
#' @description Calculate mean, variance, max, min, number of non-missingpairs, standardized mean
#'              difference (SMD), and  two sample t-test/chi-square P-values for matched pairs
#'               without missing
#'
#' @param DATA Dataframe of subjects and variables
#' @param GROUP The response binary variable
#' @param Var The matching variable
#' @return A list contains of p-values and SMD for the variable and summary statistics
#'         by group for matched pairs with no missing values
#' @export summaryNonMissingPair
#' @author Afrooz Jahedi
summaryNonMissingPair <- function(DATA, GROUP, Var) {
    # separate treated group and sort by pair number
    treated <- arrange(subset(DATA, DATA[GROUP] == 1, drop = F), pairNum)
    # which value of all variables are non-missing
    tNonMissingVars <- is.na(treated)
    # Which subject has missig
    treated$tMissing <-
        apply(tNonMissingVars, 1, function(x)
            sum(x) > 0)

    control <- arrange(subset(DATA, DATA[GROUP] != 1, drop = F), pairNum)
    cNonMissingVars <- is.na(control)
    control$cMissing <-
        apply(cNonMissingVars, 1, function(x)
            sum(x) > 0)
    tcMissing <- treated$tMissing | control$cMissing

    tNonMissing <- treated[!tcMissing, -length(treated)]
    cNonMissing <- control[!tcMissing, -length(control)]
    dataNonMissing <- rbind(tNonMissing, cNonMissing)


    if (!is.factor(dataNonMissing[, Var])) {
        pvalue <-
            t.test(dataNonMissing[, Var] ~ dataNonMissing[, GROUP])$p.value
        summary <-
            aggregate(dataNonMissing[, Var] ~ dataNonMissing[, GROUP], dataNonMissing, function(x)
                c(
                    mean = mean(x) ,
                    var = var(x),
                    n = sum(!is.na(x)),
                    min = min(x),
                    max = max(x)
                ))
        #by(DATA[,Var],DATA[,GROUP], function(x) mean(x,na.rm = T))
        smd <- smd(dataNonMissing, Var, GROUP)
    } else{
        summary <- table(DATA[, GROUP], DATA[, Var])
        pvalue <- chisq.test(summary)$p.value
        #pvalue <- chisq.test(DATA[, GROUP], DATA[, Var])$p.value
        smd <- smd(DATA, Var, GROUP)
    }

    return(
        list(
            varname = paste0(Var),
            name = cat("========", Var, "========", "\n"),
            print(c(GROUP, table(DATA[, GROUP]))),
            cat("***", paste0("Pval"), "***", "\n"),
            pval = print(pvalue),
            cat("***", paste0("SMD"), "***", "\n"),
            smd = print(smd),
            cat("*** Mean&Var&n&rane ***", "\n"),
            print(summary)
        )
    )
    #cat("pvals", "smdOpt","table(DATA$group)","\n")
}
