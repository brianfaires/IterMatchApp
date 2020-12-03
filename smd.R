#' Calculate the standardized mean difference of a variable for two groups. Note
#' the response must be coded as 0 and 1
#' @param DATA Dataframe of subjects and variables
#' @param Var The matching variable
#' @param GROUP The response binary variable
#' @return Standardized Mean Difference of a variable
#' @export smd
#' @examples
#' mtcars$am=as.factor(mtcars$am)
#' smd(mtcars,"mpg","am")
#'@author Afrooz Jahedi
smd = function(DATA, Var, GROUP) {
    #pars <- as.list(match.call())
    #browser()
    stdMeanDiff <- NULL
    if ( is.factor(DATA[, Var]) ) {


        if (nlevels(DATA[, Var]) > 2 ) {
            stdMeanDiff <- smd_dummy_binary(DATA, Var, GROUP)
        } else {
            stdMeanDiff <- stddiff.binary(DATA, gcol = names(DATA)[names(DATA) == GROUP],
                                          vcol = names(DATA)[names(DATA) == Var] )[5]
        }

    } else {
        # data is continuous

        mASD = mean(DATA[DATA[, GROUP] == levels(DATA[, GROUP])[1], Var], na.rm = TRUE)
        mTD = mean(DATA[DATA[, GROUP] != levels(DATA[, GROUP])[1], Var], na.rm = TRUE)
        varASD = var(DATA[DATA[, GROUP] == levels(DATA[, GROUP])[1], Var], na.rm = TRUE)
        varTD = var(DATA[DATA[, GROUP] != levels(DATA[, GROUP])[1], Var], na.rm = TRUE)

        if (varTD == 0 & varASD == 0) {
            stdMeanDiff = (mASD - mTD) / (sqrt((varASD + varTD + 0.0001) / 2))
        } else {
            stdMeanDiff = (mASD - mTD) / (sqrt((varASD + varTD) / 2))
        }
    }

    return(stdMeanDiff)
}

smd_dummy_binary <- function(DATA, Var, GROUP)
{
    # refactor variable to get rid of any that might not be there
    DATA[, Var] <- as.factor(DATA[, Var])

    # grab the number of levels
    k <- nlevels(DATA[, Var])

    # grab the level names
    k_levels <- levels(DATA[, Var])

    # this will store the computed values for the current level
    smd_names <- c()
    smds <- c()

    # A function to call to add to the vectors above
    collectSMD <- function(smd, level1)
    {
        smd_names <<- c(smd_names, sprintf("%s", level1) )
        smds <<- c(smds, smd)
    }

    # iterate through levels
    for (i in 1:k) {
        # grab the current level
        current_level <- k_levels[i]

        # grab the groups and the Var between them
        # We set the current level to 1 and the other levels to 0 in a
        # new column called temp. This keeps sample sizes the same.
        temp_data <- DATA[, c(Var, GROUP)]
        temp_data[, "temp"] <- rep(NA, nrow(temp_data))
        temp_data[, "temp"][ temp_data[, Var] == current_level ] <- 1
        temp_data[, "temp"][ temp_data[, Var] != current_level ] <- 0
        temp_data[, "temp"] <- as.factor(temp_data[, "temp"])

        # compupte the binary SMD with stddiff
        d <- stddiff.binary(temp_data, gcol = names(temp_data)[names(temp_data) == GROUP],
                            vcol = names(temp_data)[names(temp_data) == "temp"])

        # Append the SMD value and name to the arrays
        collectSMD(d[5], current_level)

    }

    # return a named array
    names(smds) <- smd_names
    smds
}
