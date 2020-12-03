#' Ordinalize the continious variable
#' @param x The continuos variable
#' @param y The response binary variable
#' @return Ordinalized value of a variable
#' @author Peter Calhoun
##' @examples
##' mtcars$am=as.factor(mtcars$am)
##' ordinalize(mtcars$mpg,mtcars$am)
ordinalize <- function(x, y, varGlobalOrd, sortCat = TRUE) {
    if (is.factor(x)) {
        x <- factor(x) #Remove factors not listed
        #One can randomly assign a category a distinct numerical value
        if (sortCat == FALSE) {
            cutToLvl <- t(sample(1:length(levels(x))))
            colnames(cutToLvl) = levels(x)
        } else {
            #For binary, sort data by proportion in class 1.  For continuous, sort by means
            if (is.factor(y)) {
                cutToLvl <- prop.table(table(y, x), 2)[1, , drop = FALSE]
            } else {
                cutToLvl <- t(sapply(levels(x), function(z) {
                    mean(y[x == z])
                }))
            }

            # convert cutToLvl to named numeric vector
            if (class(cutToLvl) == "table") {
                temp_vec <- rep(NA, length(cutToLvl))
                names(temp_vec) <- colnames(cutToLvl)
                temp_vec[1:length(temp_vec)] <- as.vector(cutToLvl)
                cutToLvl <- temp_vec
            }
            if ( !is.null(varGlobalOrd) & length(names(varGlobalOrd)) != length(names(cutToLvl)) ) {

                #print(levels(x))
                cutToLvl <- append( cutToLvl[ names(cutToLvl) ], varGlobalOrd[names(varGlobalOrd)[
                !names(varGlobalOrd) %in% names(cutToLvl) ] ] )
            }
        }


        #Convert lvls to numerical value. Slow method. Make this faster later.
        xTemp <- rep(NA, length(x))
        for (lvls in levels(x)) {
            #print(cutToLvl[names(cutToLvl) == lvls])
            xTemp[x == lvls] <- cutToLvl[names(cutToLvl) == lvls]
        }
    } else {
        xTemp <- x
        cutToLvl <- NULL
    }

    return(list(x = xTemp, cutToLvl = cutToLvl))
}
