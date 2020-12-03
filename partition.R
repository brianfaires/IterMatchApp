#' @title Partition a variable
#' @description Find best spiting variable based on gini index and best cutting values
#'              based on delta method
#' @param vars The data frame
#' @param y The response binary variable
#' @param subset An optional vector specifying a subset of observations
#'               to be used to grow the tree
#' @param data 
#' @param varGlobalOrd
#' @param search The defualt is set to greedy search to select a cut-point for a variable
#' @param method Indicates method of handling binary classification
#' @param split Child node splitting measurement. The defualt is set to gini index
#' @param nsplit Number: Controls the minimum node size
#' @param minsplit Number: Controls the minimum node siz. The default is 20
#' @param minbucket Number: Controls the minimum number of observations in any terminal node
#' @param surrogateargs
#' @param useRpart 
#' @return A list contains of variables with their best cutpoint
#' @export partition
#' @author Peter Calhoun, Tristan Hills
#Main function to split data
partition <-
    function(vars,
             y,
             subset,
             data,
             varGlobalOrd,
             mtry,
             search,
             method,
             split,
             nsplit,
             minsplit,
             minbucket,
             surrogateargs,
             useRpart) {
        if ((sum(subset)) < minsplit) {
            return(NULL)
        }

        # only if you want to consider a subset of data and keep a dataframe
        vars <- vars[subset, , drop = FALSE]

        # some stopping conditions if we can't split on mtry vars because of the number
        # of observations being less than minsplit in any var then we return null partition
        if ((sum(sapply(vars, function(x)
            sum(!is.na(  # number of nonmissing values is less than the minsplit.
                x
            ))) < minsplit)) == mtry) {
            return(NULL)
        }
        else{
            ((sum(
                sapply(vars, function(x)
                    sum(!is.na(x))) < minsplit
            )) != mtry)
            vars <-
                vars[, !sapply(vars, function(x)
                    sum(!is.na(x))) < minsplit, drop = F]
        }

        # enforces the number of observations in a node.
        if (sum(sapply(vars, function(x)
            sum(!is.na(x)))) < 2 * minbucket) {
            stop("Can't split tree to satisfy minbucket")
        }


        y <- y[subset]

        #If all y values are the same, rpart will give an error message.  Do not split tree
        if (length(unique(y)) == 1) {
            return(NULL)
        }

        nVars <- NCOL(vars)
        stats <- rep(NA, nVars)
        cutoff <- rep(NA, nVars)
        breakLeft <- vector("list", nVars)

        for (v in 1:nVars) {

            # If randomly picking a subset of categories, do not sort by mean.
            # Would be more likely to select variables when sorted
            if (search == "greedy" &
                !is.null(nsplit)) {
                xTemp <- ordinalize(x = vars[, v], y, varGlobalOrd[[v]], sortCat = FALSE)
            } else {
                xTemp <- ordinalize(x = vars[, v], y, varGlobalOrd[[v]])
            }
            x <- xTemp$x
            #If all x values the same, do not check optimal split
            if (abs(max(x, na.rm = T) - min(x, na.rm = T)) > 1e-8) {

                #Use greedy search in this case (or set minsplit >= 5)
                if (search == "greedy") {
                    #Note: Rpart uses impurity gain
                    if (useRpart == FALSE) {
                        cutpts <- findCutpts(x, minbucket, nsplit)
                        #It is possible (unlikely) no cutpoint can satisfy minbucket
                        if (!is.null(cutpts)) {
                          # create the split
                            mod <-
                                splitrule( # afrooz function: split by gini index
                                    y = y[!is.na(x)], # likely bug was here as y = y
                                    x = x[!is.na(x)],
                                    cutpts = cutpts,
                                    method = method,
                                    split = split
                                )
                            stats[v] <- mod$stat
                            if (is.factor(vars[, v])) {

                                breakLeft[[v]] <- getFactorSplitIndex(vars, v,
                                                                      xTemp$cutToLvl, mod$cutoff)
                                #print(breakLeft[[v]])
                            } else {
                                cutoff[v] <- mod$cutoff
                            }
                        }
                    } else {
                        # Can use rpart to do greedy search (can't do nsplit)
                        x <- vars[, v]

                        mod <- rpart(
                            y ~ x,
                            method = method,
                            parms = list(split = split),
                            control = rpart.control(
                                minsplit = 2,
                                minbucket = minbucket,
                                cp = -1,
                                xval = 0,
                                maxdepth = 1,
                                maxcompete = 1,
                                maxsurrogate = 0
                            )
                        )
                        # It is possible (very unlikely) that no single split
                        #can satisfy minbucket constraint
                        if (!is.null(mod$splits)) {
                            stats[v] <- -mod$splits[3]
                            if (is.factor(vars[, v])) {
                                # csplit - 1 left, 2 not present, 3 right.  Change to 1, NA, 2
                                breakLeft[[v]] <-
                                    as.vector(mod$csplit)
                                breakLeft[[v]][mod$csplit == 2] = NA
                                breakLeft[[v]][mod$csplit == 3] = 2L
                            } else {
                                cutoff[v] <- mod$splits[4]
                            }
                        }
                    }
                } else {
                    stop("Unexpected search")
                }
            }
        }

        # If each candidate variable cannot be split (e.g. cannot satisfy minbucket), return null
        if (all(is.na(stats))) {
            return(NULL)
        }

        # make primary split
        if  (is.na(cutoff[which.min(stats)])) {
            psplit <- partysplit(
                varid = which.min(stats),
                #varid = as.integer(colnames(vars)[which.min(stats)]),
                index = breakLeft[[which.min(stats)]],
                info = list(stats = stats)
            )
        } else {
            psplit <- partysplit(
                varid = which.min(stats),
                #varid = as.integer(colnames(vars)[which.min(stats)]),
                breaks = cutoff[which.min(stats)],
                info = list(stats = stats)
            )
        }
        col_na_mask <- !is.na(vars[, psplit$varid])
        convention <- checkTreeConvention(y[col_na_mask],
                                          kidids = kidids_split( psplit, data = vars[col_na_mask, ] ),
                                          psplit$index)
        # update the primarysplit
        if (convention$reorder) {
            psplit <- partysplit(varid = psplit$varid, breaks = psplit$breaks, info = psplit$info,
                             index = convention$index)
            #print("Switch index in partysplit")
        }
        # Build surrogate splits
        surrogate_splits <- NULL
        if ( !is.null(surrogateargs) ) {

            if ( is.na(cutoff[which.min(stats)]) ) {
               cut <- breakLeft[[which.min(stats)]]
            } else {
               cut <- cutoff[which.min(stats)]
            }

            surrogate_splits <-
                            makeSurrogates( surrogateargs,
                                cbind(vars, y),
                                varGlobalOrd = varGlobalOrd,
                                psplit,
                                which( !(which.min(stats) == 1:length(stats)) ),
                                minbucket,
                                nsplit,
                                search )

        }

        # set global primary split varid
        psplit$varid <- as.integer(colnames(vars)[which.min(stats)])

        return(list("psplit"= psplit, "ssplits"=surrogate_splits))

    }
