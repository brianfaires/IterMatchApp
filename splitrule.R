#'@title Variable Split rule creation
#'@description Create splitrule for greedy search (should match rpart)
#' matched in control and tretment group
#'@param y response binary variable
#'@param x variable values
#'@param cutpts cutpoints of a variable
#'@param method A method to select best variable
#'@return A list of cutoff and gini index for all random selected variables
#'@export splitrule
#'@author Peter Calhoun

# Why does x and y seem to be different lengths here?
splitrule <-
    function(y,
             x,
             cutpts,
             method,
             split = c("gini", "information")) {
        {
            split <- match.arg(split, c("gini", "information"))
            class1 <- levels(y)[1]

            if (split == "gini") {
                #Gini index: Q_m(T)=p*(1-p)
                stat <- sapply(cutpts, function(cutpt) {
                    splitVar <- (x <= cutpt)
                    pL <- mean(y[splitVar] == class1)
                    pR <- mean(y[!splitVar] == class1)
                    #Weight each daughter node by the number of observations (i.e. n_L or n_R)
                    sumL <- ifelse(pL %in% c(0, 1), 0, sum(splitVar) * (pL * (1 - pL)))
                    sumR <- ifelse(pR %in% c(0, 1), 0, sum(!splitVar) * (pR * (1 - pR)))
                    sumL + sumR
                })
            }
        }
        return(list(cutoff = cutpts[which.min(stat)],
                    stat = min(stat, na.rm = TRUE)))
    }
