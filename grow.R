#' Grows tree by using partition () function several times
#' @param formula Formula to build decision tree. Formula contains
#'                matching variables that are divided by "+" and
#'                the response on the left of a ~ operator must
#'                be treated as a factor
#' @param data Data to grow and prune the tree
#' @param subset An optional vector specifying a subset of observations
#'               to be used to grow the tree
#' @param search The default is set to greedy search to select a cut-point for a variable
#' @param method Indicates method of handling binary classification
#' @param split Child node splitting measurement. The default is set to Gini index
#' @param mtry Number of variables considered at each split. The default
#'             is to consider all variables
#' @param nsplit Number: controls the number of cutpoints selected. The default
#'  is to consider all distinct cutpoints
#' @param minsplit Number: Controls the minimum node size. The default is 20
#' @param minbucket Number: Controls the minimum number of observations in any terminal node.
#' The default is minsplit / 3
#' @param useRpart Set to FALSE as default not to create forest based off rpart trees
#' @param maxdepth Number: Maximum depth of tree. The default is 30
#' @return list: Tree related information
#' @export grow
#' @author Peter Calhoun, Afrooz Jahedi, Tristan Hillis
grow <-
    function (formula,
              data = NULL,
              subset = NULL,
              search = "greedy",
              method = "class",
              split = "gini",
              surrogateargs = NULL,
              mtry = NULL,
              nsplit = NULL,
              minsplit = 20,
              minbucket = round (minsplit / 3),
              maxdepth = 30,
              useRpart = FALSE) {

        search <- match.arg(search, "greedy")
        method <- match.arg(method,  "class")
        split <-
            match.arg(split, "gini")
        if (is.null(mtry)) {
            mtry <- length(all.vars(formula[[3]]))
        }

        response <- all.vars(formula)[1]
        # Complete cases only.  Rearrange data so that response comes last
        # compData <-
        #     data[complete.cases(data), c(all.vars(formula)[-1], response)]
        data <- data [, c(all.vars(formula)[-1], response)]
        #if(is.factor(data[[response]])){data[[response]]=as.numeric(data[[response]]==levels(data[[response]])[1])}

        global_ordinalized_data <- lapply(data[,-ncol(data)], function(x) {
            ord <- ordinalize (x, data[, ncol(data)], varGlobalOrd = NULL); ord$cutToLvl; } )
        #print(global_ordinalized_data)
        #global_ordinalized_data$ethnicity.desc[["Asian"]]

        if (is.null(subset)) {
            subset <- rep (TRUE, nrow(data))
        }

        # Grow tree
        nodes <-
            growtree(
                id = 1L,
                data = data,
                globalOrd = global_ordinalized_data,
                response = response,
                subset = subset,
                search = search,
                method = method,
                surrogateargs = surrogateargs,
                split = split,
                mtry = mtry,
                nsplit = nsplit,
                minsplit = minsplit,
                minbucket = minbucket,
                maxdepth = maxdepth,
                useRpart = useRpart
            )

        # Compute terminal node number for each observation
        fitted <- fitted_node(nodes, data = data)
        # Return rich constparty object
        ret <- party(
            nodes,
            data = data,
            fitted = data.frame(
                "(fitted)" = fitted,
                "(response)" = data[[response]],
                check.names = FALSE
            ),
            terms = terms(formula)
        )
        as.constparty(ret)
    }
