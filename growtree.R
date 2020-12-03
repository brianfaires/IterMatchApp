#' Grows tree by using partition() function several times
#' @param id Unique subject id
#' @param depth depth of the tree at particular node
#' @param data Data to grow and prune the tree
#' @param response A binary variable
#' @param subset An optional vector specifying a subset of observations
#'               to be used to grow the tree
#' @param search The defualt is set to greedy search to select a cut-point for a variable
#' @param method Indicates method of handling binary classification
#' @param split Child node splitting measurement. The defualt is set to gini index
#' @param mtry Number of variables considered at each split. The default
#'             is to consider all variables
#' @param nsplit Number: Controls the minimum node size
#' @param minsplit Number: Controls the minimum node siz. The default is 20
#' @param minbucket Number: Controls the minimum number of observations in any terminal node
#' @param maxdepth Number: Controls the minimum number of observations in any terminal node
#' @param surrogates Creates surrogate splits if set to TRUE, otherwise the
#'  default is FALSE. Surrogates = FALSE, assign subjects with missing values randomly to a child node
#' @param useRpart Set to FALSE as defualt not to create forest based off rpart trees
#' @return Party object that contains node information
#' @export growtree
#'@author Peter Calhoun, Afrooz Jahedi, Tristan Hillis
growtree <-
    function(id = 1L,
             depth = 1L,
             data,
             globalOrd,
             response,
             subset,
             search,
             method,
             split,
             mtry,
             nsplit,
             minsplit,
             minbucket,
             maxdepth,
             surrogateargs,
             useRpart) {

        if (depth > maxdepth) {
            return(partynode(id = id))
        }

        y <- data[[response]]

        {
            #Select candidate variables
            varSelected <-
                sort(sample(
                    1:(ncol(data) - 1),
                    mtry,
                    replace = F,
                    prob = rep(1 / (ncol(data)), ncol(data) - 1)
                ))
            vars <-
                data[, !names(data) %in% response, drop = F][, varSelected, drop = FALSE]
            colnames(vars) = varSelected #Have columns represent varid

            # grab only those relavent globally ordinalized parts
            varGlobalOrd <- globalOrd[ varSelected ]

            temp_sp <-
                partition(
                    vars = vars,
                    y = y,
                    subset = subset,
                    data = data,
                    varGlobalOrd = varGlobalOrd,
                    mtry = mtry,
                    search = search,
                    method = method,
                    split = split,
                    nsplit = nsplit,
                    minsplit = minsplit,
                    minbucket = minbucket,
                    surrogateargs = surrogateargs,
                    useRpart = useRpart
                )
            sp <- temp_sp$psplit
        }
        # if ( !is.null(temp_sp$ssplits) & length(temp_sp$ssplits) > 1 ) {

        # }

        if (is.null(sp)) {
            return(partynode(id = id))
        }



        # Split the data
        kidids <- kidids_split(sp, data = data)

        # apply surrogate splits
        if ( !is.null(temp_sp$ssplits) & any(is.na(kidids)) ) {
            #print("using surrogate splits")
            for (i in 1:length(temp_sp$ssplits)) {
                if ( all(!is.na(kidids)) ) {
                    break
                }
                # get how the surrogate splits
                surrogate_kidids <- kidids_split(temp_sp$ssplits[[i]], data = data)

                # apply surrogate answer
                kidids[is.na(kidids)] <- surrogate_kidids[is.na(kidids)]

            }
        }

        # Randomly fill in NA values to the split
        if ( any(is.na(kidids)) ) {
            kidids[is.na(kidids)] <- sample(as.numeric(levels(data[,length(data)])), size=sum(is.na(kidids)), replace=T)
        }

        depth = depth + 1

        kids <- vector(mode = "list", length = max(kidids, na.rm = TRUE))
        for (kidid in 1:length(kids)) {
            s <- subset
            s[kidids != kidid] <- FALSE
            # Node ID
            if (kidid > 1) {
                myid <- max(nodeids(kids[[kidid - 1]]))
            } else {
                myid <- id
            }
            # Start recursion on this daugther node

            kids[[kidid]] <-
                growtree(
                    id = as.integer(myid + 1),
                    depth = depth,
                    data = data,
                    globalOrd = globalOrd,
                    response = response,
                    subset = s,
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
        }

        return(partynode(
            id = as.integer(id),
            split = sp,
            kids = kids,
            surrogates = temp_sp$ssplits,
            info = list(stats = min(info_split(sp)$stats, na.rm =
                                        TRUE),
                        surrogates = length(temp_sp$ssplits))
        ))
    }

