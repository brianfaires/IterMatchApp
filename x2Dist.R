#' @title Building distance matrix
#' @description  Building distance matrix for pairs of subjects based on certain definition. If two subjects are in the same terminal node, their destance is zero, otherwise thier distance a chi-square-p-value or Fisher exact test of a 2x2 table subjects in those nodes splitted by group variable
#' @param tree Information from a tree
#' @param terminalNode Terminal node index
#' @param treeNum An integer tree index number in the forest
#' @param Group Binary respomse variable
#' @return Distance matrix based on chi-square p-values and fisher exact test p-value
#' @author Afrooz Jahedi
#' @examples x2Dist(rfRF100[[treeNum]]$tree,nodeResponse, treeNum, response)
#' @export x2Dist
#'@author Afrooz Jahedi
 x2Dist <- function(tree, terminalNode, treeNum, Group) {
    pars <- as.list(match.call())

    # Identify terminal nodes in the tree
    terNode <- nodeids(tree, terminal = TRUE)

    # How many subjects in each terminal node
    NObs = sapply(terNode, function(n) {
        nrow(tree[n]$data)
    })

    # Table subjects based on group for each terminal node
    groupObs = t(sapply(terNode, function(n) {
        table(tree[n]$data[Group])
    }))

    # make data frame with terminal node labels, #ASD, #TD in that terminal node
    tabNode <- cbind(terNode, NObs, groupObs)
    # print(tabNode)
    # chisq test for all pairs of terminal nodes
    xPval <- matrix(NA, NROW(tabNode), (NROW(tabNode)))

    for (ter1 in 1:NROW(tabNode)) {
        for (ter2 in 1:NROW(tabNode)) {
            if (tabNode[ter1, 3] < 5 | tabNode[ter1, 4] < 5) {
                xPval[ter1, ter2] <-
                    fisher.test(tabNode[c(ter1, ter2), 3:4], simulate.p.value = T)$p.value
            } else
                xPval[ter1, ter2] <-
                    (chisq.test(tabNode[c(ter1, ter2), 3:4], simulate.p.value = TRUE)$p.value)
        }
    }
     #take out diagonal elements of terminal nodes (e.g.2,2) we don't need them.
    diag(xPval) <- NA
    colnames(xPval) <- terNode
    rownames(xPval) <- terNode

    #setting up chi-square distnca matrix.
    xDistMat <-
        matrix(0,
               nrow = NROW(terminalNode),
               ncol = NROW(terminalNode))
    colnames(xDistMat) <- rownames(terminalNode)
    rownames(xDistMat) <- rownames(terminalNode)

    #find subjects that are in the same terminal node
    for (row in 1:NROW(terminalNode)) {
        #if same ter node, dist=0
        xDistMat[which(terminalNode[row, treeNum] == terminalNode[, treeNum]), row] <-
            0
        ##if not same ter node, what is the ter node for first fix subj
        a <- as.character(terminalNode[row, treeNum])
        #what is the ter node for second non-fix subj
        b <-
            as.character(terminalNode[(terminalNode[row, treeNum] != terminalNode[, treeNum]), treeNum])
        #what is the subj ID for subjects that are not in same ter node
        c <-
            which(terminalNode[row, treeNum] != terminalNode[, treeNum])
        #fill out the distance matrix accordingly
        xDistMat[c, row]  <- 1.0000000000000009 - xPval[a, b]
    }

    return(xDistMat)

    # cat("terminal node","=", terNode,"\n",
    #NObs,"\n",
    #groupObs,"\n",tabNode,"\n",xDistMat)
}
