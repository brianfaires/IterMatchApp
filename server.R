library(shiny)
library(optmatch)
library(stddiff)
library(partykit)
library(ggplot2)
library(reshape2)
library(ipc)
library(future)
plan(multisession) # Works in windows
#plan(multicore) # Not supported in windows

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    source('checkTreeConvention.R', local=TRUE)
    source('computeAgreement.R', local=TRUE)
    source('findCutpts.R', local=TRUE)
    source('formattedSummaryPrint.R', local=TRUE)
    source('getFactorSplitIndex.R', local=TRUE)
    source('grow.R', local=TRUE)
    source('growtree.R', local=TRUE)
    source('ordinalize.R', local=TRUE)
    source('partition.R', local=TRUE)
    source('smd.R', local=TRUE)
    source('splitrule.R', local=TRUE)
    source('summaryMatch.R', local=TRUE)
    source('summaryNonMissingPair.R', local=TRUE)
    source('surrogateAgreement.R', local=TRUE)
    source('surrogates.R', local=TRUE)
    source('x2Dist.R', local=TRUE)

    # Settings for iterMatch
    MIN_GROUP_SIZE = 5
    
    
    # Queue for multi-process executiong and real time graph updating
    queue <- shinyQueue()
    queue$consumer$start(1000) # Execute signals every 1000 milliseconds

    # Set flag when file is uploaded; for conditionalPanel in UI
    output$fileUploaded <- reactive({ return(!is.null(input$input_file)) })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

    # Set flag when RF/DM button is pressed
    output$algo1Complete <- eventReactive(input$btnLaunch1, { return(TRUE) })
    outputOptions(output, 'algo1Complete', suspendWhenHidden=FALSE)

    # Set flag when IterMatch button is pressed
    output$algo2Started <- eventReactive(input$btnLaunch2, { return(TRUE) })
    outputOptions(output, 'algo2Started', suspendWhenHidden=FALSE)

    output$algo2Complete <- reactive({ return(iterMatchComplete()) })
    outputOptions(output, 'algo2Complete', suspendWhenHidden=FALSE)
    observeEvent(iterMatchComplete(), {
        if(iterMatchComplete() == TRUE) {
            # Load numericControl over results, which then populates the table of matched participants
            numIters = length(allResultsReactive())
            if(numIters > 0) {
                updateNumericInput(session, "iterationNumber", max = numIters, value = numIters)
            }
            
            # re-enable controls
            shinyjs::enable("iterationNumber")
            shinyjs::enable("input_file")
            shinyjs::enable("downloadData")
            shinyjs::enable("btnLaunch2")
            shinyjs::enable("smdMethod")
            shinyjs::enable("smdAll")
            shinyjs::enable("smdThresholds")
            shinyjs::enable("exportResults")
            shinyjs::enable("exportAllResults")
            shinyjs::enable("exportLog")
        }
    })
    
    observeEvent(input$btnLaunch2, {
        shinyjs::disable("iterationNumber")
        shinyjs::disable("input_file")
        shinyjs::disable("downloadData")
        shinyjs::disable("btnLaunch2")
        shinyjs::disable("smdMethod")
        shinyjs::disable("smdAll")
        shinyjs::disable("smdThresholds")
        shinyjs::disable("exportResults")
        shinyjs::disable("exportAllResults")
        shinyjs::disable("exportLog")
        
    })
    
    ##### Things that are set when file is uploaded - Order is coded to match the event chain #####
    # Reactive version of the data set
    fileData <- reactive({
        if (is.null(input$input_file))
            return(NULL)
        
        output$errorText = NULL
        retVal <- read.csv(input$input_file$datapath, check.names = TRUE, stringsAsFactors = TRUE, sep=",", strip.white = TRUE,
            na.strings = c("", "na", "nA", "Na", "NA", "nan", "naN", "nAn", "nAN", "Nan", "NaN", "NAn", "NAN"))
        retVal <- retVal %>% distinct()
        
        # re-enable variable selections in case this is not the first upload
        shinyjs::enable("labelVar")
        shinyjs::enable("responseVar")
        shinyjs::enable("checkboxes")
        shinyjs::enable("checkboxes2")
        shinyjs::enable("btnLaunch1")
        shinyjs::enable("nTrees")
        shinyjs::enable("surrogates")
        shinyjs::disable("btnLaunch2")
        
        return(retVal)
    })

    # Set the values in the label dropdown; Auto-detect the candidate variables that have unique values for each subject
    observeEvent(input$input_file, {
        rawData <- fileData()
        allVarNames <- names(rawData)
        numRows = nrow(rawData)
        allCandidates <- list()
        
        firstPick = NULL
        for(i in 1:length(allVarNames)) {
            uniques <- length(unique(rawData[,allVarNames[[i]]]))
            if(uniques == numRows) {
                allCandidates <- c(allCandidates, allVarNames[[i]])
                if(is.null(firstPick)) {
                    # See if any entries are not integers
                    for(j in 1:length(rawData[,allVarNames[[i]]])) {
                        entry = rawData[j, allVarNames[[i]]]
                        if(!is.integer(entry) && !grepl("^[0-9]{1,}$", entry)) {
                            firstPick = allVarNames[[i]]
                        }
                    }
                }
            }
        }
        if(length(allCandidates) == 0) {
            output$errorText = renderText("No valid label variables found. A unique value is required for each data row.")
        }
        else if(is.null(firstPick)) {
            updateSelectInput(session, "labelVar", choices = allCandidates, selected = allCandidates[[1]])
        }
        else {
            updateSelectInput(session, "labelVar", choices = allCandidates, selected = firstPick)
        }
    })

    # List of vars sans Label var
    sansLabel <- reactive({
        allVars = names(fileData())
        if(!(input$labelVar %in% allVars)) { return(NULL) }
        withoutLabel <- allVars[allVars != input$labelVar]
        return(withoutLabel)
    })

    # Set the values in the response dropdown; Auto-detect the binary variables; first with 0/1 is default, unless none and then first is default
    observeEvent(sansLabel(), {
        choice_list <- sansLabel()
        rawData <- fileData()
        
        possLabels <- list()
        defaultLabel <- ""
        secondaryDefault <- ""
        
        for(i in 1:length(choice_list)) {
            uniques <- sort(unique(rawData[,choice_list[[i]]]))
            numUniques <- length(uniques)
            if(numUniques == 2) {
                possLabels <- c(possLabels, choice_list[[i]])
                if(defaultLabel == "") {
                    if(uniques[[1]] == "0" && uniques[[2]] == "1") {
                        defaultLabel = choice_list[[i]]
                    }
                    else if(uniques[[2]] == "0" && uniques[[2]] == "2") {
                        secondaryDefault = choice_list[[i]]
                    }
                }
            }
        }
        
        if(length(possLabels) < 1) {
            output$errorText <- renderText("No valid response variables found. Response variables must be binary and exist for all data rows.")
        }
        else if(defaultLabel != "") { updateSelectInput(session, "responseVar", choices = possLabels, selected = defaultLabel) }
        else if(secondaryDefault != "") { updateSelectInput(session, "responseVar", choices = possLabels, selected = secondaryDefault) }
        else { updateSelectInput(session, "responseVar", choices = possLabels, selected = possLabels[[1]]) }
    })

    # List of vars sans Label and Response vars
    sansResponse <- reactive({
        allVars = sansLabel()
        if(is.null(allVars) || !(input$responseVar %in% allVars)) { return(NULL) }
        withoutResponse <- allVars[allVars != input$responseVar]
        return(withoutResponse)
    })

    # Set checkboxes for including/excluding vars
    observeEvent(sansResponse(), {
        output$checkboxes <- renderUI({ checkboxGroupInput("checkboxes", "Included Variables:", choices = sansResponse(), selected = sansResponse()) })
    })
    
    # List of vars included in data analysis (excluding label and response)
    includedVars <- reactive({
        allVars = input$checkboxes
    })
    
    # Set per-variable controls (for included variables only) - Arbitrary limit of 20 unique values for factors
    observeEvent(includedVars(), {
        choice_list <- includedVars()
        rawData <- fileData()
        
        selected_list <- list()
        for(i in 1:length(choice_list)) {
            if(is.factor(rawData[,choice_list[[i]]]) || length(unique(rawData[,choice_list[[i]]])) < 20 || is.character(rawData[,choice_list[[i]]])) {
                selected_list <- c(selected_list, choice_list[[i]])
            }
        }

        # Checkboxes for identifying categorical vars
        output$checkboxes2 <- renderUI({
            checkboxGroupInput("checkboxes2", "Categorical Variables:", choices = choice_list, selected = selected_list)
        })

        # SMD threshold slider controls
        smds <- list()
        for(i in 1:length(choice_list)) {
            smds[[i]] = sliderInput(inputId = paste("smd", i, sep=""), label=choice_list[[i]], value=0.1, min=0, max=1, step=0.001)
        }
        output$smdThresholds <- renderUI(smds)
    })

    # Set all smdThresholds when the master control is adjusted
    observe({
        val <- input$smdAll
        for(i in 1:length(includedVars())) {
            updateSliderInput(session, inputId = paste("smd", i, sep=""), value = val)
        }
    })

    ##### Cleaning data and starting algorithm #####
    cleanedData = reactive({
        # Load data and exlude ommitted columns
        fullDF <- fileData()
        df <- fullDF[, c(includedVars(), input$responseVar)]

        # Define response and other categorical variables as factors
        df[, input$responseVar] <- factor(df[, input$responseVar], labels = c("0", "1"))
        
        # Special case when only 1 categorical variable!
        if(length(input$checkboxes2) == 1) {
            df[, input$checkboxes2] <- factor(df[, input$checkboxes2])
        }
        else {
            df[, input$checkboxes2] <- lapply(df[, input$checkboxes2], factor)
        }
        

        # Give label names to the data to be able to track subjects
        rownames(df) <- fullDF[[input$labelVar]]
        
        # Sort subjects as 1 first, 0 second.
        df <- tibble::rownames_to_column(df, "tempRowNameColumn") # dplyr does not preserve rownames
        sortedDF <- arrange(df, df[[input$responseVar]])
        rownames(sortedDF) <- sortedDF$tempRowNameColumn
        sortedDF$tempRowNameColumn <- NULL

        return(sortedDF)
    })
    
    # Row names from data frame
    rowNames = reactive({
        fullDF <- fileData()
        return(fullDF[, input$labelVar])
    })

    # Formula for classification
    formula <- reactive({
        f <- paste(input$responseVar, " ~ ", paste(as.vector(includedVars()), collapse = "+"), sep="")
        return(as.formula(f))
    })

    # All user-specified SMD thresholds
    smdValues <- reactive({
        choice_list <- includedVars()
        smds <- list()
        for(i in 1:length(choice_list)) {
            smds[[i]] = input[[paste("smd", i, sep="")]]
        }
        return(smds)
    })
    
    ##### Generate RF on button press #####
    dissMatrix <- reactiveValues(data = NULL)
    observeEvent(input$btnLaunch1, {
        cd = cleanedData()
        f = formula()
        nt = input$nTrees
        surrogates = input$surrogates
        
        shinyjs::disable("input_file")
        shinyjs::disable("downloadData")
        shinyjs::disable("labelVar")
        shinyjs::disable("responseVar")
        shinyjs::disable("checkboxes")
        shinyjs::disable("checkboxes2")
        shinyjs::disable("nTrees")
        shinyjs::disable("surrogates")
        shinyjs::disable("btnLaunch1")
        shinyjs::disable("btnLaunch2")
        
        dissMatrix$data <- GenerateDissMatrix(cleanedData(), formula(), input$nTrees, surrogates = input$surrogates, iseed = 1, outputfile = NULL, displayProgressTracking = TRUE)
        gc() # Cleanup memory from GenerateDissMatrix()
        
        shinyjs::enable("input_file")
        shinyjs::enable("downloadData")
        shinyjs::enable("btnLaunch2")
        
        updateTabsetPanel(session, "tabPanel", selected = "IterMatch")
    })

    ##### Run iterMatch on button press #####
    # Storing results of iterMatch() in reactive context
    allResultsReactive <- reactiveVal()
    iterMatchComplete <- reactiveVal()

    inter = NULL;
    # Launch iterMatch on button press; using ipc package to keep UI updating as this runs
    observeEvent(input$btnLaunch2, {
        shinyjs::disable("input_file")
        shinyjs::disable("downloadData")
        shinyjs::disable("btnLaunch2")
        shinyjs::disable("smdMethod")
        shinyjs::disable("smdAll")
        shinyjs::disable("smdThresholds")
        
        updateTabsetPanel(session, "tabPanel", selected = "Results")

        cleanData <- cleanedData()
        form <- formula()
        dissData <- dissMatrix$data
        labelVar <- input$labelVar
        smdVals <- smdValues()
        smdMet <- input$smdMethod
        
        # Calc max iterations
        maxProgress <- min(table(cleanData[[input$responseVar]])) - MIN_GROUP_SIZE
        futureTimer <- AsyncProgress$new(message = "Launching iterMatch...", min=0, max=maxProgress)

            parallelProcess <- future({
                queue$producer$fireAssignReactive("iterMatchComplete", FALSE) # Signal no completion in case this is being re-executed
                
                # async call to iterMatch outside of reactive context
                
                iterMatch(cleanData, form, dissData, labelVar, smdVals, smdMet, asyncTimer = futureTimer)
                
                futureTimer$close()
                
                queue$producer$fireAssignReactive("iterMatchComplete", TRUE) # Signal completion
                
                #quit() # Use with plan(multisession)
            })

        rm(cleanData)
        rm(form)
        rm(dissData)
        gc()
        NULL # Ensure that nothing is returned
    })

    # All SMD values from all iterations run
    allSmds <- reactive({
        allResults <- allResultsReactive()
        if(is.null(allResults)) { return(NULL) }
        
        blFirst <- 0
        iter <- 1
        
        for(result in allResults) {
            #smds <- lapply(result, function(x) unlist(x$smd))
            smds <- list()
            smdNames <- list()
            
            # factors will have multiple smds, which are named - flatten everything to one named list
            for(j in 1:length(result$smd)) {
                smd <- result$smd[[j]]
                if(length(smd) == 1) {
                    smds <- c(smds, smd)
                    smdNames <- c(smdNames, result$varname[[j]])
                }
                else {
                    flat <- unlist(smd)
                    smds <- c(smds, flat)
                    theseNames <- sapply(names(flat), function(x) paste(result$varname[[j]], ".", x, sep=""))
                    smdNames <- c(smdNames, theseNames)
                }
            }
            
            names(smds) <- smdNames
            #rownames(smds) <- paste("Iteration", iter, sep=".")
            
            # Build data frame
            if(blFirst < 1) {
                l_allSmds <- as.data.frame(smds)
                blFirst <- 1
            }
            else {
                l_allSmds <- rbind(l_allSmds, as.data.frame(smds))
            }
            
            iter <- iter+1
        }
        
        #allSmds <- sapply(1:length(allResults), function(x) lapply(allResults[[x]], function(y) unlist(y$smd)))
        l_allSmds$iteration <- 1:(iter-1)
        return(l_allSmds)
    })

    # List of all displayed variables - includes one entry for each value of each factor
    allDisplayedVars <- reactive({
        dfSMD <- allSmds()
        return(names(dfSMD[, !names(dfSMD) %in% c("iteration")]))
    })

    ##### Generates the text summary for a given iteration
    iterationTextResults <- function(iterNum) {
        results = allResultsReactive()
        iterRes = results[[iterNum]]
        
        strHeader = paste("----- Results for Iteration", iterNum, "-----", sep=" ")
        strGroupSize = paste("Group Size =", length(iterRes$matches$ID)/2, sep=" ")
        
        strVars = paste("Matched Variables:", paste(iterRes$varname, collapse=","), sep=",")
        strThresholds = paste("SMD Thresholds:", paste(smdValues(), collapse=","), sep=",")
        
        fixedSMDs <- lapply(iterRes$smd, function(x) {
            if(length(x) > 1) {
                varNames <- names(x)
                retVal = "("
                for(i in 1:length(x)) {
                    retVal = paste(retVal, varNames[[i]], "=", x[[i]], ";", sep="")
                }
                return(paste(retVal,")",sep=""))
            }
            #else if(grepl(",", x)) { gsub(",",";", gsub("`","",gsub("c(","(", x))) }
            else { return(x) }
        })
        
        strSMD = paste("SMDs", paste(fixedSMDs, collapse=","), sep=",")
        strPval = paste("P-values", paste(iterRes$pvalue, collapse=","), sep=",")
        
        
        allIDs <- iterRes$matches$ID
        negatives <- allIDs[1:(length(allIDs)/2)]
        positives <- allIDs[(length(allIDs)/2 + 1):length(allIDs)]
        
        strNeg = paste("Group1", paste(negatives, collapse=","), sep=",")
        strPos = paste("Group2", paste(positives, collapse=","), sep=",")
        fileResults = paste(strHeader, strGroupSize, "", strVars, strThresholds, "", strSMD, strPval, "",
                            "The following lists are ordered to reflect the matched pairs", strNeg, strPos,sep="\n")
        return(fileResults)
    }
    
    #### Turning results into plot #####    
    output$allIterationResultsPlot <- renderPlotly({
        if(length(allSmds()) < 1) { return(NULL) }

        smdsLongFormat <- melt(data = allSmds(),
                               id.vars = c("iteration"),
                               measure.vars = allDisplayedVars(),
                               variable.name = "variable",
                               value.name = "value")

        p <- ggplot(data=smdsLongFormat, aes(x=iteration, y=value, colour=variable)) + geom_line() + geom_point() + xlab("Iteration") + ylab("SMD") + theme(legend.title = element_blank())
        ggp <- ggplotly(p)
        ggp %>% config(displayModeBar = "static", displaylogo = FALSE)
        return(ggp)
    })

    ##### Downloading cleaned data #####
    output$downloadData <- downloadHandler(
        filename = function() {
            if (is.null(input$input_file))
                return('Empty data file.csv')
            else {
                #noExt <- sub('.csv$', '', basename(input$input_file$name))
                paste(basename(input$input_file$name), ' - Cleaned.csv', sep='')
            }
        },
        content = function(file) {
            cd <- cleanedData()
            cd[[input$labelVar]] = rownames(cd)
            
            #Sort columns
            finalDF <- cd %>% select(input$labelVar, input$responseVar, everything())
            write.csv(finalDF, file, row.names = FALSE)
        }
    )

    ##### Generate a dynamic list of the pairs #####
    output$iterationResults <- renderTable({
        results <- allResultsReactive()
        
        if(input$iterationNumber <= 0 || input$iterationNumber > length(results)) { return(NULL) } # Sanity checks

        thisResult = results[[input$iterationNumber]]
        allIDs <- thisResult$matches$ID
        Group1 <- allIDs[1:(length(allIDs)/2)]
        Group2 <- allIDs[(length(allIDs)/2 + 1):length(allIDs)]
        
        df = as.data.frame(Group1)
        df$Group2 <- Group2
        return(df)
    })
    
    
    ##### Downloading results for one iteration #####
    output$exportResults <- downloadHandler(
        filename = function() { return(paste("Iteration", input$iterationNumber, "-Results.csv", sep="")) },
        content = function(file) { 
            allText <- paste(iterationTextResults(input$iterationNumber), "\n", sep="\n")
            writeLines(allText, file)
        }
    )

    ##### Downloading results for all iterations #####
    output$exportAllResults <- downloadHandler(
        filename = function() { return("AllIterations.csv") },
        content = function(file) {
            resultsContent = ""
            for(i in 1:length(allResultsReactive())) {
                resultsContent = paste(resultsContent, iterationTextResults(i), "\n\n", sep="")
            }
            writeLines(resultsContent, file)
        }
    )
    
    ##### Downloading text log file #####
    output$exportLog <- downloadHandler(
        filename = "VerboseLog.txt",
        content = function(file) { file.copy("output.txt", file)}
    )
    


################## Original iterMatch() was broken into the following two subroutines, which were then updated to work with Shiny. ################## 

#'@title A 1-1 iterative optimal matching algorithm for data with missing values
#'@description Creates a 1-1 matched sample between two groups. After building a
#'  distance matrix based on random forest and dealing with missing values using surrogate splits, it
#'  uses a matching method called "OPTMATCH" to select subset of subjects that are
#'  matched using desirable balance threshold.
#'@param data Dataframe of subjects and variables
#'@param formula Formula which defines the response and matched variables
#'  Matching variables have to be separated by "+", and response varaible is
#'  separated by"~"
#'@param nTree An integer number of trees in the random forest
#'@param distance Claculating distance between two subjects i and j. Possible
#'  values is "0-1" distance where distance of subjects in the same
#'  terminal nodes are set to be zero, one, otherewise.
#'@param ID Unique subject ID
#'@param thresh A vector of real values defining in SMD threshold for matching
#'  variables in the order that they appear in formula
#'@param methodSMD Two methods are proposed to calculate SMD, "method-1" and
#'  "method-2"
#'@param surrogates Creates surrogate splits if set to TRUE, otherwise the
#'  default is FALSE
#'@param rfmtry Number of random variables to split at each node. The default
#'   value is set to 3
#'@param match.tol Specifies the extent to which fullmatch's output is
#'  permitted to differ from an optimal solution to the original problem. This
#'@param outputfile An output file containing results from all itterations and their balance measures
#'@return List Contains p-values and standardized mean difference for each variable after
#'  creating paired sample matching in multiple iterations and matched sample
#'@import dplyr optmatch partykit rlist stddiff
#'@export iterMatch
#'@usage iterMatch (data, formula, nTree,distance = c("0-1","p-value"),
#'   ID, thresh,methodSMD = c("method-1", "method-2"), surrogates = FALSE,
#'   rfmtry = 3, match.tol = 0.001, outputfile = NULL)
#'@examples form <-DX_GROUP ~ RMSD + AGE_AT_SCAN + PIQ + SEX + HANDEDNESS_SCORES
#'  iterMatch (data = data_1, formula = form, nTree = 100,
#'  distance = "p-value", ID = "SUB_ID", thresh = c(0.1, 0.1, 0.1, 0.1, 0.1),
#'  methodSMD = "method-1", surrogates = T, outputfile = "output")
#'@author Afrooz Jahedi, Tristan Hills

    GenerateDissMatrix <-
        function (data,
                  formula,
                  nTree,
                  rfmtry = 3,
                  surrogates = FALSE,
                  iseed = 1,
                  outputfile = NULL,
                  displayProgressTracking = FALSE) {
            
            
            #==== setting parameters ====
            #Identify the response variable from the formula
            response <- all.vars(formula)[1]
            
            #Check if the response is a factor
            if (!is.factor(data[[response]])) {
                stop("response variable has to be a factor")
            }
            
            if (nlevels(data[[response]]) != 2) {
                stop("response should have only 2 levels")
            }
            
            # if ( any( sort(unique(data[[response]]), decreasing = T) != c(1,0) ) ) {
            #   stop("response must be coded as 0 and 1")
            # }
            
            # arrange columns with the smaller group being first. Don't want user to have to do this
            count <- table(data[[response]])
            if ( levels(data[[response]])[which.min(table(data[[response]]))] < 1 ) {
                #data <- data[ order(data[[response]]), ]
                data <- data[ order(data[[response]],decreasing = T), ]
            } else {
                data <- data[ order(data[[response]], decreasing = T), ]
                
            }
            
            
            #Form dataframe using variables from the formula and response variable
            selData <- data[, all.vars(formula)]
            
            # Check selData does not consist of any
            for (x in names(selData)) {
                if (x != response & is.character(selData[, x])) {
                    stop(sprintf("%s column needs to be integer, numeric, or factor", x))
                }
            }
            
            
            #Form dataframe with only matching variables
            selVars <-
                selData[, !names(selData) %in% response, drop = F]
            
            
            # Set the surrogate arguments
            surrogateargs <- NULL # make surrogate argument null
            if ( is.list(surrogates) || surrogates ) {
                # populate list with default arguments
                surrogateargs <- list( "maxsurrogate" = 5 )
                
                if ( is.list(surrogates) ) {
                    # check that the names in this list are correct
                    if ( !(names(surrogates) %in% names(surrogateargs)) )  {
                        stop("Not valid surrogate arguments")
                    }
                    
                    # set the surrogateargs from the list
                    surrogateargs[names(surrogates)] <- surrogates[names(surrogates)]
                }
            }
            
            
            #Balance Checking Before Matching#
            #Check the balance of two groups for all matching variables
            print(summary(selData))
            
            S = list()
            for (i in 1:length(selVars)) {
                tryCatch( expr = {
                    S[[i]] = summaryMatch(selData, names(selVars)[i], response)
                    allVarsSummary <- c(S, S[[i]])
                },
                error=function(e){
                    S[[i]] = NA
                    allVarsSummary <- c(S, S[[i]])
                })
            }
            
            VarName <- sapply(S, function(x) x$varname)
            SMD <- lapply(S, function(x) x$smd)
            Pvalue <- sapply(S, function(x) x$pval)
            formattedSummaryPrint(VarName, SMD, Pvalue)
            
            #stop()
            
            
            print("Creating random forest ...")
            #Set seed and create the random forest
            set.seed(iseed)
            ntrees <- nTree
            
            if(displayProgressTracking) {
                progress <- shiny::Progress$new(min=0, max=ntrees)
                on.exit(progress$close())
            }
            else {
                progress <- NULL
            }
            
            rfRF100 <-
                growRF(
                    ntrees = ntrees,
                    formula = formula,
                    training = selData,
                    search = "greedy",
                    method = "class",
                    split = "gini",
                    surrogateargs = surrogateargs,
                    mtry = rfmtry,
                    nsplit = NULL,
                    minsplit = 20,
                    maxdepth = 10,
                    minbucket = 6,
                    progressIndicator = progress,
                    bootstrap = FALSE
                )
            
            if(is.null(rfRF100)) { return(NULL) }
            
            # Setting the number of treated group
            ASD <- selData[, response] == 1
            nMin = NROW(selData[ASD,])
            
            
            #==== Set distance definition ====
            # Chi-square p-value
            
            progress$set(message="Sending data down...", value=0)
            print("Sending data down ...")
            
            # Extracting terminal node of all subjects across all trees
            nodeResponse <- sapply(rfRF100, function(x) {
                predict(x$tree, newdata = selData, type = "node")
            })
            # Initiating distance matrix
            sumXDistMat = matrix(0,
                                 nrow = NROW(nodeResponse),
                                 ncol = NROW(nodeResponse))
            
            # Give labels to the rows of the distance matrix
            colnames(sumXDistMat) <- rownames(nodeResponse)
            
            #Give labels to the rows of the distance matrix
            rownames(sumXDistMat) <- rownames(nodeResponse)
            
            #Set number of trees
            ntrees <- nTree
            
            print("Calculate the distance ...")
            progress$set(message="Calculating distances: ", value=0, detail=paste("(0/", ntrees, ")", sep=""))
            
            #Calculate the the defined distance
            t <- proc.time()
            for (treeNum in 1:ntrees) {
                sumXDistMat <- sumXDistMat + x2Dist(rfRF100[[treeNum]]$tree,
                                                    nodeResponse, treeNum, response)
                if (treeNum %% 50 == 0) {
                    print( sprintf("Distance on tree %d (%.2f s) ... ", treeNum, (proc.time()- t)[3]) )
                }
                progress$inc(1, detail=paste("(", treeNum, "/", ntrees, ")", sep=""))
            }
            
            progress$set(message="Cleaning up...", value=ntrees)
            # Calculate the average distance
            xDistFor <- sumXDistMat / ntrees
            
            # Give unique label to the created distance matrix's rows and columns
            rownames(xDistFor) <- rownames(selData)
            colnames(xDistFor) <- rownames(selData)
            
            # Subset the distance matrix conntains only ASD as rows and TD as columns
            if (NROW(selData[ASD, ]) < NROW(selData[!ASD, ])) {
                x2distForSel <- xDistFor[1:nMin, (nMin + 1):ncol(xDistFor)]
                rownames(x2distForSel) <-
                    rownames(selData[1:nMin, ])
                colnames(x2distForSel) <-
                    rownames(selData[(nMin + 1):ncol(xDistFor), ])
                
            } else{
                x2distForSel <- xDistFor[1:nMin, (nMin + 1):ncol(xDistFor)]
                rownames(x2distForSel) <-
                    rownames(selData[1:nMin, ])
                colnames(x2distForSel) <-
                    rownames(selData[(nMin + 1):ncol(xDistFor), ])
                x2distForSel <-t(x2distForSel)
            }
            
            # Distance matrix
            return(x2distForSel)
        }
    
    
    iterMatch <-
        function (data,
                  formula,
                  DM,
                  ID,
                  thresh,
                  methodSMD,
                  match.tol = 0.001,
                  outputfile = "output.txt",
                  asyncTimer = NULL) {
            
            iterNum = 1
            allSmds <- list()
            allIncludedSubjects <- list()
            allResults <- list()
            
            #Identify the response variable from the formula
            response <- all.vars(formula)[1]
            
            #Form dataframe using variables from the formula and response variable
            selData <- data[, all.vars(formula)]
            rm(data)
            selVars <- selData[, !names(selData) %in% response, drop = F]
            
            ptm <- proc.time()
            
            
            matchSampleFile <- "matchedSample.csv"
            if (!is.null(outputfile)) {
                sink(outputfile)
                
                matchSampleFile <-
                    file.path(dirname(outputfile),
                              paste0(strsplit(basename(outputfile), ".", fixed=T)[[1]][1], "_matches.csv"))
            }
            
            
            # SMD method
            if (methodSMD == "method-1") {
                
                print("Entering method-1 ...")
                # Create a 1-1 match with sum of discrepancies for all treatments
                # and controls placed into the same matched sets.
                print("Starting fullMatch ...")
                optMatch <-
                    fullmatch(
                        DM,
                        min.controls = 1,
                        max.controls = 1,
                        omit.fraction = NULL,
                        tol = match.tol,
                        subclass.indices = NULL
                    )
                # Summary of optimal matched sample
                summary(optMatch)
                print("Done with fullMatch ...")
                
                print("Writing output to subjMatch.txt ...")
                # Write output of matched treated vs control into a file (capturing the output from terminal)
                subMatchFile <- sprintf("subjMatch_%s.txt", gsub("[.]", "-", format(Sys.time(), "%Y%m%d-%H-%M-%OS3")))
                capture.output(print(optMatch, grouped = T), file = subMatchFile)
                
                print("Reading in subjMatch.txt ...")
                # Read in the captured output as an object
                subjMatch <-
                    read.table(
                        subMatchFile,
                        header = T,
                        colClasses = c("character", "character")
                    )
                
                
                print(subjMatch)
                if (file.exists(subMatchFile)) {
                    file.remove(subMatchFile)
                }
                
                print("Binding rows ...")
                # Binding row wise of subject ID (First treated then control)into a one column
                splitSubj <-
                    do.call("rbind", strsplit(c(
                        as.character(subjMatch$Group),
                        as.character(subjMatch$Members)
                    ), ","))
                
                # Create a dataframe consisits of two columns of pair number and unique subject ID
                print("Creating dataframe ...")
                splitSubjMem <-
                    data.frame(pairNum = rep(1:length(levels(optMatch)),
                                             2), SUB_ID = splitSubj)
                print("Add subject ID data ...")
                # Add a column of subject ID to the data
                SelDataID <- data.frame(selData, SUB_ID = rownames(selData))

                print("Merge data frame of matching variables ...")
                # Merge dataframes of all matching variables, pair number by Unique subject ID
                optData <- merge(SelDataID, splitSubjMem, by = "SUB_ID")
                
                # Sorting data based on group
                optData <-
                    (optData[order(optData[, response], decreasing = F),])
                
                cat("0-iteration", "\n")
                
                a <- optData$SUB_ID
                write.csv(cbind(data.frame(ID = a),selData[a, ]), file = matchSampleFile, row.names = F)
                
                # Calculate the matched sample balance
                print("Calculate sample balance")
                S = list()
                for (i in 1:length(selVars)) {
                    tryCatch( expr = {
                        S[[i]] = summaryMatch(optData, names(selVars)[i], response)
                        allVarsSummary <- c(S, S[[i]])},
                    error=function(e){ 
                        S[[i]] = NA
                        allVarsSummary <- c(S, S[[i]])
                    })
                }
                VarName <- sapply(S, function(x) x$varname)
                SMD <- lapply(S, function(x) x$smd)
                Pvalue <- sapply(S, function(x) x$pval)
                formattedSummaryPrint(VarName, SMD, Pvalue)
                
                results <- list()
                results$matches <- cbind(data.frame(ID = a))
                results$varname <- VarName
                results$smd <- SMD
                results$pvalue <- Pvalue
                
                # build aggregated lists from all iterations
                allResults[[1]] = results
                queue$producer$fireAssignReactive("allResultsReactive", allResults) # Using ipc to update graph as iterMatch runs

                # check SMD threshold
                condition <- rep(FALSE, length(thresh))
                for (i in 1:length(S)) {
                    tryCatch(expr = {
                    x <- S[[i]]
                    x_logic <- abs(x$smd) < thresh[i]
                    x_logic <- x_logic[!is.na(x_logic)]
                    if (all(x_logic)) {
                        condition[i] <- TRUE
                    }
                    },
                    error = function(e) { condition[i] <- FALSE })
                }
                
                prodCondition <- prod(condition)
                iteration <- 1
                cat(iteration, "iteration" , sep = "-", "\n")
                
                # While the condition is not satisfied do
                print("Starting the while loop ...")
                
                asyncTimer$set(message="Iterations: ", detail=iteration, value=1)
                
                while (prodCondition == 0) {
                    if (prodCondition == 0)
                        
                        # Retrive the distance between the matched treated vs control subjects
                        # if (NROW(selData[ASD,]) < NROW(selData[!ASD,])) {
                        distance <- NULL
                    
                    gc() # clean up data from last iteration
                    
                    groupSize <- table(optData[, response])[1]
                    
                    asyncTimer$set(message="Iterations: ", detail=iteration, value=iteration)
                    
                    print(groupSize)
                    print("groupSize")
                    if(groupSize < MIN_GROUP_SIZE) {
                        print(paste("Group size < ", MIN_GROUP_SIZE, ". Exiting iterMatch.", sep=""))
                        return(allResults)
                    }
                    for (i in 1:groupSize) {
                        subj1 = as.character(splitSubj[i, 1])
                        subj2 = as.character(splitSubj[i + groupSize, 1])
                        distance <- c(distance, DM[subj1, subj2])
                    }
                    
                    # Create a dataframe paired subject ID and the corresponding distance
                    splitSubj <- data.frame(splitSubj, distance)
                    
                    
                    # Find the max distance bet
                    excSubj <- splitSubj[which.max(splitSubj[, "distance"]), "splitSubj"]
                    
                    # Print the farthest distance treated or control subject
                    print(excSubj)
                    
                    # Count number of subjects in smaller group
                    # In matching, both groups have equal number of subjs
                    allMatchSubj = splitSubj[1:(table(optData[, response])[2]), 1]
                    
                    # The remained subjects from smaller group
                    remainedASD <- as.character(allMatchSubj[allMatchSubj != excSubj])
                    rm(allMatchSubj)
                    
                    # print(remainedASD)
                    
                    # Retrieve the distance matrix with remaining smaller subject group and all subjects from larger group
                    trimedDM <- DM[remainedASD,]
                    
                    # Match remaining subjects from both groups
                    optMatch <- fullmatch(
                        trimedDM,
                        min.controls = 1,
                        max.controls = 1,
                        omit.fraction = NULL,
                        tol = match.tol,
                        subclass.indices = NULL
                    )
                    
                    # Summary of optimal matched sample
                    summary(optMatch)
                    
                    #print(optMatch, responseed = T)
                    subMatchFile <- sprintf("subjMatch_%s.txt", gsub("[.]", "-", format(Sys.time(), "%Y%m%d-%H-%M-%OS3")))
                    # Write output of matched treated vs control into a file  (capturing the output from terminal)
                    capture.output(print(optMatch, grouped = T), file = subMatchFile)
                    
                    print(optMatch, grouped = T)
                    # Read in the captured output as an object
                    subjMatch <-
                        read.table(
                            subMatchFile,
                            header = T,
                            colClasses = c("character", "character")
                        )
                    print(subjMatch)
                    if (file.exists(subMatchFile)) {
                        file.remove(subMatchFile)
                    }
                    
                    # Binding row wise of subject ID (First treated then control)into a one column
                    splitSubj <-
                        do.call("rbind", strsplit(c(
                            as.character(subjMatch$Group),
                            as.character(subjMatch$Members)
                        ), ","))
                    
                    # Subset dataframes of all matching variables, pair number by Unique subject ID
                    optData <- selData[splitSubj,]
                    
                    # Make sure the subject labels are assigned correctly
                    a <- rownames(optData)
                    print(a)
                    optimData <- selData[a,]
                    iteration <- iteration + 1
                    write.csv(cbind(data.frame(ID=a), selData[a, ]), file = matchSampleFile, row.names = T)
                    
                    cat(
                        iteration,
                        "iteration=============================================" ,
                        sep = "-",
                        "\n"
                    )
                    
                    # Calc the balance of the matched sample
                    S = list()
                    for (i in 1:length(selVars)) {
                        tryCatch( expr = {
                            S[[i]] = summaryMatch(selData[rownames(optData),], names(selVars)[i], response)
                            allVarsSummary <- c(S, S[[i]])
                        },
                        error=function(e){ 
                            S[[i]] = NA
                            allVarsSummary <- c(S, S[[i]])
                        })
                    }
                    
                    
                    varName <- sapply(S, function(x) x$varname)
                    SMD <- lapply(S, function(x) x$smd)
                    pvalue <- sapply(S, function(x) x$pval)
                    formattedSummaryPrint(varName, SMD, pvalue)
                    
                    results <- list()
                    results$matches <- cbind(data.frame(ID=a))
                    results$varname <- varName
                    results$smd <- SMD
                    results$pvalue <- pvalue
                    
                    
                    # build aggregated lists from all iterations
                    allResults[[iteration]] = results
                    queue$producer$fireAssignReactive("allResultsReactive", allResults) # Using ipc to update graph as iterMatch runs
                    
                    # Check SMD thresholds
                    condition <- rep(FALSE, length(thresh))
                    for (i in 1:length(S)) {
                        tryCatch(expr = {
                        x <- S[[i]]
                        x_logic <- abs(x$smd) < thresh[i]
                        x_logic <- x_logic[!is.na(x_logic)]
                        if (all(x_logic)) {
                            condition[i] <- TRUE
                        }
                        },
                        error = function(e) { condition[i] <- FALSE })
                    }
                    prodCondition <- prod(condition)
                }
                print(proc.time() - ptm)
                sink()
            }
            # =======================================================================
            else{ # method-2
                optMatch <-
                    fullmatch(
                        DM,
                        min.controls = 1,
                        max.controls = 1,
                        omit.fraction = NULL,
                        tol = match.tol,
                        subclass.indices = NULL
                    )
                summary(optMatch)
                
                
                #=== write output to a file to be used again as an obj in R ====
                subMatchFile <- sprintf("subjMatch_%s.txt", gsub("[.]", "-", format(Sys.time(), "%Y%m%d-%H-%M-%OS3")))
                capture.output(print(optMatch, grouped = T), file = subMatchFile)
                subjMatch <-
                    read.table(
                        subMatchFile,
                        header = T,
                        colClasses = c("character", "character")
                    )
                print(subjMatch)
                #one column treated first and control later
                splitSubj <-
                    do.call("rbind", strsplit(c(
                        as.character(subjMatch$Group),
                        as.character(subjMatch$Members)
                    ), ","))
                
                # delete temporary file
                if (file.exists(subMatchFile)) {
                    file.remove(subMatchFile)
                }
                
                # Create a dataframe of all IDs (treeated first) along pair number
                splitSubjMem <-
                    data.frame(pairNum = rep(1:length(levels(optMatch)), 2), SUB_ID = splitSubj)
                
                # Add SUB_ID variable as another variable to the dataframe
                SelDataID <-
                    data.frame(selData, SUB_ID = rownames(selData))
                
                # Merging selected matched data with the original data to create matched dataframe
                optData <-
                    merge(splitSubjMem, SelDataID, by = "SUB_ID")

                # Sorting data based on group
                optData <-
                    (optData[order(optData[, response], decreasing = F),])
                
                cat("0-iteration", "\n")
                
                
                a <- rownames(optData$SUB_ID)
                write.csv(cbind(data.frame(ID = a),selData[a, ]), file = matchSampleFile, row.names = F)
                
                # Calculate the matched sample balance
                #=== Tabling pvalues and SMD for output ====
                S = list()
                for (i in 1:length(selVars)) {
                    tryCatch( # When method-2 throws an error, use method-1
                        expr =  { 
                            S[[i]] = summaryNonMissingPair(optData, response, names(selVars)[i])
                                  allVarsSummary <- c(S, S[[i]])
                                },
                        error = function(err) { tryCatch( 
                            expr = { 
                                S[[i]] = summaryMatch(optData, names(selVars)[i], response) 
                                allVarsSummary <- c(S, S[[i]]) },
                            error = function(e) { 
                                S[[i]] = NA
                                allVarsSummary <- c(S, S[[i]])}
                        )}
                    )
                }
                VarName <- sapply( S, function(x) x$varname )
                SMD <- lapply(S, function(x) x$smd)
                Pvalue <- sapply(S, function(x) x$pval)
                formattedSummaryPrint(VarName, SMD, Pvalue)
                
                results <- list()
                results$matches <- cbind(data.frame(ID=a), selData[a, ])
                results$varname <- VarName
                results$smd <- SMD
                results$pvalue <- Pvalue
                
                # build aggregated lists from all iterations
                allResults[[1]] = results
                queue$producer$fireAssignReactive("allResultsReactive", allResults) # Using ipc to update graph as iterMatch runs
                
                # check SMD threshold which is a list
                condition <- rep(FALSE, length(thresh))
                for (i in 1:length(S)) {
                    tryCatch(expr = {
                    x <- S[[i]]
                    x_logic <- abs(x$smd) < thresh[i]
                    x_logic <- x_logic[!is.na(x_logic)]
                    if (all(x_logic)) {
                        condition[i] <- TRUE
                    }
                    },
                    error = function(e) { condition[i] <- FALSE })
                }
                
                prodCondition <- prod(condition)
                
                iteration <- 1
                cat(iteration, "iteration" , sep = "-", "\n")
                
                asyncTimer$set(message="Iterations: ", detail=iteration, value=1)
                
                #prodCondition==0
                while (prodCondition == 0) {
                    if (prodCondition == 0)
                        # Can we improve SMD? First which ASD has the largest
                        #distance to selected control subjects with equal treated
                        #and control subjects.?
                        distance <- NULL
                    
                    gc() # Clean up data from last iteration
                    
                    asyncTimer$set(message="Iterations: ", detail=iteration, value=iteration)
                    
                    groupSize <- (table(optData[, response])[1])
                    print(groupSize)
                    print("groupSize")
                    if(groupSize < MIN_GROUP_SIZE) {
                        print(paste("Group size < ", MIN_GROUP_SIZE, ". Exiting iterMatch.", sep=""))
                        return(allResults)
                    }
                    
                    for (i in 1:groupSize) {
                        distance <- c(distance, (DM[as.character(splitSubj[i, 1]), as.character(splitSubj[(i + groupSize), 1])]))
                    }
                    splitSubj <-
                        data.frame(splitSubj, distance)
                    
                    # Find the name of ASD subject has the largest distance
                    # maxSubjs <-
                    #   (splitSubj[splitSubj[, "distance"] != 100 , ])
                    excSubj <-
                        splitSubj[which.max(splitSubj[, 2]), 1]
                    print(excSubj)
                    
                    #Get the list of ASD subject that partcipate in the matching
                    allMatchSubj = splitSubj[1:(table(optData[, response])[2]), 1]
                    
                    #remaining ASD subjects
                    remainedASD <-
                        as.character(allMatchSubj[allMatchSubj != excSubj])
                    print(remainedASD)
                    rm(allMatchSubj)
                    
                    #trimed distance matrix of remaining treated and original control subjs
                    trimedDM <- DM[remainedASD, ]

                    #Redo the optimal matching
                    optMatch <-
                        fullmatch(
                            trimedDM,
                            min.controls = 1,
                            max.controls = 1,
                            omit.fraction = NULL,
                            tol = match.tol,
                            subclass.indices = NULL
                        )
                    summary(optMatch)
                    #print(optMatch, responseed = T)
                    
                    #==== write the itterated matched output to a text file after ASD exclusion ====
                    subMatchFile <- sprintf("subjMatch_%s.txt", gsub("[.]", "-", format(Sys.time(), "%Y%m%d-%H-%M-%OS3")))
                    capture.output(print(optMatch, grouped = T), file = subMatchFile)
                    
                    subjMatch <-
                        read.table(
                            subMatchFile,
                            header = T,
                            colClasses = c("character", "character")
                        )
                    # Print pairs of subjects to output
                    print(subjMatch)
                    # delete temporary file
                    if (file.exists(subMatchFile)) {
                        file.remove(subMatchFile)
                    }
                    
                    splitSubj <-
                        do.call("rbind", strsplit(c(
                            as.character(subjMatch$Group),
                            as.character(subjMatch$Members)
                        ), ","))
                    
                    optData <- selData[splitSubj,]
                    optData$SUB_ID <-
                        print(optData$SUB_ID)
                    
                    optData <-
                        data.frame(pairNum = rep(1:length(levels(optMatch)), 2),
                                   SUB_ID = rownames(optData),
                                   optData)
                    #optData <-
                    #  (optData[order(optData[, response]),])
                    
                    a <- rownames(optData)
                    print(a)
                    optimData <- selData[a,]
                    iteration <- iteration + 1
                    write.csv(cbind(data.frame(ID = a),selData[a, ]), file = matchSampleFile, row.names = F)
                    cat(
                        iteration,
                        "itteration=============================================" ,
                        sep = "-",
                        "\n"
                    )
                    
                    # Calc the balance of the matched sample
                    S <- list()
                    allVarsSummary <- list()
                    for (i in 1:length(selVars)) {
                        tryCatch( # When method-2 throws an error, use method-1
                            expr =  { 
                                S[[i]] = summaryNonMissingPair(optData, response, names(selVars)[i])
                            allVarsSummary <- c(S, S[[i]])
                            },
                            error = function(err) { tryCatch( 
                                expr = { 
                                    S[[i]] = summaryMatch(optData, names(selVars)[i], response)
                                    allVarsSummary <- c(S, S[[i]])
                                },
                                error = function(e) { 
                                    S[[i]] = NA
                                allVarsSummary <- c(S, S[[i]])
                            } 
                            )} 
                        )
                    }
                    
                    
                    varName <- sapply(S, function(x) x$varname)
                    SMD <- lapply(S, function(x) x$smd)
                    pvalue <- sapply(S, function(x) x$pval)
                    formattedSummaryPrint(varName, SMD, pvalue)
                    #==== Tabling pvalues and SMD after matching ====
                    
                    results <- list()
                    results$matches <- cbind(data.frame(ID = a))
                    results$varname <- varName
                    results$smd <- SMD
                    results$pvalue <- pvalue
                    
                    
                    # build aggregated lists from all iterations
                    allResults[[iteration]] = results
                    queue$producer$fireAssignReactive("allResultsReactive", allResults) # Using ipc to update graph as iterMatch runs
                    
                    
                    # check SMD threshold
                    condition <- rep(FALSE, length(thresh))
                    for (i in 1:length(S)) {
                        tryCatch( expr = {
                        x <- S[[i]]
                        x_logic <- abs(x$smd) < thresh[i]
                        x_logic <- x_logic[!is.na(x_logic)]
                        if (all(x_logic)) {
                            condition[i] <- TRUE
                        }
                        },
                        error = function(e) { condition[i] <- FALSE })
                    }
                    
                    prodCondition <- prod(condition)
                }
                
            }
            print(proc.time() - ptm)
            if(!is.null(outputfile)) {
                sink()
            }
            return( allResults )
        }
    

    
    
#' @title Grows a random forest for generating the proximity matrix for
#'        the optimal matching algorithm
#' @param ntrees Number of trees to build the forest
#' @param formula model which contains matching variables that are divided
#'                by + and the response on the left of a ~ operator
#'                 Categorical predictors must be treated as a factor
#' @param training Data to grow and prune the tree
#' @param search The default is set to greedy search to select a cut-point for a variable
#' @param method Indicates method of handling binary classification
#' @param split Child node splitting measurement. The default is set to Gini index
#' @param mtry Number of variables considered at each split. The default
#'             is to consider all variables
#' @param nsplit Number: Controls the minimum node size
#' @param minsplit Number: Controls the minimum node size. The default is 20
#' @param minbucket Number: Controls the minimum number of observations in any terminal node
#' @param maxdepth Number: Controls the minimum number of observations in any terminal node
#' @param useRpart Set to FALSE as default not to create forest based off rpart trees
#' @param bootsrap bootstrapping sample to build the tree
#' @return List of trees that form random forests
#' @export growRF
#' @author Peter Calhoun, Afrooz Jahedi, Tristan Hillis
#'
    #Grows a random forest
    growRF <-
        function (ntrees,
                  formula,
                  training,
                  search,
                  method,
                  split = c ("information", "gini"),
                  surrogateargs,
                  mtry,
                  nsplit,
                  minsplit,
                  minbucket,
                  maxdepth,
                  progressIndicator = NULL,
                  bootstrap = TRUE,
                  useRpart = FALSE) {
            #Construct random forest w/progress bar
            tstart <- proc.time()
            
            #progress <- shiny::Progress$new(min=0, max=ntrees)
            #on.exit(progress$close())
            if(!is.null(progressIndicator)) {
                progressIndicator$set(message="Generating Random Forest: ", value=0, detail=paste("(0/", ntrees, ")", sep=""))
            }
            
            randFor <- lapply(1:ntrees, function(b) {
                if (b %% 100 == 0) {
                    print (paste0("Tree Number: ", b))
                    print (paste0("Time Since Tree Building Start (s): ", (proc.time()-tstart)[3] ))
                }
                if (bootstrap == FALSE) {
                    obs.b <- 1:nrow(training)
                    sample.b <- training
                } else {
                    obs.b <- sample(1:nrow(training),
                                    size = nrow(training),
                                    replace = T)
                    sample.b <- training[obs.b, ]
                }
                
                tree.b <-
                    grow (
                        formula = formula,
                        data = sample.b,
                        search = search,
                        method = method,
                        split = split,
                        surrogateargs = surrogateargs,
                        mtry = mtry,
                        nsplit = nsplit,
                        minsplit = minsplit,
                        minbucket = minbucket,
                        maxdepth = maxdepth,
                        useRpart = useRpart
                    )
                if(!is.null(progressIndicator)) {
                    progressIndicator$inc(1, detail=paste("(", b, "/", ntrees, ")", sep=""))
                }
                list (tree = tree.b, cases = sort(unique(obs.b)))
            })
            return(randFor)
        }
})

