.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())    
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }        
  }
}

###############################################################################
bws1Design <- function() {
  defaults <- list(initial.catalog = 1)
  dialog.values <- getDialog("bws1Design", defaults)
  initializeDialog(title = gettextRcmdr("Generate Design for BWS1"))

  outputFrame  <- tkframe(top)
  optionsFrame <- tkframe(top)
  RNGseedFrame <- tkframe(optionsFrame)
  commentFrame <- tkframe(top)


  # name for bibd
  designName <- tclVar("design")
  design <- ttkentry(outputFrame, width = "20", textvariable = designName)

  # t: number of items
  NitemsValue  <- tclVar("4")
  NitemsSlider <- tkscale(optionsFrame, from = 4, to = 21,
                          showvalue = FALSE, 
                          variable = NitemsValue, resolution = 1, 
                          orient = "horizontal")
  NitemsShow   <- labelRcmdr(optionsFrame, textvariable = NitemsValue, 
                             width = 20, justify = "right")

  # b: number of questions
  NquesValue  <- tclVar("4")
  NquesSlider <- tkscale(optionsFrame, from = 4, to = 21,
                         showvalue = FALSE,
                         variable = NquesValue, resolution = 1,
                         orient = "horizontal")
  NquesShow   <- labelRcmdr(optionsFrame, textvariable = NquesValue,
                            width = 20, justify = "right")

  # k: number of items per question
  NitemsPquesValue  <- tclVar("3")
  NitemsPquesSlider <- tkscale(optionsFrame, from = 3, to = 8,
                               showvalue = FALSE,
                               variable = NitemsPquesValue,
                               resolution = 1,
                               orient = "horizontal")
  NitemsPquesShow   <- labelRcmdr(optionsFrame,
                                  textvariable = NitemsPquesValue,
                                  width = 20, justify = "right")

  # iter: number of iterations (optional)
  IterName <- tclVar("1000")
  Iter     <- ttkentry(optionsFrame, width = "10",
                       textvariable = IterName)

  # random number generator seed
  RNGseedName <- tclVar("")
  RNGseed     <- ttkentry(optionsFrame, width = "10",
                          textvariable = RNGseedName)


  onOK <- function() {
    designValue <- trim.blanks(tclvalue(designName))
    closeDialog()

    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste("set.seed(", as.numeric(tclvalue(RNGseedName)),
                        ")", sep = "")
    }

    cmd <- paste("find.BIB(t = ", as.numeric(tclvalue(NitemsValue)),
                        ", b = ", as.numeric(tclvalue(NquesValue)),
                        ", k = ", as.numeric(tclvalue(NitemsPquesValue)),
                        ", iter = ", as.numeric(tclvalue(IterName)), 
                 ")", sep = "")

    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      doItAndPrint(paste(cmd.seed, ";", designValue, " <- ", cmd,
                         sep = ""))
    } else {
      doItAndPrint(paste(designValue, " <- ", cmd, sep = ""))
    }
    doItAndPrint(paste(designValue))
    doItAndPrint(paste("isGYD(", designValue, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "find.BIB",
               reset       = "bws1Design",
               apply       = NULL)

  tkgrid(
   labelRcmdr(outputFrame,
              text = gettextRcmdr("Name for design ")),
   design, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  tkgrid(labelRcmdr(optionsFrame, 
                    text = gettextRcmdr("Number of items")),
         NitemsSlider, NitemsShow, sticky = "w")

  tkgrid(labelRcmdr(optionsFrame,
                    text = gettextRcmdr("Number of questions")),
         NquesSlider, NquesShow, sticky = "w")

  tkgrid(labelRcmdr(optionsFrame,
                    text = gettextRcmdr("Number of items per question")),
         NitemsPquesSlider, NitemsPquesShow, sticky = "w")

  tkgrid(
    labelRcmdr(optionsFrame,
               text = gettextRcmdr("Number of iterations")),
    Iter, sticky = "w")

  tkgrid(
    labelRcmdr(optionsFrame,
      text = gettextRcmdr("Seed for random number generator (optional) ")),
    RNGseed, sticky = "w")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1Items <- function() {
  defaults <- list(nrows = "4")
  dialog.values <- getDialog("bws1Items", defaults)
  initializeDialog(title = gettextRcmdr("Set Item Names"))

  outputFrame <- tkframe(top)
  inputsFrame <- tkframe(top)
  rowsFrame   <- tkframe(inputsFrame)
  tableFrame  <- tkframe(inputsFrame)
  noteFrame   <- tkframe(inputsFrame)


  env <- environment()
  assign(".tableFrame", tkframe(tableFrame), envir = env)

  setUpTable <- function(...) {
    tkdestroy(get(".tableFrame", envir = env))
    assign(".tableFrame", tkframe(tableFrame), envir = env)
    nrows <- as.numeric(tclvalue(rowsValue))

    make.col.names <- "labelRcmdr(.tableFrame, text = '')"
    make.col.names <- 
      paste(make.col.names, ", ", 
            "labelRcmdr(.tableFrame, text = 'Item name')", sep = "")
    eval(parse(text=paste("tkgrid(", make.col.names, ", sticky = 'w')",
               sep = "")), envir = env)

    for (i in 1:nrows){   
      varname <- paste(".tab.", i, sep = "") 
      assign(varname, tclVar("") , envir = env)
      row.varname <- paste(".rowname.", i, sep = "")
      assign(row.varname, tclVar(i), envir = env)
      make.row <- paste("labelRcmdr(.tableFrame, text =", i, ")")
      make.row <- paste(make.row, ", ",
                        "ttkentry(.tableFrame, width = '20', textvariable =", 
                        varname, ")", sep="")
      eval(parse(text = paste("tkgrid(", make.row, ")", sep = "")), 
           envir = env)
    }
    tkgrid(get(".tableFrame", envir = env), sticky = "w")
  }

  rowsValue  <- tclVar(dialog.values$nrows)
  rowsSlider <- tkscale(rowsFrame, from = 4, to = 21, showvalue = FALSE, 
                        variable = rowsValue,
                        resolution = 1, orient = "horizontal",
                        command = setUpTable)
  rowsShow   <- labelRcmdr(rowsFrame, textvariable = rowsValue,
                           width = 25, justify = "right")

  itemName <- tclVar("items")
  item     <- ttkentry(outputFrame, width = "20", textvariable = itemName)


  onOK <- function() {
    itemValue <- trim.blanks(tclvalue(itemName))
    closeDialog()
    nrows <- as.numeric(tclvalue(rowsValue))
    values <- rep("", nrows)
    for (i in 1:nrows){
      varname <- paste(".tab.", i, sep = "")
      values[i] <- eval(parse(text = paste("tclvalue(", varname,")", 
                                           sep = "")))
    }
    cmd <- paste0(itemValue, " <- c('", paste(values, collapse = "', '"), "')")
    doItAndPrint(cmd)
    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.questionnaire",
               reset       = "bws1Items",
               apply       = NULL)

  tkgrid(labelRcmdr(
    outputFrame,
    text = gettextRcmdr("Name for item names ")),
    item, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  tkgrid(labelRcmdr(rowsFrame, text = gettextRcmdr("Number of items ")),
         rowsSlider, rowsShow, sticky = "w")
  tkgrid(rowsFrame, sticky = "w")

  tkgrid(tableFrame, sticky="w")

  tkgrid(inputsFrame, sticky="w")

  setUpTable()

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1Questions <- function() {
  defaults <- list(initial.designType = "2")
  dialog.values <- getDialog("bws1Questions", defaults)
  initializeDialog(title = gettextRcmdr("Create BWS1 Questions"))

  inputsFrame <- tkframe(top)

  # choice.sets
  designName <- tclVar("design")
  design <- ttkentry(inputsFrame, width = "20",
                     textvariable = designName)

  # item.names
  itemName <- tclVar("items")
  item <- ttkentry(inputsFrame, width = "20", textvariable = itemName)

  onOK <- function() {
    designValue <- tclvalue(designName)
    itemVars    <- tclvalue(itemName)
    closeDialog()

    doItAndPrint(paste("bws.questionnaire(choice.sets = ", designValue,
                       ", design.type = 2", 
                       ", item.names = ", itemVars, ")", 
                       sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.questionnaire",
               reset       = "bws1Questions",
               apply       = NULL)

  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Name of design ")),
    design, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Name of item names ")),
    item, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1Response <- function() {
  initializeDialog(
    title = gettextRcmdr("Synthesize Responses to BWS1 Questions"))

  ###### Output
  outputFrame <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  activeFrame <- tkframe(outputFrame)

  datasetName <- tclVar("dataset")
  dataset <- ttkentry(datasetnameFrame, width = "20",
                      textvariable = datasetName)

  # set as active datase
  checkBoxes(frame = "activeFrame",
             boxes = "yes",
             initialValues = "1",
             labels = "")

  ###### Inputs
  inputsFrame <- tkframe(top)

  # choice.sets
  designName <- tclVar("design")
  design <- ttkentry(inputsFrame, width = "25", textvariable = designName)

  # item.names
  itemName <- tclVar("items")
  item <- ttkentry(inputsFrame, width = "25", textvariable = itemName)

  # number of respondents
  nrespondentsName <- tclVar("100")
  nrespondents <- ttkentry(inputsFrame, width = "25",
                           textvariable = nrespondentsName)

  # parameters
  parametersName <- tclVar("")
  parameters <- ttkentry(inputsFrame, width = "25",
                         textvariable = parametersName)

  # random number generator seed
  RNGseedName <- tclVar("")
  RNGseed <- ttkentry(inputsFrame, width = "25", textvariable = RNGseedName)

  onOK <- function() {
    closeDialog()

    if (is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste(", seed = NULL)", sep = "")
    } else {
      cmd.seed <- paste(", seed = ",  as.numeric(tclvalue(RNGseedName)), 
                        ")", sep = "")
    }

    doItAndPrint(paste(tclvalue(datasetName), 
                       " <- bws.response(design = ", tclvalue(designName),
                       ", item.names = ", tclvalue(itemName),
                       ", b = c(", tclvalue(parametersName), ")",
                       ", n = ", tclvalue(nrespondentsName),
                       cmd.seed, sep = ""))

    if (tclvalue(yesVariable) == "1") {
      doItAndPrint(paste("activeDataSet('", tclvalue(datasetName), "')",
                         sep = ""))
    }

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.response",
               reset       = "bws1Response",
               apply       = NULL)

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(datasetnameFrame, labelRcmdr(outputFrame, text = "  "), activeFrame,
         labelRcmdr(outputFrame,
                    text = gettextRcmdr("Set as active data set")),
         sticky = "nw")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  # Inputs
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Name of design")),
    design, sticky = "w")
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Name of item names")),
    item, sticky = "w")
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Number of respondents")),
    nrespondents, sticky = "w")
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Parameter values")),
    parameters, sticky = "w")
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Seed for random number generator (optional) ")),
    RNGseed, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")

  # Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1Dataset <- function() {
  defaults <- list(initial.activedataset = 1,
                   initial.designtype = 2,
                   initial.responsetype = 1,
                   initial.modeltype = "maxdiff",
                   nrows = "4")
  dialog.values <- getDialog("bws1Dataset", defaults)
  initializeDialog(title = gettextRcmdr("Create Data Set for BWS1 Analysis"))


  ###### Output frame
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  activeFrame      <- tkframe(outputFrame)

  # output name
  datasetName <- tclVar("bws1data")
  dataset <- ttkentry(datasetnameFrame, width = "20",
                      textvariable = datasetName)

  # set as active datase
  checkBoxes(frame = "activeFrame",
             boxes = "yes",
             initialValues = "1",
             labels = "")


  ###### Inputs frame
  inputsFrame <- tkframe(top)

  ### Frame in left
  leftFrame    <- tkframe(inputsFrame)
  objectsFrame <- tkframe(leftFrame)
  radio1Frame  <- tkframe(leftFrame)
  radio2Frame  <- tkframe(leftFrame)
  radio3Frame  <- tkframe(leftFrame)

  # choice.sets
  designName <- tclVar("design")
  design <- ttkentry(objectsFrame, width = "20", textvariable = designName)

  # item.names
  itemName <- tclVar("items")
  item <- ttkentry(objectsFrame, width = "20", textvariable = itemName)

  # respondent.dataset
  respondentdataName <- tclVar("dataset")
  respondent <- ttkentry(objectsFrame, width = "20",
                         textvariable = respondentdataName)

  # id
  idName <- tclVar("id")
  id <- ttkentry(objectsFrame, width = "20", textvariable = idName)

  # response.type
  radioButtons(radio1Frame, 
    name    = "responsetype",
    buttons = c("rowNumber", "itemNumber"),
    values  = c(1, 2),
    labels  = gettextRcmdr(c("Row number format", "Item number format")),
    initialValue = dialog.values$initial.responsetype,
    title   = gettextRcmdr("Response variable format"))

  # model
  radioButtons(radio3Frame,
    name = "modeltype",
    title   = gettextRcmdr("Model type"),
    buttons = c("maxdiff", "marginal", "sequential"),
    values  = c("maxdiff", "marginal", "sequential"),
    labels  = gettextRcmdr(c("Maxdiff model", "Marginal model",
                             "Marginal sequential model")),
    initialValue = dialog.values$initial.modeltype)


  ### Frame in right
  rightFrame  <- tkframe(inputsFrame)
  letterFrame <- tkframe(rightFrame)
  tableFrame  <- tkframe(rightFrame)
  rowsFrame   <- tkframe(rightFrame)


  # Table
  env <- environment()
  assign(".tableFrame", tkframe(tableFrame), envir=env)

  setUpTable <- function(...){
    tkdestroy(get(".tableFrame", envir=env))
    assign(".tableFrame", tkframe(tableFrame), envir=env)
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols <- 2

    # Set colnames
    make.col.names <- "labelRcmdr(.tableFrame, text='')"
    for (j in 1:ncols) {
      if (j == 1) {
        col.varname <- "Best"
      } else {
        col.varname <- "Worst"
      }
      make.col.names <- 
        paste(make.col.names, ", ",
              "labelRcmdr(.tableFrame, text = '", col.varname, "')",
              sep = "")
    }
    eval(parse(text=paste("tkgrid(", make.col.names, ", sticky = 'w')", 
                          sep = "")), envir = env)

    # Make rows for questions
    for (i in 1:nrows){
      if (tclvalue(lettertypeVariable) == "1") {
        b <- "B"
        w <- "W"
      } else if (tclvalue(lettertypeVariable) == "2"){
        b <- "b"
        w <- "w"
      } else {
        b <- ""
        w <- ""
      }

      Bvarname <- paste(".tab.", i, ".1", sep = "")
      if (tclvalue(lettertypeVariable) == "3") {
        eval(parse(text = paste("assign(Bvarname, tclVar(''), envir = env)",
                                sep = "")))
      } else {
        eval(parse(text = paste("assign(Bvarname, tclVar('", b, i,
                                "'), envir = env)", sep = "")))
      }
      Wvarname <- paste(".tab.", i, ".2", sep = "")
      if (tclvalue(lettertypeVariable) == "3") {
        eval(parse(text = paste("assign(Wvarname, tclVar(''), envir = env)",
                                sep = "")))
      } else {
        eval(parse(text = paste("assign(Wvarname, tclVar('", w, i,
                                "'), envir = env)", sep = "")))
      }
      row.varname <- paste("Q", i, sep = "")
      make.row <- paste("labelRcmdr(.tableFrame, text = '", row.varname,
                        "')", sep = "")
      make.row <- paste(make.row, ", ", 
                        "ttkentry(.tableFrame, width = '10', 
                        textvariable = ", Bvarname, ")", sep = "")
      make.row <- paste(make.row, ", ",
                        "ttkentry(.tableFrame, width = '10', 
                        textvariable = ", Wvarname, ")", sep = "")
      eval(parse(text=paste("tkgrid(", make.row, ", sticky = 'w')",
                            sep = "")), envir = env)
    }
    tkgrid(get(".tableFrame", envir = env), sticky = "ew", padx = 6)
  }

  # Slider
  rowsValue  <- tclVar(dialog.values$nrows)
  rowsSlider <- tkscale(rowsFrame, from = 4, to = 21, showvalue = FALSE,
                        variable = rowsValue, resolution = 1, 
                        orient = "horizontal", command = setUpTable)
  rowsShow   <- labelRcmdr(rowsFrame, textvariable = rowsValue, width = 3,
                           justify = "right")

  # letter
  radioButtons(letterFrame,
    name = "lettertype",
    title   = gettextRcmdr("Letters of best- and worst-response variables"),
    buttons = c("Uppercase", "Lowercase", "None"),
    values  = c("1", "2", "3"),
    labels  = gettextRcmdr(c("Uppercase", "Lowercase", "None")),
    initialValue = "1",
    command = setUpTable)


  onOK <- function() {
    closeDialog()

    nrows <- as.numeric(tclvalue(rowsValue))
    ncols <- 2
    k <- 0
    BWvarNames <- rep("", nrows * ncols)

    for (i in 1:nrows) {
      for (j in 1:2) {
        k <- k + 1
        BWvarname <- paste(".tab.", i, ".", j, sep = "")
        BWvarNames[k] <- 
          eval(parse(text =
            paste("as.character(tclvalue(", BWvarname, "))", sep = "")))
      }
    }

    cmd <- paste("c('", paste(BWvarNames, collapse = "','"), "')", sep = "")

    doItAndPrint(
      paste(tclvalue(datasetName), " <- bws.dataset(respondent.dataset = ",
            tclvalue(respondentdataName),
            ", response.type = ", as.numeric(tclvalue(responsetypeVariable)),
            ", choice.sets = ", tclvalue(designName),
            ", design.type = 2", 
            ", item.names = ", tclvalue(itemName),
            ", id = '", tclvalue(idName), "'",
            ", response = ", cmd,
            ", model = '", tclvalue(modeltypeVariable), "')", sep = ""))

    if (tclvalue(yesVariable) == "1") {
      doItAndPrint(
        paste("activeDataSet('", tclvalue(datasetName), "')", sep = ""))
    }

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.dataset",
               reset       = "bws1Dataset",
               apply       = NULL)

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(datasetnameFrame, labelRcmdr(outputFrame, text = "  "), activeFrame,
         labelRcmdr(outputFrame, 
                    text = gettextRcmdr("Set as active data set")),
         sticky = "nw")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  # Inputs
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Name of design")),
    design, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Name of item names")),
    item, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Name of survey data set ")),
    respondent, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("ID variable")),
    id, sticky = "w")
  tkgrid(objectsFrame, sticky = "w")

  tkgrid(responsetypeFrame, sticky = "w")
  tkgrid(radio1Frame, sticky = "w")

  tkgrid(modeltypeFrame, sticky = "w")
  tkgrid(radio3Frame, sticky = "w")

  tkgrid(labelRcmdr(rowsFrame,
    text = gettextRcmdr("Number of BWS1 questions ")),
    rowsSlider, rowsShow, sticky = "w")
  tkgrid(rowsFrame, sticky = "w")
  tkgrid(lettertypeFrame, sticky = "w")
  tkgrid(letterFrame, sticky = "w")
  tkgrid(labelRcmdr(
           tableFrame,
           text = gettextRcmdr("Names of best- and worst-response variables")),
         sticky = "w")
  tkgrid(tableFrame, sticky="w")


  tkgrid(leftFrame, labelRcmdr(inputsFrame, text = "    "),
         rightFrame, sticky = "nw")
  tkgrid(inputsFrame, sticky = "w")

  setUpTable()

  # Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}
###############################################################################
bws1Count <- function() {
  defaults <- list(initial.activedataset = 1)
  dialog.values <- getDialog("bws1Count", defaults)
  initializeDialog(title = gettextRcmdr("Calculate BWS1 Scores"))

  optionsFrame <- tkframe(top)
  datasetFrame <- tkframe(optionsFrame)
  activeFrame  <- tkframe(optionsFrame)

  # data
  dataName <- tclVar("bws1scores")
  data     <- ttkentry(datasetFrame, width = "20", textvariable = dataName)

  # set as active datase
  checkBoxes(frame = "activeFrame",
             boxes = "yes",
             initialValues = "1",
             labels = "")

  onOK <- function() {
    dataValue <- tclvalue(dataName)
    closeDialog()

    doItAndPrint(paste(dataValue," <- bws.count(data = ", ActiveDataSet(),
                       ", cl = 2)", sep = ""))

    if (tclvalue(yesVariable) == "1") {
      doItAndPrint(paste("activeDataSet('", tclvalue(dataName), "')",
                         sep = ""))
    }

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.count",
               reset       = "bws1Count",
               apply       = NULL)

  tkgrid(labelRcmdr(
    datasetFrame,
    text = gettextRcmdr("Name for scores ")),
    data, sticky = "w")
  tkgrid(datasetFrame, labelRcmdr(optionsFrame, text = "  "), activeFrame,
         labelRcmdr(optionsFrame, 
                    text = gettextRcmdr("Set as active data set")),
         sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1CountSummary <- function() {
  doItAndPrint(paste("summary(", ActiveDataSet(), ")", sep = ""))
}
###############################################################################
bws1CountBarplot1 <- function() {
  defaults <- list(initial.scoretype = "bw")
  dialog.values <- getDialog("bws1CountBarplot1", defaults)
  initializeDialog(title = gettextRcmdr("Draw Distributions of BWS1 Scores"))

  optionsFrame <- tkframe(top)
  scoreFrame   <- tkframe(optionsFrame)
  mfrowFrame   <- tkframe(optionsFrame)
  rowcolFrame  <- tkframe(mfrowFrame)

  # type of scores
  radioButtons(scoreFrame, 
    name    = "scoretype",
    buttons = c("BW", "B", "W"),
    values  = c("bw", "b", "w"),
    labels  = gettextRcmdr(c("Best-minus-Worst", "Best", "Worst")),
    initialValue = dialog.values$initial.scoretype,
    title   = gettextRcmdr("Score type"))

  # Nrows
  NrowsName <- tclVar("")
  Nrows     <- ttkentry(rowcolFrame, width = "4", textvariable = NrowsName)

  # Ncols
  NcolsName <- tclVar("")
  Ncols     <- ttkentry(rowcolFrame, width = "4", textvariable = NcolsName)

  onOK <- function() {
    closeDialog()

    if (tclvalue(NrowsName) == "" & tclvalue(NcolsName) == "" ) {
      cmd.mfrow <- paste(", mfrow = NULL", sep = "")
    } else {
      cmd.mfrow <- paste(", mfrow = c(", tclvalue(NrowsName),
                         ", ", tclvalue(NcolsName), ")", sep = "")
    }

    doItAndPrint(paste("barplot(height = ", ActiveDataSet(),
                       ", score = '", tclvalue(scoretypeVariable), "'", 
                       cmd.mfrow, ")", sep = ""))

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.count",
               reset       = "bws1CountBarplot1",
               apply       = NULL)

  tkgrid(labelRcmdr(
    mfrowFrame,
    text = gettextRcmdr("Arrangement of bar plots (optional)")), 
    sticky = "w")
  tkgrid(Nrows, labelRcmdr(rowcolFrame,
                           text = gettextRcmdr("row(s) and ")), 
         Ncols, labelRcmdr(rowcolFrame,
                           text = gettextRcmdr("column(s)")),
         sticky = "w")
  tkgrid(rowcolFrame, sticky = "w")
  tkgrid(scoretypeFrame, sticky = "w")
  tkgrid(scoreFrame, labelRcmdr(optionsFrame, text = "    "),
         mfrowFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1CountBarplot2 <- function() {
  defaults <- list(initial.scoretype = "bw",
                   initial.errorbartype = "'none'")
  dialog.values <- getDialog("bws1CountBarplot2", defaults)
  initializeDialog(title = gettextRcmdr("Draw Mean BWS1 Scores"))

  optionsFrame <- tkframe(top)
  radio1Frame  <- tkframe(optionsFrame)
  rightFrame   <- tkframe(optionsFrame)
  radio2Frame  <- tkframe(rightFrame)
  confFrame    <- tkframe(rightFrame)
  leftmarFrame <- tkframe(top) 

  # type of scores
  radioButtons(radio1Frame, 
    name    = "scoretype",
    buttons = c("BW", "B", "W"),
    values  = c("bw", "b", "w"),
    labels  = gettextRcmdr(c("Best-minus-Worst", "Best", "Worst")),
    initialValue = dialog.values$initial.scoretype,
    title   = gettextRcmdr("Score type"))

  # error.bar
  radioButtons(radio2Frame, 
    name    = "errorbartype",
    buttons = c("None", "SD", "SE", "CI"),
    values  = c("'none'", "'sd'", "'se'", "'ci'"),
    labels  = gettextRcmdr(c("None", "Standard deviation",
                             "Standard error", "Confidence interval =>")),
    initialValue = dialog.values$initial.errorbartype,
    title   = gettextRcmdr("Error bar type"))

  # conf.level
  conflevelName <- tclVar("0.95")
  conflevel     <- ttkentry(confFrame, width = "5",
                            textvariable = conflevelName)


  # left.margin
  leftmarginName <- tclVar("4.1")
  leftmargin     <- ttkentry(leftmarFrame, width = "5",
                             textvariable = leftmarginName)

  onOK <- function() {
    closeDialog()

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    cmd.leftmargin <- paste("par(mar = c(5.1,", tclvalue(leftmarginName),
                            ", 1.1, 2.1))", sep = "")

    if (tclvalue(errorbartypeVariable) == "'none'") {
      cmd.error <- paste(", error.bar = NULL", sep = "")
    } else {
      cmd.error <- paste(", error.bar = ", tclvalue(errorbartypeVariable),
                         sep = "")
    }

    if (tclvalue(errorbartypeVariable) == "'ci'") {
      cmd.conflevel <- paste(", conf.level = ", tclvalue(conflevelName),
                             sep = "")
    } else {
      cmd.conflevel <- paste("")
    }

    doItAndPrint(paste(cmd.leftmargin))
    doItAndPrint(paste("barplot(height = ", ActiveDataSet(),
                       ", score = '", tclvalue(scoretypeVariable), "'", 
                       cmd.error,
                       cmd.conflevel,
                       ", mean = TRUE, las = 1)", sep = ""))

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.count",
               reset       = "bws1CountBarplot2",
               apply       = NULL)

  tkgrid(scoretypeFrame, sticky = "w")

  tkgrid(errorbartypeFrame, sticky = "w")
  tkgrid(labelRcmdr(
    confFrame,
    text = gettextRcmdr("Confidence level")),
    conflevel, sticky = "w")
  tkgrid(radio2Frame, confFrame, sticky = "sw")

  tkgrid(radio1Frame, labelRcmdr(optionsFrame, text = "    "), rightFrame,
         sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(labelRcmdr(leftmarFrame, text = ""))
  tkgrid(labelRcmdr(
    leftmarFrame,
    text = gettextRcmdr("Left margin of plot ")),
    leftmargin, sticky = "w")
  tkgrid(leftmarFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1CountPlot <- function() {
  defaults <- list(initial.scoretype = "bw",
                   initial.positiontype = "1")
  dialog.values <- getDialog("bws1CountPlot", defaults)
  initializeDialog(title = 
    gettextRcmdr("Draw Relationship between Means and Standard Deviations of BWS1 Scores"))

  optionsFrame <- tkframe(top)
  radio1Frame  <- tkframe(optionsFrame)
  radio2Frame  <- tkframe(optionsFrame)
  limFrame     <- tkframe(optionsFrame)
  xlimFrame    <- tkframe(limFrame)
  ylimFrame    <- tkframe(limFrame)

  # type of scores
  radioButtons(radio1Frame, 
    name    = "scoretype",
    buttons = c("BW", "B", "W"),
    values  = c("bw", "b", "w"),
    labels  = gettextRcmdr(c("Best-minus-Worst", "Best", "Worst")),
    initialValue = dialog.values$initial.scoretype,
    title   = gettextRcmdr("Score type"))

  # position
  radioButtons(radio2Frame, 
    name    = "positiontype",
    buttons = c("Below", "Left", "Above", "Right"),
    values  = c("1", "2", "3", "4"),
    labels  = gettextRcmdr(c("Below", "Left", "Above", "Right")),
    initialValue = dialog.values$initial.positiontype,
    title   = gettextRcmdr("Position of point labels"))

  # xlim
  xFromName <- tclVar("")
  xFrom     <- ttkentry(xlimFrame, width = "5", textvariable = xFromName)
  xToName   <- tclVar("")
  xTo       <- ttkentry(xlimFrame, width = "5", textvariable = xToName)

  # ylim
  yFromName <- tclVar("")
  yFrom     <- ttkentry(ylimFrame, width = "5", textvariable = yFromName)
  yToName   <- tclVar("")
  yTo       <- ttkentry(ylimFrame, width = "5", textvariable = yToName)


  onOK <- function() {
    closeDialog()

    if (tclvalue(xFromName) == "" & tclvalue(xToName) == "") {
      cmd.xlim <- ""
    } else {
      cmd.xlim <- paste(", xlim = c(", tclvalue(xFromName), ", ",
                        tclvalue(xToName), ")", sep = "")
    }

    if (tclvalue(yFromName) == "" & tclvalue(yToName) == "") {
      cmd.ylim <- ""
    } else {
      cmd.ylim <- paste(", ylim = c(", tclvalue(yFromName), ", ",
                        tclvalue(yToName), ")", sep = "")
    }

    doItAndPrint(paste("plot(x = ", ActiveDataSet(),
                       ", score = '", tclvalue(scoretypeVariable), "'", 
                       ", pos = ", tclvalue(positiontypeVariable),
                       cmd.xlim, cmd.ylim, 
                       ")", sep = ""))

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.count",
               reset       = "bws1CountPlot",
               apply       = NULL)

  tkgrid(scoretypeFrame,    sticky = "w")
  tkgrid(positiontypeFrame, sticky = "w")

  tkgrid(labelRcmdr(limFrame, text = gettextRcmdr("Ranges (optional)")),
    sticky = "w")  
  tkgrid(
    labelRcmdr(xlimFrame, text = gettextRcmdr("x-axis: from ")), xFrom,
    labelRcmdr(xlimFrame, text = gettextRcmdr(" to ")), xTo,
    sticky = "w")
  tkgrid(xlimFrame, sticky = "w")
  tkgrid(
    labelRcmdr(ylimFrame, text = gettextRcmdr("y-axis: from ")), yFrom,
    labelRcmdr(ylimFrame, text = gettextRcmdr(" to ")), yTo,
    sticky = "w")
  tkgrid(ylimFrame, sticky = "w")

  tkgrid(radio1Frame, labelRcmdr(optionsFrame, text = "    "),
         radio2Frame, labelRcmdr(optionsFrame, text = "    "), 
         limFrame, sticky = "nw")

  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
bws1FitmodelSimple <- function() {
  initializeDialog(title = 
    gettextRcmdr("Fit BWS1 Model with Simple Dialog Box"))

  UpdateModelNumber()
  modelName <- tclVar(paste("BWS1model.", getRcmdr("modelNumber"), sep = ""))
  currentModel <- FALSE
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  responseVarName  <- tclVar("RES")
  responseVarFrame <- tkframe(top)
  responseVar      <- ttkentry(responseVarFrame, width = "5",
                               textvariable = responseVarName)

  strataVarName  <- tclVar("STR")
  strataVarFrame <- tkframe(top)
  strataVar      <- ttkentry(strataVarFrame, width = "5",
                             textvariable = strataVarName)

  rhsALL    <- attributes(eval(parse(text = ActiveDataSet())))$vnames
  NUMrhsALL <- length(rhsALL)

  # select base item
  radioFrame <- tkframe(top)
  radioButtons(radioFrame, 
    name         = "catalog",
    buttons      = paste("n", 1:NUMrhsALL, sep = ""),
    values       = 1:NUMrhsALL,
    labels       = gettextRcmdr(rhsALL),
    initialValue = "1",
    title        = gettextRcmdr("Base item")) 
  
  onOK <- function () {
    modelValue  <- trim.blanks(tclvalue(modelName))
    responseVar <- trim.blanks(tclvalue(responseVarName))
    strataVar   <- trim.blanks(tclvalue(strataVarName))
    k           <- as.numeric(tclvalue(catalogVariable))
    rhsVars     <- rhsALL[-k]
    rhsVars     <- paste(rhsVars, collapse = " + ")
    closeDialog()

   
    formula <- paste(responseVar, " ~ ", rhsVars, 
                     " + strata(", strataVar ,")", sep = "")
    cmd <- paste("clogit(", formula, ", data = ", ActiveDataSet(), ")",
                 sep = "")
    doItAndPrint(paste(modelValue, " <- ", cmd, sep = ""))
    doItAndPrint(paste("attributes(", modelValue, ")$baseitem <- c('",
                 rhsALL[k], "')", sep = ""))
    doItAndPrint(paste(modelValue, "; gofm(", modelValue,")", sep = ""))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "clogit", model = TRUE,
               reset       = "bws1FitmodelSimple",
               apply       = NULL)

  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))
  
  tkgrid(labelRcmdr(responseVarFrame, 
                    text = gettextRcmdr("Response variable ")),
         responseVar, sticky = "w")
  tkgrid(responseVarFrame, sticky = "w")

  tkgrid(catalogFrame, sticky = "w")
  tkgrid(radioFrame, sticky = "w")

  tkgrid(labelRcmdr(strataVarFrame, 
                    text = gettextRcmdr("Stratification variable ")),
         strataVar, sticky = "w")
  tkgrid(strataVarFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}
###############################################################################
bws1Fitmodel <- function() {
  initializeDialog(title = gettextRcmdr("Fit BWS1 Model"))

  UpdateModelNumber()
  modelName <- tclVar(paste("BWS1model.", getRcmdr("modelNumber"), sep = ""))
  currentModel <- FALSE
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  responseVarName  <- tclVar("RES")
  responseVarFrame <- tkframe(top)
  responseVar      <- ttkentry(responseVarFrame, width = "20",
                               textvariable = responseVarName)

  strataVarName  <- tclVar("STR")
  strataVarFrame <- tkframe(top)
  strataVar      <- ttkentry(strataVarFrame, width = "20",
                             textvariable = strataVarName)
  
  onOK <- function () {
    responseVar <- trim.blanks(tclvalue(responseVarName))
    strataVar   <- trim.blanks(tclvalue(strataVarName))
    modelValue  <- trim.blanks(tclvalue(modelName))
    closeDialog()

    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste(", subset = ", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }
   
    formula <- paste(responseVar, " ~ ", tclvalue(rhsVariable), 
                     " + strata(", strataVar ,")", sep = "")
    cmd <- paste("clogit(", formula, ", data = ", ActiveDataSet(), subset, ")",
                 sep = "")
    doItAndPrint(paste(modelValue, " <- ", cmd, sep = ""))
    doItAndPrint(paste(modelValue, "; gofm(", modelValue,")", sep = ""))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "clogit",
               model       = TRUE,
               reset       = "bws1Fitmodel",
               apply       = NULL)

  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  tkgrid(labelRcmdr(top, text = gettextRcmdr("Model formula"),
                    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"),
         sticky = "w")
  tkgrid(labelRcmdr(responseVarFrame, 
                    text = gettextRcmdr("1) Response variable ")),
         responseVar, sticky = "w")
  tkgrid(responseVarFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = gettextRcmdr("2) Independent variables")),
         sticky = "w")

  modelFormula(hasLhs = FALSE, rhsExtras = TRUE, formulaLabel = "")
  subsetBox(model = TRUE)

  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame, sticky = "ew")

  tkgrid(formulaFrame, sticky = "w")

  tkgrid(labelRcmdr(strataVarFrame, 
                    text = gettextRcmdr("3) Stratification variable ")),
         strataVar, sticky = "w")
  tkgrid(strataVarFrame, sticky = "w")

  tkgrid(labelRcmdr(top, text = ""))
  tkgrid(subsetFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix(preventDoubleClick = TRUE)
}
###############################################################################
bws1SharePref <- function() {
  initializeDialog(
    title = gettextRcmdr("Calculate Shares of Preferences for Items"))

  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  baseItem <- NULL
  if (currentModel) {
    baseItem <- attributes(get(.activeModel, envir = .GlobalEnv))$baseitem
  }

  inputsFrame <- tkframe(top)
  radioFrame  <- tkframe(top)

  # base
  baseitem <- variableComboBox(inputsFrame,
    initialSelection = if (is.null(baseItem)) gettextRcmdr("<no variable selected>") else baseItem,
    title = "Base item")
 
  # coef
  bName <- tclVar("")
  b <- ttkentry(inputsFrame, width = "40", textvariable = bName)

  # order
  radioButtons(radioFrame, 
    name    = "ordertype",
    buttons = c("None", "Increasing", "Decreasing"),
    values  = c("1", "2", "3"),
    labels  = gettextRcmdr(c("None", "Increasing", "Decreasing")),
    initialValue = "1",
    title   = gettextRcmdr("Order"))

  onOK <- function() {
    baseName <- getSelection(baseitem)

    closeDialog()

    if (tclvalue(bName) == "") {
      cmd.coef <- paste(", coef = NULL")
    } else {
      cmd.coef <- paste(", coef = c(", tclvalue(bName), ")", sep = "")
    }

    if (tclvalue(ordertypeVariable) == "1") {
      cmd.order <- paste(", order = FALSE)")
    } else if (tclvalue(ordertypeVariable) == "2") {
      cmd.order <- paste(", order = TRUE, decreasing = FALSE)")
    } else {
      cmd.order <- paste(", order = TRUE, decreasing = TRUE)")
    }

    doItAndPrint(paste("bws.sp(object = ", activeModel(), 
                       ", base = '", baseName, "'",
                       cmd.coef, cmd.order, sep = ""))
 
   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.sp",
               reset       = "bws1SharePref",
               apply       = NULL)

  tkgrid(getFrame(baseitem), sticky = "w")
  tkgrid(labelRcmdr(inputsFrame,
    text = gettextRcmdr("Item names (optional)")),
    sticky = "w")
  tkgrid(b, sticky = "w")
  tkgrid(ordertypeFrame, sticky = "w")
  tkgrid(inputsFrame, radioFrame, sticky = "nw")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}
###############################################################################
clogitP <- function() activeModelP() && class(get(ActiveModel()))[1] == "clogit"
bws1dataP <- function() activeDataSetP() && class(get(ActiveDataSet()))[1] == "bwsdataset"
bws1count2P <- function() activeDataSetP() && class(get(ActiveDataSet()))[1] == "bws.count2"

