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
  initializeDialog(
    title = gettextRcmdr("Generate Design for BWS1"))
  defaults <- list(
    ini.designName        = "BWS1design",
    ini.NitemsValue       = "4",
    ini.NquesValue        = "4",
    ini.NitemsPquesValue  = "3",
    ini.IterName          = "1000",
    ini.RNGseedName       = "")
  dialog.values <- getDialog("bws1Design", defaults)


  #### output frame ####
  outputFrame <- tkframe(top)

  # name for bibd
  designName <- tclVar(dialog.values$ini.designName)
  design <- ttkentry(outputFrame, width = "20", textvariable = designName)


  #### input frame ####
  optionsFrame <- tkframe(top)
  RNGseedFrame <- tkframe(optionsFrame)
  commentFrame <- tkframe(top)

  # t: number of items
  NitemsValue  <- tclVar(dialog.values$ini.NitemsValue)
  NitemsSlider <- tkscale(optionsFrame, from = 4, to = 21,
                          showvalue = FALSE, 
                          variable = NitemsValue, resolution = 1, 
                          orient = "horizontal")
  NitemsShow   <- labelRcmdr(optionsFrame, textvariable = NitemsValue, 
                             width = 20, justify = "right")

  # b: number of questions
  NquesValue  <- tclVar(dialog.values$ini.NquesValue)
  NquesSlider <- tkscale(optionsFrame, from = 4, to = 21,
                         showvalue = FALSE,
                         variable = NquesValue, resolution = 1,
                         orient = "horizontal")
  NquesShow   <- labelRcmdr(optionsFrame, textvariable = NquesValue,
                            width = 20, justify = "right")

  # k: number of items per question
  NitemsPquesValue  <- tclVar(dialog.values$ini.NitemsPquesValue)
  NitemsPquesSlider <- tkscale(optionsFrame, from = 3, to = 8,
                               showvalue = FALSE,
                               variable = NitemsPquesValue,
                               resolution = 1,
                               orient = "horizontal")
  NitemsPquesShow   <- labelRcmdr(optionsFrame,
                                  textvariable = NitemsPquesValue,
                                  width = 20, justify = "right")

  # iter: number of iterations (optional)
  IterName <- tclVar(dialog.values$ini.IterName)
  Iter     <- ttkentry(optionsFrame, width = "10",
                       textvariable = IterName)

  # random number generator seed
  RNGseedName <- tclVar(dialog.values$ini.RNGseedName)
  RNGseed     <- ttkentry(optionsFrame, width = "10",
                          textvariable = RNGseedName)


  #### onOK function ####
  onOK <- function() {
    putDialog("bws1Design", list(
      ini.designName        = tclvalue(designName),
      ini.NitemsValue       = tclvalue(NitemsValue),
      ini.NquesValue        = tclvalue(NquesValue),
      ini.NitemsPquesValue  = tclvalue(NitemsPquesValue),
      ini.IterName          = tclvalue(IterName),
      ini.RNGseedName       = tclvalue(RNGseedName)))

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
      doItAndPrint(paste0(cmd.seed))
      doItAndPrint(paste0(designValue, " <- ", cmd))
    } else {
      doItAndPrint(paste(designValue, " <- ", cmd, sep = ""))
    }
    doItAndPrint(paste(designValue))
    doItAndPrint(paste("isGYD(", designValue, ")", sep = ""))
    tkfocus(CommanderWindow())
  }


  #### specification of dialog box ####
  # OK cancel help buttons
  OKCancelHelp(helpSubject = "find.BIB",
               reset       = "bws1Design",
               apply       = "bws1Design")

  ## output
  tkgrid(
   labelRcmdr(outputFrame,
              text = gettextRcmdr("Name for design ")),
   design, sticky = "w")
  tkgrid(outputFrame, sticky = "w")

  ## blank line
  tkgrid(labelRcmdr(top, text = ""))

  ## input
  # number of items
  tkgrid(labelRcmdr(optionsFrame, 
                    text = gettextRcmdr("Number of items")),
         NitemsSlider, NitemsShow, sticky = "w")

  # number of questions
  tkgrid(labelRcmdr(optionsFrame,
                    text = gettextRcmdr("Number of questions")),
         NquesSlider, NquesShow, sticky = "w")

  # number of items per question
  tkgrid(labelRcmdr(optionsFrame,
                    text = gettextRcmdr("Number of items per question")),
         NitemsPquesSlider, NitemsPquesShow, sticky = "w")

  # number of iterations
  tkgrid(
    labelRcmdr(optionsFrame,
               text = gettextRcmdr("Number of iterations")),
    Iter, sticky = "w")

  # seed for RNG
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
  initializeDialog(
    title = gettextRcmdr("Set Item Names"))
  defaults <- list(
    ini.rowsValue = "4",
    ini.itemName   = "BWS1items")
  dialog.values <- getDialog("bws1Items", defaults)

  if(is.null(getDialog("bws1Items"))) putRcmdr("savedTableItems", NULL)

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
      assign(varname, if (is.null(ini.table)) {
                        tclVar("")
                      } else {
                        tclVar(ini.table[i])
                      }, envir = env)
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

  ini.table <- getRcmdr("savedTableItems")

  if (is.null(ini.table)) {
    rowsValue <- tclVar(dialog.values$ini.rowsValue)
  } else {
    rowsValue <- tclVar(length(ini.table))
  }

  rowsSlider <- tkscale(rowsFrame, from = 4, to = 21, showvalue = FALSE, 
                        variable = rowsValue,
                        resolution = 1, orient = "horizontal",
                        command = setUpTable)
  rowsShow   <- labelRcmdr(rowsFrame, textvariable = rowsValue,
                           width = 25, justify = "right")

  itemName <- tclVar(dialog.values$ini.itemName)
  item     <- ttkentry(outputFrame, width = "20", textvariable = itemName)


  onOK <- function() {
    putDialog("bws1Items", list(
      ini.rowsValue = tclvalue(rowsValue),
      ini.itemName   = tclvalue(itemName)))

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

    putRcmdr("savedTableItems", values)

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.questionnaire",
               reset       = "resetbws1Items",
               apply       = "bws1Items")

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

resetbws1Items <- function() {
  putRcmdr("savedTableItems", NULL)
  putDialog("bws1Items", NULL)
  bws1Items()
}

###############################################################################
bws1Questions <- function() {
  initializeDialog(
    title = gettextRcmdr("Create BWS1 Questions"))
  defaults <- list(
    ini.designName = "BWS1design",
    ini.itemName   = "BWS1items")
  dialog.values <- getDialog("bws1Questions", defaults)

  inputsFrame <- tkframe(top)

  # choice.sets
  designName <- tclVar(dialog.values$ini.designName)
  design <- ttkentry(inputsFrame, width = "20",
                     textvariable = designName)

  # item.names
  itemName <- tclVar(dialog.values$ini.itemName)
  item <- ttkentry(inputsFrame, width = "20", textvariable = itemName)

  onOK <- function() {
    putDialog("bws1Questions", list(
      ini.designName = tclvalue(designName),
      ini.itemName   = tclvalue(itemName)))

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
               apply       = "bws1Questions")

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
  defaults <- list(
    ini.datasetName      = "BWS1responses",
    ini.designName       = "BWS1design",
    ini.itemName         = "BWS1items",
    ini.nrespondentsName = "100",
    ini.parametersName   = "",
    ini.RNGseedName      = "")
  dialog.values <- getDialog("bws1Response", defaults)

  ###### Output
  outputFrame <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  activeFrame <- tkframe(outputFrame)

  datasetName <- tclVar(dialog.values$ini.datasetName)
  dataset <- ttkentry(datasetnameFrame, width = "20",
                      textvariable = datasetName)

  ###### Inputs
  inputsFrame <- tkframe(top)

  # choice.sets
  designName <- tclVar(dialog.values$ini.designName)
  design <- ttkentry(inputsFrame, width = "25", textvariable = designName)

  # item.names
  itemName <- tclVar(dialog.values$ini.itemName)
  item <- ttkentry(inputsFrame, width = "25", textvariable = itemName)

  # number of respondents
  nrespondentsName <- tclVar(dialog.values$ini.nrespondentsName)
  nrespondents <- ttkentry(inputsFrame, width = "25",
                           textvariable = nrespondentsName)

  # parameters
  parametersName <- tclVar(dialog.values$ini.parametersName)
  parameters <- ttkentry(inputsFrame, width = "25",
                         textvariable = parametersName)

  # random number generator seed
  RNGseedName <- tclVar(dialog.values$ini.RNGseedName)
  RNGseed <- ttkentry(inputsFrame, width = "25", textvariable = RNGseedName)


  onOK <- function() {
    putDialog("bws1Response", list(
    ini.datasetName      = tclvalue(datasetName),
    ini.designName       = tclvalue(designName),
    ini.itemName         = tclvalue(itemName),
    ini.nrespondentsName = tclvalue(nrespondentsName),
    ini.parametersName   = tclvalue(parametersName),
    ini.RNGseedName      = tclvalue(RNGseedName)))

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

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.response",
               reset       = "bws1Response",
               apply       = "bws1Response")

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(datasetnameFrame, sticky = "w")
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
  initializeDialog(
    title = gettextRcmdr("Create Data Set for BWS1 Analysis"))
  defaults <- list(
    ini.responsetype       = 1,
    ini.modeltype          = "maxdiff",
    ini.rowsValue          = "4",
    ini.datasetName        = "BWS1data",
    ini.designName         = "BWS1design",
    ini.itemName           = "BWS1items",
    ini.idName             = "id",
    ini.letterRB           = "1")
  dialog.values <- getDialog("bws1Dataset", defaults)


  ###### Output frame
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  activeFrame      <- tkframe(outputFrame)

  # output name
  datasetName <- tclVar(dialog.values$ini.datasetName)
  dataset <- ttkentry(datasetnameFrame, width = "20",
                      textvariable = datasetName)


  ###### Inputs frame
  inputsFrame <- tkframe(top)

  ### Frame in left
  leftFrame    <- tkframe(inputsFrame)
  objectsFrame <- tkframe(leftFrame)
  radio1Frame  <- tkframe(leftFrame)
  radio2Frame  <- tkframe(leftFrame)
  radio3Frame  <- tkframe(leftFrame)

  # choice.sets
  designName <- tclVar(dialog.values$ini.designName)
  design <- ttkentry(objectsFrame, width = "20", textvariable = designName)

  # item.names
  itemName <- tclVar(dialog.values$ini.itemName)
  item <- ttkentry(objectsFrame, width = "20", textvariable = itemName)

  # id
  idName <- tclVar(dialog.values$ini.idName)
  id <- ttkentry(objectsFrame, width = "20", textvariable = idName)

  # response.type
  radioButtons(radio1Frame, 
    name    = "responsetype",
    buttons = c("rowNumber", "itemNumber"),
    values  = c(1, 2),
    labels  = gettextRcmdr(c("Row number format", "Item number format")),
    initialValue = dialog.values$ini.responsetype,
    title   = gettextRcmdr("Response variable format"))

  # model
  radioButtons(radio3Frame,
    name = "modeltype",
    title   = gettextRcmdr("Model type"),
    buttons = c("maxdiff", "marginal", "sequential"),
    values  = c("maxdiff", "marginal", "sequential"),
    labels  = gettextRcmdr(c("Maxdiff model", "Marginal model",
                             "Marginal sequential model")),
    initialValue = dialog.values$ini.modeltype)


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
      if (is.null(ini.table)) {
        if (tclvalue(lettertypeVariable) == "3") {
          eval(parse(text = paste("assign(Bvarname, tclVar(''), envir = env)",
                                  sep = "")))
        } else {
          eval(parse(text = paste("assign(Bvarname, tclVar('", b, i,
                                  "'), envir = env)", sep = "")))
        }
      } else {
        eval(parse(text = paste("assign(Bvarname, tclVar(ini.table[", i,
                                ", 1]), envir = env)", sep = "")))
      }

      Wvarname <- paste(".tab.", i, ".2", sep = "")
      if (is.null(ini.table)) {
        if (tclvalue(lettertypeVariable) == "3") {
          eval(parse(text = paste("assign(Wvarname, tclVar(''), envir = env)",
                                  sep = "")))
        } else {
          eval(parse(text = paste("assign(Wvarname, tclVar('", w, i,
                                  "'), envir = env)", sep = "")))
        }
      } else {
        eval(parse(text = paste("assign(Wvarname, tclVar(ini.table[", i,
                                ", 2]), envir = env)", sep = "")))
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

  ini.table <- getRcmdr("savedTable")

  # Slider
  if (is.null(ini.table)) {
    rowsValue <- tclVar(dialog.values$ini.rowsValue)
  } else {
    rowsValue <- tclVar(nrow(ini.table))
  }
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
    initialValue = dialog.values$ini.letterRB,
    command = setUpTable)


  onOK <- function() {
    putDialog("bws1Dataset", list(
    ini.responsetype       = tclvalue(responsetypeVariable),
    ini.modeltype          = tclvalue(modeltypeVariable),
    ini.rowsValue          = tclvalue(rowsValue),
    ini.datasetName        = tclvalue(datasetName),
    ini.designName         = tclvalue(designName),
    ini.itemName           = tclvalue(itemName),
    ini.idName             = tclvalue(idName),
    ini.letterRB           = tclvalue(lettertypeVariable)))

    closeDialog()

    nrows           <- as.numeric(tclvalue(rowsValue))
    ncols           <- 2
    k               <- 0
    BWvarNames      <- rep("", nrows * ncols)
    BWvarNamesTable <- matrix("", nrow = nrows, ncol = ncols)

    for (i in 1:nrows) {
      for (j in 1:2) {
        k <- k + 1
        BWvarname <- paste(".tab.", i, ".", j, sep = "")
        BWvarNames[k] <- 
          eval(parse(text =
            paste("as.character(tclvalue(", BWvarname, "))", sep = "")))
        BWvarNamesTable[i, j] <- BWvarNames[k]
      }
    }

    putRcmdr("savedTable", BWvarNamesTable)

    cmd <- paste("c('", paste(BWvarNames, collapse = "','"), "')", sep = "")

    doItAndPrint(
      paste(tclvalue(datasetName), " <- bws.dataset(respondent.dataset = ",
            getRcmdr(".activeDataSet"),
            ", response.type = ", as.numeric(tclvalue(responsetypeVariable)),
            ", choice.sets = ", tclvalue(designName),
            ", design.type = 2", 
            ", item.names = ", tclvalue(itemName),
            ", id = '", tclvalue(idName), "'",
            ", response = ", cmd,
            ", model = '", tclvalue(modeltypeVariable), "')", sep = ""))

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws.dataset",
               reset       = "resetbws1Dataset",
               apply       = "bws1Dataset")

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(datasetnameFrame, sticky = "w")
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

resetbws1Dataset <- function(){
  putRcmdr("savedTable", NULL)
  putDialog("bws1Dataset", NULL)
  bws1Dataset()
}
###############################################################################
bws1Count <- function() {
  initializeDialog(title = gettextRcmdr("Calculate BWS1 Scores"))
  defaults <- list(
    ini.dataName = "BWS1scores")
  dialog.values <- getDialog("bws1Count", defaults)

  optionsFrame <- tkframe(top)
  datasetFrame <- tkframe(optionsFrame)
  activeFrame  <- tkframe(optionsFrame)

  # data
  dataName <- tclVar(dialog.values$ini.dataName)
  data     <- ttkentry(datasetFrame, width = "20", textvariable = dataName)


  onOK <- function() {
    putDialog("bws1Count", list(
    ini.dataName = tclvalue(dataName)))

    dataValue <- tclvalue(dataName)
    closeDialog()

    doItAndPrint(paste(dataValue," <- bws.count(data = ", ActiveDataSet(),
                       ", cl = 2)", sep = ""))

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws.count",
               reset       = "bws1Count",
               apply       = NULL)

  tkgrid(labelRcmdr(
    datasetFrame,
    text = gettextRcmdr("Name for scores ")),
    data, sticky = "w")
  tkgrid(datasetFrame, sticky = "w")
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
  initializeDialog(
    title = gettextRcmdr("Draw Distributions of BWS1 Scores"))
  defaults <- list(
    ini.scoretype = "bw",
    ini.NrowsName = "",
    ini.NcolsName = "")
  dialog.values <- getDialog("bws1CountBarplot1", defaults)

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
    initialValue = dialog.values$ini.scoretype,
    title   = gettextRcmdr("Score type"))

  # Nrows
  NrowsName <- tclVar(dialog.values$ini.NrowsName)
  Nrows     <- ttkentry(rowcolFrame, width = "4", textvariable = NrowsName)

  # Ncols
  NcolsName <- tclVar(dialog.values$ini.NcolsName)
  Ncols     <- ttkentry(rowcolFrame, width = "4", textvariable = NcolsName)

  onOK <- function() {
    putDialog("bws1CountBarplot1", list(
    ini.scoretype = tclvalue(scoretypeVariable),
    ini.NrowsName = tclvalue(NrowsName),
    ini.NcolsName = tclvalue(NcolsName)))

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
               apply       = "bws1CountBarplot1")

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
  initializeDialog(
    title = gettextRcmdr("Draw Mean BWS1 Scores"))
  defaults <- list(
    ini.scoretype      = "bw",
    ini.errorbartype   = "'none'",
    ini.conflevelName  = "0.95",
    ini.leftmarginName = "4.1")
  dialog.values <- getDialog("bws1CountBarplot2", defaults)

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
    initialValue = dialog.values$ini.scoretype,
    title   = gettextRcmdr("Score type"))

  # error.bar
  radioButtons(radio2Frame, 
    name    = "errorbartype",
    buttons = c("None", "SD", "SE", "CI"),
    values  = c("'none'", "'sd'", "'se'", "'ci'"),
    labels  = gettextRcmdr(c("None", "Standard deviation",
                             "Standard error", "Confidence interval =>")),
    initialValue = dialog.values$ini.errorbartype,
    title   = gettextRcmdr("Error bar type"))

  # conf.level
  conflevelName <- tclVar(dialog.values$ini.conflevelName)
  conflevel     <- ttkentry(confFrame, width = "5",
                            textvariable = conflevelName)


  # left.margin
  leftmarginName <- tclVar(dialog.values$ini.leftmarginName)
  leftmargin     <- ttkentry(leftmarFrame, width = "5",
                             textvariable = leftmarginName)

  onOK <- function() {
    putDialog("bws1CountBarplot2", list(
    ini.scoretype      = tclvalue(scoretypeVariable),
    ini.errorbartype   = tclvalue(errorbartypeVariable),
    ini.conflevelName  = tclvalue(conflevelName),
    ini.leftmarginName = tclvalue(leftmarginName)))

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
               apply       = "bws1CountBarplot2")

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
  initializeDialog(title = gettextRcmdr(
    "Draw Relationship between Means and Standard Deviations of BWS1 Scores"))
  defaults <- list(
    ini.scoretype    = "bw",
    ini.positiontype = "1",
    ini.xFromName    = "",
    ini.xToName      = "",
    ini.yFromName    = "",
    ini.yToName      = "")
  dialog.values <- getDialog("bws1CountPlot", defaults)

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
    initialValue = dialog.values$ini.scoretype,
    title   = gettextRcmdr("Score type"))

  # position
  radioButtons(radio2Frame, 
    name    = "positiontype",
    buttons = c("Below", "Left", "Above", "Right"),
    values  = c("1", "2", "3", "4"),
    labels  = gettextRcmdr(c("Below", "Left", "Above", "Right")),
    initialValue = dialog.values$ini.positiontype,
    title   = gettextRcmdr("Position of point labels"))

  # xlim
  xFromName <- tclVar(dialog.values$ini.xFromName)
  xFrom     <- ttkentry(xlimFrame, width = "5", textvariable = xFromName)
  xToName   <- tclVar(dialog.values$ini.xToName)
  xTo       <- ttkentry(xlimFrame, width = "5", textvariable = xToName)

  # ylim
  yFromName <- tclVar(dialog.values$ini.yFromName)
  yFrom     <- ttkentry(ylimFrame, width = "5", textvariable = yFromName)
  yToName   <- tclVar(dialog.values$ini.yToName)
  yTo       <- ttkentry(ylimFrame, width = "5", textvariable = yToName)


  onOK <- function() {
    putDialog("bws1CountPlot", list(
    ini.scoretype    = tclvalue(scoretypeVariable),
    ini.positiontype = tclvalue(positiontypeVariable),
    ini.xFromName    = tclvalue(xFromName),
    ini.xToName      = tclvalue(xToName),
    ini.yFromName    = tclvalue(yFromName),
    ini.yToName      = tclvalue(yToName)))

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
               apply       = "bws1CountPlot")

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
  defaults <- list(
    ini.responseVarName = "RES",
    ini.strataVarName   = "STR",
    ini.baseItem        = "1")
  dialog.values <- getDialog("bws1FitmodelSimple", defaults)

  .activeModel <- ActiveModel()
  currentModel <- if(!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }

  # remove a term 'strata' from the current model formula
  if (currentModel) {
    currentRhs <- currentFields$rhs
    currentRhs <- gsub(' +', '', currentRhs)
    currentRhs <- unlist(strsplit(currentRhs, "\\+"))
    strataPos  <- grep("strata\\(", currentRhs)
    currentRhs <- currentRhs[-strataPos]
    currentRhs <- paste(currentRhs, collapse = " + ")

    currentFields$rhs <- currentRhs
  }

  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }

  UpdateModelNumber()
  modelName  <- tclVar(paste("BWS1model.", getRcmdr("modelNumber"), sep = ""))
  modelFrame <- tkframe(top)
  model      <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  responseVarName  <- tclVar(dialog.values$ini.responseVarName)
  responseVarFrame <- tkframe(top)
  responseVar      <- ttkentry(responseVarFrame, width = "5",
                               textvariable = responseVarName)

  strataVarName  <- tclVar(dialog.values$ini.strataVarName)
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
    initialValue = dialog.values$ini.baseItem,
    title        = gettextRcmdr("Base item")) 
  

  onOK <- function () {
    putDialog("bws1FitmodelSimple", list(
      ini.responseVarName = tclvalue(responseVarName),
      ini.strataVarName   = tclvalue(strataVarName),
      ini.baseItem        = tclvalue(catalogVariable)))

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
    doItAndPrint(paste0(modelValue))
    doItAndPrint(paste0("gofm(", modelValue,")"))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "clogit", model = TRUE,
               reset       = "resetbws1FitmodelSimple",
               apply       = "bws1FitmodelSimple")

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

resetbws1FitmodelSimple <- function() {
  putRcmdr("reset.model", TRUE)
  putDialog("bws1FitmodelSimple", NULL)
  putDialog("bws1FitmodelSimple", NULL, resettable = FALSE)
  bws1FitmodelSimple()
}
###############################################################################
bws1Fitmodel <- function() {
  initializeDialog(title = 
    gettextRcmdr("Fit BWS1 Model"))
  defaults <- list(
    ini.responseVarName = "RES",
    ini.strataVarName   = "STR")
  dialog.values <- getDialog("bws1Fitmodel", defaults)

  .activeModel <- ActiveModel()
  currentModel <- if(!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }

  # remove a term 'strata' from the current model formula
  if (currentModel) {
    currentRhs <- currentFields$rhs
    currentRhs <- gsub(' +', '', currentRhs)
    currentRhs <- unlist(strsplit(currentRhs, "\\+"))
    strataPos  <- grep("strata\\(", currentRhs)
    currentRhs <- currentRhs[-strataPos]
    currentRhs <- paste(currentRhs, collapse = " + ")

    currentFields$rhs <- currentRhs
  }

  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }

  UpdateModelNumber()
  modelName <- tclVar(paste("BWS1model.", getRcmdr("modelNumber"), sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  responseVarName  <- tclVar(dialog.values$ini.responseVarName)
  responseVarFrame <- tkframe(top)
  responseVar      <- ttkentry(responseVarFrame, width = "20",
                               textvariable = responseVarName)

  strataVarName  <- tclVar(dialog.values$ini.strataVarName)
  strataVarFrame <- tkframe(top)
  strataVar      <- ttkentry(strataVarFrame, width = "20",
                             textvariable = strataVarName)
  
  onOK <- function () {
    putDialog("bws1Fitmodel", list(
      ini.responseVarName = tclvalue(responseVarName),
      ini.strataVarName   = tclvalue(strataVarName)))

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
    doItAndPrint(paste0(modelValue))
    doItAndPrint(paste0("gofm(", modelValue,")"))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "clogit",
               model       = TRUE,
               reset       = "resetbws1Fitmodel",
               apply       = "bws1Fitmodel")

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

resetbws1Fitmodel <- function() {
  putRcmdr("reset.model", TRUE)
  putDialog("bws1Fitmodel", NULL)
  putDialog("bws1Fitmodel", NULL, resettable = FALSE)
  bws1Fitmodel()
}
###############################################################################
bws1SharePref <- function() {
  initializeDialog(title = 
    gettextRcmdr("Calculate Shares of Preferences for Items"))
  defaults <- list(
    ini.baseName  = "<no variable selected>",
    ini.bName     = "",
    ini.ordertype = "1")
  dialog.values <- getDialog("bws1SharePref", defaults)

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
    initialSelection = if (is.null(baseItem)) {
        gettextRcmdr(dialog.values$ini.baseName)
      } else {
        baseItem
      } , title = "Base item")
 
  # coef
  bName <- tclVar(dialog.values$ini.bName)
  b <- ttkentry(inputsFrame, width = "40", textvariable = bName)

  # order
  radioButtons(radioFrame, 
    name    = "ordertype",
    buttons = c("None", "Increasing", "Decreasing"),
    values  = c("1", "2", "3"),
    labels  = gettextRcmdr(c("None", "Increasing", "Decreasing")),
    initialValue = dialog.values$ini.ordertype,
    title   = gettextRcmdr("Order"))

  onOK <- function() {
    baseName <- getSelection(baseitem)

   putDialog("bws1SharePref", list(
    ini.baseName  = baseName,
    ini.bName     = tclvalue(bName),
    ini.ordertype = tclvalue(ordertypeVariable)))

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
               apply       = "bws1SharePref")

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
clogitP <- function() {
  activeModelP() && class(get(ActiveModel()))[1] == "clogit"
}
bws1dataP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bwsdataset"
}
bws1count2P <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bws.count2"
}

