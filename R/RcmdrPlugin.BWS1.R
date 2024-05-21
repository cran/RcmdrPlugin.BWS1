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
    title = gettextRcmdr("Design Choice Sets for BWS1"))
  defaults <- list(
    ini.designName        = "BWS1design",
    ini.itemName          = "BWS1items",
    ini.NitemsValue       = "4",
    ini.NquesValue        = "4",
    ini.NitemsPquesValue  = "3",
    ini.IterName          = "1000",
    ini.RNGseedName       = "",
    saveVariable          = "")
  dialog.values <- getDialog("bws1Design", defaults)

  if (is.null(getDialog("bws1Design"))) putRcmdr("savedTableItems", NULL)


  #### output frame ####
  outputFrame <- tkframe(top)
  designFrame <- tkframe(outputFrame)
  itemsFrame  <- tkframe(outputFrame)
  saveFrame   <- tkframe(outputFrame)

  # name for bibd
  designName <- tclVar(dialog.values$ini.designName)
  design     <- ttkentry(outputFrame, width = "14", textvariable = designName)

  # name for items
  itemName <- tclVar(dialog.values$ini.itemName)
  item     <- ttkentry(itemsFrame, width = "14", textvariable = itemName) 


  #### input frame ####
  inputFrame      <- tkframe(top)
  optionsFrame    <- tkframe(inputFrame)
  iterationFrame  <- tkframe(inputFrame)
  RNGseedFrame    <- tkframe(inputFrame)
  TABLEFrame      <- tkframe(inputFrame)
  tableTitleFrame <- tkframe(TABLEFrame)
  tableFrame1     <- tkframe(TABLEFrame)
  tableFrame2     <- tkframe(TABLEFrame)
  tableFrame3     <- tkframe(TABLEFrame)

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
  Iter     <- ttkentry(iterationFrame, width = "10",
                       textvariable = IterName)

  # random number generator seed
  RNGseedName <- tclVar(dialog.values$ini.RNGseedName)
  RNGseed     <- ttkentry(RNGseedFrame, width = "10",
                          textvariable = RNGseedName)

  # table for items
  ## Initial settings
  env <- environment()
  assign(".tableFrame1", tkframe(tableFrame1), envir = env)
  tkdestroy(get(".tableFrame1", envir = env))
  assign(".tableFrame2", tkframe(tableFrame2), envir = env)
  tkdestroy(get(".tableFrame2", envir = env))
  assign(".tableFrame3", tkframe(tableFrame3), envir = env)
  tkdestroy(get(".tableFrame3", envir = env))
  assign(".tableFrame1", tkframe(tableFrame1), envir = env)
  assign(".tableFrame2", tkframe(tableFrame2), envir = env)
  assign(".tableFrame3", tkframe(tableFrame3), envir = env)
  nrows <- 7
  ncols <- 3
  
  initial.table <- getRcmdr("savedTableItems")
  
  ## Names of columns
  tkgrid(labelRcmdr(.tableFrame1, text = ""),
         labelRcmdr(.tableFrame1, text = "Item 1 to 7"),
         sticky = "w")
  tkgrid(labelRcmdr(.tableFrame2, text = ""),
         labelRcmdr(.tableFrame2, text = "Item 8 to 14"),
         sticky = "w")
  tkgrid(labelRcmdr(.tableFrame3, text = ""),
         labelRcmdr(.tableFrame3, text = "Item 15 to 21"),
         sticky = "w")

  ## Names of rows and cells
  ### Colmun 1
  for (i in 1:nrows) {
    varname <- paste0(".tab.", i, ".1")
    assign(varname, if (is.null(initial.table)) {
                      tclVar("")
                    } else {
                      tclVar(initial.table[i, 1])
                    }, envir = env)
    row.varname <- paste0(".rowname.", i)
    make.row <- paste0("labelRcmdr(.tableFrame1, text = '", i, "')")
    make.row <- paste0(make.row, ", ",
                      "ttkentry(.tableFrame1, width = '20', textvariable =",
                      varname, ")")
    eval(parse(text = paste0("tkgrid(", make.row, ")")), envir = env)
  }
  ### Colmun 2
  for (i in 1:nrows) {
    varname <- paste0(".tab.", i, ".2")
    assign(varname, if (is.null(initial.table)) {
                      tclVar("")
                    } else {
                      tclVar(initial.table[i, 2])
                    }, envir = env)
    row.varname <- paste0(".rowname.", i)
    make.row <- paste0("labelRcmdr(.tableFrame2, text = '", i + 7, "')")
    make.row <- paste0(make.row, ", ",
                      "ttkentry(.tableFrame2, width = '20', textvariable =",
                      varname, ")")
    eval(parse(text = paste0("tkgrid(", make.row, ")")), envir = env)
  }
  ### Colmun 3
  for (i in 1:nrows) {
    varname <- paste0(".tab.", i, ".3")
    assign(varname, if (is.null(initial.table)) {
                      tclVar("")
                    } else {
                      tclVar(initial.table[i, 3])
                    }, envir = env)
    row.varname <- paste0(".rowname.", i)
    make.row <- paste0("labelRcmdr(.tableFrame3, text = '", i + 14, "')")
    make.row <- paste0(make.row, ", ",
                      "ttkentry(.tableFrame3, width = '20', textvariable =",
                      varname, ")")
    eval(parse(text = paste0("tkgrid(", make.row, ")")), envir = env)
  }

  tkgrid(get(".tableFrame1", envir = env), sticky = "w")
  tkgrid(get(".tableFrame2", envir = env), sticky = "w")
  tkgrid(get(".tableFrame3", envir = env), sticky = "w")

  # Save
  saveVariable <- tclVar(dialog.values$saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)
  
  

  #### onOK function ####
  onOK <- function() {
    putDialog("bws1Design", list(
      ini.designName        = tclvalue(designName),
      ini.itemName          = tclvalue(itemName),
      ini.NitemsValue       = tclvalue(NitemsValue),
      ini.NquesValue        = tclvalue(NquesValue),
      ini.NitemsPquesValue  = tclvalue(NitemsPquesValue),
      ini.IterName          = tclvalue(IterName),
      ini.RNGseedName       = tclvalue(RNGseedName),
      saveVariable          = tclvalue(saveVariable)))

    itemValue   <- trim.blanks(tclvalue(itemName))
    designValue <- trim.blanks(tclvalue(designName))

    closeDialog()

    # Item table
    nrows <- 7
    ncols <- 3
    varNames <- matrix("", nrow = nrows, ncol = ncols)
    
    for (i in 1:nrows) {
      for (j in 1:ncols) {
        varname <- paste0(".tab.", i, ".", j)
        varNames[i, j] <- eval(parse(text = paste0("as.character(tclvalue(", varname, "))")))
      }
    }

    a <- length(varNames[varNames != ""])
    b <- as.numeric(tclvalue(NitemsValue))
    if (a != b) {
      errorCondition(message = gettextRcmdr("Number of items set by the slider differs from number of elements filled with items"))
      return()
    }

    # Store items into savedTableItems
    putRcmdr("savedTableItems", varNames)

    # Vector containing items
    itemNamesVector <- as.vector(varNames)
    itemNamesVector <- itemNamesVector[1:as.numeric(tclvalue(NitemsValue))]

    # Set random seed
    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste0("set.seed(", as.numeric(tclvalue(RNGseedName)), ")")
    }

    # Search BIBD    
    cmd <- paste0("find.BIB(t = ", as.numeric(tclvalue(NitemsValue)),
                        ", b = ", as.numeric(tclvalue(NquesValue)),
                        ", k = ", as.numeric(tclvalue(NitemsPquesValue)),
                        ", iter = ", as.numeric(tclvalue(IterName)), ")")

    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      doItAndPrint(paste0(cmd.seed))
      doItAndPrint(paste0(designValue, " <- ", cmd))
    } else {
      doItAndPrint(paste0(designValue, " <- ", cmd))
    }
    
    doItAndPrint(paste0(designValue))
    doItAndPrint(paste0("isGYD(", designValue, ")"))

    # Create object containing items
    cmd <- paste0(itemValue, " <- c('", paste(itemNamesVector, collapse = "', '"), "')")
    doItAndPrint(cmd)
    
    # Save choice sets and items
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          '{"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(designValue, ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', designValue,
                    ', ', itemValue,
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(paste0(gettextRcmdr("BWS1 design and items were exported to file: "),
                     saveFile),
              type = "note")
    }
        
    tkfocus(CommanderWindow())
  }


  #### specification of dialog box ####
  # OK cancel help buttons
  OKCancelHelp(helpSubject = "bws1Design",
               reset       = "resetBws1Items",
               apply       = "bws1Design")

  ## output
  tkgrid(labelRcmdr(designFrame,
                    text = gettextRcmdr("Name for design ")),
         design, sticky = "w")
  tkgrid(labelRcmdr(itemsFrame,
                    text = gettextRcmdr("Name for items ")),
         item, sticky = "w")
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame,
                    text = gettextRcmdr("Save to file")),
         sticky = "w")
  tkgrid(designFrame, labelRcmdr(outputFrame, text = "   "),
         itemsFrame,  labelRcmdr(outputFrame, text = "   "),
         saveFrame,   sticky = "w")  
  tkgrid(outputFrame, sticky = "w")

  ## blank line
  tkgrid(labelRcmdr(top, text = ""))

  ## input
  # design parameters
  tkgrid(labelRcmdr(optionsFrame,
                    text = gettextRcmdr("Design parameters:")),
         sticky = "w")

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

  tkgrid(optionsFrame, sticky = "w")

  # item table
  tkgrid(labelRcmdr(tableTitleFrame, text = gettextRcmdr("Items:")),
         sticky = "w")
  tkgrid(tableTitleFrame, sticky = "w")
  tkgrid(tableFrame1, labelRcmdr(TABLEFrame, text = "  "),
         tableFrame2, labelRcmdr(TABLEFrame, text = "  "),
         tableFrame3, sticky = "nw")
  tkgrid(TABLEFrame,  sticky = "nw")

  # number of iterations
  tkgrid(labelRcmdr(iterationFrame, text = gettextRcmdr("Search option:")),
         sticky = "w")
  tkgrid(labelRcmdr(iterationFrame, 
                    text = gettextRcmdr("Number of iterations")),
         Iter, sticky = "w")
  tkgrid(iterationFrame, sticky = "w")

  # seed for RNG
  tkgrid(labelRcmdr(RNGseedFrame, text = gettextRcmdr("Reproducibility:")),
         sticky = "w")
  tkgrid(labelRcmdr(RNGseedFrame,
           text = gettextRcmdr("Seed for random number generator (optional) ")),
         RNGseed, sticky = "w")
  tkgrid(RNGseedFrame, sticky = "w")

  tkgrid(inputFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetBws1Items <- function() {
  putRcmdr("savedTableItems", NULL)
  putDialog("bws1Design", NULL)
  bws1Design()
}

###############################################################################
bws1Questions <- function() {
  initializeDialog(
    title = gettextRcmdr("Display BWS1 Questions"))
  defaults <- list(
    ini.designName = "BWS1design",
    ini.itemName   = "BWS1items")
  dialog.values <- getDialog("bws1Questions", defaults)

  inputsFrame <- tkframe(top)
  designFrame <- tkframe(inputsFrame)
  itemsFrame  <- tkframe(inputsFrame)

  # choice.sets
  designName <- tclVar(dialog.values$ini.designName)
  design     <- ttkentry(designFrame, width = "14",
                         textvariable = designName)

  # item.names
  itemName <- tclVar(dialog.values$ini.itemName)
  item <-     ttkentry(itemsFrame, width = "14",
                       textvariable = itemName)

  onOK <- function() {
    putDialog("bws1Questions", list(
      ini.designName = tclvalue(designName),
      ini.itemName   = tclvalue(itemName)))

    designValue <- tclvalue(designName)
    itemVars    <- tclvalue(itemName)
    closeDialog()

    doItAndPrint(paste0("bws.questionnaire(choice.sets = ", designValue,
                        ", design.type = 2", 
                        ", item.names = ", itemVars, ")"))
    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws1Questions",
               reset       = "bws1Questions",
               apply       = "bws1Questions")

  tkgrid(labelRcmdr(designFrame,
                    text = gettextRcmdr("Design ")),
         design, sticky = "w")
  tkgrid(labelRcmdr(itemsFrame,
                    text = gettextRcmdr("Items ")),
         item, sticky = "w")
  tkgrid(designFrame, labelRcmdr(inputsFrame, text = "   "), itemsFrame,
         sticky = "w")
  tkgrid(inputsFrame, sticky = "w")

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
    ini.letterRB           = "1",
    saveVariable           = "0")
  dialog.values <- getDialog("bws1Dataset", defaults)

  if(is.null(getDialog("bws1Dataset"))) putRcmdr("savedTableBws1Dataset", NULL)

  ###### Output frame
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  saveFrame        <- tkframe(outputFrame)

  # output name
  datasetName <- tclVar(dialog.values$ini.datasetName)
  dataset     <- ttkentry(datasetnameFrame, width = "14",
                          textvariable = datasetName)

  # save
  saveVariable <- tclVar(dialog.values$saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)  


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
  design     <- ttkentry(objectsFrame, width = "14",
                         textvariable = designName)

  # item.names
  itemName <- tclVar(dialog.values$ini.itemName)
  item     <- ttkentry(objectsFrame, width = "14",
                       textvariable = itemName)

  # id
  idName <- tclVar(dialog.values$ini.idName)
  id     <- ttkentry(objectsFrame, width = "14",
                     textvariable = idName)

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

  ini.table <- getRcmdr("savedTableBws1Dataset")

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
    ini.responsetype = tclvalue(responsetypeVariable),
    ini.modeltype    = tclvalue(modeltypeVariable),
    ini.rowsValue    = tclvalue(rowsValue),
    ini.datasetName  = tclvalue(datasetName),
    ini.designName   = tclvalue(designName),
    ini.itemName     = tclvalue(itemName),
    ini.idName       = tclvalue(idName),
    saveVariable     = tclvalue(saveVariable),
    ini.letterRB     = tclvalue(lettertypeVariable)))

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

    activeDataSet(tclvalue(datasetName))
    
    # Save to file
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          '{"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(tclvalue(datasetName), ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', tclvalue(datasetName),
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(paste(gettextRcmdr("Dataset for BWS1 analysis was exported to file: "),
                    saveFile),
              type = "note")
    }


    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws1Dataset",
               reset       = "resetBws1Dataset",
               apply       = "bws1Dataset")

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame, text = gettextRcmdr("Save to file")),
         sticky = "w")
  tkgrid(datasetnameFrame,
         labelRcmdr(outputFrame, text = "  "),
         saveFrame, sticky = "w")
  tkgrid(outputFrame, sticky = "w")

  # Blank
  tkgrid(labelRcmdr(top, text = ""))

  # Inputs
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Design ")),
    design, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Items ")),
    item, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("ID variable ")),
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

resetBws1Dataset <- function(){
  putRcmdr("savedTableBws1Dataset", NULL)
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
  data     <- ttkentry(datasetFrame, width = "14", textvariable = dataName)


  onOK <- function() {
    putDialog("bws1Count", list(
    ini.dataName = tclvalue(dataName)))

    dataValue <- tclvalue(dataName)
    closeDialog()

    doItAndPrint(paste(dataValue," <- bws.count(data = ", ActiveDataSet(),
                       ", cl = 2)", sep = ""))

    activeDataSet(dataValue)

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws1Count",
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


  OKCancelHelp(helpSubject = "bws1CountBarplot1",
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
    buttons = c("BW", "SBW", "B", "W"),
    values  = c("bw", "sbw", "b", "w"),
    labels  = gettextRcmdr(c("Best-minus-Worst (BW)", "Standardized BW",
                             "Best", "Worst")),
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


  OKCancelHelp(helpSubject = "bws1CountBarplot2",
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


  OKCancelHelp(helpSubject = "bws1CountPlot",
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
bws1Model <- function() {
  initializeDialog(title = 
    gettextRcmdr("Fit Model to BWS1 Data"))
  defaults <- list(
    ini.responseVarName  = "RES",
    ini.covariateVarName = NULL,
    ini.strataVarName    = "STR",
    ini.baseItem         = "1")
  dialog.values <- getDialog("bws1Model", defaults)

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

##### Output Frame
  UpdateModelNumber()
  outputFrame <- tkframe(top)
  modelName   <- tclVar(paste("BWS1model.", getRcmdr("modelNumber"), sep = ""))
  model       <- ttkentry(outputFrame, width = "14", textvariable = modelName)

##### Input Frame
  inputFrame <- tkframe(top)
  
## Frames in left
  leftFrame        <- tkframe(inputFrame)
  responseVarFrame <- tkframe(leftFrame)
  strataVarFrame   <- tkframe(leftFrame)
  radioFrame       <- tkframe(leftFrame)

# set response variable (responseVarFrame)
  responseVarName  <- tclVar(dialog.values$ini.responseVarName)
  responseVar      <- ttkentry(responseVarFrame, width = "5",
                               textvariable = responseVarName)

# set strata variable (strataVarFrame)
  strataVarName  <- tclVar(dialog.values$ini.strataVarName)
  strataVar      <- ttkentry(strataVarFrame, width = "5",
                             textvariable = strataVarName)

# select base item (radioFrame)
  rhsALL    <- attributes(eval(parse(text = ActiveDataSet())))$vnames
  NUMrhsALL <- length(rhsALL)

  radioButtons(radioFrame, 
    name         = "catalog",
    buttons      = paste("n", 1:NUMrhsALL, sep = ""),
    values       = 1:NUMrhsALL,
    labels       = gettextRcmdr(rhsALL),
    initialValue = dialog.values$ini.baseItem,
    title        = gettextRcmdr("Base item")) 
  
## Frames in right
  rightFrame      <- tkframe(inputFrame)
  covariatesFrame <- tkframe(rightFrame)

# select covariates
  availableCovariates <- 
    sort(attributes(eval(parse(text = ActiveDataSet())))$respondent.characteristics)
  covariatesBox <- variableListBox(
                     covariatesFrame,
                     availableCovariates,
                     title = gettextRcmdr("Covariates (pick zero or more)"),
                     selectmode = "multiple",
                     listHeight = 5,
                     initialSelection = varPosn(dialog.values$ini.covariateVarName,
                                                vars = availableCovariates))


  onOK <- function () {
    putDialog("bws1Model", list(
      ini.responseVarName  = tclvalue(responseVarName),
      ini.strataVarName    = tclvalue(strataVarName),
      ini.covariateVarName = getSelection(covariatesBox),
      ini.baseItem         = tclvalue(catalogVariable)))

    modelValue  <- trim.blanks(tclvalue(modelName))
    responseVar <- trim.blanks(tclvalue(responseVarName))
    strataVar   <- trim.blanks(tclvalue(strataVarName))
    k           <- as.numeric(tclvalue(catalogVariable))
    rhsVars     <- rhsALL[-k]
    rhsVars     <- paste(rhsVars, collapse = " + ")
    covariates  <- getSelection(covariatesBox)
    closeDialog()
   
    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste(", subset = ", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }

    if (length(covariates) == 0) {
      formula <- paste(responseVar, " ~ ", rhsVars, 
                       " + strata(", strataVar ,")", sep = "")
    } else {
      covariates <- paste(covariates, collapse = "+")
      formula <- paste(responseVar, " ~ (", rhsVars, ") * (", covariates, 
                       ") - (", covariates, ") + strata(", strataVar ,")", 
                       sep = "")
    }

    cmd <- paste("clogit(", formula, ", data = ", ActiveDataSet(), subset, 
                 ")", sep = "")

    doItAndPrint(paste(modelValue, " <- ", cmd, sep = ""))
    justDoIt(paste("attributes(", modelValue, ")$baseitem <- c('",
                   rhsALL[k], "')", sep = ""))
    doItAndPrint(paste0(modelValue))
    doItAndPrint(paste0("gofm(", modelValue,")"))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws1Model", model = TRUE,
               reset       = "resetBws1Model",
               apply       = "bws1Model")

## Output
  tkgrid(labelRcmdr(outputFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

## Inputs
# Frames in left  
  tkgrid(labelRcmdr(responseVarFrame, 
                    text = gettextRcmdr("Response variable ")),
         labelRcmdr(responseVarFrame,
                    text = tclvalue(responseVarName),
                    relief = "solid",
                    foreground = "green"),
         sticky = "w")
  tkgrid(responseVarFrame, sticky = "w")

  tkgrid(catalogFrame, sticky = "w")
  tkgrid(radioFrame, sticky = "w")

  tkgrid(labelRcmdr(strataVarFrame,
                    text = gettextRcmdr("Stratification variable ")),
         labelRcmdr(strataVarFrame,
                    text = tclvalue(strataVarName),
                    relief = "solid",
                    foreground = "green"),
         sticky = "w")
  tkgrid(strataVarFrame, sticky = "w")

# Frames in right
  tkgrid(getFrame(covariatesBox), sticky = "nw")
  tkgrid(covariatesFrame, sticky = "w")

# Inputs Frame
  tkgrid(leftFrame, labelRcmdr(inputFrame, text = "   "), 
         rightFrame, sticky = "nw")
  tkgrid(inputFrame, sticky = "w")
  
# subset
  subsetBox(rightFrame, model = TRUE)
  tkgrid(labelRcmdr(rightFrame, text = ""))
  tkgrid(subsetFrame, sticky = "w")

# Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetBws1Model <- function() {
  putRcmdr("reset.model", TRUE)
  putDialog("bws1Model", NULL)
  putDialog("bws1Model", NULL, resettable = FALSE)
  bws1Model()
}
###############################################################################
bws1SharePreference <- function() {
  initializeDialog(title = 
    gettextRcmdr("Calculate Shares of Preferences"))
  defaults <- list(
    ini.baseName  = "<no variable selected>",
    ini.bName     = "",
    ini.ordertype = "1")
  dialog.values <- getDialog("bws1SharePreference", defaults)

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

   putDialog("bws1SharePreference", list(
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

  OKCancelHelp(helpSubject = "bws1SharePreference",
               reset       = "bws1SharePreference",
               apply       = "bws1SharePreference")

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
bws1Load <- function() {
  file <- tclvalue(tkgetOpenFile(filetype = gettextRcmdr(
            '{"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}')))
  if (file == "") {
    return()
  }
  setBusyCursor()
  on.exit(setIdleCursor)
  
  cmd <- paste0('load("', file, '")')
  loadedObjects <- justDoIt(cmd)
  logger(cmd)
  Message(paste0(gettextRcmdr("Names of loaded objects: "),
                 paste(loadedObjects, collapse = ", ")),
          type = "note")
          
  tkfocus(CommanderWindow())
}
###############################################################################
bws1ClogitP <- function() {
  activeModelP() && 
  class(get(ActiveModel()))[1] == "clogit" &&
  class(get(ActiveDataSet()))[1] == "bwsdataset"
}
bws1DataP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bwsdataset"
}
bws1Count2P <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bws.count2"
}

