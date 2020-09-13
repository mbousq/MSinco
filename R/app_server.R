#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 geom_line labs geom_col geom_text ggsave aes
#' @noRd

app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  options(future.supportsMulticore.unstable = "quiet")
  options(stringsAsFactors=FALSE)
  options(shiny.reactlog=TRUE)
  options(digits=3)
  shinyOptions(cache = memoryCache(max_size = 300e6))
  onStop(function(state) future::plan("sequential"))
  session$onSessionEnded(stopApp)
  n_undo = 0
  paramFile_header <- c("name", "rt_left", "rt_right", "mass_0", "lab_atoms")
  paramFile_num_header <- c("rt_left", "rt_right", "mass_0", "lab_atoms")

    # TODO this is a dirsty hack for direcotry and paramFile
  values <- reactiveValues()
  values[["DF"]] <- NULL
  values[["lastValues"]] <- NULL
  values[["directory"]] <- NULL
  values[["paramFile"]] <- NULL
  values[["runNo"]] <- 0
  values[["rawData"]] <- NULL
  values[["lastFragment"]] <- NULL
  values[["plots_tic"]] <- NULL
  values[["plots_sim"]] <- NULL
  values[["plots_ms"]] <- NULL
  values[["file"]] <- NULL
  values[["rawData_tic"]] <- NULL
  values[["plotIndex"]]  <- NULL
  # values[["Fragment"]] <- NULL
  
  
  
  # values[["paramFileTable"]] <- NULL
  # values[["paramFile"]] <- NULL
  # values2 <- reactiveValues(DF = NULL)
  
  # onStop(parallel::stopCluster(cl))
  
  # output[["mass0"]] <- NULL
  # outputOptions(output, "mass0", suspendWhenHidden = FALSE)
  
  
  # directory <- eventReactive({
  #   input$folderButton
  #   input$importDirButton
  #   # input$navbar
  #   }, {
  #   # browser()
  #   # req(input$fileButton)
  #   # req(paramFile())
  # 
  #   tryCatch(
  # 
  #     # if (isNamespaceLoaded("rstudioapi")) {
  #     #
  #     #   rstudioapi::selectDirectory()
  #     # } else {
  #     #
  #     easycsv::choose_dir()
  # 
  #     # }
  #     ,
  #     warning = function(cond) {message("Please choose a valid directory"); return(NULL) }
  #     # error = function(cond) {message(); return(NULL)}
  #   )
  # 
  # })
  
  
  # TODO dirty hack. Need to change all values[["directory"]] -> directory
  #   directory <- eventReactive({
  # values$directory
  #     # input$navbar
  #   }, {
  # 
  # values[["directory"]]
  #     
  #   })
  
  # paramFile <- eventReactive({
  #   values$paramFile
  #   # input$navbar
  # }, {
  #   
  #   values[["paramFile"]]
  #   
  # })
  
  
  observeEvent(input$quitButton, {
    stopApp()
  })
  
  observeEvent({
    input$ok
    # input$newDirButton
    # input$navbar
  }, {
    # browser()
    shiny::removeModal()
    updateNavbarPage(session, "navbar",selected = NULL)
    
    # files <- input$rawDataFiles
    # rawDataFilesPath <- tools::file_ext(files$datapath)
    # req(input$fileButton)
    # req(paramFile())
    
    tryCatch({
      
      # if (isNamespaceLoaded("rstudioapi")) {
      #
      #   rstudioapi::selectDirectory()
      # } else {
      #
      tmp <- easycsv::choose_dir()
      
      pathDir <- paste0(tmp, "/Experiment_", format(Sys.time(), "%d-%m-%y_%H%M%S"))
      dir.create(pathDir, mode = "0777")
      
      if (input$includeSampleData == TRUE) {
        file.copy(system.file("extdata","Netcdfs" ,package = "MSinco"), pathDir, recursive = T)
      }
      
      if (input$includeParameters == TRUE) {
        file.copy(system.file("extdata","Parameters" ,package = "MSinco"), pathDir, recursive = T)
      }
      
      # if (input$addDataFiles == TRUE) {
      #   files <- choose.file()
      #   file.copy(files, paste0(pathDir,"/Netcdfs"))
      # }
      
      return(pathDir)
      
      # }
    },
    warning = function(cond) {message("Please choose a valid directory"); return(NULL) }
    # error = function(cond) {message(); return(NULL)}
    )
    
    
  })
  
  
  # newDir <- function() {
  #   showModal(
  #     modalDialog(
  #       h5("Create new directory"),
  #       checkboxInput("includeSampleData","include sample data ?", value = FALSE),
  #       checkboxInput("includeParameters","include Parameters examples ?", value = TRUE),
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("ok", "OK")
  #       )
  #     )
  #
  #   )
  #
  # }
  observeEvent({
    # input$newDirButton
    input$newDirButton
    # input$navbar == "newdir"
  }, {
    
    showModal(
      modalDialog(
        h5("Create new directory"),
        checkboxInput("includeSampleData","include sample data ?", value = FALSE),
        checkboxInput("includeParameters","include Parameters examples ?", value = TRUE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
      
    )
    
  },ignoreInit = T)
  
  observeEvent({
    # input$newDirButton
    input$navbar
    # input$navbar == "newdir"
  }, {
    # req(input$navbar != "Home", input$navbar != "Settings", input$navbar != "Parameters")
    # browser()
    
    # Import experiment directory PATH
    if (input$navbar == "Import experiment") {
      
      values[["runNo"]] <- values[["runNo"]] + 1
      
      # browser()
      
      tryCatch({
        
        # if (isNamespaceLoaded("rstudioapi")) {
        #
        #   rstudioapi::selectDirectory()
        # } else {
        #
        
        values[["directory"]] <- easycsv::choose_dir()
        
        
      }
      #}
      ,
      warning = function(cond) {message("Please choose a valid directory"); return(NULL) }
      )
      
      if (is.null(values[["directory"]])) {
        updateTabsetPanel(session,inputId = "navbar",selected = "Home")
        req(FALSE)
      }
      
      # Import parameters
      
      
      values[["paramFile"]] <- paste0(values[["directory"]], "/Parameters/ParameterFile.xlsx")
      
      if (!is.null(values[["paramFile"]])) {
        
        
        # browser()
        tmp <- openxlsx::read.xlsx(values[["paramFile"]],sheet = 1, cols = 1:5) %>% data.table::setDT() #,.name_repair="minimal"
        
        # expected_header <- c("name", "RT", "lOffset", "rOffset", "Mass0", "LabelAtoms")
        # 
        
        validate(
          
          if (identical(paramFile_header, colnames(tmp)[1:5])) {
            
            NULL
            
          } else {
            
            #     output$table <- DT::renderDT({
            #   #browser()
            #
            #       DT::datatable(isolate(values[["DF"]]), options = list(
            #         pageLength = 50,
            #         lengthMenu = c(10, 25, 50, 100, 1000)
            #       ),
            #       editable = 'cell',selection = 'none',)
            # })
            
            warning("The parameterFile is not properly formatted.")
            showNotification("Error: the parameterFile is not properly formatted.")
            
          }
        )
        
        values[["DF"]] <- tmp
      }
      
      # Import data
      
      # browser()
      if ("parallel" %in% input$settings) {
        
        future::plan("multiprocess")
        
      }
      # browser()
      # progressr::without_progress()
      # cat(format(Sys.time(), "%X"))
      progressr::withProgressShiny(message = 'Extracting data ...',value = 0,  {
        
        netCDFs <- list.files(paste0(values[["directory"]],"/Netcdfs"), full.names = TRUE, pattern = ".CDF$", ignore.case = T) %>% gtools::mixedsort()
        
        p <- progressr::progressor(steps = length(netCDFs)*3)
        
        
        msnExp <- future.apply::future_lapply(netCDFs, function(netCDFs) {
          
          p()
          MSnbase::readMSData(netCDFs, mode = "onDisk")
          
        })
        
        # browser()
        # incProgress(1/8)
        #filesName <- 
        # cat(format(Sys.time(), "%X"))
        names(msnExp) <- lapply(msnExp, function(msnExp) row.names(msnExp@phenoData)) #)rownames(MSnbase::phenoData(msnExp[[1]])) #filesName
        
        rawData <- future.apply::future_lapply(msnExp, FUN = function(msnExp) {
          p()
          msnExp %>% methods::as("data.frame") %>% data.table::setDT() %>% `attr<-`("fileName", row.names(msnExp@phenoData))
        })

        # rawData <- future.apply::future_lapply(msnExp, FUN = function(msnExp) {
        #   # p()
        #   tmp <- MSnbase::spectrapply(msnExp , function(msnExp) {
        # 
        #     data.table(rt = MSnbase::rtime(msnExp) ,mz = MSnbase::mz(msnExp), i = MSnbase::intensity(msnExp))
        # 
        #   }) %>% data.table::rbindlist(idcol=T)
        # 
        #   attr(tmp,"fileName") <- rownames(MSnbase::phenoData(msnExp))
        # 
        #   return(tmp)
        # })
        
        # cat(format(Sys.time(), "%X"))
        
        rawData_tic <- lapply(rawData, function(rawData) {
          p()
          rawData[, .(tic = sum(i)), by = .(rt)] %>% `attr<-`("fileName", attr(rawData,"fileName")) #%>% `attr<-`("file", rownames(MSnbase::phenoData(msnExp[[1]])))
        })
        

        
        
        # incProgress(2/2)
        # cat(format(Sys.time(), "%X"))
        # 

        
        
        # browser()
        values[["plotIndex"]] <- findInterval(length(msnExp)/2, seq_len(length(msnExp)), all.inside = T)
        values[["rawData"]] <- rawData
        values[["rawData_tic"]] <- rawData_tic
        values[["rawData_msnExp"]] <- msnExp
        updateTabsetPanel(session,inputId = "navbar",selected = "Visualization")
        
      })
      
      
      
      
    } else { 
      if (input$navbar == "Create experiment") {
        showModal(
          modalDialog(
            h5("Create new directory"),
            checkboxInput("includeSampleData","include sample data ?", value = FALSE),
            checkboxInput("includeParameters","include Parameters examples ?", value = TRUE),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("ok", "OK")
            )
          )
          
        )
        updateTabsetPanel(session,inputId = "navbar",selected = "Visualization")
      }
      
      
    }
    # cat(format(Sys.time(), "%X"))
    
  },ignoreInit = T)
  
  
  
  # observeEvent(input$saveButton, {
  #   xlsx::write.xlsx(ParameterFile.tbl(), file = paramFile())
  #    showNotification("Saved !")
  # })
  
  # rawData <- eventReactive(input$start,{
  
  # observeEvent(values[["directory"]], {
  #   
  #   
  #   req(values[["directory"]])
  #   # future::plan("multiprocess")
  #   # browser()
  #   # req(input$fileButton != 0)
  #   #if (!is.null(values[["directory"]])) {
  #   progressr::withProgressShiny(message = 'Extracting data ...', value = 0, {
  #     
  #     netCDFs <- list.files(paste0(values[["directory"]],"/Netcdfs"), full.names = TRUE, pattern = ".CDF$", ignore.case = T)
  #     
  #     p <- progressr::progressor(along = netCDFs)
  #     # browser()
  #     tmp <- future.apply::future_lapply(seq_along(netCDFs), function(i) {
  #       p()
  #       
  #       data <- MSnbase::readMSData(gtools::mixedsort(netCDFs)[i], mode = "onDisk")
  #       df <- methods::as(data,  "data.frame") %>% data.table::setDT()
  #       df$phenoData <- data@phenoData %>% rownames()
  #       df
  #     })
  #     
  #     plotIndex <<- findInterval(length(tmp)/2, seq_len(length(tmp)), all.inside = T)
  #   })
  #   
  #   # future::plan("sequential")
  #   values[["rawData"]] <- tmp
  #   # } else { NULL }
  #   
  #   
  # })
  # rawData <- reactive({
  # 
  # 
  # })
  
  
  # output$selectedFragment_tic <- renderUI({
  #   # req(input$tabs)
  #   req(values[["rawData"]], paramFileTable(), isolate(input$tabs) == "TIC")
  #   # req(input$tabs == "SIM" | input$tabs == "MSpectrum" | input$tabs == "TIC" | input$tabs == "Parameters")
  #
  #   selectInput('selectedFragment', 'Fragments', c("TIC", isolate(values[["DF"]][[1]])), multiple= F, selectize=TRUE)
  #
  # })
  
  output$plot_hover_coord <- renderPrint({
    
    # browser()
    req(!is.null(names(input)))
    
    list_plots_hover <- grep("plot_hover", names(input), value = T)
    plot_hover_data <- lapply(list_plots_hover, function(x) {
      
      input[[x]]
      
    })
    
    plot_hover_data <- unlist(plot_hover_data)
    
    validate(
      if (!is.null(plot_hover_data)) {
        NULL
      } else {
        ""
      }
    )
    
    plot_hover_data <- plot_hover_data[1:2] %>% as.numeric() %>%  round(digits = 2)
    names(plot_hover_data) <- c("x","y")
    plot_hover_data
  })
  
  # output$saveActivePlotsButton <- renderUI({
  #
  #   req(values[["rawData"]], input$tabs != "Parameters")
  #
  #   actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  #
  # })
  #
  # output$saveTotableButton <- renderUI({
  #
  #   req(values[["rawData"]], input$tabs == "SIM" | input$tabs == "Parameters")
  #
  #   actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px")
  #
  # })
  #
  # output$undoButton <- renderUI({
  #
  #   req(input$tabs == "Parameters")
  #
  #   actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
  #
  # })
  #
  # output$runButton <- renderUI({
  #
  #   req(values[["rawData"]], input$tabs == "Parameters")
  #
  #   # For jscode2 to work. Binding enterkey to runButton
  #   tagList(
  #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #     ),
  #
  #   )
  # })
  
  #   observeEvent({
  #     values[["rawData"]]
  #     values[["DF"]]
  #     # paramFileTable()
  #     # input$selectedFragment
  #     # input$selectedFragment_tic
  #     input$tabs
  #     #  paramFileTable
  #   }, {
  # 
  #     # browser()
  #     req(values[["rawData"]], input$tabs,input$selectedFragment)
  # 
  #     # req(values[["DF"]])
  # 
  #     switch (input$tabs,
  # #             'MSinco' = {},
  #             'TIC' = {
  #  # browser()
  #               req(input$tabs == 'TIC')
  #               output$ticUI <- renderUI({
  # 
  #                 tagList({
  #               # output$rtimeL <- renderUI({
  # 
  #                 # req(input$tabs != "MSpectrum")
  # 
  #                 if (input$selectedFragment == "TIC") {
  # 
  #   numericInput(inputId = "rtimeL",
  #                label = "Retention time (left)",
  #                #min = min(rawData1()@featureData@data$retentionTime),
  #                #max = max(rawData1()@featureData@data$retentionTime),
  #                value = tryCatch(round(min(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                step = 0.1)
  # 
  #                 } else {
  # 
  #                   numericInput(inputId = "rtimeL",
  #                                label = "Retention time (left)",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = values[["DF"]][name == input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                                step = 0.1)
  # 
  #                 }
  #                 }, {
  #               # })
  # 
  #               # output$rtimeR <- renderUI({
  # 
  #                 # req(input$tabs != "MSpectrum")
  # 
  # #
  #                 if (input$selectedFragment == "TIC") {
  # 
  # 
  #                   numericInput(inputId = "rtimeR",
  #                                label = "Retention time (right)",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = tryCatch(round(max(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                                step = 0.1)
  # 
  # 
  #                 } else {
  # 
  #                   numericInput(inputId = "rtimeR",
  #                                label = "Retention time (right)",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = values[["DF"]][name == input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                                step = 0.1)
  # 
  #                 }
  #                 },
  #               # })
  # 
  #               # output$saveActivePlotsButton <- renderUI({
  # 
  #                 # req(input$tabs != "Parameters")
  # 
  #                 actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  # 
  #               # })
  # 
  #               # output$runButton <- renderUI({
  #               #
  #               #   req(input$tabs != "Parameters")
  #               #
  #               #   # For jscode2 to work. Binding enterkey to runButton
  #               #   tagList(
  #               #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #               #     ),
  #               #
  #               #   )
  #               # })
  #                 )
  # })
  #             } ,
  #             'MSpectrum' = {
  # 
  #               req(input$tabs == 'MSpectrum')
  # 
  #               output$msUI <- renderUI({
  #               # output$rtime <- renderUI({
  # tagList(
  #                 # req(input$tabs == "MSpectrum")
  #                 # browser()
  # 
  #                 numericInput(inputId = "rtime",
  #                              label = "Retention time",
  #                              #min = min(rawData1()@featureData@data$retentionTime),
  #                              #max = max(rawData1()@featureData@data$retentionTime),
  #                              #value = rows(values[["DF"]],values[["DF"]][[1]] ==input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
  #                              value = (values[["DF"]][name == input$selectedFragment, 2] + values[["DF"]][name == input$selectedFragment, 3])/2, # to be used with  input$selectedFragment2: as.numeric(gsub(".*@",replacement = "",  input$selectedFragment2)),
  #                              step = 0.1),
  #               # })
  # 
  #               # output$labelThreshold <- renderUI({
  # 
  #                 # req(input$tabs == "MSpectrum")
  # 
  # 
  #                 numericInput(inputId = "labelThreshold",
  #                              label = "Threshold intensity for labels",
  #                              #min = 0,
  #                              #max = 1,
  #                              value = {if ( input$selectedFragment == "TIC") {NULL} else {10}},
  #                              step = 5
  #                 ),
  #               # })
  # 
  #               # output$saveActivePlotsButton <- renderUI({
  # 
  #                 # req(values[["rawData"]], input$tabs != "Parameters")
  # 
  #                 actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  # 
  #               # })
  # 
  #               # output$runButton <- renderUI({
  #               #
  #               #   req(values[["rawData"]], input$tabs != "Parameters")
  #               #
  #               #   # For jscode2 to work. Binding enterkey to runButton
  #               #   tagList(
  #               #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #               #     ),
  #               #
  #               #   )
  #               # })
  # )
  # })
  #             },
  # 
  #             'SIM' = {
  # 
  #               req(input$tabs == 'SIM')
  # 
  # 
  #             },
  # 
  #             'Parameters' = {
  # 
  #               req(input$tabs == "Parameters")
  # 
  #             paramUI  <- renderUI({
  #               # output$rtimeL <- renderUI({
  # 
  #                 # req(input$tabs != "MSpectrum")
  # 
  #                 numericInput(inputId = "rtimeL",
  #                              label = "Retention time (left)",
  #                              #min = min(rawData1()@featureData@data$retentionTime),
  #                              #max = max(rawData1()@featureData@data$retentionTime),
  #                              value = values[["DF"]][name == input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                              step = 0.1)
  # 
  # 
  #               # })
  # 
  #               # output$rtimeR <- renderUI({
  # 
  #                 # req(input$tabs != "MSpectrum")
  # 
  #                 numericInput(inputId = "rtimeR",
  #                              label = "Retention time (right)",
  #                              #min = min(rawData1()@featureData@data$retentionTime),
  #                              #max = max(rawData1()@featureData@data$retentionTime),
  #                              value = values[["DF"]][name == input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                              step = 0.1)
  #               # })
  # 
  #               # output$mass0 <- renderUI({
  # 
  #                 # req(input$tabs == "SIM" | input$tabs == "Parameters")
  # 
  # 
  #                 numericInput(inputId = "mass0",
  #                              label = "mass0 (M0)",
  #                              #min = 40,
  #                              #max = 600,
  #                              value = values[["DF"]][name == input$selectedFragment, 4],
  #                              step = 1
  #                 )
  #               # })
  # 
  #               # output$N_atom <- renderUI({
  # 
  #                 # req(input$tabs == "SIM" | input$tabs == "Parameters")
  # 
  # 
  #                 numericInput(inputId = "N_atom",
  #                              label = "Number of isotopomers",
  #                              #min = 1,
  #                              #max = 10,
  #                              value = values[["DF"]][name == input$selectedFragment, 5],
  #                              step = 1
  #                 )
  #               # })
  # 
  # 
  #               # output$mzd <- renderUI({
  # 
  #                 # req(input$tabs == "SIM" | input$tabs == "Parameters")
  # 
  # 
  #                 numericInput(inputId = "mzd",
  #                              label = "Mass difference",
  #                              #min = 0,
  #                              #max = 1,
  #                              value = {if ( input$selectedFragment == "TIC") {NULL} else {0.3}},
  #                              step = 0.1
  #                 )
  #               # })
  # 
  #               # output$saveTotableButton <- renderUI({
  # 
  #                 # req(values[["rawData"]], input$tabs == "SIM" | input$tabs == "Parameters")
  # 
  #                 actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px")
  # 
  #               # })
  # 
  #               # output$undoButton <- renderUI({
  # 
  #                 # req(values[["rawData"]], input$tabs == "Parameters")
  # 
  #                 actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
  # 
  #               # })
  #               })
  #             }
  #     )
  # 
  #     # cat(file=stderr(),"at inputs:", input$mass0)
  # 
  # 
  #   }, label = "inputs",ignoreInit = T)
  
  
  
  
  
  
  # observeEvent(input$selectedFragment{
  
  # req(values[["DF"]])
  # output$saveTotable <- renderUI({
  #   
  #   req(input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   actionButton("saveTotable", "Save to table")
  #   
  # })
  # 
  # output$saveActivePlotsButton <- renderUI({
  #   
  #   req(input$tabs != "Parameters")
  #   
  #   actionButton("saveActivePlotsButton", "Save active plots")
  #   
  # })
  # 
  # output$run1 <- renderUI({
  #   
  #   req(input$tabs == "TIC" )
  #   
  #   actionButton("run1", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$run2 <- renderUI({
  #   
  #   req(input$tabs == "MSpectrum")
  #   
  #   actionButton("run2", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$run3 <- renderUI({
  #   
  #   req(input$tabs == "SIM")
  #   
  #   actionButton("run3", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$rtime <- renderUI({
  #   
  #   req(input$tabs == "MSpectrum")
  #   
  #   
  #   numericInput(inputId = "rtime",
  #                label = "Retention time",
  #                #min = min(rawData1()@featureData@data$retentionTime),
  #                #max = max(rawData1()@featureData@data$retentionTime),
  #                #value = rows(paramFileTable(),paramFileTable()[[1]] == input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
  #                value = (values[["DF"]][name ==input$selectedFragment, 2] + values[["DF"]][name ==input$selectedFragment, 3])/2, # to be used with input$selectedFragment2: as.numeric(gsub(".*@",replacement = "", input$selectedFragment2)),
  #                step = 0.1)
  # })
  # 
  # output$labelThreshold <- renderUI({
  #   
  #   req(input$tabs == "MSpectrum")
  #   
  #   
  #   numericInput(inputId = "labelThreshold",
  #                label = "Threshold intensity for labels",
  #                #min = 0,
  #                #max = 1,
  #                value = {if (input$selectedFragment == "TIC") {NULL} else {10}},
  #                step = 5
  #   )
  # })
  # 
  # output$rtimeL <- renderUI({
  #   
  #   req(input$tabs == "TIC" | input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   
  #   if (req(input$selectedFragment) == "TIC" && input$tabs == "TIC") {
  #     
  #     numericInput(inputId = "rtimeL",
  #                  label = "Retention time (left)",
  #                  #min = min(rawData1()@featureData@data$retentionTime),
  #                  #max = max(rawData1()@featureData@data$retentionTime),
  #                  value = tryCatch(round(min(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                  step = 0.1)
  #     
  #   } else {
  #     
  #     numericInput(inputId = "rtimeL",
  #                  label = "Retention time (left)",
  #                  #min = min(rawData1()@featureData@data$retentionTime),
  #                  #max = max(rawData1()@featureData@data$retentionTime),
  #                  value = values[["DF"]][name ==input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                  step = 0.1)
  #     
  #   }
  # })
  # 
  # output$rtimeR <- renderUI({
  #   
  #   # browser()
  #   req(input$tabs == "TIC" | input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   
  #   if (req(input$selectedFragment) == "TIC" && input$tabs == "TIC") {
  #     
  #     
  #     numericInput(inputId = "rtimeR",
  #                  label = "Retention time (right)",
  #                  #min = min(rawData1()@featureData@data$retentionTime),
  #                  #max = max(rawData1()@featureData@data$retentionTime),
  #                  value = tryCatch(round(max(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                  step = 0.1)
  #     
  #   } else {
  #     
  #     numericInput(inputId = "rtimeR",
  #                  label = "Retention time (right)",
  #                  #min = min(rawData1()@featureData@data$retentionTime),
  #                  #max = max(rawData1()@featureData@data$retentionTime),
  #                  value = values[["DF"]][name ==input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                  step = 0.1)
  #     
  #   }
  # })
  # 
  # output$mass0 <- renderUI({
  #   
  #   req(input$navbar != 0, input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   
  #   numericInput(inputId = "mass0",
  #                label = "mass0 (M0)",
  #                #min = 40,
  #                #max = 600,
  #                value = values[["DF"]][name ==input$selectedFragment, 4],
  #                step = 1
  #   )
  # })
  # 
  # output$N_atom <- renderUI({
  #   
  #   req(input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   
  #   numericInput(inputId = "N_atom",
  #                label = "Number of isotopomers",
  #                #min = 1,
  #                #max = 10,
  #                value = values[["DF"]][name ==input$selectedFragment, 5],
  #                step = 1
  #   )
  # })
  # 
  # 
  # output$mzd <- renderUI({
  #   
  #   req(input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   
  #   numericInput(inputId = "mzd",
  #                label = "Mass difference",
  #                #min = 0,
  #                #max = 1,
  #                value = {if (input$selectedFragment == "TIC") {NULL} else {0.3}},
  #                step = 0.1
  #   )
  # })
  # 
  # 
  # output$undoButton <- renderUI({
  #   
  #   req(input$tabs == "Parameters")
  #   
  #   actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
  #   
  # })
  #################################  ##################################  ##################################  ##################################
  
  
  # observeEvent(input$tabs, {
  #   
  #   switch (input$tabs,
  #           "TIC" = {
  #             shinyjs::showElement("selectedFragment")
  #             },
  #           "SIM" = {
  #             shinyjs::showElement("selectedFragment")
  #             },
  #           "MSpectrum" = {
  #             shinyjs::showElement("selectedFragment")
  #             },
  #           "Parameters" = {
  #             shinyjs::hideElement("selectedFragment")
  #             }
  #   )
    
    
  # },priority = 2)
  
  # 
  # output$selectedFragment <- renderUI({
  #   # req(input$tabs)
  #   req(values[["rawData"]])
  #   
  #   selectedFragment <- input$selectedFragment
  #   tagList(
  #     
  #     
  #   # if (isolate(input$tabs) == "TIC") {
  #   # selectInput('selectedFragment', 'Fragments', isolate(c("TIC",values[["DF"]][[1]])), multiple= F, selectize=TRUE),
  #   selectInput("selectedFiles", 'Files', names(values[["rawData"]]), multiple=TRUE, selectize=T,selected = names(values[["rawData"]]))
  #   )
  #   #
  #   # } else {
  #   
  #   # selectInput('selectedFragment', 'Fragments', isolate(values[["DF"]][[1]]), multiple= F, selectize=TRUE)
  #   
  #   # }
  # })
  
  
  output$selectedFiles <- renderUI({

    # req(input$tabs == "SIM" | input$tabs == "TIC" | input$tabs == "MSpectrum", values[["rawData"]])

    selectInput("selectedFiles", 'Files', names(values[["rawData"]]), multiple=TRUE, selectize=T,selected = names(values[["rawData"]]))
  })

  observeEvent({
    input$tabs
    values[["rawData"]]}, {
      
      selectedFragment <- input$selectedFragment
      
      
        switch (input$tabs,
                "TIC" = {
                  updateSelectInput(session,"selectedFragment", choices =  isolate(c("TIC",values[["DF"]][[1]])), selected = selectedFragment)
                },
                "SIM" = {
                  updateSelectInput(session,"selectedFragment", choices =  isolate(values[["DF"]][[1]]), selected = selectedFragment)
                },
                "MSpectrum" = {
                  updateSelectInput(session,"selectedFragment", choices =  isolate(values[["DF"]][[1]]), selected = selectedFragment)
                }
        )
      
    # updateSelectInput(session,"selectedFiles", choices =  names(values[["rawData"]]), selected = names(values[["rawData"]]))
    # updateSelectInput(session,"selectedFragment", choices =  isolate(c("TIC",values[["DF"]][[1]])), selected = selectedFragment)
    
  
  },ignoreInit = T)
  
  
  # output$saveTotable <- renderUI({
  #   
  #   req(input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   actionButton("saveTotable", "Save to table")
  #   
  # })
  # 
  # output$saveActivePlotsButton <- renderUI({
  #   
  #   req(input$tabs != "Parameters", values[["runNo"]] > 0 )
  #   
  #   actionButton("saveActivePlotsButton", "Save active plots")
  #   
  # })
  
  # output$run1 <- renderUI({
  #   
  #   req(input$tabs == "TIC", values[["runNo"]] > 0)
  #   
  #   actionButton("run1", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$run2 <- renderUI({
  #   
  #   req(input$tabs == "MSpectrum")
  #   
  #   actionButton("run2", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$run3 <- renderUI({
  #   
  #   req(input$tabs == "SIM")
  #   
  #   actionButton("run3", "run", width = "100%", style="margin-bottom:8px")
  #   
  # })
  
  output$rtime <- renderUI({
    
    req(input$tabs == "MSpectrum",values[["DF"]])
    
    
    numericInput(inputId = "rtime",
                 label = "Retention time",
                 #min = min(rawData1()@featureData@data$retentionTime),
                 #max = max(rawData1()@featureData@data$retentionTime),
                 #value = rows(paramFileTable(),paramFileTable()[[1]] == input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
                 value = (values[["DF"]][name ==input$selectedFragment, 2] + values[["DF"]][name ==input$selectedFragment, 3])/2, # to be used with input$selectedFragment2: as.numeric(gsub(".*@",replacement = "", input$selectedFragment2)),
                 step = 0.1)
  })
  
  output$labelThreshold <- renderUI({
    
    req(input$tabs == "MSpectrum", input$selectedFragment)
    
    
    numericInput(inputId = "labelThreshold",
                 label = "Threshold intensity for labels",
                 #min = 0,
                 #max = 1,
                 value = {if (input$selectedFragment == "TIC") {NULL} else {10}},
                 step = 5
    )
  })
  
  output$rtimeL <- renderUI({
    
    req(input$tabs == "TIC" | input$tabs == "SIM", values[["DF"]])
    
    
    if (req(input$selectedFragment) == "TIC" && input$tabs == "TIC") {
      
      numericInput(inputId = "rtimeL",
                   label = "Retention time (left)",
                   #min = min(rawData1()@featureData@data$retentionTime),
                   #max = max(rawData1()@featureData@data$retentionTime),
                   value = tryCatch(round(min(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2-10,
                   step = 0.1)
      
    } else {
      
      numericInput(inputId = "rtimeL",
                   label = "Retention time (left)",
                   #min = min(rawData1()@featureData@data$retentionTime),
                   #max = max(rawData1()@featureData@data$retentionTime),
                   value = values[["DF"]][name ==input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
                   step = 0.1)
      
    }
  })
  
  output$rtimeR <- renderUI({
    
    # browser()
    req(input$tabs == "TIC" | input$tabs == "SIM",values[["DF"]])
    
    
    if (req(input$selectedFragment) == "TIC" && input$tabs == "TIC") {
      
      
      numericInput(inputId = "rtimeR",
                   label = "Retention time (right)",
                   #min = min(rawData1()@featureData@data$retentionTime),
                   #max = max(rawData1()@featureData@data$retentionTime),
                   value = tryCatch(round(max(values[["rawData"]][[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2+10,
                   step = 0.1)
      
    } else {
      
      numericInput(inputId = "rtimeR",
                   label = "Retention time (right)",
                   #min = min(rawData1()@featureData@data$retentionTime),
                   #max = max(rawData1()@featureData@data$retentionTime),
                   value = values[["DF"]][name ==input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
                   step = 0.1)
      
    }
  })
  
  output$mass0 <- renderUI({
    
    req(input$navbar != 0, input$tabs == "SIM",values[["DF"]])
    
    
    numericInput(inputId = "mass0",
                 label = "mass0 (M0)",
                 #min = 40,
                 #max = 600,
                 value = values[["DF"]][name ==input$selectedFragment, 4],
                 step = 1
    )
  })
  
  output$N_atom <- renderUI({
    
    req(input$tabs == "SIM",values[["DF"]])
    
    
    numericInput(inputId = "N_atom",
                 label = "Number of isotopomers",
                 #min = 1,
                 #max = 10,
                 value = values[["DF"]][name ==input$selectedFragment, 5],
                 step = 1
    )
  })
  
  
  output$mzd <- renderUI({
    
    req(input$tabs == "SIM",input$selectedFragment)
    
    
    numericInput(inputId = "mzd",
                 label = "Mass difference",
                 #min = 0,
                 #max = 1,
                 value = {if (input$selectedFragment == "TIC") {NULL} else {0.3}},
                 step = 0.1
    )
  })
  
  
  # output$undoButton <- renderUI({
  #   
  #   req(input$tabs == "Parameters")
  #   
  #   actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
  #   
  # })
  
  # })
  #   observeEvent({
  #     values[["rawData"]]
  #     values[["DF"]]
  #     # paramFileTable()
  #     # input$selectedFragment
  #     # input$selectedFragment_tic
  #     input$tabs
  #     input$selectedFragment
  #     #  paramFileTable
  #   }, {
  # 
  #     # browser()
  #     req(values[["rawData"]], input$tabs,input$selectedFragment)
  # 
  #     # req(values[["DF"]])
  # 
  #     switch (input$tabs,
  # #             'MSinco' = {},
  #             'TIC' = {
  #  # browser()
  #               req(input$tabs == 'TIC')
  # 
  # 
  # 
  # 
  #                 if (input$selectedFragment == "TIC") {
  # 
  #   updateNumericInput(session, "rtimeL",
  #                #min = min(rawData1()@featureData@data$retentionTime),
  #                #max = max(rawData1()@featureData@data$retentionTime),
  #                value = tryCatch(sprintf(round(min(values[["rawData"]][[1]]$rt)/60), fmt = '%#.3f'), error = function(cond) {message(); return(NULL)}) # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                )
  # 
  #                 } else {
  # 
  #                   updateNumericInput(session, "rtimeL",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = sprintf(values[["DF"]][name == input$selectedFragment, 2], fmt = '%#.3f') # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                                )
  #                   
  # #
  #                 if (input$selectedFragment == "TIC") {
  # 
  # 
  #                   updateNumericInput(session, "rtimeR",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = tryCatch(sprintf(round(max(values[["rawData"]][[1]]$rt)/60), fmt = '%#.3f'), error = function(cond) {message(); return(NULL)}) # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                               )
  # 
  # 
  #                 } else {
  # 
  #                   updateNumericInput(session,"rtimeR",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = sprintf(values[["DF"]][name == input$selectedFragment, 3], fmt = '%#.3f') # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                               )
  # 
  #                 }
  #                 }
  #               # })
  # 
  #               # output$saveActivePlotsButton <- renderUI({
  #               # 
  #               #   req(input$tabs != "Parameters")
  #               # 
  #               #   actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  #               # 
  #               # })
  # 
  #               # output$runButton <- renderUI({
  #               #
  #               #   req(input$tabs != "Parameters")
  #               #
  #               #   # For jscode2 to work. Binding enterkey to runButton
  #               #   tagList(
  #               #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #               #     ),
  #               #
  #               #   )
  #               # })
  #                 
  # 
  #             } ,
  #             'MSpectrum' = {
  # 
  #               req(input$tabs == 'MSpectrum')
  # 
  #               updateNumericInput(session, "rtime",
  #                              #min = min(rawData1()@featureData@data$retentionTime),
  #                              #max = max(rawData1()@featureData@data$retentionTime),
  #                              #value = rows(values[["DF"]],values[["DF"]][[1]] ==input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
  #                              value = sprintf((values[["DF"]][name == input$selectedFragment, 2] + values[["DF"]][name == input$selectedFragment, 3])/2, fmt = '%#.3f'), # to be used with  input$selectedFragment2: as.numeric(gsub(".*@",replacement = "",  input$selectedFragment2)),
  #                              step = 0.1)
  #               # })
  # 
  #               # output$labelThreshold <- renderUI({
  # 
  #                 # req(input$tabs == "MSpectrum")
  # 
  # 
  #               updateNumericInput(session, "labelThreshold",
  #                              #min = 0,
  #                              #max = 1,
  #                              value = {if ( input$selectedFragment == "TIC") {NULL} else {10}}
  #                 )
  #               # })
  # 
  #               # output$saveActivePlotsButton <- renderUI({
  #               # 
  #               #   req(values[["rawData"]], input$tabs != "Parameters")
  #               # 
  #               #   actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  #               # 
  #               # })
  # 
  #               # output$runButton <- renderUI({
  #               #
  #               #   req(values[["rawData"]], input$tabs != "Parameters")
  #               #
  #               #   # For jscode2 to work. Binding enterkey to runButton
  #               #   tagList(
  #               #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #               #     ),
  #               #
  #               #   )
  #               # })
  # 
  #             },
  # 
  #             'SIM' = {
  # 
  #    
  # 
  #                   
  #                   updateNumericInput(session, "rtimeL",
  #                                label = "Retention time (left)",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = sprintf(values[["DF"]][name == input$selectedFragment, 2], fmt = '%#.3f'), # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                                step = 0.1)
  #       
  #           
  #               
  #                   updateNumericInput(session,"rtimeR",
  #                                label = "Retention time (right)",
  #                                #min = min(rawData1()@featureData@data$retentionTime),
  #                                #max = max(rawData1()@featureData@data$retentionTime),
  #                                value = sprintf(values[["DF"]][name == input$selectedFragment, 3], fmt = '%#.3f'), # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                                step = 0.1)
  #         
  #               
  #                   updateNumericInput(session, "mass0",
  #                            label = "mass0 (M0)",
  #                            #min = 40,
  #                            #max = 600,
  #                            value = sprintf(values[["DF"]][name ==input$selectedFragment, 4], fmt = '%#.3f'))
  #              
  #               
  #                            updateNumericInput(session, "N_atom",
  #                            label = "Number of isotopomers",
  #                            #min = 1,
  #                            #max = 10,
  #                            value = values[["DF"]][name ==input$selectedFragment, 5]
  #                            )
  #                           
  #              
  #               
  #               
  #                            updateNumericInput(session,"mzd",
  #                            label = "Mass difference",
  #                            #min = 0,
  #                            #max = 1,
  #                            value = {if (input$selectedFragment == "TIC") {NULL} else {0.3}},
  #                            step = 0.1
  #               )
  #               
  # 
  #               # 
  #               # actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px"),
  #               # actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px"),
  #               # 
  # 
  #               
  #               
  #             },
  # 
  #             'Parameters' = {
  #               updateNumericInput(session, "rtimeL",
  #                                  label = "Retention time (left)",
  #                                  #min = min(rawData1()@featureData@data$retentionTime),
  #                                  #max = max(rawData1()@featureData@data$retentionTime),
  #                                  value = values[["DF"]][name == input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
  #                                  step = 0.1)
  #               
  #               
  #               
  #               updateNumericInput(session,"rtimeR",
  #                                  label = "Retention time (right)",
  #                                  #min = min(rawData1()@featureData@data$retentionTime),
  #                                  #max = max(rawData1()@featureData@data$retentionTime),
  #                                  value = values[["DF"]][name == input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
  #                                  step = 0.1)
  #               
  #               
  #               updateNumericInput(session,"mass0",
  #                                  label = "mass0 (M0)",
  #                                  #min = 40,
  #                                  #max = 600,
  #                                  value = values[["DF"]][name ==input$selectedFragment, 4])
  #               
  #               
  #               updateNumericInput(session,"N_atom",
  #                                  label = "Number of isotopomers",
  #                                  #min = 1,
  #                                  #max = 10,
  #                                  value = values[["DF"]][name ==input$selectedFragment, 5]
  #               )
  #               
  #               
  #               
  #               
  #               updateNumericInput(session, "mzd",
  #                                  label = "Mass difference",
  #                                  #min = 0,
  #                                  #max = 1,
  #                                  value = {if (input$selectedFragment == "TIC") {NULL} else {0.3}},
  #                                  step = 0.1
  #               )
  #             }
  #     )
  # 
  #     # cat(file=stderr(),"at inputs:", input$mass0)
  # 
  # 
  #   }, label = "inputs",ignoreInit = T)
  
  # Solution: use updateNumericInput to maybe flush directly to the client.
  # # OK: ajouteau debut de chaque observevent pour creer des plots (sim, tic, mspec) les valeurs, ex:
  #
  #   N_atom <- paramFileTable()[name == input$selectedFragment, 5] etc pour decoreller UI et SEVER.
  # Ensuite sur les tabs SIM etc utilise updatecheckbox poour choisir selected = input$selected fragment, comme ca l utilisateur n arrive jamais sur le "TIC"
  #
  # observeEvent({
  #   input$rtimeR
  #   input$rtimeL
  #   # input$tabs
  # }, {
  #   browser()
  #   # observe({
  #   req(input$tabs == "TIC", input$rtimeL, input$rtimeR)# , input$selectedFragment, input$rtimeL, input$rtimeR)
  #   # browser()
  #   #  if (isTRUE(input$tabs == "TIC")) {  #  replaced by req(input$tabs == "TIC")
  # 
  #   TICplots <<- lapply(seq_along(values[["rawData"]]), function(i) {
  # 
  #     #browser()
  # 
  #     DT <- values[["rawData"]][[i]][, .(tic = sum(i)), by = .(rt, phenoData)]
  #     dataIndexL <- MALDIquant::match.closest(input$rtimeL*60,DT$rt)
  #     dataIndexR <- MALDIquant::match.closest(input$rtimeR*60,DT$rt)
  #     DT <- DT[dataIndexL:dataIndexR]
  #     title = paste0("TIC_",DT$phenoData[[1]])
  # 
  #     canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
  # 
  #   })
  #   
  #   mapply(TICplots, i, FUN =  function(TICplots, i) {
  #     
  #     output[[paste0("tic", i, "_f", values[["runNo"]])]] <- renderPlot({
  #       
  #       TICplots
  #       
  #     })
  #     
  #   })
  #   # browser()
  #   output$plots_ticA <- renderUI({
  #     # browser()
  #     # req(TICplots())
  #     # req(values[["rawData"]])
  #     plots_tic =  lapply(seq_len(plotIndex), function(i) {
  # 
  #       plotOutput(paste0("tic",i, "_f", values[["runNo"]]), hover = paste0("plot_hover_tic",i, "_f", values[["runNo"]])) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
  #     })
  # 
  #     plots_tic
  #   })
  # 
  #   output$plots_ticB <- renderUI({
  #     #browser()
  #     #   req(TICplots())
  #     req(length(values[["rawData"]]) > 1)
  #     # create tabPanel with datatable in it
  #     plots_ticB = lapply((plotIndex+1):length(values[["rawData"]]), function(i) {
  # 
  #       plotOutput(paste0("tic",i, "_f", values[["runNo"]]),hover = paste0("plot_hover_tic",i, "_f", values[["runNo"]])) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
  #       #plotOutput(paste0("MSpectrum",i))
  # 
  #     })
  # 
  #     plots_ticB # do.call(mainPanel, myTics)
  #   })
  #   #    }
  #   #})
  #   
  #   # browser()
  # },ignoreInit = T, once = T,label = "tic_init")
  
  
  # observeEvent(input$selectedFragment, {
  # 
  #   values[["Fragment"]] <- input$selectedFragment
  # 
  # 
  # },priority = 2)
  
  # create tic
  
  # values[["DF"]][name ==input$selectedFragment, 2] <- input$rtimeL
  # values[["DF"]][name ==input$selectedFragment, 3] <- input$rtimeR
  # 
  
  observeEvent({
    # req(values[["rawData"]], paramFileTable())
    # values[["Fragment"]]
    # input$run2
    # input$selectedFragment
    input$run1
    # input$selectedFiles
    # isolate(input$tabs)
    # req(input$tabs == "TIC")
    # req(isolate(input$tabs) == "TIC")
    # input$selectedFragment
    
  }, {
    # browser()
    # selectedFragment  <- values[["Fragment"]] 
    # browser()
    # observe({
    
    req(input$rtimeL, input$rtimeR) # , input$selectedFragment, input$rtimeL, input$rtimeR)
    # browser()
    #  if (isTRUE(input$tabs == "TIC")) {  #  replaced by req(input$tabs == "TIC")
    
    # browser()
    
    plots_tic <- lapply(values[["rawData_tic"]][input$selectedFiles], function(rawData_tic) {
      
      # browser()
      # OLD
      
      # DT <- rawData[, .(tic = sum(i)), by = .(rt)]
      dataIndexL <- MALDIquant::match.closest(input$rtimeL*60,rawData_tic$rt)
      dataIndexR <- MALDIquant::match.closest(input$rtimeR*60,rawData_tic$rt)
      DT <- rawData_tic[dataIndexL:dataIndexR]
      # 
      # # TODO NEW VERSION! (the one commented)
      # dataIndexL <- MALDIquant::match.closest(input$rtimeL*60,rawData$rt)
      # dataIndexR <- MALDIquant::match.closest(input$rtimeR*60,rawData$rt)
      # DT <- rawData[dataIndexL:dataIndexR, .(tic = sum(i)), by = .(rt, phenoData)]
      # 
      
      if (input$selectedFragment == "TIC") {
        title = paste0("TIC_", attr(rawData_tic, "fileName"))
      } else {
        title = paste0("TIC_", input$selectedFragment,"_",attr(rawData_tic,"fileName"))
      }
      
      # # tic_p  <- canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
      # canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
      # 
      # browser()
      
      if ("lattice" %in% input$settings) {
        
        lattice::xyplot(tic~(rt/60), data = DT, type="l", xlab="Retention time (min)", ylab= "Intensity", main=title, col="black", scales= list(tck=c(1,0))
                        
        )
      } else {
        
        # browser()
        canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
        
      }
      # output[[paste0("tic",i, "_f", input$folderButton)]] <- renderPlot({
      # 
      #   tic_p
      # 
      # })
      # return(tic_p)
    })
    # browser()
    
    #TODO remove thee input$run1
    values[["plots_tic"]] <- plots_tic
    # cat(format(Sys.time(), "%X"))
    
    
    if ("cache" %in% input$settings) {
      # browser()
      
      lapply(seq_along(plots_tic), FUN =  function(i) {
        output[[paste0("tic", i,  "_f", values[["runNo"]])]] <- renderCachedPlot({
          
          plots_tic[[i]]
          
        }, cacheKeyExpr = list(input$rtimeL,input$rtimeR))
        
      })
      
    } else {
      
      lapply(seq_along(plots_tic), FUN =  function(i) {
        output[[paste0("tic",i , "_f", values[["runNo"]], input$run1)]] <- renderPlot({
          
          plots_tic[[i]]
          
        })
      })
      
    }
    # browser()
    splitIndex <- findInterval(length(plots_tic)/2, seq_len(length(plots_tic)), all.inside = T)
    # cat(format(Sys.time(), "%X"))
    
    output$plots_ticA <- renderUI({
      # browser()
      #req(TICplots())
      # req(values[["rawData"]])
      lapply(seq_len(splitIndex), function(i) {
        
        plotOutput(paste0("tic",i, "_f", values[["runNo"]], input$run1), hover = paste0("plot_hover_tic",i, "_f", values[["runNo"]], input$run1)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
      })
      
    })
    
    output$plots_ticB <- renderUI({
      # browser()
      #   req(TICplots())
      req(length(plots_tic) > 1)
      # create tabPanel with datatable in it
      lapply((splitIndex+1):length(plots_tic), function(i) {
        
        plotOutput(paste0("tic",i, "_f", values[["runNo"]], input$run1), hover = paste0("plot_hover_tic",i, "_f", values[["runNo"]], input$run1)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
    })
    
    
    # return(plots_tic)
    
    #    }
    #})
  },label = "ticOut")
  
  # 
  # observeEvent(values[["DF"]], {
  #   
  #   rtime <- reactiveValues("rtime")
  #   
  #   tic <- reactiveValues("tic")
  #   
  #   values[["rtime"]] <- NULL
  # 
  #   
  # })
  #scale = list(alternating = 1)
  
  # create mass spectrum
  
  #  MSpecplots <- reactive({
  
  observeEvent({
    # req(input$run,cancelOutput = T)
    input$run2
    # req(input$tabs == "MSpectrum")
    # req(isolate(input$tabs) == "MSpectrum")
    # input$selectedFragment
  }, {
    
    
    req(input$rtime) # if I add input$selectedFragment != "TIC" here in req() isntead of the if loop it doesnt work properly (triggers plotting twice)
    
    
    # rtime_update_server <- (values[["DF"]][name == input$selectedFragment, 2] + values[["DF"]][name == input$selectedFragment, 3])/2 # to be used with  input$selectedFragment2: as.numeric(gsub(".*@",replacement = "",  input$selectedFragment2)),
    
    
    #if (input$selectedFragment != "TIC") {
    #browser()
    # i <- seq_along(values[["rawData"]])
    # cat(format(Sys.time(), "%X"))
    plots_ms <- lapply(values[["rawData"]][input$selectedFiles], function(rawData) {
      
      
      # NEW VERSION
      rtime <- rawData[, unique(rt)]
      specIndex <- MALDIquant::match.closest(input$rtime*60,rtime)
      # intensities <- DT2[rt == rtime[specIndex], i]
      # masses <- DT2[rt == rtime[specIndex], mz]
      DT <- rawData[rt == rtime[specIndex], .(i = i/max(i)*100, mz)]
      title = paste("MS_",input$selectedFragment,"_", attr(rawData,"fileName"),"~", input$rtime, "min")
      
      # intensities <- subset.data.frame(rawData,rt == rtime[specIndex])$i
      # masses <-subset.data.frame(values[["rawData"]][[i]],rt == rtime[specIndex])$mz
      # df <- data.table::data.table("mz" = masses, "intensity" = intensities) %>% transform(intensity = intensity/max(intensity)*100)
      # df[,2] <- df[,2]/max(df[,2])*100
      # data.labels <- subset(df, intensity > input$labelThreshold)
      
      # FOR TESTING
      # rtime <- unique(rawData4[[1]]$rt)
      # specIndex <- MALDIquant::match.closest(18.2*60,rtime) #input$rtime
      # intensities <- subset.data.frame(rawData4[[1]],rt == rtime[specIndex])$i
      # masses <- subset.data.frame(rawData4[[1]],rt == rtime[specIndex])$mz
      # df <- data.table::data.table("mz" = masses, "intensity" = intensities) %>% transform(intensity = intensity/max(intensity)*100)
      # check: isTRUE(length(masses) == length(intensities))
      
      #browser()
      # position_nudge(x=3,y=2)
      data.labels <- DT[i > input$labelThreshold]
      # , position = "auto"
      # canvas_plots + ggplot2::geom_col(data = DT, aes(mz,i)) + ggplot2::geom_text(data= data.labels, aes(x= mz, y= i, label = round(i)), nudge_y = 2) + ggplot2::labs(title = title, x = "m/z", y = "Relative intensity")
      
      # TODO change folderButton by $importButton
      
      # browser()
      
      if ("lattice" %in% input$settings) {
        lattice::xyplot(i ~ mz,data = DT, type="h",col="black", xlab="m/z", ylab= "Relative intensity", main = title, scales = list(tck=c(1,0), x=list(at=pretty(DT$mz))),
                        panel=function(...) { 
                          lattice::panel.text(data.labels$mz, data.labels$i,
                                              round(data.labels$mz),adj = c(1,0),pos = 3)
                          lattice::panel.xyplot(...)
                          # panel.axis(side = "bottom", outside = F)
                          # panel.axis(side = "left",, outside = F, ticks = T)
                        })
      } else {
        
        canvas_plots + ggplot2::geom_col(data = DT, aes(mz,i)) + ggplot2::geom_text(data= data.labels, aes(x= mz, y= i, label = round(i)), nudge_y = 2) + ggplot2::labs(title = title, x = "m/z", y = "Relative intensity")
        
      }
      
    })
    
    
    values[["plots_ms"]] <- plots_ms
    
    
    if ("cache" %in% input$settings) {
      
      lapply(seq_along(plots_ms), FUN =  function(i) {
        
        output[[paste0("mspec", i, "_f", values[["runNo"]]), input$run2]] <- renderCachedPlot({
          
          plots_ms[[i]]
          
        },cacheKeyExpr = list(input$rtime,input$labelThreshold))
        
      })
      
    } else {
      
      lapply(seq_along(plots_ms), FUN =  function(i) {
        
        output[[paste0("mspec", i, "_f", values[["runNo"]], input$run2)]] <- renderPlot({
          
          plots_ms[[i]]
          
        })
      })
      
    }
    
    splitIndex <- findInterval(length(plots_ms)/2, seq_len(length(plots_ms)), all.inside = T)
    output$plots_msA <- renderUI({
      # req(MSpecplots())
      #req(values[["rawData"]])
      # create tabPanel with datatable in it
      lapply(seq_len(splitIndex), function(i) {
        
        plotOutput(paste0("mspec",i, "_f", values[["runNo"]],input$run2),hover = paste0("plot_hover_mspec",i, "_f", values[["runNo"]],input$run2)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
    })
    
    output$plots_msB <- renderUI({
      # req(MSpecplots())
      req(length(values[["rawData"]]) > 1)
      # create tabPanel with datatable in it
      lapply((splitIndex+1):length(plots_ms), function(i) {
        
        plotOutput(paste0("mspec",i, "_f", values[["runNo"]],input$run2),hover = paste0("plot_hover_mspec",i, "_f", values[["runNo"]],input$run2)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
    })
    
    
    #}
  },label = "msOut")
  
  
  # create SIM
  #SIMplots <- reactive({
  
  observeEvent({
    # req(input$run,cancelOutput = T)
    input$run3
    # req(input$tabs == "SIM")
    # req(isolate(input$tabs) == "SIM")
    # input$selectedFragment
  }, {
    
    
    # browser()
    req(input$mass0, input$N_atom,input$rtimeL,input$rtimeR)
    
    
    # })
    
    
    # i <- seq_along(values[["rawData"]])
    
    # browser()
    # NEW
    rtMatrix <- matrix(c(rep(input$rtimeL*60, input$N_atom), rep(input$rtimeR*60, input$N_atom)),nrow = input$N_atom, ncol = 2)
    mzMatrix <- matrix(c(seq(input$mass0 - input$mzd, input$mass0 + (input$N_atom - 1) - input$mzd, by = 1) , seq(input$mass0 + input$mzd, input$mass0 + (input$N_atom - 1) + input$mzd, by = 1)), nrow = input$N_atom, ncol = 2)
    # browser()
    
    plots_sim <- lapply(values[["rawData"]][input$selectedFiles], function(rawData) {
      
      # browser()
      
      # rtMatrix <- matrix(nrow = input$N_atom, ncol = 2)
      # mzMatrix <- matrix(nrow = input$N_atom, ncol = 2)
      # 
      # mzMatrix[,1] <- input$mass0:(input$mass0+(input$N_atom-1)) - input$mzd
      # mzMatrix[,2] <- input$mass0:(input$mass0+(input$N_atom-1)) + input$mzd
      # 
      # rtMatrix[,1] <- rep(input$rtimeL*60 , input$N_atom) # - 0.5*60,  RT low,# +1 to account for the M0 isotopomer
      # rtMatrix[,2] <- rep(input$rtimeR*60, input$N_atom) # + 0.5*60
      
      
      rtL <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
      rtR <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
      # TODO est ce que rtL et rtL doivent aller icI ? au dessus de myData lapply.
      myData <- lapply(1:nrow(rtMatrix), function(j) {
        
        # browser()
        
        DT <- rawData[data.table::between(rt, rt[[rtL]], rt[[rtR]])][data.table::between(mz, mzMatrix[j, 1], mzMatrix[j, 2])]
        
      })
      
      # TODO what is rtl2 et rrtR2 ?
      
      # rtL2 <- MALDIquant::match.closest(input$rtimeL*60, values[["rawData"]]$rt)
      # rtR2 <- MALDIquant::match.closest(input$rtimeR*60, values[["rawData"]]$rt)
      
      # browser()
      # TO DO
      # beofre it was myData  <- do.call(rbind, myData). Check that bind rows work
      myData  <- data.table::rbindlist(myData)
      # TODO checkl if data data.table:rbindlist() is better
      
      if (nrow(myData) > 0) {
        myData
      } else {
        myData <- data.table::data.table(phenoData = attr(rawData,"fileName"))
        
      }
      
      title = paste0("SIM_",input$selectedFragment,"_",attr(rawData,"fileName"))
      
      # if (length(myData) != 1) {
      #   canvas_plots + ggplot2::geom_line(data = myData, mapping = aes(x = rt/60, y = i, colour = factor(round(mz)))) + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity", col= "")
      # } else {
      #   canvas_plots + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity")
      # }
      
      # browser()
      # lattice::xyplot(i ~ (rt/60), data = myData, groups= factor(round(mz)) ,type="l", xlab="Retention time (min)", ylab= "Intensity", main= "SIM",scales = list(tck=c(1,0)))
      
      if ("lattice" %in% input$settings) {
        
        if (length(myData) != 1) {
          mzGroups <- round(myData$mz)
          lattice::xyplot(i ~ (rt/60), data = myData,groups= mzGroups,type="l", xlab="Retention time (min)", ylab= "Intensity", main = title, scales = list(tck=c(1,0)), auto.key=list(points = FALSE, lines = TRUE, title="", corner = c(1, 1), x = 1, y = 1, size = 2))
          
        } else {
          
          lattice::xyplot(0~0, type="l", xlab="Retention time (min)", ylab= "Intensity",main = title, scales = list(tck=c(1,0)), auto.key=list(points = FALSE, lines = TRUE, title="", corner = c(1, 1), x = 1, y = 1, size = 2))
          
        }
        
      } else {
        
        if (length(myData) != 1) {
          canvas_plots + ggplot2::geom_line(data = myData, mapping = aes(x = rt/60, y = i, colour = factor(round(mz)))) + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity", col= "")
        } else {
          canvas_plots + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity")
        }
        
      }
      
    })
    # browser()
    values[["plots_sim"]] <- plots_sim
    
    
    if ("cache" %in% input$settings) {
      
      lapply(seq_along(plots_sim), FUN =  function(i) {
        output[[paste0("sim", i, "_f", values[["runNo"]])]] <- renderCachedPlot({
          
          plots_sim[[i]]
          
        }, cacheKeyExpr = list(input$mzd, input$N_atom,input$mass0,input$rtimeL,input$rtimeR))
      })
      
    } else {
      lapply(seq_along(plots_sim), FUN =  function(i) {
        output[[paste0("sim", i, "_f", values[["runNo"]],input$run3)]] <- renderPlot({
          
          plots_sim[[i]]
          
        })
      })
    }
    
    splitIndex <- findInterval(length(plots_sim)/2, seq_len(length(plots_sim)), all.inside = T)
    
    output$plots_simA <- renderUI({
      #req(values[["rawData"]])
      
      # create tabPanel with datatable in it
      plots_simA <-  lapply(seq_len(values[["plotIndex"]]), function(i) {
        # req(SIMplots())
        plotOutput(paste0("sim",i, "_f", values[["runNo"]],input$run3),hover = paste0("plot_hover_sim",i, "_f", values[["runNo"]],input$run3)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
    })
    
    output$plots_simB <- renderUI({
      #req(SIMplots())
      req(length(plots_sim) > 1)
      # create tabPanel with datatable in it
      plots_simB <- lapply((splitIndex+1):length(plots_sim), function(i) {
        
        plotOutput(paste0("sim",i, "_f", values[["runNo"]],input$run3),hover = paste0("plot_hover_sim",i, "_f", values[["runNo"]],input$run3)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
    })
    
    
    
  }, label = "simOut")
  
  
  
  
  
  ################# THIS VERSION ADDS MISSING VALUES IN DATA.
  
  #   extractData <- reactive({
  #
  # #browser()
  #     req(input$tabs == "SIM")
  #     req(input$N_atom)
  #     # req(input$tabs == "SIM", input$selectedFragment, input$rtimeR,input$rtimeL,input$N_atom,input$mass0, input$mzd)
  #
  #      # req(values[["rawData"]])
  #
  #        if (!is.null(input$mass0) & !is.null(input$N_atom)) {
  #     # extractData.df <- vector("list",length = values[["rawData"]])
  #     extractData <- lapply(values[["rawData"]], function(rawData) {
  #
  #
  #
  # # rtMatrix <- matrix(nrow = 6, ncol = 2) #input$N_atom
  # # mzMatrix <- matrix(nrow = 6, ncol = 2) # input$N_atom, input$mass0
  # # rtMatrix[,1] <- rep(600, 6) # RT low,# +1 to account for the M0 isotopomer
  # # rtMatrix[,2] <- rep(700, 6)
  # # mzMatrix[,1] <- 459:(459+5) - 0.3
  # # mzMatrix[,2] <- 459:(459+5) + 0.3
  #
  # rtMatrix <- matrix(nrow = input$N_atom, ncol = 2)
  # mzMatrix <- matrix(nrow = input$N_atom, ncol = 2)
  #
  # mzMatrix[,1] <- input$mass0:(input$mass0+(input$N_atom-1)) - input$mzd
  # mzMatrix[,2] <- input$mass0:(input$mass0+(input$N_atom-1)) + input$mzd
  #
  # rtMatrix[,1] <- rep(input$rtimeL*60 , input$N_atom) # - 0.5*60,  RT low,# +1 to account for the M0 isotopomer
  # rtMatrix[,2] <- rep(input$rtimeR*60, input$N_atom) # + 0.5*60
  #
  # # browser()
  #
  # myData <- lapply(1:nrow(rtMatrix), rawData=rawData, function(i, rawData, rtL = rtL,rtR = rtL) {
  #
  # # browser()
  # rtL <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
  # rtR <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
  # mzL <- mzMatrix[i, 1]
  # mzR <- mzMatrix[i, 2]
  #
  # DT <- rawData[rt %between% c(rt[[rtL]], rt[[rtR]])][mz %between% c(mzL, mzR)]
  #
  # # # filter retention time
  # # rtFilter <- rawData %>% filter(between(rt,rt[[myRTL]] ,rt[[myRTR]]))
  # #
  # # # filter mz
  # # mzFilter <- rtFilter %>% filter(between(mz, mzL , mzR))
  #
  #
  # })
  #
  # rtL2 <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
  # rtR2 <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
  #
  #   # browser()
  # myData  <- do.call(rbind, myData)
  #
  # if (nrow(myData) > 0) {
  #   myData
  # } else {
  #   myData <- data.table(phenoData = rawData$phenoData[[1]])
  #   myData
  # }
  #
  # #   myData <- data.table(rt= rawData[rt == rt[[rtL2]] | rt == rt[[rtR2]], unique(rt)], i = c(0,0), phenoData = rawData$phenoData[[1]])
  #
  # # myData <- lapply(1:nrow(rtMatrix), function(i) {
  # #
  # # myRTL <- MALDIquant::match.closest((12.67-0.07)*60, p[[1]]$rt)
  # # myRTR <- MALDIquant::match.closest((12.67+0.07)*60, p[[1]]$rt)
  # # mzL <- mzMatrix[i, 1]
  # # mzR <- mzMatrix[i, 2]
  # #
  # # # filter retention time
  # # rtFilter <-p[[1]] %>% filter(between(rt,rt[[myRTL]] ,rt[[myRTR]]))
  # #
  # # # filter mz
  # # mzFilter <- rtFilter %>% filter(between(mz, mzL , mzR))
  # #
  # #
  # # })
  #
  #
  # # browser()
  #
  # # diff <- lapply(myData, function(i) i$rt) %>% unlist %>% unique
  # # phenoData <- lapply(myData, function(i) i$phenoData) %>% unlist %>% unique
  # #
  # #
  # # if (length(diff) != 0) {
  # #
  # # addMissingValues <- mapply(i=myData,k = 1:length(myData), FUN = function(i,k) {
  # #
  # #   if (nrow(i) == 0 ) {
  # #
  # #     i = data.frame(file= 1, rt = diff[[1]], mz = round(mean(mzMatrix[k,])),i = 0,  phenoData = phenoData)
  # #
  # #   } else {
  # #     i
  # #   }
  # #
  # # },SIMPLIFY = F)
  # #
  # # addMissingValues2 <- lapply(addMissingValues, function(k) {
  # #
  # #   #browser()
  # #   for (i in diff) {
  # #
  # #     if (i %in% k$rt) {
  # #
  # #   } else {
  # #
  # #     k <- bind_rows(k, data.frame(file= 1, rt= i, mz=k$mz[[1]], i = 0, phenoData = phenoData))
  # #   # it was bind_rows before instead of full_join!
  # #   }
  # #   }
  # #
  # #   return(k)
  # #
  # # })
  # #
  # # do.call(rbind.data.frame, addMissingValues2) %>% arrange(rt)
  # # } else {
  # #    if (nrow(do.call(rbind.data.frame, myData)) == 0) {
  # #
  # #   tmp  <- mapply(i=myData,k = 1:length(myData), function(i,k) {
  # #        i =  data.frame(file= 1, rt = (input$rtimeL + input$rtimeR)/2, mz = round(mean(mzMatrix[k,])), i = 0,  phenoData = "phenoData")
  # #        }, SIMPLIFY = F)
  # #   do.call(rbind.data.frame, tmp) # same rt for all
  # #
  # #    } else {
  # #   do.call(rbind.data.frame, myData) %>% arrange(rt)
  # #    }
  # # }
  #
  #
  #
  # })
  #     extractData
  #
  # } #if
  #     }) # reactive
  
  
  
  
  
  
  # output$debug <- renderText({
  #     c("You have selected", "folder:",input$folderButton, "file:",input$folderButton, "rtimeL:",input$rtimeL, "rtimeR:",input$rtimeL, input$selectedFragment, input$tabs,input$runParameters,input$mzd2, input$rtime) #input$mass0,input$rtimeL, input$N_atom,input$rtimeR,input$mzd
  #
  # })
  
  #################################     ################################# New method
  
  # rtMatrix <- matrix(nrow = 8, ncol = 2) #input$N_atom
  # mzMatrix <- matrix(nrow = 8, ncol = 2) # input$N_atom, input$mass0
  # rtMatrix[,1] <- rep(500, 8) # RT low,# +1 to account for the M0 isotopomer
  # rtMatrix[,2] <- rep(700, 8)
  # mzMatrix[,1] <- 72:(72+7) - 0.3
  # mzMatrix[,2] <- 72:(72+7) + 0.3
  #
  # fileNames(rawDataL)
  # sps <- rawDataL %>% filterRt(rt = c(500, 700)) # %>% filterMz(mz = mzMatrix[2,])
  # sps <- split(sps, fromFile(rawDataL))
  #
  # myChrList <- lapply(1:length(fileNames(rawDataL)),function(i) {
  #   sps %>% filterMz(mz = mzMatrix[i,])
  #
  # })
  #
  #
  # xcms::chromatogram(values[["rawData"]][[i]], rtMatrix, mzMatrix)
  # # %>% filterMz(mz = c(458.7, 466.3))
  # # rts <- split(filterRt(rawDataL, rt = c(10, 1500)), fromFile(filterRt(rawDataL, rt = c(10, 1500))))
  # p <- plot(sps[[6]])
  # length(sps)
  # sapply(sps, fromFile)
  #
  # sps2 <- sps %>% filterMz(mzMatrix[1,])
  #
  #
  #
  #
  # rts <- split(sps, fromFile(filterRt(rawDataL, rt = c(600, 700))))
  #
  # test <- ldply(rts, function(i) {
  #
  #   ldply(1:length(i), function(x) rbind(rtime(x[[1]])))
  # })
  #
  # test2 <- ldply(rts, function(i) {
  #   ldply(intensity(i[[1]]), rbind)
  # })
  # test3 <- ldply(rts, function(i) {
  #   ldply(mz(i[[1]]), rbind)
  # })
  #
  # sapply(sps, fromFile)
  #
  # sps2 <-  split(sps, fromFile(rawDataL))
  #
  #     rts <- split(filterRt(c(180, 181)), rawDataL)
  #     rest2 <-
  #
  #     extractData.df <- lapply(seq_len(length(rawData)), function(i) {
  #
  #     #MSsubset <- lapply(seq_len(length(rawData)), function(i) {
  #
  #  MSsubset <- xcms::filterRt(rawData[[i]],rtMatrix[1,])
  #   # xcms::filterMz(MSsubset[[i]],mzMatrix[1,])
  #
  # #})
  #
  #  #seq_len(length(N_atom))
  # SIMsubset <- lapply(seq_len(length(rawData)), function(i) {
  #
  #   xcms::filterMz(MSsubset,mzMatrix[i,])
  #   # xcms::filterMz(MSsubset[[i]],mzMatrix[i,])
  #
  # })
  #
  # SIMdatalist <- lapply(seq_len(length(rawData)), function(i) {
  #
  # intensity <- xcms::intensity(SIMsubset[[i]]) %>% sapply(FUN = num0_remove)
  # rtime <- xcms::rtime(SIMsubset[[i]])
  #
  # extractData.df <- data.frame("intensity" = intensity, "rtime" = rtime, "mass0" = "459",check.names = F)
  #   #input$mass0
  # })
  #
  #
  #
  # })
  #
  #         num0_remove <- function(x)({
  #
  #       if (length(x) == 0) {
  #         x <- 0
  #       } else return(x)
  #
  #     })
  
  #
  #     testthat::is_equivalent_to(xcms::intensity(SIMsubset[[i]])[1],numeric(0))
  
  ###################################################### end new method
  
  
  
  # paramFile <- eventReactive(input$fileButton, {
  #   # stopifnot(is.null(input$fileButton))
  # 
  #   tryCatch(file.choose(),
  #            error = function(cond) {message("file choice cancelled"); return(NULL)})
  # 
  # })
  
  
  
  
  # # Data table
  # observeEvent(values[["directory"]], {
  # 
  #   # browser()
  #   values[["paramFile"]] <- paste0(values[["directory"]], "/Parameters/ParameterFile.xlsx")
  #   
  #   if (!is.null(values[["paramFile"]])) {
  # 
  #     
  #     # browser()
  #     tmp <- openxlsx::read.xlsx(values[["paramFile"]],sheet = 1, cols = 1:5) %>% data.table::setDT() #,.name_repair="minimal"
  # 
  #     # expected_header <- c("name", "RT", "lOffset", "rOffset", "Mass0", "LabelAtoms")
  # 
  #     validate(
  # 
  #       if (identical(paramFile_header, colnames(tmp)[1:5])) {
  # 
  #         NULL
  # 
  #       } else {
  # 
  #         #     output$table <- DT::renderDT({
  #         #   #browser()
  #         #
  #         #       DT::datatable(isolate(values[["DF"]]), options = list(
  #         #         pageLength = 50,
  #         #         lengthMenu = c(10, 25, 50, 100, 1000)
  #         #       ),
  #         #       editable = 'cell',selection = 'none',)
  #         # })
  # 
  #         warning("The parameterFile is not properly formatted.")
  #         showNotification("Error: the parameterFile is not properly formatted.")
  # 
  #       }
  #     )
  # 
  #     values[["DF"]] <- tmp
  #     
  #   }
  # },label = "dataTable")
  
  
  # callback <- c(
  #   "var tbl = $(table.table().node());",
  #   "var id = tbl.closest('.datatables').attr('id');",
  #   "table.on('autoFill', function(e, datatable, cells){",
  #   "  var out = [];",
  #   "  for(var i=0; i<cells.length; ++i){",
  #   "    var cells_i = cells[i];",
  #   "    for(var j=0; j < cells_i.length; ++j){",
  #   "      var c = cells_i[j];",
  #   "      var value = c.set === null ? '' : c.set;", # null causes problem in R
  #   "      out.push({row: c.index.row+1, col: c.index.column, value: value});",
  #   # if you want to color the autofilled cells, uncomment the the two lines below  
  #   #  "      $(table.cell(c.index.row, c.index.column).node())",
  #   #  "        .css('background-color', 'yellow');",
  #   "    }",
  #   "  }",
  #   "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  #   "  table.rows().invalidate();", # this updates the column type
  #   "});"
  # )
  # 
  # 
  # 
  # output$table <- DT::renderDT({
  #   # browser()
  #   
  #   DT::datatable(isolate(values[["DF"]]), style = "bootstrap", extensions = 'AutoFill', options = list(
  #     autoFill = TRUE,
  #     pageLength = 50,
  #     callback = JS(callback), 
  #     lengthMenu = c(10, 25, 50, 100, 1000)
  #   ),
  #   editable = 'cell', selection = 'none')
  #   
  # })
  # 
  # 
  # observeEvent(input$table_cell_edit, {
  #   cell <- input[["table_cell_edit"]]
  #   
  #   
  #     values[["DF"]][cell$row, cell$col] <- cell$value
  #     
  #   
  #   # browser()
  #   
  # }, label = "table_cell_edit")
  # 
  # 
  # paramFileTable <- reactive({
  #   # req(values$paramFile)
  #   # req(values[["DF"]])
  #   #finalDF3 <- isolate(values[["DF"]])
  #   
  #   values[["DF"]] %>% data.table::setDT()
  #   
  #   # browser()
  # })
  
  
  # RHANDSONTABLE
  
  # observeEvent({input$fileButton | input$saveTotable}, {
  # 
  #   # browser()
  # 
  #   if (!is.null(paramFile())) {
  # 
  #     values[["DF"]] <- openxlsx::read.xlsx(paramFile(),sheet = 1) #,.name_repair="minimal"
  # 
  #     # expected_header <- c("name", "RT", "lOffset", "rOffset", "Mass0", "LabelAtoms")
  #     expected_header <- c("name", "rt_left", "rt_right", "Mass0", "LabelAtoms")
  # 
  #     validate(
  # 
  #       if (isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5]))) {
  #         NULL
  # 
  #       } else {
  # 
  #         output$hot <- rhandsontable::renderRHandsontable({
  #           # DF  <- values[["DF"]]
  #           rhandsontable::rhandsontable(data.frame(NULL), colHeaders = NULL)
  #         })
  # 
  #         "Error: the parameterFile is not properly formatted."
  #       }
  #     )
  # 
  # 
  # 
  #   }
  # })
  
  #   observe({
  #     req(values[["DF"]],input$selectedFragment, input$selectedFragment != "TIC")
  # 
  # # browser()
  #     # DF[c(DF[,1] == input$selectedFragment), 2] <- input$rtimeL
  #     # DF[c(DF[,1] == input$selectedFragment), 3] <- input$rtimeR
  #     # 
  #     values[["DF"]][c(values[["DF"]][,1] == input$selectedFragment), 2] <- input$rtimeL
  #     values[["DF"]][c(values[["DF"]][,1] == input$selectedFragment), 3] <- input$rtimeR
  #     
  #   })
  
  output$hot <- rhandsontable::renderRHandsontable({
    req(values[["DF"]])
    
    DF  <- values[["DF"]]
    rhandsontable::rhandsontable(DF, stretchH = "all",columnSorting = TRUE) %>% rhandsontable::hot_cols(valign="htCenter")
    
  })
  
  observeEvent(input$hot, {
    
    output$hot <- rhandsontable::renderRHandsontable({
      # browser()
      
      
      DF = NULL
      if (!is.null(input$hot)) {
        
        if (is.even(n_undo)) {
          # browser()
          values[["lastValues"]] <- isolate(values[["DF"]])
        }
        
        DF = rhandsontable::hot_to_r(input$hot)
        values[["DF"]] = DF
        
      } else if (!is.null(values[["DF"]])) {
        DF = values[["DF"]]
      }
      
      if (!is.null(DF)) {
        n_undo <<- n_undo + 1
        # cat(i)
        rhandsontable::rhandsontable(DF, stretchH = "all",columnSorting = TRUE) %>% rhandsontable::hot_cols(valign="htCenter")
        
      }
    })
  } ,ignoreInit = T)
  
  
  observeEvent(input$undoButton, {
    
    # browser()
    
    lastValues <- isolate(values[["lastValues"]])
    
    if(!is.null(lastValues) && nrow(dplyr::setdiff(lastValues, isolate(values[["DF"]]))) != 0) {
      
      #browser()
      
      # cat(i)
      
      output$hot <-rhandsontable::renderRHandsontable({
        
        rhandsontable::rhandsontable(lastValues, stretchH = "all",columnSorting = TRUE) %>% rhandsontable::hot_cols(valign="htCenter")
        
        
      })
      
      n_undo <<- n_undo + 1
    }
    
    
    # DF <<- isolate(values$DF)
    
  })
  
  # validate(
  #     need(isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5])), message = "need 1")
  # )
  
  # validate(
  #     if (isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5]))) {
  #         NULL
  #     } else {
  #
  #         values[["DF"]] <- NULL
  #         "Error: the parameterFile is not properly formatted."
  #     }
  # )
  
  
  # selection_val  <- reactiveValues()
  # selection_rowI  <- reactiveValues()
  # selection_colI  <- reactiveValues()
  #
  # observeEvent({input$hot_select},{
  #   browser()
  #
  #  selection_colI <- input$hot_select$select$r
  #  selection_rowI <- input$hot_select$select$c
  #  selection <- input$hot_select$data[[selection_rowI]][[selection_colI]]
  #
  # })
  
  #     observeEvent(input$undoButton,{
  #     browser()
  #
  #     output$hot <- renderRHandsontable({
  #         DF  <- isolate(values2[["DF"]])
  #           rhandsontable(DF,columnSorting = TRUE, selectCallback =F)
  # })
  #
  #   })
  

  observeEvent(input$saveTotable, {
    
    # browser()
    DF <- isolate(values[["DF"]])
    
    # If you use data table then the syntax is: DF[c(DF[,1] == input$selectedFragment), 2] <- input$rtimeL
    DF[c(DF[,1] == input$selectedFragment), 2] <- input$rtimeL
    DF[c(DF[,1] == input$selectedFragment), 3] <- input$rtimeR
    DF[c(DF[,1] == input$selectedFragment), 4] <- input$mass0
    DF[c(DF[,1] == input$selectedFragment), 5] <- input$N_atom
    # TODO, replace corresponding values
    
    values[["DF"]] <- DF
    
    openxlsx::write.xlsx(DF, file = values[["paramFile"]], row.names = F, keepNA = F)
    
    # output$table <- DT::renderDT({
    #   #browser()
    #   
    #   DT::datatable(DF,extensions = 'AutoFill', style = "bootstrap", options = list(
    #     autoFill = TRUE,
    #     pageLength = 50,
    #     lengthMenu = c(10, 25, 50, 100, 1000)
    #   ),
    #   editable = 'cell',selection = 'none')
    #   
    # })
    
    showNotification("Saved !")
    
  },label = "saveTotable")
  
  
  
  
  
  
  
  observeEvent(input$saveButton, {
    
    finalDF <- isolate(values[["DF"]])
    finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
    #finalDF <- finalDF[-1] # remove the first column (automatic index when importing data)
    #na.replace(finalDF,"")
    #replace(finalDF,"#N/A")
    
    # na.replace(parameterFile," ")
    
    # xlsx::write.xlsx(finalDF, file = paramFile(),row.names = F,showNA = F) #"/home/mathieu/Documents/Data/MSinco/MSTFA over time/Parameters/ParameterFile.xlsx"
    openxlsx::write.xlsx(finalDF, file = values[["paramFile"]], row.names = FALSE, keepNA = F)
    showNotification("Saved !")
  },label = "saveButton")
  
  observeEvent({input$runButton2}, {
    
    # browser()
    
    finalDF <- isolate(values[["DF"]])
    finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
    openxlsx::write.xlsx(finalDF, file = values[["paramFile"]],row.names = F,keepNA = F)
    showNotification("Saved prior integration ...")
    
    if (isTRUE("savePlots" %in% input$runParameters)) {
      saveAllPlots = TRUE } else { saveAllPlots = FALSE }
    
    if (isTRUE("baselineCorrection" %in% input$runParameters)) {
      baselineCorrection = TRUE } else { baselineCorrection = FALSE }
    
    
    if (isTRUE("isotopeCorrection" %in% input$runParameters)) {
      isotopeCorrection = TRUE } else {isotopeCorrection <- FALSE }
    
    if (isTRUE("correctTracerImpurity" %in% input$runParameters)) {
      correctTracerImpurity = TRUE } else {correctTracerImpurity = FALSE }
    
    if (is.null(input$mzd2)) {
      mzd <- 0.3 } else {mzd <- isolate(input$mzd2)}
    
    
    dir2 <- isolate(values[["directory"]])
    rawData <- isolate(values[["rawData"]])
    
    # browser()
    
    
    # callModule(intCorServer,"intCor", rawData, dir2,saveAllPlots, baselineCorrection,isotopeCorrection, mzd, correctTracerImpurity)
    intCor(rawData, dir2,saveAllPlots, baselineCorrection,isotopeCorrection, mzd, correctTracerImpurity)
    
  },label = "runButton2")
  
  #refreshInputsButton, input$fileButton
  # fragments <- eventReactive({
  #   input$refreshInputsButton
  #   }, {
  #
  #   finalDF2 <- isolate(values[["DF"]]) # readxl::read_excel(paramFile())
  #   return(finalDF2[1])
  #
  # })
  
  
  
  observeEvent(input$saveActivePlotsButton, {
    #stopifnot(!is.null(input$checkboxGroupInput))
    if (!is.null(input$saveActivePlotsButton)) {
      
      # if directory exists() ?????
      activePlotsDir <- paste0(values[["directory"]],"/Active Plots")
      
      if (!dir.exists(activePlotsDir)) {
        dir.create(activePlotsDir,mode = "0777")
      }
      
      withProgress(message = 'Saving ...', value = 0, {
        
        if (input$tabs == "TIC" && !is.null(TICplots)) {
          # browser()
          sapply(seq_along(values[["rawData"]]),function(i) {
            
            incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
            #   system.time({
            # png(paste0("/home/mathieu/Documents/MSinco demo/MSinco demo/Active Plots/", TICplots[[i]]$labels$title,".png"))
            # print(TICplots[[i]])
            # dev.off()
            #   })
            # system.time({
            ggplot2::ggsave(paste0(TICplots[[i]]$labels$title,".png"), plot = TICplots[[i]], path = activePlotsDir, device = "png")
            # })
          })
        }
        
        
        if (input$tabs == "SIM" && !is.null(SIMplots)) {
          
          sapply(seq_along(values[["rawData"]]),function(i) {
            
            incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
            # png(paste0(activePlotsDir, "/SIM_", i,".png"))
            # print(SIMplots()[[i]])
            # dev.off()
            
            ggplot2::ggsave(paste0(SIMplots[[i]]$labels$title,".png"), plot = SIMplots[[i]], path = activePlotsDir, device = "png")
          })
        }
        
        if (input$tabs == "MSpectrum" && !is.null(MSpecplots)) {
          
          sapply(seq_along(values[["rawData"]]),function(i) {
            
            incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
            # png(paste0(activePlotsDir, "/MSpec_", i,".png"))
            # print(MSpecplots()[[i]])
            # dev.off()
            
            ggplot2::ggsave(paste0(MSpecplots[[i]]$labels$title,".png"), plot = MSpecplots[[i]], path = activePlotsDir, device = "png")
            
          })
        }
      })
      
    }
    
  }, label = "saveActivePlotsButton")
  
  
  #   observeEvent(input$savePlotsButton, {
  #     #stopifnot(!is.null(input$checkboxGroupInput))
  #  if (!is.null(input$savePlotsButton)) {
  #
  #     dir.create(paste0(values[["directory"]],"/Active Plots"),mode = "0777")
  #     activePlotsDir <- paste0(values[["directory"]],"/Active Plots")
  #
  #
  #       withProgress(message = 'Saving ...', value = 0, {
  #
  #     if (isTRUE("TIC" %in% input$savePlots & !is.null(TICplots()))) {
  #
  #       sapply(seq_along(values[["rawData"]]),function(i) {
  #
  #         incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/TIC_", i,".png"))
  #       print(TICplots()[[i]])
  #       dev.off()
  #         # ggsave(paste0("TIC_",i), plot = TICplots()[[i]], path = getwd(), device = "png")
  #       })
  #     }
  #
  #
  #       if (isTRUE("SIM" %in% input$savePlots)) {
  #
  #       sapply(seq_along(values[["rawData"]]),function(i) {
  #
  #       incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/SIM_", i,".png"))
  #       print(SIMplots()[[i]])
  #       dev.off()
  #       #ggsave(paste0("SIM_",i), plot = SIMplots()[[i]], path = getwd(), device = "png")
  #       })
  #     }
  #
  #       if (isTRUE("MSpectrum" %in% input$savePlots)) {
  #
  #       sapply(seq_along(values[["rawData"]]),function(i) {
  #
  #       incProgress(1/(length(values[["rawData"]])*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/MSpec_", i,".png"))
  #       print(MSpecplots()[[i]])
  #       dev.off()
  #       #ggsave(paste0("MSpectrum_",i), plot = MSpecplots()[[i]], path = getwd(), device = "png")
  #
  #       })
  #       }
  #   })
  #
  # }
  #
  #  })
}
