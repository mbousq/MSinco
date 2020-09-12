# intCorUI <- function(id) {
#   ns <- NS(id)
# }
# , pathExperimentFolder, saveAllPlots = TRUE, baselineCorrection= TRUE,isoCor = TRUE

# intCorServer <- function(input,output,session, rawData, pathExperimentFolder, saveAllPlots, baselineCorrection, isotopeCorrection, mzd, correctTracerImpurity= FALSE,correctAlsoMonoisotopic = FALSE) {
intCor <- function(rawData, pathExperimentFolder, saveAllPlots, baselineCorrection, isotopeCorrection, mzd, correctTracerImpurity= FALSE,correctAlsoMonoisotopic = FALSE) {
  
  library(readxl);library(xcms);library(reshape2);library(xcms);library(ggplot2);library(gtools);library(knitr);
  library(tidyverse);library(plyr);library(IsoCorrectoR);library(matrixStats);library(ecipex);library(xlsx);
  library(xlsxjars)
  #;library(rowr)
  
  # observe({ 
  
  # browser()
  
  withProgress(message = 'Runing ...', value = 0, {
    
    # if (isTRUE("savePlots" %in% input$runParameters)) {
    #   savePlots = TRUE } else { savePlots <- FALSE }
    # 
    # if (isTRUE("baselineCorrection" %in% input$runParameters)) {
    #   baselineCorrection = TRUE } else { baselineCorrection <- FALSE }
    # 
    # 
    # if (isTRUE("isotopeCorrection" %in% input$runParameters)) {
    #   isotopeCorrection = TRUE} else {isotopeCorrection <- FALSE }
    # 
    # if (isTRUE("correctTracerImpurity" %in% input$runParameters)) {
    #   correctTracerImpurity = TRUE } else {correctTracerImpurity <- FALSE }
    # 
    # 
    # if (is.null(input$mzd2)) {
    #   mzd <- 0.3} else {mzd <- isolate(input$mzd2)}
    
    # pathExperimentFolder <- dir()
    # rawData <- rawData()
    #browser()
    #      pathExperimentFolder <- dir2
    # pathExperimentFolder <- dir2
    # Leave ExtendWindow as FALSE. Issue when it's true.
    # MSinco_v2(pathExperimentFolder, savePlots=FALSE,mzd = 0.8)
    
    # Default value for lb and rb are 0.1 (this can be changed here)
    # mzd <- 0.3
    # savePlots <- FALSE
    # baselineCorrection <- TRUE
    # isotopeCorrection <- TRUE
    # Create results folder and copy the parameter File to that folder (to keep a copy)
    #mzd <- 0.3
    
    if (isEmpty(grep("Results1", list.dirs(pathExperimentFolder, recursive = FALSE, full.names = FALSE))) == TRUE) {
      folder <- paste0(pathExperimentFolder, "/", format(Sys.time(), "%d-%m-%y"), " ", "Results1")
      dir.create(folder, mode = "0777")
      dir.create(paste0(folder, "/PLOTS", sep = ""), mode = "0777")
      file.copy(paste0(pathExperimentFolder, "/Parameters/ParameterFile.xlsx"), folder)
      file.copy(paste0(pathExperimentFolder, "/Parameters/MoleculeFile.xlsx"), folder)
    } else {
      folder <- paste0(pathExperimentFolder, "/", format(Sys.time(), "%d-%m-%y"), " ", "Results", max(as.numeric(gsub("Results", "", str_extract(grep("Results*", list.files(pathExperimentFolder), value = TRUE), pattern = "Results[0-9]{1,4}"))), na.rm = TRUE) + 1)
      dir.create(folder, mode = "0777")
      dir.create(paste0(folder, "/PLOTS"), mode = "0777")
      file.copy(paste0(pathExperimentFolder, "/Parameters/ParameterFile.xlsx"), folder)
      file.copy(paste0(pathExperimentFolder, "/Parameters/MoleculeFile.xlsx"), folder)
    }
    
    # Set paths
    parameterFile <- read_excel(paste0(pathExperimentFolder, "/Parameters", "/ParameterFile.xlsx"))
    moleculeFile <- read_excel(paste0(pathExperimentFolder, "/Parameters", "/MoleculeFile.xlsx"))
    # netCDFs <- paste0(pathExperimentFolder, "/Netcdfs/") # where your netCDF files are
    results <- paste0(folder, "/")
    
    # Save some parameter info
    write(knitr::kable(data.frame(Sys.time(), mzd, baselineCorrection), align = "c", row.names = FALSE), file = paste0(folder, "/conf.txt"))
    
    # Load netCDFs files
    #tictoc::tic("Load netCDFs files")
    
    # rawData <- c()
    # for (i in seq_along(list.files(netCDFs, full.names = TRUE, pattern = ".CDF$", ignore.case = TRUE))) {
    #   rawData <- c(rawData, readMSData(mixedsort(list.files(netCDFs, full.names = TRUE, pattern = ".CDF$", ignore.case = TRUE))[i], mode = "onDisk"))
    # }
    
    # browser()
    netCDFs <- list.files(paste0(pathExperimentFolder,"/Netcdfs"), full.names = TRUE, pattern = ".CDF$", ignore.case = T)
    
    rawData <- future_lapply(seq_along(netCDFs), function(i) {
      
      readMSData(mixedsort(netCDFs)[i], mode = "onDisk", msLevel. = 1)
      
    })
    #tictoc::toc()
    # Objects
    # integrals.array <- paste0(rep("integrals.ls", length(rawData)), seq_along(rawData))
    integrals.array2 <- paste0(rep("integrals.df", length(rawData)), seq_along(rawData))
    
    
    
    ######################################
    
    rtMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 2)
    mzMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 2)
    namesMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 1)
    namesMatrix2 <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 1)
    namesMatrix3 <- c()
    
    #  incProgress(1/length(unique(namesMatrix)))
    
    k <- 0
    for (a in seq_along(parameterFile[[1]])) {
      
      # namesMatrix3 <- c(namesMatrix3, paste(unlist(strsplit(parameterFile[[1]][a], split = " "))[1], as.numeric(unlist(strsplit(parameterFile[[1]][a], split = " "))[2]) + seq_len(parameterFile[[6]][a] + 1) - 1))
      
      temp <- str_split(parameterFile[[1]][a], pattern = "(-)|(_)| |(\\.)", simplify = T)
      if (length(temp) <= 2) {
        namesMatrix3 <- c(namesMatrix3, paste(temp[1], as.numeric(temp[2]) + seq_len(parameterFile[[5]][a] + 1) - 1))
      } else {
        namesMatrix3 <- c(namesMatrix3, paste(temp[1], paste0(as.numeric(temp[2]) + seq_len(parameterFile[[5]][a] + 1) - 1, "-", temp[3])))
      }
      # str_split(parameterFile[[1]][a]," ",simplify = T)[2] %>% gsub(pattern = "(-.*)|(_.*)||( .*)",replacement = "" ,.)
    }
    
    
    namesMatrix2[, 1] <- namesMatrix3
    # rep(unlist(strsplit(parameterFile[[1]], split = " "))[1],parameterFile[[6]]+1)
    
    k <- 0
    for (a in seq_along(parameterFile[[1]])) {
      for (i in 1:(parameterFile[[5]][a] + 1)) { # +1 to account for the M0 isotopomer
        mzMatrix[k + i, 1] <- (parameterFile[[4]][a] + i - 1 - mzd)
        mzMatrix[k + i, 2] <- (parameterFile[[4]][a] + i - 1 + mzd)
      }
      k <- k + i
    }
    
    rtMatrix[, 1] <- rep(parameterFile[[2]], (parameterFile[[5]] + 1)) * 60 # RT low,# +1 to account for the M0 isotopomer
    rtMatrix[, 2] <- rep(parameterFile[[3]], (parameterFile[[5]] + 1)) * 60 # RT high,# +1 to account for the M0 isotopomer
    namesMatrix[, 1] <- rep(parameterFile[[1]], parameterFile[[5]] + 1)
    
    
    
    
    
    metaboliteData <- function(x) {
      # REMOVE NA VALUES in data
      if (all(is.na(myChr[x]@intensity)) == FALSE &
          all(is.na(myChr[x]@rtime)) == FALSE) {
        data.frame(intensity = subset(myChr[x]@intensity, is.na(myChr[x]@intensity) == FALSE), rtime = subset(myChr[x]@rtime, is.na(myChr[x]@intensity) == FALSE), mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), length(subset(myChr[x]@rtime, is.na(myChr[x]@intensity) == FALSE))))
      } else {
        if (all(is.na(myChr[x]@intensity)) == TRUE &
            all(is.na(myChr[x]@rtime)) == TRUE) {
          # this case arise if the user input retention times that does not exist in the file (so intensity, rtime are NA). here I add intensity 0 at the very first scan
          data.frame(
            intensity = rep(0, 1),
            rtime = rawData[[j]]@featureData@data$retentionTime[1],
            mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), 1)
          )
        } else {
          # this case arise if there is simply no intensities in the retention time range input (only intensities are NA)
          data.frame(
            intensity = rep(0, length(myChr[x]@rtime)),
            rtime = myChr[x]@rtime,
            mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), length(myChr[x]@rtime))
          )
        }
      }
    }
    
    for (j in seq_along(rawData)) {
      # tictoc::tic("only One myChr")
      incProgress(1/(nrow(unique(namesMatrix))+ length(rawData)))
      myChr <- chromatogram(rawData[[j]], rtMatrix, mzMatrix)
      extractData.ls <- lapply(seq_len(nrow(myChr)), FUN = metaboliteData) # list of data.frames
      extractData.ls2 <- extractData.ls
      names(extractData.ls) <- namesMatrix
      names(extractData.ls2) <- namesMatrix2
      extractData.df <- ldply(.data = extractData.ls, .fun = rbind())
      # tictoc::toc()
      
      predictions.ls2 <- list()
      correctedIntensities4 <- list()
      
      if (baselineCorrection == TRUE) {
        
        for (i in seq_along(namesMatrix)) {
          
          if (all(is.na(extractData.ls2[[i]]$intensity[1])) == FALSE) {
            rtWindow <- data.frame(rtime = extractData.ls2[[i]]$rtime)
            leftScan <- subset(extractData.ls2[[i]], is.na(intensity) == FALSE)[1, ]
            rightScan <- subset(extractData.ls2[[i]], is.na(intensity) == FALSE)[nrow(subset(extractData.ls2[[i]], is.na(intensity) == FALSE)), ] # Instead of using only one value, we could do an AVERAGE intensity of multiple scan in each side of the integration window.
            modelData <- rbind(leftScan, rightScan)
            model <- lm(intensity ~ rtime, data = modelData)
            # x <- extractData.ls2[[1]]$intensity
            # lm()
            # modelPoly2 <- lm(intensity ~ rtime, data = modelData)
            # modelPoly <- lm(intensity ~ rtime, data = modelData)
            
            prediction <- predict(model, newdata = rtWindow, type = "response")
            
            # prediction <- tryCatch(predict(model, newdata = rtWindow, type = "response"),
            # warning=function(cond) {
            #     #print(model)
            #     #print(c("intensity:",extractData.ls2[[i]]$intensity))
            #     #print(c("rtime:",extractData.ls2[[i]]$rtime))
            #     #print(c("mass0:",extractData.ls2[[i]]$mass0))
            #     #print(modelData)
            #     #print(i)
            #     #print(extractData.ls2[[i]])
            #     #print(modelData)
            #     },finally = predict(model, newdata = rtWindow, type = "response")
            #     )
            
            prediction2 <- data.frame(intensity = prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            predictions.ls <- list(prediction2)
            names(predictions.ls) <- namesMatrix[i] # names(extractData.ls2[i])
            predictions.ls2 <- append(predictions.ls2, predictions.ls)
            
            correctedIntensities <- data.frame(intensity = extractData.ls2[[i]]$intensity - prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            correctedIntensities$intensity[correctedIntensities$intensity < 0] <- 0 # removes negative values
            correctedIntensities3 <- list(correctedIntensities)
            names(correctedIntensities3) <- namesMatrix2[i]
            correctedIntensities4 <- append(correctedIntensities4, correctedIntensities3)
          } else {
            prediction <- 0 # data.frame(data.frame(intensity=0,rtime=extractData.ls2[[i]]$rtime,mass0=extractData.ls2[[i]]$mass0))
            predictions.ls <- list(data.frame(intensity = 0, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0))
            names(predictions.ls) <- namesMatrix[i] # names(extractData.ls2[i])
            predictions.ls2 <- append(predictions.ls2, predictions.ls)
            warning("No intensities were found at the the RT low parameter (NA). Baseline correction could not be applied. Please change the RT low parameter")
            
            correctedIntensities <- data.frame(intensity = extractData.ls2[[i]]$intensity - prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            # correctedIntensities$intensity[correctedIntensities$intensity < 0] <- 0 #removes negative values
            correctedIntensities3 <- list(correctedIntensities)
            names(correctedIntensities3) <- namesMatrix2[i]
            correctedIntensities4 <- append(correctedIntensities4, correctedIntensities3)
          }
        }
        predictions.df <- ldply(.data = predictions.ls2, .fun = rbind())
        integrals.ls <- sapply(correctedIntensities4, function(x) sum2(x$intensity, na.rm = TRUE)) # sum(x$intensity)
        # correctedIntensities4.df <- ldply(.data = correctedIntensities4, .fun = rbind()) #COULD PLOT THIS ONE AS WELL
        integrals.df <- data.frame(namesMatrix2, integrals.ls, namesMatrix)
        colnames(integrals.df) <- c("isotopomer", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "Subset")
        assign(integrals.array2[j], integrals.df)
      } else {
        integrals.ls <- sapply(extractData.ls2, function(x) sum2(x$intensity, na.rm = TRUE)) # sum(x$intensity)
        # correctedIntensities4.df <- ldply(.data = correctedIntensities4, .fun = rbind()) #COULD PLOT THIS ONE AS WELL
        integrals.df <- data.frame(namesMatrix2, integrals.ls, namesMatrix)
        colnames(integrals.df) <- c("isotopomer", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "Subset")
        assign(integrals.array2[j], integrals.df)
      }
      
      if (saveAllPlots == TRUE & baselineCorrection == TRUE) {
        
        for (i in unique(namesMatrix)) {
          incProgress(1/(3*nrow(unique(namesMatrix))))
          
          if (dir.exists(paste0(folder, "/PLOTS/", i)) == FALSE) {
            dir.create(paste0(folder, "/PLOTS/", i), mode = "0777")
          }
          
          png(paste0(folder, "/PLOTS/", i, "/File_", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "_", i, ".png"))
          
          print(
            lattice::xyplot(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$intensity ~ subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$rtime/60, groups= subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$mass0,type="l", xlab="Retention time (min)", ylab= "Intensity", main= gsub(".CDF", "", row.names(rawData[[j]]@phenoData)),scales = list(tck=c(1,0)), auto.key=list(points = FALSE, lines = TRUE, title=paste0(sub(" .*", "", unique(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$.id))),cex.title=1.2, corner = c(1, 1), x = 1, y = 1, size = 1.5)) + 
              
              latticeExtra::as.layer(lattice::xyplot(subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$intensity~ subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$rtime/60, groups= subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$mass0,type="l",lty=2, lwd = 0.5)) 
          )
          dev.off()
          
          # ggplot() + geom_line(data = subset.data.frame(extractData.df, subset = extractData.df[, 1] == i), mapping = aes(x = rtime / 60, y = intensity, colour = factor(mass0))) + geom_line(data = subset.data.frame(predictions.df, subset = extractData.df[, 1] == i), mapping = aes(x = rtime / 60, y = intensity, colour = factor(mass0)), linetype = "dashed", size = 0.2) + labs(title = gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), x = "Time (min)", y = "Intensity", colour = paste0(sub(" .*", "", unique(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$.id)), " m/z")) +
          #   theme(
          #     axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          #     axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          #     plot.title = element_text(size = 20, color = "Black")
          #   ) + ggsave(paste0(folder, "//PLOTS//", i, "//File_", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "_", i, ".png"))
          
        }
      } else {
        if (saveAllPlots == TRUE & baselineCorrection == FALSE) { ## CHECK THIS LINE (14-05-2019)
          for (i in unique(namesMatrix)) {
            if (dir.exists(paste0(folder, "/PLOTS/", i)) == FALSE) {
              dir.create(paste0(folder, "/PLOTS/", i), mode = "0777")
            }
            
            ggplot() + geom_line(data = subset.data.frame(extractData.df, subset = extractData.df[, 1] == i), mapping = aes(x = rtime / 60, y = intensity, colour = factor(mass0))) + labs(title = gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), x = "Time (min)", y = "Intensity", colour = paste0(sub(" .*", "", unique(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$.id)), " m/z")) +
              theme(
                axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
                axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
                plot.title = element_text(size = 20, color = "Black")
              ) + ggsave(paste0(folder, "/PLOTS/", i, "/File_", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "_", i, ".png"))
          }
        }
      }
    }
    
    #browser()
    
    # TODO add this function in a separate place/file
    multi_full_join <- function(list) Reduce(function(x,y) full_join(x,y , by ="Measurements/Samples"), list)
    
    mytable <- matrix()
    for (i in unique(namesMatrix)) {
      
      incProgress(1/(nrow(unique(namesMatrix)) + length(rawData)))
      
      # for (x in seq_along(rawData)) {
      #   mytable <- cbind(mytable, subset.data.frame(get(integrals.array2[x]), subset = Subset == i, select = 2))
      # }
      
      mytable <- lapply(seq_along(rawData), function(x) {
        
        get(integrals.array2[x]) %>% filter(Subset == i) %>% .[2] %>% mutate("Measurements/Samples" = paste0(i, "_", 0:(nrow(.) - 1))) %>% .[2:1]
        
      })
      
      mytable <- multi_full_join(mytable)
      
      browser()
      
      xlsx::write.xlsx(mytable, paste0(results, i, ".xlsx"),row.names = F)
      
      mytable2 <- mytable[-1]
      # isotopomers <- paste0(i, "_", 0:(nrow(mytable2) - 1))
      # row.names(mytable2) <- isotopomers
      # # dir.create(paste0(results, i))
      # 
      # # Create input MeasurementFile for IscocorrectorR
      # wb1 <- createWorkbook(type = "xlsx")
      # sheet1 <- createSheet(wb1)
      # addDataFrame(mytable2, sheet1)
      # allRows <- getRows(sheet1)
      # allCells <- getCells(allRows)
      # setCellValue(allCells[[1.2]], "Measurements/Samples")
      # saveWorkbook(wb1, paste0(results, i, ".xlsx"))
      # # saveWorkbook(wb1, paste0(results, i, "//", i, ".xlsx"))
      # 
      # mytable <- matrix() # resets the matrix for the next iteration
      
      if (isotopeCorrection == TRUE) {
        # Perform isotopic correction using isocorrectorR.
        IsoCorrection(MeasurementFile = paste0(results, i, ".xlsx"), ElementFile = paste(pathExperimentFolder, "/Parameters/ElementFile.xlsx", sep = ""), MoleculeFile = paste0(pathExperimentFolder, "/Parameters/MoleculeFile.xlsx"), DirOut = paste0(results), FileOutFormat = "xls", FileOut = i, CorrectAlsoMonoisotopic = correctAlsoMonoisotopic,CorrectTracerImpurity =  correctTracerImpurity)
        
        file.remove(paste0(results, i, ".xlsx"))
        dirList <- list.dirs(results, recursive = FALSE, full.names = TRUE)
        file.rename(dirList[1], paste0(results, i)) # format(Sys.time(),"%H%M%S")
      }
      
      
      
      
      # dirListInfo <- file.info(dirList)
      # details = dirListInfo[with(dirListInfo, order(as.POSIXct(mtime))), ]
      # files = rownames(details)
      if (isotopeCorrection == TRUE) {
        # Create a RawDataFractions sheet in the isocorrectorR file
        m <- sapply(mytable2, function(x) {
          as.numeric(x) / sum(x)
        })
        rownames(m) <- rownames(mytable2)
        
        # tictoc::tic("Checks and theoritical MIDs")
        wb2 <- loadWorkbook(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        try(RawDataFractions <- createSheet(wb2, sheetName = "RawDataFractions"), silent = TRUE)
        RawDataFractions <- createSheet(wb2, sheetName = "RawDataFractions")
        addDataFrame(data.frame(m, check.names = FALSE), RawDataFractions)
        xlsx::saveWorkbook(wb2, list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # get the theoritical MIDs
        formula <- gsub("LabC[0-9]{1,3}", "", subset(moleculeFile, moleculeFile[1] == i, select = 2))
        theoriticalMID <- ecipex(formula)
        theoriticalMID2 <- sapply(theoriticalMID, function(x) {
          x[3] <- round(x[1])
          myfill <- matrix(nrow = nrow(unique(x[3])), ncol = 3)
          k <- 1
          for (l in unlist(unique(x[3]))) {
            s <- subset(x, mass.1 == l)
            mySum <- sum(s$abundance)
            myMass <- weighted.mean(s$mass, s$abundance)
            myfill[k, 1] <- myMass
            myfill[k, 2] <- mySum
            k <- k + 1
          }
          myfill[, 3] <- myfill[, 2] / max(myfill[, 2]) * 100
          x <- list(data.frame("mass" = myfill[, 1], "abundance% theory" = myfill[, 2], "mol% theory" = myfill[, 3], check.names = FALSE))
        })
        
        isocorFractions <- read_excel(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE), sheet = "CorrectedFractions")
        isocorFractions <- isocorFractions[, -1]
        # To be deleted if no error : RawData <- read_excel(list.files(paste(results,i,sep=""),pattern =paste("IsoCorrectoR_",i,".xls$",sep=""),recursive=TRUE,full.names = TRUE))
        # To be deleted if no errors: RawData <- RawData[,-1]
        
        if (length(rawData) > 1) {
          
          ## raw data ratios
          # To be deleted if no error :   m <- sapply(RawData, function(x) {
          # To be deleted if no error :     as.numeric(x)/sum(x)
          # To be deleted if no error :   })
          meanRawData <- matrix(rowMeans(m, na.rm = TRUE))
          sdsRawData <- matrix(rowSds(as.matrix(m), na.rm = TRUE))
          
          ## raw data mol%
          m2 <- sapply(data.frame(m), function(x) {
            # x/max(mydf[l])*100
            as.numeric(x) / x[1] * 100
          })
          
          meanMolRawData <- matrix(rowMeans(m2, na.rm = TRUE))
          sdsMolRawData <- matrix(rowSds(m2, na.rm = TRUE))
          
          # isocor data ratios
          meanIsocorData <- matrix(rowMeans(sapply(isocorFractions, function(x) {
            as.numeric(x)
          }), na.rm = TRUE))
          sdsIsocorData <- matrix(rowSds(sapply(isocorFractions, function(x) {
            as.numeric(x)
          }), na.rm = TRUE))
          
          # isocor data mol%
          m3 <- sapply(isocorFractions, function(x) {
            as.numeric(x) / as.numeric(x)[1] * 100
          })
          
          meanMolIsocorData <- matrix(rowMeans(m3, na.rm = TRUE))
          sdsMolIsocorData <- matrix(rowSds(m3, na.rm = TRUE))
          
          # Diff mol% raw data and theoritical mol%
          difference <- meanMolRawData - theoriticalMID2[[1]]$`mol% theory`[1:length(meanMolRawData)]
          group <- rowr::cbind.fill("mean abundance data" = meanRawData, "mean mol% data" = meanMolRawData, "stdev mol% data" = sdsMolRawData, "difference" = difference, "mean abundance isocor" = meanIsocorData, fill = NA)
          colnames(group) <- c("mean abundance data", "mean mol% data", "stdev mol% data", "difference", "mean abundance isocor")
        } else {
          
          ## raw data ratios
          singleRawData <- RawData[1] / sum(RawData[1])
          
          ## raw data mol%
          singleMolRawData <- singleRawData / singleRawData[[1]][1] * 100
          
          # isocor data ratios
          singleIsocorData <- isocorFractions[1]
          
          # isocor data mol%
          singleMolIsocorData <- singleIsocorData / singleIsocorData[[1]][1] * 100
          
          # Diff mol% raw data and theoritical mol%
          difference <- subtract(singleMolRawData, theoriticalMID2[[1]]$`mol% theory`[1:nrow(singleMolRawData)])
          group <-  rowr::cbind.fill("abundance data" = singleRawData, "mol% data" = singleMolRawData, "difference" = difference, "abundance isocor" = singleIsocorData, fill = NA)
          colnames(group) <- c("abundance data", "mol% data", "difference", "abundance isocor")
        }
        
        ## Write new sheet
        wb3 <- loadWorkbook(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # cell styles
        cs1 <- CellStyle(wb3) + Font(wb3, isBold = TRUE, boldweight = 700) + Border(color = "black", position = c("TOP", "BOTTOM")) + Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs2 <- CellStyle(wb3) + Font(wb3, isBold = TRUE, boldweight = 700) + Border(color = "black", position = "BOTTOM") + Alignment(h = "ALIGN_LEFT", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs3 <- CellStyle(wb3) + Font(wb3) + Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs4 <- CellStyle(wb3) + Font(wb3) + Alignment(h = "ALIGN_LEFT", v = "VERTICAL_CENTER") + DataFormat("0.00")
        
        try(sheet <- createSheet(wb3, sheetName = "Checks"), silent = TRUE)
        Checks <- createSheet(wb3, sheetName = "Checks")
        
        addDataFrame(data.frame(c("Formula", "Exact mass"), c(names(theoriticalMID2), round(theoriticalMID2[[1]][[1]][1], 3))), Checks, startRow = 2, row.names = FALSE, col.names = FALSE)
        addDataFrame(theoriticalMID2[[1]], Checks, startRow = 4, row.names = FALSE, colnamesStyle = cs1)
        addDataFrame(as.data.frame(group), Checks, startRow = 4, startColumn = 4, row.names = FALSE, colnamesStyle = cs1) # colnamesStyle gives an error ...
        addDataFrame(i, Checks, startRow = 1, startColumn = 1, row.names = FALSE, col.names = FALSE)
        myRows <- getRows(Checks)
        myCells <- createCell(myRows[1], colIndex = 2:10)
        sapply(getCells(myRows[1]), function(x) {
          setCellStyle(x, cellStyle = cs2)
        })
        sapply(getCells(myRows[2:3]), function(x) {
          setCellStyle(x, cellStyle = cs4)
        })
        sapply(getCells(myRows[5:length(myRows)]), function(x) {
          setCellStyle(x, cellStyle = cs3)
        })
        
        # saveWorkbook(wb,paste("//home//mathieu//Documents//lol.xls",sep = ""))
        xlsx::saveWorkbook(wb3, list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # Write some basic parameters used for the analysis
        
        
        # tictoc::toc()
        
        
        # Concatenate data from all sheets into one single sheet
        paths <- list.files(folder, pattern = "(.*IsoCorrectoR_)(.*xls$)", recursive = TRUE, full.names = TRUE)
        info <- file.info(paths)
        sortedPaths <- rownames(info[with(info, order(as.POSIXct(mtime))), ])
        
        mergedRawData <- plyr::ldply(seq_along(sortedPaths), function(i) {read_excel(sortedPaths[i], "RawData")}) #  %>% do.call(rbind.data.frame,.)
        mergedCorrectedRawData <- plyr::ldply(seq_along(sortedPaths), function(i) {read_excel(sortedPaths[i], "Corrected")}) %>% do.call(rbind.data.frame,.)
        mergedRawDataFractions <- plyr::ldply(seq_along(sortedPaths), function(i) {read_excel(sortedPaths[i], "RawDataFractions")}) %>% do.call(rbind.data.frame,.)
        mergedCorrectedFractions <- plyr::ldply(seq_along(sortedPaths), function(i) {read_excel(sortedPaths[i], "CorrectedFractions")}) %>% do.call(rbind.data.frame,.)
        
        wb4 <- createWorkbook(type = "xlsx")
        mySheets <- c("RawData", "Corrected", "RawDataFractions", "CorrectedFractions")
        sapply(mySheets, FUN = function(x) createSheet(wb4, x))
        addDataFrame(mergedRawData, sheet = getSheets(wb4)$"RawData", row.names = FALSE)
        addDataFrame(mergedCorrectedRawData, sheet = getSheets(wb4)$"Corrected", row.names = FALSE)
        addDataFrame(mergedRawDataFractions, sheet = getSheets(wb4)$"RawDataFractions", row.names = FALSE)
        addDataFrame(mergedCorrectedFractions, sheet = getSheets(wb4)$"CorrectedFractions", row.names = FALSE)
        saveWorkbook(wb4, file = paste0(folder, "/mergedData.xlsx"))
      }
    }
    
  })
  #})
}
