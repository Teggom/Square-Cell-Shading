if(!("png"%in%installed.packages())){install.packages("png");library("png")}else{library("png")}
if(!("abind"%in%installed.packages())){install.packages("abind");library("abind")}else{library("abind")}

setwd("~")

Picture <- "1401320252636" #specify picture path here
draw_box <- function(TopLeftx, TopLefty, BotRightx, BotRighty, Edge=TRUE, Fill=FALSE, Fill_Color="#000000", Line_Color="#000000", Picture = .GlobalEnv$PNG){
  ToEdit <- Picture
  Fill_pixel <- c(strtoi(paste("0x", gsub("\\#|....$", "", Fill_Color), sep = "")),
                  strtoi(paste("0x", gsub("\\#..|..$", "", Fill_Color), sep = "")),
                  strtoi(paste("0x", gsub("\\#....", "", Fill_Color), sep = "")))
  Line_pixel <- c(strtoi(paste("0x", gsub("\\#|....$", "", Line_Color), sep = "")),
                  strtoi(paste("0x", gsub("\\#..|..$", "", Line_Color), sep = "")),
                  strtoi(paste("0x", gsub("\\#....", "", Line_Color), sep = "")))
  if(Fill){
    if(TopLeftx < 0 || TopLefty < 0 || BotRightx > ncol(Picture) || BotRighty > nrow(Picture)){
      stop("Dimensions Error")
    }
    Picture[TopLefty:BotRighty, TopLeftx:BotRightx,1] <- Fill_pixel[1]
    Picture[TopLefty:BotRighty, TopLeftx:BotRightx,2] <- Fill_pixel[2]
    Picture[TopLefty:BotRighty, TopLeftx:BotRightx,3] <- Fill_pixel[3]
  }
  if(Edge){
    if(TopLeftx < 0 || TopLefty < 0 || BotRightx > ncol(Picture) || BotRighty > nrow(Picture)){
      stop("Dimensions Error")
    }
    Picture[TopLefty:BotRighty, TopLeftx,1] <- Line_pixel[1]
    Picture[TopLefty:BotRighty, TopLeftx,2] <- Line_pixel[2]
    Picture[TopLefty:BotRighty, TopLeftx,3] <- Line_pixel[3]
    
    Picture[TopLefty:BotRighty, BotRightx,1] <- Line_pixel[1]
    Picture[TopLefty:BotRighty, BotRightx,2] <- Line_pixel[2]
    Picture[TopLefty:BotRighty, BotRightx,3] <- Line_pixel[3]
    
    Picture[TopLefty, TopLeftx:BotRightx,1] <- Line_pixel[1]
    Picture[TopLefty, TopLeftx:BotRightx,2] <- Line_pixel[2]
    Picture[TopLefty, TopLeftx:BotRightx,3] <- Line_pixel[3]
    
    Picture[BotRighty, TopLeftx:BotRightx,1] <- Line_pixel[1]
    Picture[BotRighty, TopLeftx:BotRightx,2] <- Line_pixel[2]
    Picture[BotRighty, TopLeftx:BotRightx,3] <- Line_pixel[3]
  }
  
  return(Picture)
}
CheckSlice <- function(Slice){
  #Slice <- draw_box(TopLeftx = 1, TopLefty = 1, BotRightx = ncol(Slice), BotRighty = nrow(Slice), Picture = Slice)
  #if(nrow(Slice)<8){
  #  return(Slice)
  #}
  SliceV <- Slice[2:(ncol(Slice)-1), 2:(nrow(Slice)-1),]
  SliceR <- SliceV[,,1]
  SliceG <- SliceV[,,2]
  SliceB <- SliceV[,,3]
  
  SliceRAverage <- sum(SliceR)/length(SliceR)
  SliceRShift <- SliceR-SliceRAverage
  SliceRAbs <- abs(SliceRShift)
  SliceRAbsAvg <- sum(SliceRAbs)/length(SliceRAbs)
  
  SliceGAverage <- sum(SliceG)/length(SliceG)
  SliceGShift <- SliceG-SliceGAverage
  SliceGAbs <- abs(SliceGShift)
  SliceGAbsAvg <- sum(SliceGAbs)/length(SliceGAbs)
  
  SliceBAverage <- sum(SliceB)/length(SliceB)
  SliceBShift <- SliceB-SliceBAverage
  SliceBAbs <- abs(SliceBShift)
  SliceBAbsAvg <- sum(SliceBAbs)/length(SliceBAbs)
  MaxSolVal <- .1*255
  MaxSumVal <- .2*255
  MaxDiffVal <- .3*255
  if(nrow(Slice)<8 || (SliceRAbsAvg <= MaxSolVal && SliceGAbsAvg <= MaxSolVal && SliceBAbsAvg <= MaxSolVal && SliceRAbsAvg+SliceGAbsAvg+SliceBAbsAvg <= MaxSumVal && max(SliceRAbs) <= MaxDiffVal && max(SliceGAbs) <= MaxDiffVal && max(SliceBAbs) <= MaxDiffVal)){
    #if(ceiling(255*SliceRAverage) > 255) {print(sprintf("%X", ceiling(255*SliceRAverage)))}
    #if(ceiling(255*SliceGAverage) > 255) {print(sprintf("%X", ceiling(255*SliceGAverage)))}
    #if(ceiling(255*SliceBAverage) > 255) {print(sprintf("%X", ceiling(255*SliceBAverage)))}
    Color <- paste("#", ifelse(length(strsplit(sprintf("%X", ceiling(SliceRAverage)), split = "")[[1]]) < 2, paste("0", sprintf("%X", ceiling(SliceRAverage)), sep = ""), sprintf("%X", ceiling(SliceRAverage))),
                   ifelse(length(strsplit(sprintf("%X", ceiling(SliceGAverage)), split = "")[[1]]) < 2, paste("0", sprintf("%X", ceiling(SliceGAverage)), sep = ""), sprintf("%X", ceiling(SliceGAverage))),
                   ifelse(length(strsplit(sprintf("%X", ceiling(SliceBAverage)), split = "")[[1]]) < 2, paste("0", sprintf("%X", ceiling(SliceBAverage)), sep = ""), sprintf("%X", ceiling(SliceBAverage))),
                   sep = "")
    
    #Happy <<- unique(c(Happy, Color))
    Slice <- draw_box(TopLeftx = 1, TopLefty = 1, BotRightx = ncol(Slice),BotRighty = nrow(Slice), Edge = F, Fill = T, Fill_Color = Color, Picture = Slice)
  } else {
    Slice[1:ceiling(ncol(Slice)/2), 1:ceiling(nrow(Slice)/2),] <- CheckSlice(Slice[1:ceiling(ncol(Slice)/2), 1:ceiling(nrow(Slice)/2),])
    Slice[1:ceiling(ncol(Slice)/2), ceiling(nrow(Slice)/2):nrow(Slice),] <- CheckSlice(Slice[1:ceiling(ncol(Slice)/2), ceiling(nrow(Slice)/2):nrow(Slice),])
    Slice[ceiling(ncol(Slice)/2):ncol(Slice), 1:ceiling(nrow(Slice)/2),] <- CheckSlice(Slice[ceiling(ncol(Slice)/2):ncol(Slice), 1:ceiling(nrow(Slice)/2),])
    Slice[ceiling(ncol(Slice)/2):ncol(Slice), ceiling(nrow(Slice)/2):nrow(Slice),] <- CheckSlice(Slice[ceiling(ncol(Slice)/2):ncol(Slice), ceiling(nrow(Slice)/2):nrow(Slice),])
  }
  return(Slice)
}

for(Picture in gsub("\\.png$", "", dir(), ignore.case = T)[!grepl("_Rec", dir())]){
  PNG <- ceiling(255*readPNG(paste(Picture, ".png", sep = ""))[,,1:3])
  print(paste("Working on", Picture, "Now!"))
  
  BASE_SIZE <- 256
  
  SpotsRow <- floor(nrow(PNG)/BASE_SIZE)
  SpotsCol <- floor(ncol(PNG)/BASE_SIZE)
  #for(Row in 1:SpotsRow){
  #  for(Col in 1:SpotsCol){
  #    PNG <- draw_box(TopLeftx = 1+(Col-1)*BASE_SIZE,
  #             TopLefty = 1+(Row-1)*BASE_SIZE,
  #             BotRightx = 1+Col*BASE_SIZE,
  #             BotRighty = 1+Row*BASE_SIZE)
  #    print(paste(round(((Row-1)*SpotsCol+Col)/(SpotsRow*SpotsCol), digits = 3)))
  #  }
  #}
  #PNG <- PNG[1:(BASE_SIZE*SpotsRow+1),
  #           1:(BASE_SIZE*SpotsCol+1),
  #           1:3]
  
  
  
  #writePNG(image = PNG, target = 'Box.png')
  
  
  if(floor((nrow(PNG))/BASE_SIZE) == nrow(PNG)/BASE_SIZE){
    PNGR <- PNG[,,1]
    PNGG <- PNG[,,2]
    PNGB <- PNG[,,3]
    PNGR <- rbind(PNGR, PNGR[,1])
    PNGG <- rbind(PNGG, PNGG[,1])
    PNGB <- rbind(PNGB, PNGB[,1])
    PNG1 <- abind(PNGR, abind(PNGG, PNGB, along = 3), along = 3)
    PNG <- PNG1
  }
  if(floor((ncol(PNG))/BASE_SIZE) == ncol(PNG)/BASE_SIZE){
    PNGR <- PNG[,,1]
    PNGG <- PNG[,,2]
    PNGB <- PNG[,,3]
    PNGR <- cbind(PNGR, PNGR[1,])
    PNGG <- cbind(PNGG, PNGG[1,])
    PNGB <- cbind(PNGB, PNGB[1,])
    PNG2 <- abind(PNGR, abind(PNGG, PNGB, along = 3), along = 3)
    PNG <- PNG2
  }
  SpotsRow <- floor((nrow(PNG)-1)/BASE_SIZE)
  SpotsCol <- floor((ncol(PNG)-1)/BASE_SIZE)
  PNG <- PNG[1:(BASE_SIZE*SpotsRow+1),
             1:(BASE_SIZE*SpotsCol+1),
             1:3]
  
  for(Row in 1:SpotsRow){
    for(Col in 1:SpotsCol){
      #PNG <- draw_box(TopLeftx = 1+(Col-1)*BASE_SIZE,
      #                TopLefty = 1+(Row-1)*BASE_SIZE,
      #                BotRightx = 1+Col*BASE_SIZE,
      #                BotRighty = 1+Row*BASE_SIZE)
      PNG[(1+(Row-1)*BASE_SIZE):(1+Row*BASE_SIZE), (1+(Col-1)*BASE_SIZE):(1+Col*BASE_SIZE),] <- CheckSlice(PNG[(1+(Row-1)*BASE_SIZE):(1+Row*BASE_SIZE), (1+(Col-1)*BASE_SIZE):(1+Col*BASE_SIZE),])
      print(paste(round(((Row-1)*SpotsCol+Col)/(SpotsRow*SpotsCol), digits = 3)))
    }
  }
  print(paste("Saving", Picture))
  PNG <- (1- abs(PNG - 255)/255)
  writePNG(image = PNG, target = paste(Picture, "_Rec.png", sep = ""))
}

#writePNG(image = PNG, target = 'Box.png')


#Slice <- PNG[(1+1*BASE_SIZE):(1+2*BASE_SIZE), (1+2*BASE_SIZE):(1+3*BASE_SIZE),]
#writePNG(image = Slice, target = "Slice.png")

#Slice <- CheckSlice(Slice = Slice)
#writePNG(image = Slice, target = "RecSlice_Attp3.png")

