####
#### Wrapper function
####

baselines <- function(tex = "$x - \\mu$", 
                      fileName = "baselineAlgorithm", 
                      algorithm = "dviMoves",
                      dviMovesMethod = "index",
                      dviMovesSelection = 1,
                      x = 0.5,
                      y = 0.5,
                      displayOption = "single text",
                      addNullBaseline = FALSE,
                      useTikz = FALSE) {
  
  # If y not a 'unit', assume it is 'npc'
  if (!(class(y) == "unit")) {
    y <- unit(y, "npc")
  }
  
  # Check if algorithm is supported
  if (!(algorithm %in% c("alex",
                         "dviMoves",
                         "preview",
                         "dvipng",
                         "none"))) {
    stop(paste("Baseline algorithm '", algorithm, "' not supported", sep = ""))
  }
  
  # Create and process TeX file
  createTexFile(tex, fileName, algorithm, useTikz)
  processTexFile(fileName) 
  
  # Calculate baseline value(s) depending on algorithm, return as unit vector
  v <- switch(algorithm,
              "alex" = algorithmAlex(fileName),
              "dviMoves" = algorithmDviMoves(fileName, 
                                             dviMovesMethod,
                                             dviMovesSelection),
              "preview" = algorithmPreview(fileName),
              "dvipng" = algorithmDviPng(fileName),
              "none" = unit(0, "npc"))
  
  # Option for adding '0' baseline at start of other baseline(s) for reference
  if (addNullBaseline) {
    v <- unit.c(unit(0, "npc"), v)
  }
  
  # Return baseline(s)
  v
  
  ## Display result of baseline value(s) on graphics device

  # 
  # if (displayOption == "single line") {
  #   x <- seq(0, 1, length.out = length(v) * 2 + 1)[1:length(v) * 2]
  #   for (i in seq_along(v)) {
  #     dvir::grid.dvi(paste0(fileName, ".dvi"), x = x[i], y = y - v[i], just = "bottom")
  #   }
  #   grid.segments(x0 = 0,
  #                 x1 = 1, 
  #                 y0 = y, 
  #                 y1 = y, 
  #                 gp = gpar(col = "red", size = 0.25, alpha = 0.25))
  #   
  # } else if (displayOption == "single text") {
  #   dvir::grid.dvi(paste0(fileName, ".dvi"), x = x, y = y, just = "bottom")
  #   grid.segments(x0 = 0, 
  #                 x1 = 1, 
  #                 y0 = y + v, 
  #                 y1 = y + v, 
  #                 gp = gpar(col = "red", size = 0.25, alpha = 0.25))
  }
}

####
#### Algorithm functions
####

algorithmAlex <- function(fileName) {
  
  # Use 'alex' algorithm to determine baseline, and return as unit vector
  
  x <- readDVIFile(paste0(fileName, ".dvi"))
  boundBoxOp <- FALSE
  downCount <- 0
  
  # Initialise the baseline height as 0, in case no "up" move before first character
  baselineHeight <- 0
  # *only* looking for 'down_' op codes
  downOpCodes <- 157:160
  
  for (i in seq_along(x)) {
    # Look for 'HiResBoundingBox statement
    if (boundBoxDefinition(x[[i]])) boundBoxOp <- TRUE
    if (boundBoxOp && isOpCode(x[[i]], downOpCodes) && downCount < 2) {
      # Second 'down' value after 'HiResBoundingBox' statement is distance
      # from bottom of bounding box to baseline in scaled points (negative value)
      
      # If baseline is bottom of bounding box, then there may not be a second 'down'
      # in which case we use initialised value of 0 
      if (downCount == 1) baselineHeight <- getOpParams(x[[i]])
      downCount <- downCount + 1
    }
  }
  
  # 'baselineHeight' should be negative value (or 0) as it is *upward* move
  # from bottom of bounding box so negate that value (making it positive)
  baselineValues <- -baselineHeight
  baselineUnits <- "scaledpts"
  
  # Return distance from bottom of bounding box to the 'baseline'
  unit(baselineValues, baselineUnits)
}

algorithmDviMoves <- function(fileName,
                              dviMovesMethod,
                              dviMovesSelection){
    
  # Use 'dviMoves' algorithm to determine baseline(s), and return as unit vector
  
  x <- readDVIFile(paste0(fileName, ".dvi"))
  # op codes for 'push', 'pop', 'down_', 'y_', and 'z_'
  downOpCodes <- c(141:142, 157:170)
  setCharOpCodes <- 0:127
  allDownValues <- numeric(0)
  allDownOpCodes <- numeric(0)
  allDownNextSetChar <- character(0)
  boundBoxOp <- FALSE
  recordPosition <- FALSE
  recordNextSetChar <- FALSE
  pushes <- 0
  pushDown <- numeric(0)
  pushY <- numeric(0)
  pushZ <- numeric(0)
  y <- numeric(0)
  z <- numeric(0)
  
  for (i in seq_along(x)) {
    # Need to keep track of any saved y, z, and pushes and pops, so that after
    # 'HiResBoundingBox' statement we can store all actual movements in bounding
    # box but still have access to any values stored from anywhere else in DVI
    # file
    if (boundBoxDefinition(x[[i]])) boundBoxOp <- TRUE
    if (isOpCode(x[[i]], c(downOpCodes, setCharOpCodes))) {
      if (isOpCode(x[[i]], 157:160)) {
        # down1 to down4
        down <- getOpParams(x[[i]])
        recordPosition <- TRUE
      }
      else if (isOpCode(x[[i]], 162:165)) {
        # y1 to y4 (save y and move down)
        down <- getOpParams(x[[i]])
        y <- down
        recordPosition <- TRUE
      }
      else if (isOpCode(x[[i]], 167:170)) {
        # x1 to x4 (save x and move down)
        down <- getOpParams(x[[i]])
        z <- down
        recordPosition <- TRUE
      }
      else if (isOpCode(x[[i]], 161)) {
        # y0 (use saved y)
        down <- y
        recordPosition <- TRUE
      }
      else if (isOpCode(x[[i]], 166)) {
        # z0 (use saved z)
        down <- z
        recordPosition <- TRUE
      }
      else if (isOpCode(x[[i]], 141)) {
        # push - need to save current position
        # in absolute terms for 'pop' later
        pushes <- pushes + 1
        pushDown[pushes] <- sum(allDownValues)
        pushY[pushes] <- y
        pushZ[pushes] <- z
      }
      else if (isOpCode(x[[i]], 142)) {
        # pop - change absolute value 'push' to
        # make relative to current position (just
        # before pop)
        down <- pushDown[pushes] - sum(allDownValues)
        y <- pushY[pushes]
        z <- pushZ[pushes]
        pushes <- pushes - 1
        recordPosition <- TRUE
      }
      
      if (isOpCode(x[[i]], setCharOpCodes)) {
        setChar <- getOpCode(x[[i]])
        if (recordNextSetChar) {
          allDownNextSetChar <- c(allDownNextSetChar, setChar)
          recordNextSetChar <- FALSE
        }
      }
      
      if (recordPosition && boundBoxOp) {
        if (recordNextSetChar) {
          # Haven't met a 'set_char_xxx' before next 'down' move
          allDownNextSetChar <- c(allDownNextSetChar, "")
        }
        allDownValues <- c(allDownValues, down)
        # 'allDownOpCodes' only needed if we want to know what the op code
        # was for all the baselines
        allDownOpCodes <- c(allDownOpCodes, getOpCode(x[[i]]))
        # Need to record next 'set_char_xxx' op value
        recordNextSetChar <- TRUE
      }
    }
    recordPosition <- FALSE
  }
  
  # Last 'down' move(s) may not have a 'set_char_xxx' after it,
  # in which case label with empty string. Assumes...
  # length(allDownValues) >= length
  lengthDiff <- length(allDownValues) - length(allDownNextSetChar)
  
  if (lengthDiff > 0) allDownNextSetChar <- c(allDownNextSetChar, rep("", lengthDiff))
  
  # Cumulative sum of up and down movements gives distance from *top* of bounding box
  # Subtract from height of bounding box to get distance from *bottom* of bounding
  # box to baseline
  # (assumes first down after 'HiResBoundingBox' statement is total height of bounding 
  # box)
  
  baselineValues <- allDownValues[1] - cumsum(allDownValues)
  
  # As we now have labels for each 'baseline', do not take only unique baseline values
  # baselineValues <- unique(allDownValues[1] - cumsum(allDownValues))
  baselineUnits <- "scaledpts"
  
  baselineReturn <- dviMovesBaselineChoice(baselineValues, 
                                           dviMovesMethod, 
                                           dviMovesSelection,
                                           allDownNextSetChar)
  
  # Return distance from bottom of bounding box to 'baseline'
  unit(baselineReturn, baselineUnits)
}

algorithmPreview <- function(fileName) {

  # Use 'preview' algorithm to determine baseline, and return as unit vector 

  # Is it okay to assume baseline height unit is always points?
  
  logFile <- readLines(paste0(fileName, ".log"))
  # Should this regular expression be more strict (e.g. ensure 
  # there are numbers and a unit?)
  baselineLines <- grep("MatplotlibBox:.+", logFile)
  values <- strsplit(gsub(".*Box:\\(|pt\\).*", "", logFile[baselineLines]),
                     "pt\\+")
  
  # If multiple baseline values, each is the second element of each
  # list component
  baselineValues <- sapply(values, function(x) x[2])
  baselineUnits <- "points"
  unit(baselineValues, baselineUnits)
}

algorithmDviPng <- function(fileName) {
  
  # Use 'dvipng' algorithm to determine baseline(s), and return as unit vector 
  
  systemOut <- system(paste0("dvipng --depth ", fileName, ".dvi"), 
                      intern = TRUE)
  depthLines <- grep("depth=.+", systemOut)
    
  # Is this gsub() stuff comprehensive/robust enough?
  depth <- gsub(" .+", "", gsub(".+depth=", 
                                "", 
                                systemOut[depthLines]))
  
  # Pixels to inches conversion
  baselineValues <- as.numeric(depth) / 96
  baselineUnits <- "inches"
  unit(baselineValues, baselineUnits)
}

####
#### Helper functions
####

dviMovesBaselineChoice <- function(baselineValues, 
                                   dviMovesMethod, 
                                   dviMovesSelection,
                                   allDownNextSetChar) {
  
  # 'dviMovesMethod' possibilities
  ### "all"
  ### "index" - numeric index of baseline
  ### "bottomUp" - like numeric index, but baselines are ordered from bottom to top (smallest offset first)
  ### "nextChar" - first character(s) after baseline move
  ### "prevChars' - last character(s) after baseline move
  ### "bestGuess" - to be renamed related to the algorithm chosen as the best choice
  
  # 'dviMovesSelection' is selection related to above method
  
  # Need logical checks to ensure correct methods and selections are specified. 
  # What to do if they aren't correct
  # Return error? Return first baseline with a warning? return all baselines?
  # also need to account for if index is out of bounds of length of baselines
  # or if 
  if (dviMovesMethod == "all") {
    
    baselineValues
    
  } else if (dviMovesMethod == "index") {
    
    if (!is.numeric(dviMovesSelection) || length(dviMovesSelection) != 1) 
      stop("baseline selection must be numeric and of length 1 with baseline 
           selection method 'index'")
    baselineValues[dviMovesSelection]
    
  } else if (dviMovesMethod == "bottomUp") {
    
    if (!is.numeric(dviMovesSelection) || length(dviMovesSelection) != 1) 
      stop("baseline selection must be numeric and of length 1 with baseline 
           selection method 'index'")
    baselineValues[order(baselineValues)][dviMovesSelection]
    
  } else if (dviMovesMethod == "nextChar") {
    
    if (!is.character(dviMovesSelection) || length(dviMovesSelection) != 1) 
      stop("baseline selection must be character and of length 1 with baseline 
           selection method 'nextChar'")
    selection <- which(allDownNextSetChar == dviMovesSelection)[1]
    if (!is.finite(selection) || length(selection) != 1)
      stop(paste0("baseline selection '", dviMovesSelection, "' not found"))
    # Selects first match
    baselineValues[selection]
    
  } else {
    
    stop("baseline selection method not supported")
    
  }
}

boundBoxDefinition <- function(x) {
  # Logical test for if block is opcode 239 and is HiResBoundingBox definition
  boundBoxRegExp <- "HiResBoundingBox"
  hexView:::blockValue(x$blocks$op.opcode) == 239 && 
    grepl(boundBoxRegExp,
          paste(hexView:::blockValue(x$blocks$op.opparams.string), collapse=""))
}

createTexFile <- function(tex, fileName, algorithm, useTikz) {
  
  # If using 'preview' algorithm, need to add 'preview' environment
  # around out TeX code for it to work. Need same for 'dvipng' algorithm
  if (isAlgorithm(algorithm, c("preview", "dvipng"))) 
      tex <- paste("\\begin{preview}", tex, "\\end{preview}", sep = "\n")
  
  header <- createTexHeader(algorithm, useTikz)
  footer <- createTexFooter()
  
  texLines <- c(header, tex, footer)
  writeLines(texLines, con = paste0(fileName, ".tex"))
}

createTexHeader <- function(algorithm, useTikz) {
    
  # If using 'preview' algorithm, need to add 'preview' package info 
  # in header of TeX file. Need same for 'dvipng' too
  previewPackageText <- paste("\\usepackage[active,showbox,tightpage]{preview}",
                              "\\def\\showbox#1%%",
                              "{\\immediate\\write16{MatplotlibBox:(\\the\\ht#1+\\the\\dp#1)x\\the\\wd#1}}\n",
                              sep = "\n")
  tikzPackageText <- "\\usepackage{tikz}\n"
  
  paste(c("\\documentclass{standalone}\n",
          ifelse(useTikz,
                 tikzPackageText,
                 ""),
          ifelse(isAlgorithm(algorithm, c("preview", "dvipng")), 
                 previewPackageText, 
                 ""),
          "\\begin{document}"),
          collapse = "")
}

createTexFooter <- function() {
  "\\end{document}"
}

processTexFile <- function(fileName) {
    
  # dvipng will need different function here, to return 
  # system 'output' of running this command (that's why algorithm
  # is function argument). Might be best to do this outside function?
  # or just make the object for algorithms and then object is only used
  # for that algorithm (and ignored for all others)?

  # should it be 'pdflatex' or 'pdftex'?
  
  system(paste0("latex ", fileName, ".tex"))
}

readDVIFile <- function(fileName) {
  dvir::readDVI(fileName)
}

isOpCode <- function(x, opCodes) {
  hexView::blockValue(x$blocks$op.opcode) %in% opCodes
} 

getOpParams <- function(x) {
  hexView::blockValue(x$blocks$op.opparams)
}

getOpCode <- function(x) {
  hexView::blockValue(x$blocks$op.opcode)
}

isAlgorithm <- function(algorithmSelected, algorithm) {
  algorithmSelected %in% algorithm
}


