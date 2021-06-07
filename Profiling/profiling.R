# Profiling script

# library(dvir)
# Get functions to make simple and motivating examples
# source("dvirMotivFunc.R")
library(profvis)
numRuns <- 1

# Libraries of different 'dvir' versions
dvirLibs <- c("/media/sf_Honours_project/VM_18.04/dvir0.2-1",
              "/media/sf_Honours_project/VM_18.04/dvir0.2-2",
              "/media/sf_Honours_project/VM_18.04/dvir0.3-1",
              "/media/sf_Honours_project/VM_18.04/dvir0.3-2")
# .libPaths(c(.libPaths(), 
#             dvirLibs)) 

# Where to store profiling results
simpleProfResults <- paste0("simple_", gsub("/.+/.+/.+/", "", dvirLibs))
yeeProfResults <- gsub("simple", "yee", simpleProfResults)

# Initialise 'dvir' info, for checking versions etc.
dvirPackageInfo <- list(NULL,
                        NULL,
                        NULL,
                        NULL)

simpleExample <- FALSE
# unload dvir if it is loaded
if ("dvir" %in% (.packages())) detach(package:dvir, unload=TRUE)

for (i in 1:1) {
  # Load specific version of 'dvir' from directory 
  library(dvir, 
          lib.loc = dvirLibs[i])
  # Store the package version loaded for checking
  dvirPackageInfo[[i]]$packageVersion <- packageVersion('dvir')
  
  # Warm up run (for package compiling etc.)
  source(if (simpleExample) "simpleExample.R" else "yeeExample.R")
  source(if (simpleExample) "simpleExample.R" else "yeeExample.R")
  source(if (simpleExample) "simpleExample.R" else "yeeExample.R")
  
  # Rprof(if (simpleExample) simpleProfResults[i] else yeeProfResults[i], 
  #       line.profiling = TRUE, 
  #       interval = 0.01)
  for (j in 1:numRuns) {
    p <- profvis(source(if (simpleExample) "simpleExample.R" else "yeeExample.R"))
    filename <- paste0(ifelse(simpleExample, simpleProfResults[i], yeeProfResults[i]), ".html")
    htmlwidgets::saveWidget(p, filename)
    # source(if (simpleExample) "simpleExample.R" else "yeeExample.R")
  }
  # Rprof(NULL)
  
  # Unload the package
  detach(package:dvir, unload=TRUE)
  # unlink(paste0(tempdir(), "/dvir"), recursive = TRUE)
}

# Summary info calculations are in R markdwon document itself!


# What to record (divided by number of runs):
# Total time 
# Total time in grid.latex() - as that's what really matters

# For first speed up:
# Time in metric_info.DVI() and grid_op.DVI() to emphasise decrease

# For second speed up:
# ... (the font def functions...? See code I changed as that should guide me!)
# Could try finind percentage difference in function durations between the 0.3-x versions, and see if max difference refers to font one?

# for each version of dvir: {
#   for the simple and Thomas Yee's example: {
#     identify which function(s) have highest oercentage of time spent in them
#       
#   }
# }

# Should profiling be for cairo_pdf, or on screen device?
# Test profiling doing 50 runs, or 10, to see if get similar average results (have timings converged/averaged out?). Or redo 20 runs and see if I get close to same values
# NOTE: for last speed up changes, may need to do this twice with initFonts srt to (TRUE | FALSE)
# Note my time interval is default (0.02 I think)
# Remember second change probably won't speed up simple example at all.
# As part of investigating the problem to begin with, should I calculate from first run where most time is being spent?

# What is Rprof() measuring - isn't it 1 for 1 time for humans? As it is based on the call stack? Is this okay? maybe just mention this in my notes - that it will depend on plenty of other things as well