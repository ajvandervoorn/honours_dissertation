source("algorithms.R")
library(dvir)
testText <- c("test",       # Text with no danglies
              "testing",    # Text with danglies
              "var",        # Short text with no danglies
              "varying",    # Short text with danglies
              "$\\sum_{n=1}^{\\infty} 2^{-n} = 1$",             # Equation only (inline)
              "$\\sum\\limits_{n=1}^{\\infty} 2^{-n} = 1$",     # Equation only (inline, but like 'displaymath')
              "$\\sum\\limits_{n=1}^{} 2^{-n} = 1$",            # Equation only (inline, but like 'displaymath' and without upper summation limit)
              "The equation is $x + \\frac{\\mu^2}{2}$",        # Text then equation
              "The equation is $x + \\frac{\\mu}{2}$",          # Text then equation (with lower equation height)
              "$\\frac{\\mu^2}{2} + x$ is the equation",        # Equation then text (with fraction first)
              "$x + \\frac{\\mu}{2}$ is the equation",          # Equation then text (with lower equation height, and fraction first)
              "\\begin{minipage}{1in}Paragraph, with some line wrapping!\\end{minipage}")  # Multiline text/paragraph

algo <- "dviMoves"
filename <- "baselineAlgorithms"

####
#### Selecting a 'dviMoves' baseline selection method:
#### (This example sets baseline to the 'i' in example 11)
####
x <- 0.5
y <- 0.5
i <- 11

grid.newpage()
baselines(testText[i], 
          fileName = filename, 
          algorithm = algo,
          dviMovesMethod = "nextChar",
          dviMovesSelection = "105",
          x = x,
          y = y,
          displayOption = "single text",
          addNullBaseline = FALSE,
          useTikz = FALSE)


####
#### to create graphic of all methods of baselines to create PDFs
####

algo <- rep(c("alex", "dviMoves", "preview", "dvipng"), 2)
TikZ <- rep(c(TRUE, FALSE), each = 4)
filenames <- paste0(algo, rep(c("WithTikZ", "WithoutTikZ"), each = 4))
y <- seq(1, 0, length.out = length(testText) + 2)[-c(1, length(testText) + 2)]
x <- 0.5

for (i in seq_along(filenames)) {
  cairo_pdf(paste0(filenames[i], ".pdf"))
  for (j in seq_along(testText)) {
    baselines(testText[j], 
              fileName = filenames[i], 
              algorithm = algo[i],
              dviMovesMethod = "all",
              dviMovesSelection = 1,
              x = x,
              y = y[j],
              displayOption = "single text",
              addNullBaseline = TRUE,
              useTikz = TikZ[i])
  }
  dev.off()
}



####
#### To print all examples above with a given algorithm:
####

y <- seq(1, 0, length.out = length(testText) + 2)[-c(1, length(testText) + 2)]
x <- 0.5

grid.newpage()
for (i in seq_along(testText)) {
  baselines(testText[i], 
            fileName = filename, 
            algorithm = NULL,
            dviMovesMethod = "all",
            dviMovesSelection = 1,
            x = x,
            y = y[i],
            displayOption = "single text",
            addNullBaseline = TRUE,
            useTikz = FALSE)
}


####
#### To print to PDF:
####

cairo_pdf("baselines.pdf")
grid.newpage()
for (i in seq_along(testText)) {
  baselines(testText[i], 
            fileName = filename, 
            algorithm = algo,
            dviMovesMethod = NULL,
            dviMovesSelection = 1,
            x = x,
            y = y[i],
            displayOption = "single text",
            addNullBaseline = TRUE,
            useTikz = FALSE)
}
dev.off()


