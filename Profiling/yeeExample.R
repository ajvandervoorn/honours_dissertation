# file <- "diagram.pdf"
# cairo_pdf(file, width=7, height=3.8)
# dev.control("enable")
## Where the arrows end up
xpos <- c(0, 0.25,  0.7, 1)  
## Coordinate system 
myplot <- function(abcd = "(a)", col = "black") {
  plot(1:9, 1:9, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       bty = "n", axes = F, xlab = "", ylab = "")
  arrows(0.5, 1, xpos, 0, length = 0.12, lwd = .7,
         col = col)  # All the arrows
  text(0.05, y = 1.1, xpd = TRUE, labels = abcd, cex = 1.0,
       font = 1, col = col)
}  # myplot
par(mfrow=c(2, 2),
    mar = c(2.6, 4, 1.5, 2) + 0.1,
    font = 3,  # italic
    las = 1)
myplot()
## Convert to grid
library(gridGraphics)
grid.echo()
## Make arrows "nicer" ? 
grid.edit("arrows", grep=TRUE,
          arrow=arrow(angle=10, length=unit(.12, "in"), type="closed"),
          gp=gpar(fill="black"))
## Navigate to plot window
downViewport("graphics-window-1-1")
## Use 'dvir' to draw labels
library(dvir)
grid.latex("\\dots", x = 0.44, y = -0.1, default.units="native")
grid.latex("$Y_* =$",
           x = 0.5, y = 1.1, default.units="native")
# Bottom RHS:
grid.latex("$a_1$", xpos[1], y = -0.1, default.units="native")
grid.latex("$a_2$", xpos[2], y = -0.1, default.units="native")
grid.latex("$a_{L_A}$", xpos[3], y = -0.1, default.units="native")
grid.latex("$Y_{\\pi} | Y_{\\pi} \\notin \\cal{A}$",
           x = xpos[4], y = -0.1, default.units="native")
# Bottom row:
grid.latex("$\\omega_1$",
           x = 0.18, y = 0.50, default.units="native")
grid.latex("$\\omega_2$",
           x = 0.32, y = 0.50, default.units="native")
grid.latex("$\\dots$",
           x = 0.44, y = 0.50, default.units="native")
grid.latex("$\\omega_{L_A}$",
           x = 0.54, y = 0.50, default.units="native")
grid.latex("$1 - \\sum_{s=1}^{L_A}\\omega_s$",
           x = 0.95, y = 0.50, default.units="native")
# dev.off()