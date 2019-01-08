# hyperbolic paraboloid (a "saddle surface") example
require(lattice)

g <- expand.grid(x = -10:10, 
                 y = -10:10, 
                 gr = 1:2)

g$z <- g$x^2 - g$y^2
#https://www.getdatajoy.com/examples/python-plots/surface-plot-and-wireframe

#png(filename="/home/toasty/Desktop/name.png")
plot(
  wireframe(z ~ x * y, 
            xlab = "", #expression(mu), #"X", 
            ylab = "",#expression(alpha),
            zlab = "",#expression(alpha),
            data = g, 
            groups = gr,
            drape = TRUE, 
            colorkey = FALSE,
            screen = list(z = 30, x = -60),
            par.box = list(col=NA)#,
  #          par.settings = list(axis.line = list(col = "transparent"))
            )
  
)
#dev.off()
