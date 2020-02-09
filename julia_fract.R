library(viridisLite)
library(viridis)
library(scico)

# Complex parameter, connected to coordinate of the Mandelbrot set in a complex plane
a <- -0.4
b <- -0.6
fixed_limits <- c(-2,2) 
max_iteration <- 1000 

#Colors
col_palettes <- c(scico::scico(250, palette= 'bilbao'),
                  viridis::plasma(400),
                  viridis::viridis(800),
                  viridis::inferno(500),
                  viridis::cividis(400),
                  viridis::magma(500))

#Steps
steps <- seq(fixed_limits[1], fixed_limits[2], by= 0.01)

#collects the points that have escaped
points_matrix <- array(0, dim= c(length(steps) * length(steps), 3))
a1 <- 0 #point Z0 starts with

for(x in steps) {
  for(y in steps) {
    n <- 0
    distance <- 0
    # Copy original x and y
    x1 <- x
    y1 <- y 
    
    while(n < max_iteration & distance<4) {
      newx <- x1^2 - y1^2 + a
      newy <- 2 * x1 * y1 + b
      distance <- newx^2 + newy^2
      x1 <- newx
      y1 <- newy
      n <- n+1
    }
    # Assign colors
    if(distance < 4){
      pick_color <- 24
    }
    else{
      pick_color <- n*10
    }
    #move Z0, and put points in the matrix/array
    a1 <- a1 + 1
    points_matrix[a1, ]=c(x,y, pick_color)  
    
  }
}

#Save the plot with attributes
png(file = 'Julia_unoptimized.png')
plot(x= points_matrix[ ,1], y= points_matrix[ ,2], xlim= fixed_limits, ylim= fixed_limits, 
     col= col_palettes[points_matrix[,3]], pch=".", xlab = "Re(z)", ylab = "lm(z)", sub = "c= -0.4-0.6i",
     main = "the Julia Set", col.axis= "violetred4", col.lab= "seagreen3", col.main= "palevioletred4", 
     fg= "lavender", font.main= 3, cex.main=1.5, cex.lab=1.25, cex.axis=0.75)
dev.off()
