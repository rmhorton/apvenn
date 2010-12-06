ANGLES <- (1:360) * (2 * pi / 360)	# parts of the circle to plot, in radians

drawCircle <- function(x, y, r, ...){
	polygon(x + r*sin(ANGLES), y + r * cos (ANGLES), ...)
}

randomPointsInCircle <- function(x, y, r, n){
	# generate n random points within the circle centered at (x,y) with radius r
	# vector version
	randTheta <- 2 * pi * runif(n)	# random angle in radians
	randR <- r * sqrt(runif(n))
	randX <- x + randR * sin(randTheta)
	randY <- y + randR * cos(randTheta)
	return(list(x=randX, y=randY))
}

################
plot(-10:50, -10:50, type="n")
drawCircle(20, 20, 15, col=rgb(0,0,1,1.0), density=c(10,20), angle=c(-45,45))
drawCircle(10, 20, 15, col=rgb(0,1,0,1.0), density=c(10,20), angle=c(45,-45))
drawCircle(10, 10, 10, col=rgb(1,0,0,0.5))

points(randomPointsInCircle(10, 10, 10, 10000), pch='.', col="red")

px <- 20 +c(10, 12, 13, 15, 16, 17, 15, 14, 13, 12, 10)
py <- 10 + c(10, 13, 24, 25, 26, 15, 14, 14, 12, 11, 10)
polygon(px, py)

boundingBox <- function(xv, yv){
	minX <- min(xv)
	maxX <- max(xv)
	minY <- min(yv)
	maxY <- max(yv)
	return ( list(x=c(minX, maxX, maxX, minX), y=c(maxY, maxY, minY, minY) ))
}

bb <- boundingBox(px, py)
polygon(bb, col=rgb(0,1,0,0.3))

# The area of a polygon is the area of its bounding box times the fraction of the bounding box that lies within the polygon.
# Generating a uniform distribution of random points within the rectangular bounding box is straightforward.
# This can be easily approximated using Monte Carlo methods, assuming we have a good test for whether a point is within a polygon.

