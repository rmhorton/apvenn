# example program to plot approximate proportional Venn diagram for two sets

# Copyright Robert M. Horton, 2010

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details (see 
#    http://www.gnu.org/licenses)

# set operations from base package:
# union(x, y), intersect(x, y), setdiff(x, y), setequal(x, y), is.element(el, set)

DIMENSION <- 1000 	# length of each side of graph area
TOTAL_SET_AREA <- (DIMENSION^2)/3	# using half the total area makes some circles go out of bounds. A third is good.	SCORE_TOLERANCE <- 0.001
SCORE_TOLERANCE <- 0.001
MAX_SCORE <- 1/SCORE_TOLERANCE^2

drawCircle <- function(x, y, r, ...){
	ANGLES <- (1:360) * (2 * pi / 360)
	polygon(x + r*sin(ANGLES), y + r*cos(ANGLES), ...)
}

apvenn2 <- function(setA, setB, sNames){
	SET_COLORS <- c( rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(1,1,0,0.5))
	Y_LEVEL = DIMENSION/2
	nA <- length(setA)
	nB <- length(setB)
	nTot <- nA + nB
	
	nBoth <- length( intersect(setA, setB) )
	
	areaA <- TOTAL_SET_AREA * nA/nTot
	areaB <- TOTAL_SET_AREA * nB/nTot
	rA <- sqrt (areaA / pi)
	rB <- sqrt (areaB / pi)
	
	# start with the two groups far apart, then inch them together
	STEP <- 1	#2
	numSteps <- 0
	xA <- 0 #+ rA
	xB <- DIMENSION #- rB
	newScore <- 0
	oldScore <- 0
	for (i in 1:(xB - xA)/2 ){
		newScore <- scoreCircles(xA, Y_LEVEL, rA, xB, Y_LEVEL, rB, nA, nB, nBoth)
		if ( (newScore - oldScore) < -0.5 ) break;
		oldScore <- newScore
		xA <- xA + STEP
		xB <- xB - STEP
		if (newScore == MAX_SCORE) break;
		numSteps <- numSteps + 1
	}
	
	cat("numSteps=", numSteps, ", oldScore=", oldScore, ", newScore=", newScore,"\n")
	
#	xA <- 20
#	xB <- 30

	drawCircle( xA, Y_LEVEL, rA, col=SET_COLORS[[1]] )
	text( xA, Y_LEVEL, sNames[[1]], col=SET_COLORS[[1]] )
	drawCircle( xB, Y_LEVEL, rB, col=SET_COLORS[[2]] )
	text( xB, Y_LEVEL, sNames[[2]], col=SET_COLORS[[2]] )
	
	return( c(xA, Y_LEVEL, rA, xB, Y_LEVEL, rB, nA, nB, nBoth) )
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

isPointInCircle <- function(px, py, cx, cy, cr){
	return ( cr^2 > (px - cx)^2 + (py - cy)^2 )
}

# isPointInCircle(rpic$x, rpic$y, 30, 30, 10)
filterPointsInCircle <- function( pts, c2x, c2y, c2r ){
	pic_select <- isPointInCircle(pts$x, pts$y, c2x, c2y, c2r)
	inX <- pts$x[pic_select]
	inY <- pts$y[pic_select]
	return(list(x=inX, y=inY))
}

scoreCircles <- function(x1, y1, r1, x2, y2, r2, n1, n2, nOverlap){
	# score reflects how closely overlap of circle areas reflects 
	# overlap of number of set elements
	N_SPOTS <- 20000
	rpic1 <- randomPointsInCircle(x1, y1, r1, N_SPOTS)
	rpic1c2 <- filterPointsInCircle(rpic1, x2, y2, r2)
	expectedRatio <- nOverlap/n1
	observedRatio <- length(rpic1c2$x)/length(rpic1$x)
	score <- ifelse( abs(expectedRatio - observedRatio) < SCORE_TOLERANCE, MAX_SCORE, 1/((expectedRatio - observedRatio)^2))
cat("score=",score,"\n")
	return ( score )
}
###########################

plot(0:1000, 0:1000, type="n")

setLabels <- c("One", "Two")

set1 <- LETTERS[1:10]	# ("A", "B", "C", ...)
set2 <- LETTERS[5:26]	# ("J", "K", "L", ... , "Z")
vennParams <- apvenn2(set1, set2, setLabels)

scoreCircles(vennParams[[1]], vennParams[[2]], vennParams[[3]], 
	vennParams[[4]], vennParams[[5]], vennParams[[6]], 
	vennParams[[7]], vennParams[[8]], vennParams[[9]])
