ra <- 5
rb <- 4
distances <- seq(0, ra + rb + 1, by=0.01)

### numerical approach ###

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

filterPointsInCircle <- function( pts, c2x, c2y, c2r ){
	pic_select <- isPointInCircle(pts$x, pts$y, c2x, c2y, c2r)
	inX <- pts$x[pic_select]
	inY <- pts$y[pic_select]
	return(list(x=inX, y=inY))
}

Ao_shotgun <- function(ra, rb, d){
	n <- 1000
	xa <- 0
	ya <- 0
	xb <- d
	yb <- 0
	pb <- randomPointsInCircle(xb, yb, rb, n)	# points in B
	pba <- filterPointsInCircle( pb, xa, ya, ra )
	Ab <- pi * rb^2
	return( (length(pba$x)/length(pb$x))*Ab )
}

### analytical approach ###

Ao <- function(ra, rb, d){	# ra must be >= rb
	if ( ra + rb <= d ){
		return(0)
	}
	if ( d <= abs(ra - rb) ){
		smallR = min(ra, rb)
		return(pi * smallR^2)
	}
	area = ra^2 * acos( (d^2 - (rb^2 - ra^2))/(2*d*ra) ) + rb^2 * acos( (d^2 + (rb^2 - ra^2))/(2*d*rb) ) - d*sqrt(ra^2 - (d^2 + ra^2 - rb^2 )^2/(4*d^2))
	
	return(area)
}


i <- 1
areasAnalytical = real(length(distances))
areasNumerical = real(length(distances))
for (d in distances){
	areasAnalytical[i] = Ao(ra, rb, d)
	areasNumerical[i] = Ao_shotgun(ra, rb, d)
	i <- i + 1
}

plot(distances, areasNumerical, pch='.', ylab="area of overlap", xlab="distance between centers")

lines(distances, areasAnalytical, col="Blue")

