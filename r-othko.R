# Pointillism meets Rothko


# center around zero
# cut width in halves, on both sides of the zero lines
# cut height in halves, on both sides
# make samplenum-th samples on width
# make samplenum-th samples on height
# pair up the widths and heights to form an xy-coordinate for the dots
# somehow, gradually reduce the borderly dots


rothkodistribution <- function(width, height, dotsperunit, smoothborder=F){

	# generate num of samples
	samplenum <- width * height * dotsperunit

	# cut width in halves, center on zero, 
	# then generate samples
	xwidth <- width/2
	x <- runif(samplenum, min=-xwidth, max=xwidth)

	# cut height in halves, center on zero,
	# then generate samples
	yheight <- height/2
	y <- runif(samplenum, min=-yheight, max=yheight)


	if(smoothborder==T){

		# manual way to subset min-max 5-percentile
		# to create "smoothing" at border

		xwidth95 <- xwidth*0.05
		x95 <- subset(x, x < (-xwidth+xwidth95) | x > (xwidth-xwidth95))
		xToRemove <- sample(x95, round(samplenum*0.05*0.5))
		x <- x[!x %in% xToRemove]

		yheight95 <- yheight*0.05
		y95 <- subset(y, y < (-yheight+yheight95) | y > (yheight-yheight95))
		yToRemove <- sample(y95, round(samplenum*0.05*0.5))
		y <- y[!y %in% yToRemove]
	}

	coordinates <- as.data.frame(cbind(x,y))

	return(coordinates)
}




# to plot
# put in x1, x2, y1, y2
# 	find the x- and y- centers
#	create a rothkodistribution that uses x- and y-centers




# background
rothkoplot1 <- function(x1, x2, y1, y2, dotsperunit, smoothborder=F, r=0, g=0, b=0, a=255){

	xwidth = max(x1, x2) - min(x1, x2)
	ywidth = max(y1, y2) - min(y1, y2)

	xcenter = (xwidth / 2) + min(x1, x2)
	ycenter = (ywidth / 2) + min(y1, y2)

	dots <- rothkodistribution(xwidth, ywidth, dotsperunit, smoothborder)	
	dots$x <- dots$x + xcenter
	dots$y <- dots$y + ycenter
	plot(dots$x, dots$y, axes=F, ann=F, pch=19, 
		col = rgb(red = r, green = g, blue = b, alpha = a, maxColorValue=255))
	
}


# on top
rothkoplot2 <- function(x1, x2, y1, y2, dotsperunit, smoothborder=F, r=0, g=0, b=0, a=255){

	xwidth = max(x1, x2) - min(x1, x2)
	ywidth = max(y1, y2) - min(y1, y2)

	xcenter = (xwidth / 2) + min(x1, x2)
	ycenter = (ywidth / 2) + min(y1, y2)

	dots <- rothkodistribution(xwidth, ywidth, dotsperunit, smoothborder)	
	dots$x <- dots$x + xcenter
	dots$y <- dots$y + ycenter
	points(dots$x, dots$y, pch=19, 
			col = rgb(red = r, green = g, blue = b, alpha = a, maxColorValue=255))
}






############
# Examples #
############

dir.create("examples")

# no 61 rust and blue
# https://en.wikipedia.org/wiki/No._61_(Rust_and_Blue)
png(file="examples/rust-and-blue.png", width=450, height=600)
rothkoplot1(0, 9, 0, 12, dotsperunit=250, r=46, g=53, b=123, a=200)
rothkoplot2(0.5, 8.5, 7, 11.5, smoothborder=T, dotsperunit=180, r=55, g=13, b=28, a=100)
rothkoplot2(0.5, 8.5, 4.5, 7, smoothborder=T, dotsperunit=180, r=57, g=90, b=182, a=100)
rothkoplot2(0.5, 8.5, 0.5, 4.5, smoothborder=T, dotsperunit=150, r=58, g=48, b=84, a=100)
dev.off()



# orange, red, yellow
# https://en.wikipedia.org/wiki/Orange,_Red,_Yellow
png(file="examples/orange-red-yellow.png", width=400, height=450)
rothkoplot1(0, 8, 0, 9, dotsperunit=450, r=171, g=123, b=104, a=180)

rothkoplot2(0.55, 7.45, 8, 8.75, smoothborder=T, dotsperunit=350, r=124, g=67, b=62, a=100)
rothkoplot2(0.85, 7.15, 8.1, 8.65, smoothborder=T, dotsperunit=200, r=148, g=113, b=6, a=75)

rothkoplot2(0.5, 7.5, 5.5, 7.75, smoothborder=T, dotsperunit=300, r=164, g=71, b=42, a=100)
rothkoplot2(0.7, 7.3, 5.7, 7.55, smoothborder=T, dotsperunit=240, r=196, g=103, b=37, a=100)

rothkoplot2(0.5, 7.5, 0.25, 5.25, smoothborder=T, dotsperunit=300, r=164, g=71, b=42, a=100)
rothkoplot2(0.7, 7.3, 0.45, 5.05, smoothborder=T, dotsperunit=300, r=198, g=86, b=35, a=100)
dev.off()



# black in deep red
# https://en.wikipedia.org/wiki/Black_in_Deep_Red
png(file="examples/black-in-deep-red.png", width=400, height=550)
rothkoplot1(0, 5, 0, 11, dotsperunit=500, r=140, g=62, b=53, a=150)

rothkoplot2(0.25, 4.75, 8, 10, smoothborder=T, dotsperunit=600, r=7, g=8, b=10, a=80)
rothkoplot2(0.2, 4.8, 3, 7.75, smoothborder=T, dotsperunit=600, r=7, g=8, b=10, a=80)

rothkoplot2(0.15, 4.87, 0.7, 2.75, smoothborder=T, dotsperunit=600, r=160, g=3, b=28, a=40)
rothkoplot2(0.23, 4.70, 0.8, 2.6, smoothborder=T, dotsperunit=600, r=89, g=17, b=30, a=60)
dev.off()


