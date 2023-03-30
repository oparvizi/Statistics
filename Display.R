# setwd("D:/CODES/R")

# Grafics-----------------------------------------------------------------------
# Color spaces and color palettes
# Metaphorical scales versus optophysical scales
layout (matrix (c(1,3,2,3),2,2))
attach (iris); K <- length(levels(Species))
pie (x=table (Species), labels=levels (Species),
     col=rainbow (K))
plot (x=Sepal.Length*Sepal.Width, y=Petal.Length*Petal.Width,
      col=rainbow(K)[Species], pch=19)
barplot (as.matrix (aggregate(iris[-5],iris[+5],mean)[-1]),
         col=rainbow(K))

# Display of 2D value curves
# Parameter type - Funktionen versus Relationen
layout (matrix
        (1:12,3,4,byrow=TRUE))
x <- seq (-3, +3, by=0.5)
y <- (x-2) * x * (x+2)
for (ty in unlist(
  strsplit(
    "plhsbocn","")))
  plot (x, y,
        main=sub
        ("@",ty,"type=’@’"),
        type=ty, lwd=3, cex=2,
        col="blue")
for (ty in unlist(
  strsplit(
    "plhs","")))
  plot (y, x,
        main=sub
        ("@",ty,"type=’@’"),
        type=ty, lwd=3, cex=2,
        col="magenta")


# Text inscription and figure legend
# legend (x, y=NULL, legend, fill=NULL, col, lty, lwd, pch, bg, )
x <- seq (-3,+3,0.1)
txt <- c("Mulde","Hügel","Welle")
Y <- list (x^2, -x^2, cos(x)/2)
clr <- 1:3+1
typ <- 1:3
wid <- 1:3*2
cha <- 1:3+16
plot (x, sin(x),
      type="n", xlab="", ylab="")
for (i in seq(along=Y))
  lines (x, Y[[i]],
         col=clr[i], lty=typ[i], lwd=wid[i])
par (cex=1.4)
legend ("topleft", legend=txt, fill=clr)
legend ("topright", legend=txt, lty=typ)
legend ("bottomright", legend=txt, lwd=wid)
legend ("bottomleft", legend=txt,
        lwd=2, lty=typ, col=clr)
legend ("right", legend=txt,
        pch=cha, bg=gray(.9))
legend ("left", legend=txt,
        pch=cha, col=clr, lwd=1, bg=gray(1))
for (pos in c("top","bottom","center"))
  legend (pos, legend=pos)


# Funktionen 
# text (x, y=NULL, labels, pos, cex, )
# mtext (text, side=3, cex, col, )
z <- exp (2i*pi/12 * 1:12)
plot (Re(z), Im(z),
      pch=23, cex=4,
      col="blue", bg="yellow")
text (z,
      labels=(12:1+1)%%12+1, cex=1.5)
text (x=0, y=1:8/10,
      labels=1:8, col=palette())
text (x=0, y=1:8/10,
      labels=palette(), pos=4)
points (x=-1/3, y=-1/3,
        pch=19, cex=5, col="cyan")
text (x=-1/3, y=-1/3,
      labels=1:4, pos=1:4, cex=3)
text (x=-1/3, y=-1/3,
      labels=0, cex=16)
mtext (
  text=paste ("side",1:4,sep="="),
  side=1:4, col="blue", cex=2)

# Formulation variants for 2D coordinates
plot (sqrt(0:30), type="b", col=2)
lines (cbind (runif(12,0,30), runif(12,0,6)), col=3)
points (iris, col=4)
text (list (x=cars$speed, y=log(cars$dist)),
      labels=rownames(cars), col=5)
legend ("topleft", fill=2:5, legend=c
        ("vector","matrix","data.frame","list"))


## Univariate and bivariate representations-------------------------------------

# Histograms — event counts in drawers
# hist (x, breaks, freq, density, angle, x/ylim, axes, plot, labels, ...)
require (lattice)
require (HSAUR2)
par(mfrow=c(2,1))
hist (log (USairpollution$popul), col="blue", labels=TRUE, xlab="")
hist (rnorm (1000), col="red", breaks=32, xlab="")


# Box-Whisker-Diagramm
# boxplot (x, ..., notch, out, names, log, horizontal, add, )
require (HSAUR2)
boxplot (USairpollution, log="y", notch=TRUE,
         col=cm.colors (length (USairpollution)), main="US air pollution")


# Bar chart for vector and matrix data
# barplot (height, width=1, legend.text, beside, horiz, col, axes, )
par(mfrow=c(2,2))
for(tb in list (VADeaths, t(VADeaths)))
  for (bs in c(FALSE,TRUE))
    barplot (height=tb, legend=TRUE, beside=bs, args.legend=list(x="top"))


# curve shape (expression object)
# curve (expr, from, to, n=101, add=FALSE, type=’l’, ylab, log, ...)
par(mfrow=c(2,1))
mu=2; sd=3;
curve ((x-mu)*(x+mu)*x, from=-1.1*mu, to=+1.1*mu, col="red", lwd=5)
hist (rnorm (1000,mu,sd), freq=FALSE, col="gray", breaks=32, xlab="")
curve (dnorm (x,mu,sd), add=TRUE, lwd=3)


# Curve progression (function object)
# curve (x, y=0, to=1, from=y, xlim=NULL, ...) oder gleichwertig plot («dto.»)
log1plus <- function(a) log(1+a)
par(mfrow=c(2,2))
for (ll in c("", "x", "y", "xy"))
  curve (log1plus, 1, 1000, log=ll, col=2+nchar(ll), xlab="",
         sub=paste ("log=’", ll, "’", sep=""))
z <- 2^(0:9); points (z, log1plus(z), pch=21, cex=2, bg="magenta")


# Points and lines in the coordinate plane
# points (x, y=NULL, type=’p’, pch, cex, bg, ...) oder
# lines (x, y=NULL, type=’l’, lwd, lty, ...)
par(mfrow=c(2,2))
x <- rnorm(50); y <- rnorm(50); irr1 <- cumsum (x); irr2 <- cumsum (x + 1i*y)
plot (irr1, col="red"); lines (irr1, lty=2)
plot (irr2, col="red"); lines (irr2, lty=2)
plot (cars, col="blue"); lines (lowess (cars), col="red", lwd=3, lty="82")
plot (irr1, col=rainbow(50)); irr1[10:15] <- NA; lines (irr1-3, col=rainbow(50))


# straight lines and rectangles
# abline (a, b, h, v, reg, coef, ...) oder
# rect (xleft, ybottom, xright, ytop, col, border, lty, lwd, ...)
par(mfrow=c(2,1))
plot (c(-2,3), c(-1,5), type="n", xlab="", ylab="", asp=1)
abline (h=-1:5, v=-2:3, col="lightgray", lty=3)
abline (a=1, b=2, col="red")
abline (h=pi, v=pi, col="blue")
attach (cars)
plot (cars)
abline (lsfit (speed, dist), col="blue")
abline (lsfit (speed, dist, intercept=FALSE), col="cyan")
N <- 1:5
rect (xl=0+4*N, yb=75+5*N, xr=3+4*N, yt=120,
      col=1+N, lty=N, lwd=N)


# Arrows and Polygons
# arrows (x0, y0, x1=x0, y1=y0, length, angle, code, ...) oder
# polygon (x, y, density, angle, border, col, lty, ...)
par(mfrow=c(2,1))
plot (x <- seq (-3, +3, len=12), cos(x))
curve (cos, add=TRUE, col="red", lwd=3)
arrows (0, -1, x, cos(x), length=0.1, col=topo.colors(24))
arrows (0, +100, x, cos(x), angle=10, col=heat.colors(24))
x <- seq (0, 2012, len=(n<-12))
y <- cumsum (rnorm (n))
s <- abs (rnorm (n,3))
plot (c(x,x), c(y-s,y+s))
polygon (c(x,rev(x)), c(y-s,rev(y+s)), col="yellow", border="red")
lines (x, y, lty=3, col="blue")


# Text markings in the coordinate plane
# text (x, y, labels, adj, pos, offset, vfont, cex, col, font, ...)
# abbreviate (names,  min=4, strict=FALSE)
plot (popul ~ manu, data=USairpollution, xlab="Manufacturing enterprises with 20 or more workers", 
                    ylab="Population size (1970 census) in thousands", log="xy", col="red")
text (popul ~ manu, data=USairpollution, pos=1, labels=rownames (USairpollution))
text (rep(60,5), 400*2:6, labels=paste (1:5, "abc...XYZ"), font=1:5, col="green4")
text (2000, 200, cex=2, col="red2", labels=expression (bar(x) == sum(frac(x[i], n), i==1, n)))

# Support point markings on the coordinate axis
# rug (x, ticksize=0.03, side=1, lwd=0.5, col=par(’fg’), quiet=, ...)
jitter (x, factor=1, amount=NULL)
plot (popul ~ manu, data=USairpollution, xlab="Manufacturing enterprises with 20 or more workers",
                   ylab="Population size (1970 census) in thousands", log="xy", col="blue", cex=3)
rug (USairpollution$manu, side=1, col="red"); rug (USairpollution$manu, side=3, ticks=-0.06)
rug (USairpollution$popul, side=2, col="red"); rug (USairpollution$popul, side=4, lwd=5)

## Spatial and multidimensional representations---------------------------------
# Raster graphics for f(x;y)
# image (x, y, z, zlim, xlim, ylim, col=heat.colors(12), add, breaks, ...)
par(mfrow=c(2,2))
library (jpeg); logo <- readJPEG("Rlogo.jpg")[, , 1]
image (logo, asp=1, main="col = heat.colors(12)")
image (t(logo[nrow(logo):1,]), asp=1, col=gray (0:20/20), main="Grauwertbild")
x <- dnorm (-30:+30/20)
image (z=x%o%x, col=gray (0:50/50), asp=1, main="2D-Gaußglocke")
image (volcano, col=topo.colors(48)[-(1:10)], main="Volcano (topo.colors)")


# Contour plot for f(x;y)
# contour (x, y, z, nlevels=10, levels, labels, method, col, lty, add, ...)
par(mfrow=c(2,2))
x <- seq (-2, +2, len=100); contour (x=x, y=x, z=outer (dnorm(x), dnorm(x)), nlevels=12, asp=1)
x <- -6:16; z <- outer(x, sqrt(abs(x)), FUN="/"); contour(x,x,z, col="blue", method="edge", nlev=32)
image (z=volcano, col=topo.colors(32)[-(1:10)]); contour (z=volcano, add=TRUE, lwd=2)
contour (x=sqrt(1:nrow(volcano)), y=sqrt(1:ncol(volcano)), z=volcano, col=topo.colors(12), lwd=5)


# Perspective graph for f(x;y)
# persp (x,y,z, theta,phi, r, expand, col, border, shade, box, ...)
par(mfrow=c(2,2))
x <- dnorm (-10:+10/5)
persp (z=x%o%x, theta=30, phi=30, expand=0.5, col="lightblue", main="2D Gaussian bell")
persp (z=sqrt(logo), theta=45, phi=65, shade=0.75, col="yellow", border=NA, box=FALSE, main="R logo (shaded)")
persp (z=volcano, theta=150, phi=30, expand=0.4, col="green", shade=0.75, box=F, border=NA, main="volcano (shaded)")
persp (z=volcano [1:nrow(volcano)%%3==0, 1:ncol(volcano)%%3==0],
theta=150, phi=30, expand=0.4, box=FALSE, main="volcano (barred)")


# 3D scatterplot for (x;y;z) point clouds
# scatterplot3d (x,y,z, color, highlight.3d, x/y/zlim, x/y/zlab, scale,
# angle, grid, box, type, col.*, cex.*, font.*, lty.*, ...)
par(mfrow=c(2,2))
z <- seq(-10, +10, 0.1); scatterplot3d (x=cos(z), y=sin(z), z=z, pch=19, highlight.3d=TRUE)
scatterplot3d (iris[4:2], color=1+unclass(iris$Species))
scatterplot3d (iris[3:1], type="h", pch=" ", lwd=5, color=rep(rainbow(10),15))
rb <- rainbow (151); rb3 <- t (col2rgb (rb)); scatterplot3d (rb3, color=rb, box=FALSE, axis=F) -> geo
tp <- cm.colors (151); tp3 <- t (col2rgb (tp)); geo$points3d (tp3, pch="X", col=tp)


# Scatter plot: n n 2D clouds
# pairs (x, labels, panel, , cex.labels, font.labels, gap=1, ...)
library (HSAUR2)
pairs (USairpollution, cex=8, pch=".", col="magenta")

# Scatterplot with individual layout
# pairs (x, labels, panel, lower.panel, upper.panel, diag.panel, text.panel, ...)
library (HSAUR2)
pairs (USairpollution[c(1,2,3)],
       lower.panel=points,
       upper.panel=function (x,y,...)
       {
         points (x, y, col="cyan3")
         abline (lsfit(x,y), lty=2)
       },
       diag.panel=function (x,...)
         lines (
           seq (
             min(x),
             max(x),
             len=length(x)
           ),
           sort(x), lwd=3, ...
         ),
       cex=2, pch=24, bg="magenta")

# Iconic 3D representation: "bubble plot"
# symbols (x, y, circles, squares, inches, add=FALSE, fg, bg, ...)
with (USairpollution,
      plot (temp, wind, main="Sulphure dioxide <− temperature & wind speed", xlab="Average annual temperature (Fahrenheit)", ylab="Average annual wind speed (m.p.h.)", pch=24, bg="red"))
with (USairpollution,
      symbols (temp, wind, add=TRUE, circles=SO2, inches=0.75, fg="blue"))


# Iconic representation of >= 3D point clouds
# symbols (x, y, rectangles, stars, thermometers, boxplots, ...)



# star graphic
# stars (x, full, scale, radius, labels, location, nrow, ncol, len,
#        key.loc, draw.segments=FALSE, col.segments, col.stars, ...)
stars (USairpollution, draw.segments=FALSE,
       col.stars=rainbow(41), key.loc=c(14,2), flip.labels=FALSE, cex=0.8)


# Radar graphic and spider web
par(mfrow=c(2,1))
stars (USairpollution,
       draw.segments=FALSE,
       locations=c(0,0), key.loc=c(0,0))

require (HSAUR2); stars (pottery, draw.segments=TRUE, key.loc=c(13,2))


# Chernoff faces: physiognomic representation
# faces (xy, fill=FALSE, nrow, ncol, scale=TRUE, labels)
require (TeachingDemos)
N=150; M=4; K=3; idx <- round ((1:(K*M)-.5)*N/K/M)
lab <- paste (idx, iris$Species[idx], sep="=")
faces (iris[idx,-5], nrow=K, ncol=M, labels=lab, fill=FALSE)


# Clothesline graphic: parallel coordinates
# parallel (x, data=NULL, groups=NULL, subset=TRUE, ...)

require (lattice)
require (HSAUR2)
parallel (x=USairpollution)


require (lattice)
parallel (x=iris[1:4],
          groups=iris$Species,
          horizontal.axis=FALSE
)

# (FFactorial) grouped representations
# coplot (formula, data, col, pch, bar.bg, panel, axlabels, number, ...)

coplot (Petal.Width
        ~ Sepal.Width
        | Species,
        data=iris, rows=1,
        panel=panel.smooth,
        pch=19, col="lightblue")

load ("./data/Everitt.rda")
hypo <- Data$hypo
coplot (weight ~ age
        | sex * health,
        data=hypo,
        pch=19, col="magenta3", cex=5)

# Shingles: fuzzy univariate quantization
# constructor: equal.count (x, number=6, overlap=0.5, ...)
equal.count (rnorm (50,5,1))
require (lattice)
set.seed (4711)
z <- rnorm (n=50, mean=5)
shingle <- equal.count (z)
plot (shingle)

# Numerically grouped representations
# coplot (formula, data, col, pch, bar.bg, panel, axlabels, number, ...)

par (pch=19)
cls <- 1+unclass(iris$Species)
coplot (Petal.Width
        ~ Sepal.Width
        | Petal.Length,
        data=iris, col=cls)

coplot (Petal.Width
        ~ Sepal.Width
        | Petal.Length
        * Sepal.Length,
        data=iris, col=cls,
        number=c(3,3))


