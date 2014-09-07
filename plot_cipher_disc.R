plot_cipher_ring <- function(chars=LETTERS, radius=1, xlim=c(-1,1)*radius, ylim=xlim, innerRing=0.8, cexLab=1.5, radius_label_exp=0.91) {
    
    #chars <- LETTERS
    chars <- rev(chars)
    numSlicesPts <- 360
    thetaPts <- seq(from=0, to=2*pi, length.out=numSlicesPts)
    
    # Define the center
    ctr.x <- 0
    ctr.y <- 0
    
    ## Define the radius and the scope of the plot
    #radius <- 1
    #axes_exp <- 1.1
    #xlim <- c(ctr.x - (radius * axes_exp), ctr.x + (radius * axes_exp))
    #ylim <- c(ctr.y - (radius * axes_exp), ctr.y + (radius * axes_exp))
    
    ## Start out with a blank plot
    plot(NULL, xlim=xlim, ylim=ylim, asp=1, xlab="", ylab="", axes=F)
    
    ## Add the outer circle
    xcircle <- ctr.x + radius * cos(thetaPts)
    ycircle <- ctr.y + radius * sin(thetaPts)
    points(xcircle, ycircle, asp=1, pch=16, type="l", lwd=3)
    
    ## Add the inner circle if innerRing > 0
    if (innerRing > 0) {
        xinner <- ctr.x + radius * innerRing * cos(thetaPts)
        yinner <- ctr.y + radius * innerRing * sin(thetaPts)
        points(xinner, yinner, asp=1, pch=16, type="l", lwd=3)
    }
    
    # Add the spokes
    numSpokes <- length(chars)
    thetaSpokes <- seq(from=0, to=2*pi, length.out=(numSpokes + 1))
    thetaSpokes <- thetaSpokes[-length(thetaSpokes)]
    xspokes <- ctr.x + radius * cos(thetaSpokes)
    yspokes <- ctr.y + radius * sin(thetaSpokes)
    segments(x0=0, y0=0, x1=xspokes, y1=yspokes)
    
    ## Re-Draw a blank plot (for quick testing)
    #plot(NULL, xlim=c(ctr.x - (radius * axes_exp), ctr.x + (radius * axes_exp)), ylim=c(ctr.y - (radius * axes_exp), ctr.y + (radius * axes_exp)), asp=1, xlab="", ylab="")
    #points(xcircle, ycircle, asp=1, pch=16, type="l", lwd=2)
    #segments(x0=0, y0=0, x1=xspokes, y1=yspokes)
        
    ## Add the labels
    thetaLabels <- thetaSpokes + (thetaSpokes[2] / 2)
    thetaLabels
    thetaLabelsDeg <- 360 * thetaLabels / (2 * pi)
    offset_labels <- -8
    xlbl <- ctr.x + radius_label_exp * radius * cos(thetaLabels)
    ylbl <- ctr.y + radius_label_exp * radius * sin(thetaLabels)
    invisible(lapply(1:length(chars), function(i) text(xlbl[i], ylbl[i], labels=chars[(i + offset_labels) %% length(chars) + 1], srt=thetaLabelsDeg[i] - 90, col="black", cex=cexLab)))

}
