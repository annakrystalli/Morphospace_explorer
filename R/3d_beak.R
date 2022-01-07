


data_3d_get <- function(coords) {
    
    # Performs a principal components analysis
    pca_res <- stats::prcomp(geomorph::two.d.array(coords, sep = "_"))
    row.names(pca_res$x) <- gsub('.{2}$', '', row.names(pca_res$x))
    
    list(k = dim(coords)[2],
         p = dim(coords)[1],
         n = dim(coords)[3],
         # calculate reference mean beak shape
         ref = geomorph::mshape(coords),
         # Convert (p x k x n) data array into 2D  (n x [p x k]) data matrix 
         rotation = pca_res$rotation,
         pca_data = pca_res$x
    )
}

beak_3d <- function(beak_data, data_3d, sliders, colour = cols[4], lwd = 3, alpha = 1) {
    
    shape <- geomorph::arrayspecs(
        beak_data %*% (t(data_3d$rotation)), 
        data_3d$p, data_3d$k)[,,1] + data_3d$ref
    #rgl::par3d(so)
    for (i in 1:nrow(sliders)) {
        segments3d(rbind(shape[sliders[i,1],], 
                         shape[sliders[i,2],]), 
                   lwd = lwd, color = colour, box=FALSE, axes=FALSE, 
                   xlab="", ylab="", zlab="")
    }
}

plot_beaks <- function(data_3d, xaxis = "PC1", yaxis = "PC2", xval = 0, yval = 0, 
                       clickspp = NULL, spp1 = "Search here...") {

    pca_ref <- stats::setNames(rep(0, ncol(data_3d$pca_data)), 
                                colnames(data_3d$pca_data))
    pca_ref[xaxis] <- xval
    pca_ref[yaxis] <- yval
    
    if (!is.null(clickspp)) {
        pca_val2 <- data_3d$pca_data[clickspp, ]
    } else {
        pca_val2 <- NULL
    }
    if (spp1 != "Search here...") {
        pca_val3 <- data_3d$pca_data[spp1, ]
    } else {
        pca_val3 <- NULL
    }
    
    
    # Plot beaks
    beak_3d(pca_ref, data_3d, sliders, colour = "black", lwd = 1, alpha = 0.5)
    
    if (!is.null(pca_val2)) {
        beak_3d(pca_val2, data_3d, sliders, colour = cols[4])
    }
    try(if (!is.null(pca_val3)) {
        beak_3d(pca_val3, data_3d, sliders, colour = cols[3])
        }, silent=T)
    rglwidget()
}



