


data_3d_get <- function(coords) {
    
    # Convert (p x k x n) data array into 2D  (n x [p x k]) data matrix 
    # Performs a principal components analysis
    pca_res <- stats::prcomp(geomorph::two.d.array(coords, sep = "_"))
    row.names(pca_res$x) <- gsub('.{2}$', '', row.names(pca_res$x)) %>%
      gsub("_", " ", .)
    
    list(k = dim(coords)[2],
         p = dim(coords)[1],
         n = dim(coords)[3],
         # calculate reference mean beak shape
         ref = geomorph::mshape(coords),
         rotation = pca_res$rotation,
         pca_data = pca_res$x
    )
}

beak_3d <- function(beak_data, data_3d, sliders, colour, lwd = 3, alpha = 1) {
    
    coords3d <- array2to3D(beak_data, data_3d, sliders)
    
        rgl::segments3d(coords3d, 
                   lwd = lwd, color = colour, box=FALSE, axes=FALSE, 
                   xlab="", ylab="", zlab="", alpha = alpha)
}

plot_selected_beak <- function(data_3d, selected_species = NULL, all_selected = NULL,
                               sliders, colour, 
                               alpha = 1, lwd = 5) {
    
    if (!is.null(selected_species)) {
        pca_spp <- data_3d$pca_data[selected_species, , drop = FALSE]
        beak_3d(pca_spp, data_3d, sliders, colour = colour, 
                alpha = alpha, lwd = lwd)
    }
  

}

plot_ref_beak <- function(data_3d, xaxis = "PC1", yaxis = "PC2", xval = 0, yval = 0, 
                    sliders, colour = "black", lwd = 3, alpha = 1) {
    
    pca_ref <- stats::setNames(rep(0, ncol(data_3d$pca_data)), 
                               colnames(data_3d$pca_data))
    pca_ref[xaxis] <- xval
    pca_ref[yaxis] <- yval
    
    # Plot beaks
    beak_3d(pca_ref, data_3d, sliders, colour = colour, alpha = alpha, lwd = lwd)
}


array2to3D <- function(beak_data, data_3d, sliders) {
  
  # convert 2D to 3D array
  shape <- geomorph::arrayspecs(
    beak_data %*% (t(data_3d$rotation)), 
    data_3d$p, data_3d$k)[,,1] + data_3d$ref
  
  # reshape array using slider indices
  vertices <- NULL
  
  for (i in 1:nrow(sliders)) {
    vertices <- rbind(vertices,
                      rbind(shape[sliders[i,1],], 
                            shape[sliders[i,2],]
                      )
    )
  }
  vertices
}
