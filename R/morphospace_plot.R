
# Morphospace plot
plot_morphospace <- function(scores, xaxis, yaxis, plot_v_axis, plot_h_axis, plot_centre,
                             stroke_params, fill_params, a = 80, b = 40, font_size = 12) {
    
    fill_data <- morphoplot_subset_pts(scores, pts = fill_params$fill_pts)
    stroke_data <- morphoplot_subset_pts(scores, pts = stroke_params$stroke_pts)

    scores %>%
        ggvis(x = prop("x", as.symbol(xaxis)), y = prop("y", as.symbol(yaxis))) %>%
        add_axis("x", title = xaxis) %>%
        add_axis("y", title = yaxis) %>%
        layer_points(stroke := stroke_params$stroke_cols[1], fill := fill_params$fill_cols[1], 
                     size = ~LogCenSize, size.hover := 300, strokeOpacity := 0.7,
                     fillOpacity := 0.2, fillOpacity.hover := 0.2, key := ~Spp2)  %>%
        layer_points(data = fill_data, 
                     x = prop("x", as.symbol(xaxis)), y = prop("y", as.symbol(yaxis)),
                     size = ~LogCenSize, size.hover := 300,
                     fillOpacity := 0.85, fillOpacity.hover := 0.95,
                     key := ~Spp2,
                     fill = ~pts)  %>%
        layer_points(data = stroke_data, 
                     x = prop("x", as.symbol(xaxis)), y = prop("y", as.symbol(yaxis)),
                     size = ~LogCenSize, size.hover := 300,
                     key := ~Spp2, stroke = ~pts, strokeWidth := 3)  %>%
        layer_paths(data = plot_v_axis, x = ~X, y= ~Y, stroke:="red",
                    strokeWidth:=3, strokeOpacity:=0.5) %>%
        layer_paths(data = plot_h_axis, x = ~X, y= ~Y, stroke:="red",
                    strokeWidth:=3, strokeOpacity:=0.5) %>%
        layer_points(data = plot_centre, x = ~X, y= ~Y, stroke:="red",
                     size := 50, fillOpacity := 0, opacity:=0.5, strokeWidth:=3) %>%
        set_options(height = 500, duration = 0) %>%
        scale_nominal("stroke", range = if(length(stroke_params$stroke_cols) == 1){
            rep(stroke_params$stroke_cols, 2)}else{stroke_params$stroke_cols})  %>%
        scale_nominal("fill", range = if(length(fill_params$fill_cols) == 1){
            rep(fill_params$fill_cols, 2)}else{fill_params$fill_cols})  %>%
        add_legend("size", title = "Bill size (log)", properties = legend_props(legend = list(y = 0),
                                                                                labels = list(fontSize = font_size),
                                                                                title = list(fontSize = font_size + 2)))  %>%
        add_legend("fill", title = "Subsetted", properties = legend_props(legend = list(y = a),
                                                                            labels = list(fontSize = font_size),
                                                                            title = list(fontSize = font_size + 2))) %>%
        add_legend("stroke", title = "Selected", properties = legend_props(legend = list(y = (length(fill_params$fill_cols) * b) + a),
                                                                                         labels = list(fontSize = font_size),
                                                                           title = list(fontSize = font_size + 2),
                                                                           symbol = list(size = 120, fill = "black", 
                                                                                         strokeWidth = 3#, 
                                                                                         #stroke = stroke_params$stroke_cols
                                                                                         ))) %>%
        identity()
    
}

# Generate indices and color scale for point fill properties (taxonomic subset)
morphoplot_fill_params_fn <- function(scores, subset_fam, subset_gen,
                                      fill_grey, fill_pal) {
     
     fill_pts <- rep("All species", nrow(scores))
     names(fill_pts) <- scores$Spp
     
     fill_pts[scores$Family == subset_fam] <- subset_fam
     fill_pts[scores$Genus == subset_gen] <- subset_gen
     
     flevels <- c("All species", subset_fam, subset_gen)
     flevels <- flevels[flevels != ""]
     fill_pts <- factor(fill_pts, levels = flevels)
     
     fill_col_select <- c("family", "genus")[c(!is.null(subset_fam), 
                                               !is.null(subset_gen))]
     
     fill_cols <- unname(c(fill_grey, fill_pal[fill_col_select]))

     list(fill_pts = fill_pts, 
          fill_cols = fill_cols)
    
 }
 
# Generate indices and color scale for point stroke properties (selected species) 
morphoplot_stroke_params_fn <- function(scores, selected, stroke_grey,
                                        stroke_pal) {
    
     stroke_pts <- rep("All species", nrow(scores))
     names(stroke_pts) <- scores$Spp2
     
     
     if (!is.null(selected)) {
         
         stroke_pts[match(selected, names(stroke_pts))] <- selected
         stroke_pts <- factor(stroke_pts, levels=c("All species", selected))
         stroke_cols <- c(stroke_grey, stroke_pal[seq_along(selected)])
     }else{
         stroke_pts <- factor(stroke_pts, levels = c("All species"))
         stroke_cols <- stroke_grey
     }
     
     list(stroke_pts = stroke_pts, 
          stroke_cols = stroke_cols)
}

# Subset and scores df by subset / selected pts
morphoplot_subset_pts <- function(scores, pts) {
    scores$pts <- pts
    data <- scores[pts != "All species", ]
    data[order(data$pts), ]
}

