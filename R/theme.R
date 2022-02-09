# ---- Plotting color palettes:
max_selected <- 6

# Palettes
fill_pal <- setNames(MetBrewer::met.brewer(name="Signac", n = 20)[c(5, 15)], c("family", "genus"))
fill_grey <- RColorBrewer::brewer.pal(9, name="Set1")[9]

set.seed(20220121)
stroke_pal <- MetBrewer::met.brewer(name="Peru1", n = max_selected)[sample(1:max_selected, max_selected)]
stroke_grey <- RColorBrewer::brewer.pal(8, name="Dark2")[8]

# Plot settings
ref_beak_lwd <- 2
beak_lwd <- 5
morphoplot_font_size <- 13

# Theme colours
primary <- fill_pal[1]
secondary <- fill_pal[2]

morphospace_theme <- bslib::bs_theme(version = 5,
                                     primary = primary,
                                     secondary = secondary,
                                     warning = primary,
                                     spacer = "1rem",
                                     "nav-tabs-link-active-border-color" = paste(rep(primary, 4), collapse = " "),
                                     "nav-tabs-border-color" = paste(primary, "!default"),
                                     "border-color" = primary,
                                     "nav-tabs-border-color" = paste(rep(primary, 3), collapse = " "),
                                     "nav-tabs-link-active-color" = "white",
                                     "nav-link-font-weight" = "bold",
                                     "nav-tabs-link-active-bg"= primary,
                                     "nav-tabs-border-width" = "3px",
                                     "nav-link-font-size" = morphoplot_font_size * 4,
                                     "nav-link-padding-x" = "1.3rem",
                                     "nav-link-padding-y" = "0rem",
                                     "card-group-margin" = "1rem")


#bslib::bs_theme_preview(morphospace_theme)
