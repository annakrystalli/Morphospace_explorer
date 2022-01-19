get_genus_options <- function(scores, fam1) {
    if(is.null(fam1)){
        sort(scores$Genus)
    }else{
    sort(scores[scores$Family == fam1, "Genus"])
    }
}

get_spp_options <- function(scores, fam1, gen1) {
    if(is.null(gen1)){
        if(is.null(fam1)){
        sort(setNames(scores$Spp, scores$Spp2)) %>% as.list()
        }else{
            sort(setNames(scores[scores$Family == fam1, "Spp"],   
                     scores[scores$Family == fam1, "Spp2"])) %>% as.list()
        }
    }else{
        sort(setNames(scores[scores$Genus == gen1, "Spp"],   
                 scores[scores$Genus == gen1, "Spp2"])) %>% as.list()
    }
}