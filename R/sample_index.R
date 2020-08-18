#' sample size for positive and negative dengue cases and the ovitraps by case
#'
#' @param x is the element of nested dataset gruped by year and week.
#' @param sinave_new is TRUE for dengue dataset of 2016 to 2020. FALSE for dengue dataset from 2008 to 2015. 
#'
#' @return a dataframe.
#' @export
#'
#' @examples
sample_index <- function(x, sinave_new){
    set.seed(45)
    if(sinave_new == TRUE){
        x$id_caso_control <- paste(x$ESTATUS_CASO, x$index, sep = "_")
        pos <- x %>% dplyr::filter(ESTATUS_CASO == "POSITIVO")
        neg <- x %>% dplyr::filter(ESTATUS_CASO != "POSITIVO") 
    } else{
        x$id_caso_control <- paste(x$VEC_EST, x$index, sep = "_")
        pos <- x %>% dplyr::filter(VEC_EST == "POSITIVO")
        neg <- x %>% dplyr::filter(VEC_EST != "POSITIVO") 
    }
   
    if (length(unique(pos$index)) < length(unique(neg$index))){
        a <- neg[neg$index %in% c(sample(unique(neg$index),size = length(unique(pos$index)))),]
        y <- rbind(pos, a)
    } else if(length(unique(neg$index)) < length(unique(pos$index))) {
        a <-  pos[pos$index %in% c(sample(unique(pos$index),size = length(unique(neg$index)))),]
        y <- rbind(neg, a)
    } else {
        y <- x
    }
    
    n <- min(table(y$id_caso_control))
    y <- y %>%
        dplyr::group_by(id_caso_control) %>%
        tidyr::nest() %>%
        dplyr::mutate(sample_ovi = purrr::map(data, 
                                              dplyr::sample_n,
                                              size = n)) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = c(sample_ovi)) %>%
        as.data.frame()
    y
    
}