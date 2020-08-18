#' Sample cases bye year and week.
#'
#' @param x is is the element of nested dataset gruped by year and week.
#' @param sinave_new is TRUE for dengue dataset of 2016 to 2020. FALSE for dengue dataset from 2008 to 2015.
#'
#' @return a dataframe.
#' @export
#'
#' @examples
sample_cases <- function(x, sinave_new){
    set.seed(45)
    if(sinave_new == TRUE){
        a <- x %>% dplyr::filter(ESTATUS_CASO == "POSITIVO")
        b <- x %>% dplyr::filter(ESTATUS_CASO == "NEGATIVO")
        if(nrow(a) > nrow(b)){
            c <- dplyr::sample_n(tbl = a, size = nrow(b))
            rbind(c, b)
        } else if(nrow(b) > nrow(a)){
            c <- dplyr::sample_n(tbl = b, size = nrow(a))
            rbind(c, a)
        } else {
            x
        }
    } else {
        a <- x %>% dplyr::filter(VEC_EST == "POSITIVO")
        b <- x %>% dplyr::filter(VEC_EST == "NEGATIVO")
        if(nrow(a) > nrow(b)){
            c <- dplyr::sample_n(tbl = a, size = nrow(b))
            rbind(c, b)
        } else if(nrow(b) > nrow(a)){
            c <- dplyr::sample_n(tbl = b, size = nrow(a))
            rbind(c, a)
        } else {
            x
        }
    }
}

