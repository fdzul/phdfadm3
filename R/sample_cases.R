sample_cases <- function(x){
    set.seed(45)
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
}