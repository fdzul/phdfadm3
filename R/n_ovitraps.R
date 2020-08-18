#' calculate the ovitraps number by year, week and dengue case status.
#'
#' @param x is the dataset.
#' @param buffer is the buffer.
#' @param sinave_new is TRUE for dengue dataset of 2016 to 2020. FALSE for dengue dataset from 2008 to 2015. 
#'
#' @return a dataframe
#' @export
#'
#' @examples
n_ovitraps <- function(x, buffer, sinave_new){
    if(sinave_new == TRUE){
        y <- x %>%
            sf::st_drop_geometry() %>%
            dplyr::select(ANO, SEM,ESTATUS_CASO, 427:453) %>%
            dplyr::group_by(ANO, SEM) %>%
            tidyr::nest() %>%
            dplyr::mutate(sample_index_ovi = purrr::map(data,
                                                        sample_index,
                                                        sinave_new = TRUE)) %>%
            dplyr::select(-data) %>%
            tidyr::unnest(cols = c(sample_index_ovi)) %>%
            dplyr::select(index, ANO, SEM, ESTATUS_CASO, ovitrap) %>%
            dplyr::group_by(ANO, SEM, ESTATUS_CASO) %>%
            dplyr::summarise(n_ovitraps = dplyr::n(),
                             n_cases = length(unique(index)))
        y$buffer <- buffer
        y
    } else {
        y <- x %>%
            sf::st_drop_geometry() %>%
            dplyr::select(ANO, SEM, VEC_EST, 185:213) %>%
            dplyr::group_by(ANO, SEM) %>%
            tidyr::nest() %>%
            dplyr::mutate(sample_index_ovi = purrr::map(data,
                                                        sample_index,
                                                        sinave_new = FALSE)) %>%
            dplyr::select(-data) %>%
            tidyr::unnest(cols = c(sample_index_ovi)) %>%
            dplyr::select(index, ANO, SEM, VEC_EST, ovitrap) %>%
            dplyr::group_by(ANO, SEM, VEC_EST) %>%
            dplyr::summarise(n_ovitraps = dplyr::n(),
                             n_cases = length(unique(index)))
        y$buffer <- buffer
        y
    }
}