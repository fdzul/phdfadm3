glmm_binary <- function(x, aproximation, integration, sinave_new){
    
    if(sinave_new == TRUE){
        x$caso_control <- ifelse(x$ESTATUS_CASO == "POSITIVO", 1, 0)
    } else{
        x$caso_control <- ifelse(x$VEC_EST == "POSITIVO", 1, 0)
    }
    
    #family1 = "binomial"
    #control.family1 = list(control.link=list(model= link))
    
    INLA::inla(formula = caso_control ~ eggs + 
                   f(index, model = "iid") + ## id of cases
                   f(manzana, model = "iid") +
                   f(sector, model = "iid") +
                   f(loc, model = "iid") +
                   f(mpo, model = "iid") +
                   f(week, model = "iid"),
               data = x,
               verbose=TRUE,
               family = "binomial",
               #control.family = list(control.link = list(model = link)),
               #control.family = control.family1,
               #control.link = list(model=link),
               control.inla = list(strategy = aproximation, 
                                   int.strategy = integration,
                                   correct = TRUE),
               control.predictor = list(compute = TRUE, link = 1),
               control.compute = list(dic = TRUE))
}
