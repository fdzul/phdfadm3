


extract_betas <- function(x, name, link) {
    #y <- x$summary.fixed[2, -c( 4, 6,7)]
    y <- x$summary.fixed
    #colnames(y) <- c("mean","sd" ,"lower", "upper")
    #se <- sqrt(y$sd/(nrow(by_time$data[[8]]) - nrow(mod_nbr_inla[[1]]$summary.fixed)))
    #se <- sqrt(y$sd/(nrow(w) - nrow(x$summary.fixed)))
    #se <- y$sd/sqrt(nrow(w))
    y
    if (name == "IRR") {
        y$IRR <- exp(y[,1])
        y$IRR_lower <- exp(y[,3])
        y$IRR_upper <- exp(y[,5])
        #y$IRR_lower_b <- exp(y[,1] - (1.96*se))
        #y$IRR_upper_b <- exp(y[,1] + (1.96*se))
        y
    } else if (name == "OR") {
        if ("logit" ==  link) {
            y$OR <- exp(y[,1])
            y$OR_lower <- exp(y[,3])
            y$OR_upper <- exp(y[,5])
            y 
            
        } else if("probit" ==  link) {
            y$OR <- y[,1]
            y$OR_lower <- y[,3]
            y$OR_upper <- y[,5]
            y 
            
        } else if ("cloglog" ==  link) {
            y$OR <- 1 - exp(-exp(y[,1]))
            y$OR_lower <- 1 - exp(-exp(y[,3]))
            y$OR_upper <- 1 - exp(-exp(y[,5]))
            y 
        } else if ("loglog" ==  link){
            y$OR <- exp(-exp(-y[,1]))
            y$OR_lower <- exp(-exp(-y[,3]))
            y$OR_upper <- exp(-exp(-y[,5]))
            y  
        } else if ("log" ==  link){
            y$OR <- exp(y[,1])
            y$OR_lower <- exp(y[,3])
            y$OR_upper <- exp(y[,5])
            y 
        } else if ("cauchit" ==  link){
            y$OR <- y[,1]
            y$OR_lower <- y[,3]
            y$OR_upper <- y[,5]
            y 
        } else if ("identity" ==  link){
            y$OR <- y[,1]
            y$OR_lower <- y[,3]
            y$OR_upper <- y[,5]
            y 
        }
    }
    
    cbind(y[2, c(1,3, 5, 8,9,10)], 
          data.frame(dic = x$dic$dic,
                     link = link))
}
