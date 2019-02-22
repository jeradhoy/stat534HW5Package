#' Simulate a spatial process
#'
#' This function allows you to simulate a spatial process given lat and lon.
#' @param lat latitude
#' @param lon longitude
#' @export
#' @examples
#' simulate_spatial_process(lat, lon)

simulate_spatial_process <- function(lat, lon, sigmasq.true = 3, tausq.true = .50, phi.true = 2, alpha.true = 0, beta.true = 1){
    
    # simulate data
    df <- data.frame(lat, lon)
    num.pts <- nrow(df)
    
    x1 <- df$lon
    x2 <- df$lat
    
    d <- dist(cbind(x1,x2), upper=T, diag = T) %>% as.matrix()
    Omega <- sigmasq.true * exp(-d * phi.true) + tausq.true * diag(num.pts)
    
    x.reg <- rnorm(num.pts)
    y = rmnorm(1, x.reg*beta.true, Omega)
    
    GP.dat <- data.frame(x1 = x1, x2 = x2, y = y)
    return(GP.dat)
}