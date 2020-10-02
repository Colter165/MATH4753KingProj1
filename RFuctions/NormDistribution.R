#' Normal distribution curve graph and calculator
#'
#' @param mu    #Mean value
#' @param sigma #Standard Deviation
#' @param a     #Y<a value
#'
#' @return
#' @export
#'
#' @examples
#' mycurve(my=10, sigma=3, 8)
myncurve = function(mu, sigma,a){
  #Creates curve of normal distribution
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  #Fills in curve for desired region of probability
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  #Calculates and displays probability
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  
}