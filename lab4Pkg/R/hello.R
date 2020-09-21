#' linear regression
#' @name linreg
#' 
#' @param formula linear expression
#' @param data data source
#' 
#' @description calculate coefficients of linear regression
#' 
#' 
#' @return return the coefficients and coefficient names
#' @usage linreg(y~x,data)
#' 
#' @examples 
#' linreg(Petal.Length~Species, data = iris)
#' linreg(Petal.Length~Species, data = iris)
#' @export

l <- list(coefficients='Beta',residual_variance='sigma2')

class(l) <- 'linreg'

linreg <- function(formula,data) {
    # v_name <- all.vars(formula)
    y <- data[[all.vars(formula)[1]]]
    coe_x <- model.matrix(formula,data)
    # x1 <- model.matrix(y~coe_x)
    Beta <- solve(t(coe_x)%*%coe_x)%*%t(coe_x)%*%y
    #Beta <- as.vector(Beta)
    est_y <- coe_x%*%Beta      #fitted value
    est_e <- y-coe_x%*%Beta   #the residuals
    n <- length(coe_x)   #the number of observations
    p <- length(Beta)  #the number of parameter
    deg_f <- n-p  #degree of freedom
    sigma2 <- t(est_e)%*%est_e/deg_f #residual variance
    var_Beta <- solve(t(coe_x)%*%coe_x)*as.numeric(sigma2)#variance of the regression coefficients
    t = Beta/sqrt(diag(var_Beta))
    t1 = Beta[1,1]/sqrt(var_Beta[1,1])
    t2 = Beta[2,1]/sqrt(var_Beta[2,2])
    #plot(coe_x, y)
    est_e <- as.vector(est_e)
    est_y <- as.vector(est_y)
    
    row_name <- rownames(Beta)
    print(row_name)
    Beta <- as.vector(Beta)
    names(Beta) <- row_name
    
    l <- list(Coefficients=Beta,fitted_values=est_y,residuals=est_e,degree_freedom=deg_f,residual_variance=sigma2,variance_coefficient=var_Beta,
              t_value=t,formula1=formula,data1=data)
    
    attr(l, "class") <- "linreg"
    
    return(l)
    
}

print.linreg <- function(lr){
  cat('Call:',"\n")
  cat(lr$data1)
  cat('Linreg(formula=',"\n")
  cat("Coefficients:",'\n')
  return(lr$Coefficients)
}

plot.linreg <- function(lr){
  fitted_values<- lr$fitted_values
  residuals <- lr$residuals
  df1 <- data.frame(fitted_values,residuals)
  #print(df)
  
  # ggplot(data=df1) +
  # aes(x=fitted_values , y= residuals ) +
  # geom_point() +
  # geom_line (stat="smooth",method='loess') 
    
    ggplot(data=df1) +
    aes(x=fitted_values , y= sqrt(abs(residuals))) +
    geom_point () +
    geom_line (stat="smooth",method='loess')
}

resid.linreg <- function(lr){
  return(lr$residuals)
}

pred <- function(x){
  UseMethod("pred")
}
pred.linreg <- function(lr){
  return(lr$fitted_values)
}

coef <- function(x){
  UseMethod("coef")
}
coef.linreg <- function(lr,formula){
  cat("Coefficients:",'\n')
  return(lr$Coefficients)
}

summary <- function(lr){
  
}

attributes(l)

data(iris)
lout <- linreg(Petal.Length~Species, data = iris)
class(lout)

print <- function(lst){
  UseMethod("print")
}
print(lout)

print.linreg <- function(lst) {
  cat("The Beta values are:",lst$Coefficients, "\n")
  cat("The residual variance is:",lst$residual_variance,"\n")
}
print(lout)
print(lout)

#dx2 <- iris$Sepal.Length
#dcoe_x <- iris$Petal.Width
#dy <- iris$Petal.Length
#df <- data.frame(dcoe_x,dy)

#X <- model.matrix(dy~dcoe_x)
#df[all.vars(df$y~df$x)][1]
#Y <- df[all.vars(dy~dcoe_x)[1]]
#Beta <- solve(t(X)%*%X)%*%t(X)%*%dy

#est_y <- X%*%Beta      #fitted value
#est_e <- dy-X%*%Beta   #the residuals
#n <- length(dcoe_x)   #the number of observations
#p <- length(Beta)  #the number of parameter
#deg_f <- n-p  #degree of freedom
#sigma2 <- t(est_e)%*%est_e/deg_f #residual variance
#var_Beta <- solve(t(X)%*%X)*as.numeric(sigma2)#variance of the regression coefficients

#t1 = Beta[1,1]/sqrt(var_Beta[1,1])
#t2 = Beta[2,1]/sqrt(var_Beta[2,2])

# plot(dcoe_x, dy)
# par(new = TRUE)
# plot(dcoe_x, est_y)

