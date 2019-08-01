# Penalized quantile regression using the slack variable representation. This is a parallel implementation. 
# The QPADMslackpara() function is written using Rcpp. To use the code, we should install the R package, "Rcpp", first.
# QPADMslackpara(eps, y, x, betatrue, tau, lambda, a, pho, K, funname, maxstep)
# "y" The response vector
# "x" The design matrix (without intercept)
# "betatrue" The real value of the coefficient
# "tau" The quantile of interest
# "lambda" The penalization parameter
# "a" The shape parameter for the SCAD/MCP penalty
# "pho" The augmentation parameter for the ADMM
# "K" The number of partitions
# "funname" Name of the penalty to use, currently support ("scad", "mcp")
# "maxstep" Maximum number of iterations allowed
# "eps" The tolerance parameter for convergence
# "intercept" Whether to include the intercept into the model
# value: The absolute estimation error, the running time and the number of iterations
n = 300000
p = 100
K = 100
set.seed(66)
X = matrix(rnorm(n*p),nrow=n) 
cov = gcov(p, rho)
X = X%*%chol(cov)
X[,1] = pnorm(X[,1])
e = rnorm(n)
Y = X[,6]+X[,12]+X[,15]+X[,20]+0.7*X[,1]*e
beta_true = rep(0, p)
beta_true[1] = 0.7*qnorm(0.7)
beta_true[6] = beta_true[12] = beta_true[15] = beta_true[20] = 1
QPADMslackpara_scad = QPADMslackparaCPP(Y, X, beta_true, 0.7, 230, 3.7, 15, K, "scad", 500, 1e-04, FALSE)
AE = QPADMslackpara_scad[1]
time = QPADMslackpara_scad[2]
iteration = QPADMslackpara_scad[3]
