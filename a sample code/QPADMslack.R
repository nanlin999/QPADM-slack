# Penalized quantile regression Using the Slack Variable Representation. This is a non-parallel implementation.
# QPADMslack(eps, y, x, tau, lambda, a, pho, funname, maxstep)
# "eps" The terminate condition
# "y" The response vector
# "x" The design matrix (with intercept)
# "tau" The quantile of interest
# "lambda" The penalization parameter
# "a" The shape parameter for the SCAD/MCP penalty
# "pho" The augmentation parameter for the ADMM
# "funname" Name of the penalty to use, currently support ("scad","mcp")
# "maxstep" Maximum number of iterations allowed
# value:The coefficient estimation of the linear quantile regression model, the number of iterations and the running time
n = 30000 
p = 1000   
set.seed(66)
X = matrix(rnorm(n*p),nrow=n) 
cov = gcov(p, 0.5)
X = X%*%chol(cov)
X[,1] = pnorm(X[,1])
e = rnorm(n)
Y = X[,6]+X[,12]+X[,15]+X[,20]+0.7*X[,1]*e
QPADMslack_scad = QPADMslack(1e-03, Y, X, 0.3, 15, 3.7, 1, "scad", 500)
beta_qadmm_scad = QPADMslack_scad[1:p]
step = QPADMslack_scad[p+1]
time = QPADMslack_scad[p+2]
