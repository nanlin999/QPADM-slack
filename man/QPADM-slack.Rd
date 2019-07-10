\name{QPADM-slack}
\title{Penalized quantile regression Using the Slack Variable Representation}
\usage{
QPADMslack(eps, y, x, tau, lambda, a, pho, funname, maxstep)
}
\arguments{
\item{eps}{The terminate condition}

\item{y}{The response vector}

\item{x}{The design matrix (with intercept)}

\item{tau}{The quantile of interest}

\item{lambda}{The penalization parameter}

\item{a}{The shape parameter for the SCAD/MCP penalty}

\item{pho}{The augmentation parameter for the ADMM}

\item{funname}{Name of the penalty to use, currently support ("scad","mcp")}

\item{maxstep}{Maximum number of iterations allowed}
}
\value{
The coefficient estimation of the linear quantile regression model
The number of iterations
The running time
}
\description{
This is a non-parallel implementation. 
}
\examples{
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
beta_qadmm_scad3 = QPADMslack_scad[1:p]
step = QPADMslack_scad[p+1]
time = QPADMslack_scad[p+2]
}
