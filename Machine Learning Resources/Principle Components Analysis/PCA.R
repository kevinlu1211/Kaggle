#Generate data

m=50
n=100

x <- (seq(m)*2*pi)/m
t <- (seq(n)*2*pi)/n

#True field
Xt <- 
  outer(sin(x), sin(t)) + 
  outer(sin(2.1*x), sin(2.1*t)) + 
  outer(sin(3.1*x), sin(3.1*t)) +
  outer(tanh(x), cos(t)) + 
  outer(tanh(2*x), cos(2.1*t)) + 
  outer(tanh(4*x), cos(0.1*t)) + 
  outer(tanh(2.4*x), cos(1.1*t)) + 
  tanh(outer(x, t, FUN="+")) + 
  tanh(outer(x, 2*t, FUN="+"))

Xt <- t(Xt)
dim(Xt)

# PCA 
res <- prcomp(Xt, center = TRUE, scale = FALSE)
names(res)

# this gives our unit eigenvectors where the first column is the axis that explains the most variance 
res$rotation

Xt_scale <- scale(Xt, center = res$center, scale = FALSE)
dim(Xt_scale %*% res$rotation)
dim(res$x)

# these are equivalent 
(Xt_scale %*% res$rotation)[1,1:3]
res$x[1, 1:3]

# res$x is the projection of original x data onto the axis which is also called components, with the projection 
# of the data onto the eigenvector associated with the highest eigenvalue being called the principle component
# that is defined by the eigenvectors or components (with the eigenvector that has
# the highest eigenvalue being called the principle component)
# so to reiterate res$x = Xt_scale %*% res$rotation, where res$rotation are the eigenvectors

# we see that the first 3 PC's explain 93% of the variance
summary(res)

pc.use <- 3
?prcomp
# so we take the first three projections as these are the ones that yield the most variance, we can think of each of these principle components
# as the projection of the data points onto one dimension, one dimension for each principle axis (the eigenvectors), so right now we are 
# compressing data from 50 dimensions to 3, as we are only picking the first 3 components 
Xt_scale_projected <- res$x[,1:pc.use]
dim(Xt_scale_projected)

# to reconstruct the data we decompress it from 3 dimensions to 50 dimensions
# to do this, note that each of the 3 dimensions that the projected data is on contain information about the original 50 dimensions, the reason being 
# that they are a linear combination of the 50 dimensions. when we multiply Xt_scale_projected by t(res$rotation[,1:pc.use]) we are multiplying the 
# 3 dimensions (each of which contain info from 50 dimension as mentioned before) by the axis that constructed the 3 dimensions. 
#
# So:
#  | pc1_row1 pc2_row1 pc3_row1       |       | dim1_eigen1 dim2_eigen1 ... dim50_eigen1 |   | pc1_row1 * dim1_eigen1 + pc2_row1 * dim1_eigen2 + pc3_row1 * dim1_eigen3 ... |
#  |           ...                    |  %*%  | dim1_eigen2 dim2_eigen2 ... dim50_eigen2 | = |                            ...                                               |
#  | pc1_row100 pc2_row100 pc3_row100 |       | dim1_eigen3 dim2_eigen3 ... dim50_eigen3 |   |                            ...                                               |
#             
#             100 * 3                                           3 * 50                                                  100 * 50
#
# It is evident here that when we do this we can think of it like each eigenvector holds some information as to how the projected data (in 3 dimensions)
# is to be converted back to it's original dimensionality. Moreover, we can also see that the more eigenvalues (and as a consequence use more dimensions 
# to represent out original data) the more accurately the projected data can be reconstructed

Xt_scale_reconstructed <- Xt_scale_projected %*% t(res$rotation[,1:pc.use])
dim(Xt_scale_reconstructed)
trunc <- res$x[,1:pc.use] %*% t(res$rotation[,1:pc.use])

# scale the data back to the original axis
if(res$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/res$scale)
}
if(res$center != FALSE){
  trunc <- scale(trunc, center = -1 * res$center, scale=FALSE)
}

