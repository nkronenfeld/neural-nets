package ndk.math.linalg

class GradientDescent {
  // Notes from http://digitheadslabnotebook.blogspot.com/2011/10/machine-learning-gradient-descent.html
  //
  // Notation:
  //   m: the number of training examples
  //   n: the number of input features
  //   x:i : the ith variable of the vector X
  //   theta: the transformation matrix
  //   alpha: the learning rate
  //
  // Cost function:
  //   J(theta) = 1/2m sum(i=1->m, (h_theta(x:i) - y:i)^2
  //   d J(theta)/d theta_j = 1/m sum(i=1->m, (h_theta(x:i) - y:i) x_j:i)
  //
  // Gradient Descent (non-matrix form):
  //   repeat until convergence: {
  //     for all j <- 1 to n
  //       theta_j := theta_j - alpha/m sum(i=1->m, (h_theta(x:i) - y:i) x_j:i)
  //   }
  //
  // to matricize:
  //   X is an m x n matrix
  //     m training examples
  //     n features
  //   add a column of all 1s for the intercept term (don't understand why yet)
  //   Y is a m x 1 vector, the expected outputs for each input column of X
  //   Theta is the parameters of our model, an (n+1)x1 vector
  //       but from hereforth, assume n = n+1, X has been expanded as described already
  //
  //   Cost function:
  //     1/2m (X.Theta - Y)^T (X.Theta - Y)
  //   or alternatively:
  //     1/2m (X.Theta - Y)^2 (where each element is squared in place, not matrix multiply)
  //   update rule:
  //     repeat until convergence: {
  //       delta = 1/m X^T (X.Theta - Y)              nxm mx1 = nx1
  //       Theta = Theta - alpha delta
  //     }
  //
  //
  // Now for my own extension to p parameters:
  //   X is an mxn matrix
  //     first column 1
  //     m training examples
  //     n features (including the first feature = 1)
  //   Y is an mxq matrix, the expected output for each input
  //   Theta is the parameters of our model, an nxq matrix
  //
  //   Cost function:
  //     1/2m (X.Theta - Y)^T (X.Theta - Y)             qxm * mxq = qxq? that probably doesn't make sense
  //
  //   update rule:
  //     repeat until convergence: {
  //       delta = 1/m X^T (X.Theta - Y)                nxm mxq = nxq check
  //       Theta = Theta - alpha delta
  //
  //  axb matrix is a rows, b columns
  //  axb * bxc = axc

}
