<!-- Component-Wise EM Algorithm for Mixtures -->
## EM algorithm
___
In statistics, an **_expectation–maximization (EM) algorithm_** is an iterative method to find maximum likelihood or maximum a posteriori (MAP) estimates of parameters in statistical models, where the model depends on unobserved latent variables. The EM iteration alternates between performing an expectation (E) step, which creates a function for the expectation of the log-likelihood evaluated using the current estimate for the parameters, and a maximization (M) step, which computes parameters maximizing the expected log-likelihood found on the E step. These parameter-estimates are then used to determine the distribution of the latent variables in the next E step.

![EM1](images/EM1.png)
![EM2](images/EM2.png)

## SVM
___
This model constitutes an adaptation of the classical RBF network where the outputs represent the class conditional distributions. Since the network outputs correspond to probability density functions, training process is treated as maximum likelihood problem and an expectation-maximization (EM) algorithm is proposed for adjusting the network parameters. Experimental results show that proposed architecture exhibits superior classification performance compared to the classical RBF network.

![RBF1](images/RBF1.png)
![RBF2](images/RBF2.png)