## M3 Forecasting in WindView
The Machine Learning-based Multi-Model (M3) forecasting framework is able to generate both short-term deterministic and probabilistic forecasts. The deterministic forecasts are generated by a two-layer machine learning based method. Multiple machine learning algorithms with several kernels generate forecasts individually in the first layer. Then the forecasts are blended by a machine learning algorithm in the second layer, which gives the final forecasts. Machine learning algorithms used in M3 deterministic forecasts include artificial neural networks, support vector regression, gradient boosting machines, and random forests.

The M3 probabilistic forecasts are generated based on the M3 deterministic forecasts and pinball loss optimization. The M3 deterministic forecast is assumed to be the mean value of the predictive distribution at each forecasting time stamp. Then, the optimal unknown parameter (i.e., standard deviation) of the predictive distribution is estimated by minimizing the pinball loss. Finally, probabilistic forecasts are generated from the predictive distribution.

## WindView 
WindView is wind power visualization software which enables power system operators to better understand static and forecast information on their power system.
https://github.com/windview


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Once R and RStudio are installed. Open R or RStudio and install the following packages.

```
install.packages('gbm)
```

## Running the tests

Source the excutive_function.R code to run a test case.

## Authors

* **Cong Feng** - *UT Dallas*
* **Mucun Sun** - *UT Dallas*
* **Jie Zhang** - *UT Dallas*

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

