\name{EEMDARIMA}
\alias{EEMDARIMA}
\title{
Ensemble Empirical Mode Decomposition Based ARIMA Model
}
\description{
The EEMDARIMA function computes forecasted value with different forecasting evaluation criteria for Ensemble Empirical Mode Decomposition based ARIMA Model.
}
\usage{
EEMDARIMA(data, stepahead=10,
num.IMFs=emd_num_imfs(length(data)), s.num=4L,
num.sift=50L, ensem.size=250L, noise.st=0.2)
}
\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
  \item{num.IMFs}{
Number of Intrinsic Mode Function (IMF) for input series.
}
  \item{s.num}{
Integer. Use the S number stopping criterion for the EMD procedure with the given values of S. That is, iterate until the number of extrema and zero crossings in the signal differ at most by one, and stay the same for S consecutive iterations.
}
  \item{num.sift}{
Number of siftings to find out IMFs.
}
  \item{ensem.size}{
Number of copies of the input signal to use as the ensemble.
}
  \item{noise.st}{
Standard deviation of the Gaussian random numbers used as additional noise. This value is relative to the standard deviation of the input series.
}
}
\details{
To overcome the problem of mode mixing in EMD decomposition technique, Ensemble Empirical Mode Decomposition (EEMD) method was developed by Wu and Huang (2009). EEMD significantly reduces the chance of mode mixing and represents a substantial improvement over the original EMD.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalEEMDARIMA_forecast }{Final forecasted value of the EEMD based ARIMA model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_EEMDARIMA }{Mean Absolute Error (MAE) for EEMD based ARIMA model.}
  \item{MAPE_EEMDARIMA }{Mean Absolute Percentage Error (MAPE) for EEMD based ARIMA model.}
  \item{rmse_EEMDARIMA }{Root Mean Square Error (RMSE) for EEMD based ARIMA model.}
}
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1--41.
}
\seealso{
emdARIMA
}
\examples{
\donttest{
Data("Data_Maize")
EEMDARIMA(Data_Maize)
}
}
\keyword{eemd}
