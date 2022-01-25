\name{emdARIMA}
\alias{emdARIMA}
\title{
Empirical Mode Decomposition Based ARIMA Model
}
\description{
The emdARIMA function gives forecasted value of Empirical Mode Decomposition based ARIMA Model with different forecasting evaluation criteria.
}
\usage{
emdARIMA(data, stepahead=10,
num.IMFs=emd_num_imfs(length(data)),
s.num=4L, num.sift=50L)
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
}
\details{
This function firstly, decompose the nonlinear and nonstationary time series into several independent intrinsic mode functions (IMFs) and one residual component (Huang et al., 1998). Secondly, ARIMA is used to forecast these IMFs and residual component individually. Finally, the prediction results of all IMFs including residual are aggregated to form the final forecasted value for given input time series.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalEMDARIMA_forecast }{Final forecasted value of the EMD based ARIMA model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_EMDARIMA }{Mean Absolute Error (MAE) for EMD based ARIMA model.}
  \item{MAPE_EMDARIMA }{Mean Absolute Percentage Error (MAPE) for EMD based ARIMA model.}
  \item{rmse_EMDARIMA }{Root Mean Square Error (RMSE) for EMD based ARIMA model.}
}
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Huang, N.E., Shen, Z., Long, S.R., Wu, M.C., Shih, H.H., Zheng, Q. and Liu, H.H. (1998) The empirical mode decomposition and the Hilbert spectrum for nonlinear and non stationary time series analysis. In Proceedings of the Royal Society of London A: mathematical, physical and engineering sciences. 454, 903--995.

Jha, G.K. and Sinha, K. (2014) Time delay neural networks for time series prediction: An application to the monthly wholesale price of oilseeds in India. Neural Computing and Applications, 24, 563--571.
}
\seealso{
EEMDARIMA
}
\examples{
\donttest{
data("Data_Maize")
emdARIMA(Data_Maize)
}
}
\keyword{emd}
