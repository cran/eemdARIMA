#' @importFrom Rlibeemd emd_num_imfs eemd
#' @importFrom forecast auto.arima forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

EEMDARIMA <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                     s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2){
  n.IMF <- num.IMFs
  AllIMF <- eemd(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                 S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EEMDARIMAFit <- forecast::auto.arima(as.ts(IndIMF))
    EEMDARIMA_fcast=forecast::forecast(EEMDARIMAFit, h=stepahead)
    EEMDARIMA_fcast_Mean=EEMDARIMA_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDARIMA_fcast_Mean))
  }
  FinalEEMDARIMA_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDARIMA=mean(abs(data_test - FinalEEMDARIMA_fcast))
  MAPE_EEMDARIMA=mean(abs(data_test - FinalEEMDARIMA_fcast)/data_test)
  rmse_EEMDARIMA=sqrt(mean((data_test - FinalEEMDARIMA_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDARIMA_forecast=FinalEEMDARIMA_fcast, MAE_EEMDARIMA=MAE_EEMDARIMA,
              MAPE_EEMDARIMA=MAPE_EEMDARIMA, rmse_EEMDARIMA=rmse_EEMDARIMA,
              AllIMF_plots=AllIMF_plots))
}
