#' @importFrom Rlibeemd emd_num_imfs emd
#' @importFrom forecast auto.arima forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
emdARIMA <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                    s.num=4L, num.sift=50L){
  n.IMF <- num.IMFs
  AllIMF <- emd(data, num_imfs = n.IMF, S_number = s.num, num_siftings = num.sift)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EMDARIMAFit <- forecast::auto.arima(as.ts(IndIMF))
    EMDARIMA_fcast=forecast::forecast(EMDARIMAFit, h=stepahead)
    EMDARIMA_fcast_Mean=EMDARIMA_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EMDARIMA_fcast_Mean))
  }
  FinalEMDARIMA_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EMDARIMA=mean(abs(data_test - FinalEMDARIMA_fcast))
  MAPE_EMDARIMA=mean(abs(data_test - FinalEMDARIMA_fcast)/data_test)
  rmse_EMDARIMA=sqrt(mean((data_test - FinalEMDARIMA_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEMDARIMA_forecast=FinalEMDARIMA_fcast, MAE_EMDARIMA=MAE_EMDARIMA,
              MAPE_EMDARIMA=MAPE_EMDARIMA, rmse_EMDARIMA=rmse_EMDARIMA,
              AllIMF_plots=AllIMF_plots))
}
