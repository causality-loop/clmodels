#' @importFrom magrittr '%T>%' 
#' @title GAN
#' @description As in GANn, the trader.
#' @param deploy boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE
#' @param model_units, how many units to allocate to this model
#' @return An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
#' @details 3 weeks down, 3 weeks up.
#' @export 
#' @importFrom quantmod Lag
#' @importFrom dplyr coalesce
#' @importFrom zoo na.locf
#' @importFrom xts reclass
#' @importFrom utils head
GAN <- function(deploy = TRUE, model_units = 1)
{

  asset_symbols <- c('SPY', 'DIA', 'QQQ')
  rets <- clhelpers::make_closes_xts(asset_symbols, 70*deploy + -1*!deploy) %>% 
    clhelpers::calculate_returns('week')

  gan_units <- lapply(names(rets), function(x) {
    rets[,x] %>%
      {cbind(sign(.), quantmod::Lag(sign(.), 1:2))} %>%
      {rowSums(.) == -3} %>%
      {c(NA, diff(.) > 0)} %T>% 
      {.[!.] <- NA} %>% 
      { list( ., c(rep(NA, 3), utils::head(., -3)) %T>% {.[.==1] <- 0}) } %>%
      dplyr::coalesce(!!!.) %>%
      zoo::na.locf(na.rm = FALSE) %T>% 
      {.[is.na(.)] <- 0}
  }) %>% 
    do.call(what = cbind) %>%
    xts::reclass(rets) %>%
    `names<-`(names(rets))

  out <- sapply(1:ncol(gan_units), function(x) {
    as.numeric(x == apply(
      gan_units, 
      1, 
      function(x) max.col(t(x), 'first') * as.numeric(sum(x) != 0)))
  }) %>% 
    xts::reclass(gan_units) %>% 
    {. * model_units} %>%
    `names<-`(names(gan_units))

  if (deploy) out %>% utils::tail(2 - any(clhelpers::show_special_day() == 'EOW')) %>% utils::head(1)
  else out

}

