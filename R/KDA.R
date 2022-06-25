units_kda <- function(deploy, model_units)
{

  mom_wts <- c(12, 4, 2, 1)
  kda_symbols <- c('SPY', 'VGK',   'EWJ',  'EEM',  'VNQ',  'RWX',  'IEF',  
    'TLT', 'DBC', 'GLD', 'VWO', 'AGG')  
  empty_vec <- data.frame(t(rep(0, 10))) %>% `names<-`(kda_symbols[1:10])

  rets <- lapply(kda_symbols, function(x) {
    readRDS(file.path('prices', x))[,4] %>%
      clhelpers::calculate_returns() %>%
      `names<-`(x)
  }) %>% 
    `names<-`(kda_symbols) %>%
    do.call(what = xts::merge.xts) %>%
    stats::na.omit() %>%
    utils::tail((-!deploy) + deploy*430) %>%

  ep <- clmodels::clean_endpoints(rets)
   
  lapply(1:(length(ep)-12), function(x) {

    ret_sub <- rets[c((ep[x]+1):ep[(x+12)]),]
    ep_sub <- ep[x:(x+12)]
    six_months <- rets[(ep_sub[7]+1):ep_sub[13],]
    three_months <- rets[(ep_sub[10]+1):ep_sub[13],]
    one_month <- rets[(ep_sub[12]+1):ep_sub[13],]
    ccr <- clhelpers::calculate_cumulative_returns
    moms <- ccr(one_month) * mom_wts[1] + 
      ccr(three_months) * mom_wts[2] + 
      ccr(six_months) * mom_wts[3] + 
      ccr(ret_sub) * mom_wts[4]
    asset_moms <- moms[,1:10]
    cp_moms <- moms[,11:12]
    high_rank_assets <- rank(asset_moms) >= 6
    pos_return_assets <- asset_moms > 0
    selected_assets <- high_rank_assets & pos_return_assets
    invested_assets <- empty_vec

    if (sum(selected_assets) == 1) {

      invested_assets <- invested_assets + selected_assets 

    } else if (sum(selected_assets) != 1 & sum(selected_assets) != 0) {

      idx <- which(selected_assets)
      cors <- (stats::cor(one_month[,idx]) * mom_wts[1] + 
        stats::cor(three_months[,idx]) * mom_wts[2] + 
        stats::cor(six_months[,idx]) * mom_wts[3] + 
        stats::cor(ret_sub[,idx]) * mom_wts[4]) / sum(mom_wts)
      vols <- clhelpers::calculate_sd(one_month[,idx])
      covs <- t(vols) %*% vols * cors
      min_vol_rets <- t(matrix(rep(1, sum(selected_assets))))
      n_col = ncol(covs)
      zero_mat <- array(0, dim = c(n_col, 1))
      one_zero_diagonal_a <- 
        cbind(1, diag(n_col), 1 * diag(n_col), -1 * diag(n_col))
      min_wgt <- rep(.05, n_col)
      max_wgt <- rep(1, n_col)
      bvec_1_vector_a <- c(1, rep(0, n_col), min_wgt, -max_wgt)
      meq_1 <- 1
       
      # solution 1
      mv_port_noshort_a <- quadprog::solve.QP(Dmat = covs, dvec = zero_mat, 
        Amat = one_zero_diagonal_a, bvec = bvec_1_vector_a, meq = meq_1)
       
      min_vol_wt <- mv_port_noshort_a$solution %>% `names<-`(rownames(covs))
       
      # solution 2
      #min_vol_wt <- tseries::portfolio.optim(x=min_vol_rets, covmat = covs)$pw
      #names(min_vol_wt) <- colnames(covs)

      invested_assets[,selected_assets] <- min_vol_wt

    }

    pct_aggro <- mean(cp_moms > 0)
    invested_assets <- invested_assets * pct_aggro 
    pct_cp <- 1 - pct_aggro
     
    if(asset_moms['IEF'] > 0)
      invested_assets['IEF'] <- invested_assets['IEF'] + pct_cp
     
    if (pct_aggro == 1) invested_assets %<>% {. * model_units}
     
    xts::xts(invested_assets, xts::last(zoo::index(ret_sub)))

  }) %>% do.call(what = xts::rbind.xts)

}

units_kda_no_treasury <- function(rets, model_units = 1.5)
{

  mom_wts <- c(12, 4, 2, 1)
  kda_symbols <- c('SPY', 'VGK', 'EWJ', 'EEM', 'VNQ', 'RWX', 'DBC', 'GLD', 
    'VWO', 'AGG')  
  empty_vec <- data.frame(t(rep(0, 8))) %>% `names<-`(kda_symbols[1:8])

  rets <- lapply(kda_symbols, function(x) {
    readRDS(file.path('prices', x))[,4] %>%
      clhelpers::calculate_returns() %>%
      `names<-`(x)
  }) %>% 
    `names<-`(kda_symbols) %>%
    do.call(what = xts::merge.xts) %>%
    stats::na.omit() %>%
    utils::tail((-!deploy) + deploy*430)

  ep <- clmodels::clean_endpoints(rets)

  lapply(1:(length(ep)-12), function(x) {

    ret_sub <- rets[c((ep[x]+1):ep[(x+12)]),]
    ep_sub <- ep[x:(x+12)]
    six_months <- rets[(ep_sub[7]+1):ep_sub[13],]
    three_months <- rets[(ep_sub[10]+1):ep_sub[13],]
    one_month <- rets[(ep_sub[12]+1):ep_sub[13],]
    ccr <- clhelpers::calculate_cumulative_returns
    moms <- ccr(one_month) * mom_wts[1] + 
      ccr(three_months) * mom_wts[2] + 
      ccr(six_months) * mom_wts[3] + 
      ccr(ret_sub) * mom_wts[4]
    asset_moms <- moms[,1:(ncol(moms)-2),drop=FALSE]
    cp_moms <- moms[,(ncol(moms)-1):ncol(moms)]
    selected_assets <- asset_moms > 0
    invested_assets <- empty_vec

    if (sum(selected_assets) == 1) {

      invested_assets <- invested_assets + selected_assets

    } else if (sum(selected_assets) != 1 & sum(selected_assets) != 0) {

      idx <- which(selected_assets)
      cors <- (stats::cor(one_month[,idx]) * mom_wts[1] +
        stats::cor(three_months[,idx]) * mom_wts[2] +
        stats::cor(six_months[,idx]) * mom_wts[3] +
        stats::cor(ret_sub[,idx]) * mom_wts[4]) / sum(mom_wts)
      vols <- clhelpers::calculate_sd(one_month[,idx])
      covs <- t(vols) %*% vols * cors

      min_vol_rets <- t(matrix(rep(1, sum(selected_assets))))
      min_vol_wt <- tseries::portfolio.optim(x=min_vol_rets,covmat=covs)$pw %>%
        `names<-`(colnames(covs))
      invested_assets[,selected_assets] <- min_vol_wt

    }

    pct_aggro <- mean(cp_moms > 0)
    invested_assets %<>% {. * pct_aggro}

    if (pct_aggro == 1) invested_assets %<>% {. * model_units}

    xts::xts(invested_assets, xts::last(zoo::index(ret_sub)))

  }) %>% do.call(what = xts::rbind.xts)

}

#' @importFrom magrittr '%<>%' 
#' @title KDA
#' @description Kipnis Defensive Adaptive Asset Allocation.
#' @param deploy boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE
#' @param model_units numeric, the number of units to allocate to this model, Default: 1.5
#' @param units_fun character, the KDA variant's function name, Default: 'units_kda'
#' @return An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
#' @details See References.
#' @references
#' \url{https://quantstrattrader.com/2019/02/27/kda-robustness-results/}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  ## non-treasury KDA variant
#'  KDA(units_fun = 'units_kda_no_treasury')
#'  }
#' }
#' @export 
#' @importFrom utils tail head
#' @importFrom clhelpers show_special_day
KDA <- function(deploy = TRUE, model_units = 1.5, units_fun = 'units_kda') 
{

  # units_fun can be units_kda or units_kda_no_treasury
  out <- get(units_fun)(deploy, model_units)

  if (deploy) {
    out %<>% 
      utils::tail(2 - any(clhelpers::show_special_day() == 'EOM')) %>% 
      utils::head(1)
  }

  out

}

