units_vim <- function(rets_xts, vix_close_xts, model_units)
{

  n_vect <- c(1:9, seq(10, 95, 5), seq(100, 200, 10))
  mom_wts <- c(12, 4, 2)
  spy_rets_xts <- rets_xts[,'SPY']

  # vix dt ---------------------------------
  vix_dt <- data.table::data.table(vix_close_xts, keep.rownames = TRUE) %>%
    data.table::setnames(2, 'cl')

  pgrid <- expand.grid(gt = n_vect, lt = n_vect) %>%
    .[.$gt > .$lt, ] %>%
    `rownames<-`(seq(nrow(.)))

  for (i in seq(nrow(pgrid))) {
    gt <- pgrid[i,1]
    lt <- pgrid[i,2]
    vix_dt[
      , paste0(gt, '.', lt) :=
        (data.table::shift(TTR::EMA(cl, gt) > TTR::EMA(cl, lt))) %T>%
        { .[!.] <- NA }]
  }

  vix_xts <- vix_dt %>%
    {xts::xts(.[,-c(1:2)], .[,index])} %>%
    xts::merge.xts(spy_rets_xts, join = 'right') %>%
    .[,names(.) != 'SPY'] %>%
    {. * as.numeric(spy_rets_xts)}

  # back to the program ------------------------
  rets <- rets_xts %>% .[,names(.) != 'SPY']
  ep <- clean_endpoints(rets)
  empty_vec <- data.frame(t(rep(0, ncol(rets)))) %>% `names<-`(names(rets))
  ccr <- get('calculate_cumulative_returns')
  asset_units_list <- list()
  vix_rownum_list <- list()

  make_vix_rownum <- function(vix_xts, ep, i)
  {
    vix_subset <- vix_xts[(ep[i]+1):ep[(i+12)], ]

    sharpes <- apply(vix_subset, 2, function(x) {
        mean(x, na.rm = TRUE) / stats::sd(x, na.rm = TRUE)
    }) %>%
      stats::na.omit() %>%
      t

    xts::xts(
      max.col(sharpes, ties.method = 'first'), utils::tail(zoo::index(vix_subset), 1))
  }

  for(i in 1:(length(ep)-12)) {

    ret_subset <- rets[(ep[i]+1):ep[i+12],]
    ep_sub <- ep[i:(i+12)]
    six_months <- rets[(ep_sub[7]+1):ep_sub[13],]
    three_months <- rets[(ep_sub[10]+1):ep_sub[13],]
    one_month <- rets[(ep_sub[12]+1):ep_sub[13],]

    moms <- ccr(one_month)*mom_wts[1] + ccr(three_months)*mom_wts[2] +
      ccr(six_months)*mom_wts[3] + ccr(ret_subset)

    moms %<>% .[ ,!is.na(moms)]
    top_asset <- utils::tail(sort(moms), 1) %>% .[. > 0]
    bot_asset <- utils::head(sort(moms), 1) %>% .[. <= 0]
    selected_assets <- unique(names(c(top_asset, bot_asset)))

    if (length(selected_assets) == 0) {
      invested_assets <- empty_vec
    } else if (length(selected_assets) == 1) {
      invested_assets <- empty_vec
      invested_assets[ ,selected_assets] <- model_units
    } else {
      cors <- (stats::cor(one_month[,selected_assets])*mom_wts[1] +
                 stats::cor(three_months[,selected_assets])*mom_wts[2] +
                 stats::cor(six_months[,selected_assets])*mom_wts[3] +
                 stats::cor(ret_subset[,selected_assets])) / sum(mom_wts)
      vols <- clhelpers::calculate_sd(one_month[,selected_assets])
      covs <- t(vols) %*% vols * cors
      min_vol_wt <-
        tseries::portfolio.optim(t(rep(1, 2)), covmat = covs)$pw * model_units
      names(min_vol_wt) <- colnames(covs)
      invested_assets <- empty_vec
      invested_assets[,selected_assets] <- min_vol_wt
    }

    asset_units_list[[i]] <- xts::xts(
      invested_assets, utils::tail(zoo::index(ret_subset), 1))
    vix_rownum_list[[i]] <- make_vix_rownum(vix_xts, ep, i)

  }

  all_asset_units <- do.call(rbind, asset_units_list)
  all_vix_rownums <- do.call(rbind, vix_rownum_list)

  list(all_asset_units, all_vix_rownums, pgrid, vix_dt)

}

.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(
    c('.', 'cl', '.SD', '.N', 'Sig', 'clhelpers::make_rets_xts', 'Symbol', 
      'Date_Range', 'sig'), 
    utils::packageName()) 
 
#' @importFrom magrittr '%$%' '%<>%' '%T>%' 
#' @importFrom data.table ':=' 
#' @title VIM
#' @description VIx Momentum
#' @param deploy boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE
#' @param model_units, how many units to allocate to this model
#' @return An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
#' @details The same momentum sorter from CAN selects 1 best and 1 worst Dow component for the month.  Long entry determined by EMA-based VIX filter.
#' @export 
#' @importFrom tidyquant tq_index
#' @importFrom data.table data.table setnames melt merge.data.table
#' @importFrom TTR EMA
#' @importFrom clhelpers scrape_wiki_dow_components
#' @importFrom xts merge.xts xts
#' @importFrom zoo index na.locf
#' @importFrom stats lag
VIM <- function(deploy = TRUE, model_units = 1)
{

  vix_close_xts <- clhelpers::make_closes_xts('^VIX', -1)

  if (deploy) {

    rets <- c(tidyquant::tq_index('DOW')$symbol, 'SPY') %>%
      sort %>%
      clhelpers::make_rets_xts()

    units_and_vix_list <- units_vim(rets, vix_close_xts, model_units) %T>%
      {
        .[1:2] %<>% lapply(function(x) {
          utils::tail(x, 2 - any(clhelpers::show_special_day() == 'EOM')) %>% utils::head(1)
        })
        rownum <- .[[2]]
        .[[3]] %<>% .[rownum, ]
      }

    pgrid <- units_and_vix_list[[3]]
    vix_dt <- data.table::data.table(vix_close_xts, keep.rownames = TRUE) %>%
      data.table::setnames(2, 'cl')

    vix_sig <- vix_dt[
      , .(Sig = TTR::EMA(cl, pgrid[,1]) > TTR::EMA(cl, pgrid[,2]))][
      , .SD[.N]][
      , as.numeric(Sig)]

    units_and_vix_list[[1]] * vix_sig

  } else {

    wiki_data <- clhelpers::scrape_wiki_dow_components()
    rets_xts <- clhelpers::make_rets_xts(unique(wiki_data$Symbol), -1) %>%
      lapply(function(x) {
        x[ wiki_data[Symbol == names(x), 2][ ,Date_Range] ]
      }) %>%
      do.call(what = xts::merge.xts) %>%
      xts::merge.xts(clhelpers::make_rets_xts('SPY', -1))

    units_and_vix_list <- units_vim(rets_xts, vix_close_xts, model_units)

    vix_best_col <- units_and_vix_list[[2]]
    vix_dt <- units_and_vix_list[[4]]

    vix_dt_long <- vix_dt[,!c('index', 'cl')] %>% 
      `names<-`(as.character(seq(ncol(vix_dt)-2))) %>%
      cbind(index = vix_dt[,index], .) %>%
      data.table::melt(id.vars = 'index', variable.name = 'vix_best_col', 
        value.name = 'sig') %>%
      .[, vix_best_col := as.numeric(vix_best_col)]
      
    units_and_vix_sig <- xts::xts(vix_dt[,cl], vix_dt[,index]) %>%
      xts::merge.xts(vix_best_col) %>%
      .[,2] %>%
      .[paste0(utils::head(zoo::index(vix_best_col), 1), '/')] %>%
      stats::lag() %>%
      zoo::na.locf(na.rm = TRUE) %>%
      data.table::data.table(keep.rownames = TRUE) %>%
      data.table::merge.data.table(vix_dt_long) %$%
      xts::xts(sig, index) %T>%
      {.[is.na(.)] <- FALSE} %>%
      xts::merge.xts(units_and_vix_list[[1]], all = TRUE) %>%
      zoo::na.locf(na.rm = TRUE)

    asset_units <- units_and_vix_sig[,-1]
    vix_sig <- as.numeric(units_and_vix_sig[,1])
    asset_units * vix_sig

  }

}

