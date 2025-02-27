library(tidyverse)
library(progressr)
library(furrr)
library(DirectEffects)
handlers(global = TRUE)
handlers("progress")


expand_bases <- function(vars) {
      sq_terms <- paste0("I(", vars, " ^ 2)")
      int_terms <- outer(vars, vars, paste, sep = " * ")
      int_terms <- int_terms[upper.tri(int_terms)]
      bases <- c(vars, sq_terms, int_terms)
      return(bases)
}


simulation_de_ml <- function(n, t, m0, m1, beta0, rho_1, rho_2, rho_3, rho_4,
                             sims, specification, n_splits = 100, n_boots = 500){


  methods <- c("logit", "lasso", "rf", "out_reg", "ipw")
  qois <- c("marg_est", "marg_se", "cond0_est", "cond0_se", "cond1_est", "cond1_se")
  cols <- apply(expand.grid(methods, qois), 1, paste0, collapse = "_")
  
  results <- data.frame(logit_marg_est = rep(NA, sims),
                        logit_marg_se = rep(NA, sims),
                        logit_cond0_est = rep(NA, sims),
                        logit_cond0_se = rep(NA, sims),
                        logit_cond1_est = rep(NA, sims),
                        logit_cond1_se = rep(NA, sims),
                        lasso_marg_est = rep(NA, sims),
                        lasso_marg_se = rep(NA, sims),
                        lasso_cond0_est = rep(NA, sims),
                        lasso_cond0_se = rep(NA, sims),
                        lasso_cond1_est = rep(NA, sims),
                        lasso_cond1_se = rep(NA, sims),
                        rf_marg_est = rep(NA, sims),
                        rf_marg_se = rep(NA, sims),
                        rf_cond0_est = rep(NA, sims),
                        rf_cond0_se = rep(NA, sims),
                        rf_cond1_est = rep(NA, sims),
                        rf_cond1_se = rep(NA, sims),
                        true_marg = rep(NA, sims),
                        true_0 = rep(NA, sims),
                        true_1 = rep(NA, sims),
                        did_no_z_est = rep(NA, sims),
                        did_no_z_se = rep(NA, sims),
                        did_with_z_est = rep(NA, sims),
                        did_with_z_se = rep(NA, sims),
                        out_reg_marg_est = rep(NA, sims),
                        out_reg_marg_se = rep(NA, sims),
                        out_reg_cond0_est = rep(NA, sims),
                        out_reg_cond0_se = rep(NA, sims),
                        out_reg_cond1_est = rep(NA, sims),
                        out_reg_cond1_se = rep(NA, sims),
                        ipw_marg_est = rep(NA, sims),
                        ipw_marg_se = rep(NA, sims),
                        ipw_cond0_est = rep(NA, sims),
                        ipw_cond0_se = rep(NA, sims),
                        ipw_cond1_est = rep(NA, sims),
                        ipw_cond1_se = rep(NA, sims),
                        m_value = rep(NA, sims),
                        specification = rep(specification, sims),
                        N = rep(n, sims))
    
  for (s in 1:sims) {
    p()
    if (s %% 10 == 0) cat(s, " ")
    if (s %% 50 == 0) cat("\n")
    
    u1 <- rnorm(n, 0, 0.1)
    u2 <- rnorm(n, 0, 0.1)
    u3 <- rnorm(n, 0, 0.1)
    u4 <- rnorm(n, 0, 0.1)
    
    x1 <- rnorm(n, 0, 0.1)
    x2 <- rnorm(n, 0, 0.1)
    x1_star <- (exp(x1/2) - 1)*2
    x2_star <- x2 / (1 + exp(x1)) + 10 ##x1*x2*10
    
    t_assign <- as.numeric(u3 > 0)
    
    z1_0 <- 5 * u2 + rnorm(n, 0, 0.2)
    z1_1 <- z1_0 + 0.25 + rnorm(n, 0, 0.05)
    z2_0 <- 5 * u4 + rnorm(n, 0, 0.2)
    z2_1 <- z2_0 + 0.25 + rnorm(n, 0, 0.05)
    z1 <- ifelse(t_assign == 1, z1_1, z1_0)
    z2 <- ifelse(t_assign == 1, z2_1, z2_0)
    z1_star <- (x1 * z1 / 25 + 0.6)^3 
    z2_star <- (x2 + z2 + 20) ^ 2
    
    m0_lp <- x1 + x2 + rnorm(n, 0, 0.1)
    m0_assign <- as.numeric(m0_lp > 0)
    m_lp <- -1 + rho_2 * t_assign + 0.4 * m0_assign +  rho_4/2 * z1 + rho_4/2 * z2
    m_assign <- as.numeric((m_lp + rnorm(n, 0, 1)) > 0)

    true_ps <- pnorm(-m_lp, lower.tail = FALSE)
    y0 <- beta0 + 0.4 * m0_assign + 0.5 * x1 + 0.5 * x2 + rho_1 * t_assign + u1

    y1_00 <- y0 + rho_3*t_assign + 0.4 * m0_assign + 5 * u4 + 5 * u2 + rnorm(n, 0, 0.1)
    y1_10 <- y0 + t + rho_3*t_assign + 0.4 * m0_assign + 5 * u4  + 5 * u2 + rnorm(n, 0, 0.1)
    y1_01 <- y0 + m0 + rho_3*t_assign + 0.4 * m0_assign + 5 * u4 + 5 * u2 + rnorm(n, 0, 0.1)
    y1_11 <- y0 + t + m1 + rho_3*t_assign + 0.4 * m0_assign + 5 * u4  + 5 * u2 + rnorm(n, 0, 0.1)

    y1 <- y1_00 * (1 - t_assign) * (1 - m_assign) +
      y1_10 * t_assign * (1 - m_assign) +
      y1_01 * (1 - t_assign) * m_assign +
      y1_11 * t_assign * m_assign

    results$true_0[s] <- mean(c(y1_10 - y1_00)[m0_assign == 0])
    results$true_1[s] <- mean(c(y1_11 - y1_01)[m0_assign == 1])
    results$true_marg[s] <- results$true_0[s] * mean(m0_assign == 0) + 
      results$true_1[s]*mean(m0_assign == 1)


    dat <- data.frame(y1 = y1, y0 = y0, y_diff = y1 - y0, d = t_assign, m = m_assign, m0 = m0_assign,
                      x1_or = scale(x1), x2_or = scale(x2), z1_or = scale(z1), z2_or = scale(z2),
                      x1_ps = scale(x1), x2_ps = scale(x2), z1_ps = scale(z1), z2_ps = scale(z2))

    or_correct <- FALSE
    ps_correct <- FALSE
    if (specification == "Correct") {
      or_correct <- TRUE
      ps_correct <- TRUE
    } else if (specification == "Incorrect Propensity Score") {
      or_correct <- TRUE
    } else if (specification == "Incorrect Outcome Regressions") {
      ps_correct <- TRUE
    } 


    ## Overwrite covariates if misspecified
    if (!or_correct) {
      dat$x1_or <- scale(x1_star)
      dat$x2_or <- scale(x2_star)
      dat$z1_or <- scale(z1_star)
      dat$z2_or <- scale(z2_star)
    }

    if (!ps_correct) {
      dat$x1_ps <- scale(x1_star)
      dat$x2_ps <- scale(x2_star)
      dat$z1_ps <- scale(z1_star)
      dat$z2_ps <- scale(z2_star)
    }

    ## Create basis expansions (list of squared terms and interactions)
    or_bases_xz <- expand_bases(c("x1_or", "x2_or", "z1_or", "z2_or"))
    or_bases_x <- expand_bases(c("x1_or", "x2_or"))
    ps_bases_xz <- expand_bases(c("x1_ps", "x2_ps", "z1_ps", "z2_ps"))

    ## create formulas
    or_xz_form_expanded <- reformulate(c("d", "m", "m0", or_bases_xz))
    or_x_form_expanded <- reformulate(c("d", "m0", or_bases_x))
    ps_xz_form_expanded <- reformulate(c("d", "m0", ps_bases_xz), "m")
    
    or_xz_form <- reformulate(c("d", "m", "m0", "x1_or", "x2_or", "z1_or", "z2_or"))
    or_x_form <- reformulate(c("d", "m0", "x1_or", "x2_or"))
    ps_xz_form <- reformulate(c("d", "m0", "x1_ps", "x2_ps", "z1_ps", "z2_ps"), "m")


    ## Logit 
    my_did <- cde_did_aipw(base_mediator = m0, trim = c(0.01, 0.99), aipw_blip = FALSE) |>
        set_treatment(d, ~ m0) |>
        treat_model(engine = "logit") |>
        outreg_model(engine = "lm", formula =  ~ d +  m0, separate = FALSE) |>
        set_treatment(m) |>
        treat_model(engine = "logit", formula = ps_xz_form, separate = FALSE) |>
        outreg_model(engine = "lm", formula =  or_xz_form, separate = FALSE) |>
        estimate(y_diff ~ d + m, data = dat, n_folds = 5, n_splits = n_splits)
      results[s, c('logit_cond0_est', 'logit_cond1_est', 'logit_marg_est')] <- my_did$estimates$estimate
      results[s, c('logit_cond0_se', 'logit_cond1_se', 'logit_marg_se')] <- my_did$estimates$std.err

    ## ## LASSO
    my_did_lasso <- cde_did_aipw(base_mediator = m0, trim = c(0.01, 0.99), aipw_blip = FALSE) |>
        set_treatment(d) |>
        treat_model(engine = "logit", formula = d ~ m0) |>
        outreg_model(engine = "lm", formula =  ~d +  m0, separate = FALSE) |>
        set_treatment(m) |>
        treat_model(engine = "rlasso_logit", formula = ps_xz_form_expanded, separate = FALSE) |>
        outreg_model(engine = "rlasso", formula = or_xz_form_expanded, separate = FALSE) |>
        estimate(y_diff ~ d + m, data = dat, n_folds = 5, n_splits = n_splits)
      results[s, c('lasso_cond0_est', 'lasso_cond1_est', 'lasso_marg_est')] <- my_did_lasso$estimates$estimate
      results[s, c('lasso_cond0_se', 'lasso_cond1_se', 'lasso_marg_se')] <- my_did_lasso$estimates$std.err

    ## ## Random Forests
    my_did_rf <- cde_did_aipw(base_mediator = m0, trim = c(0.01, 0.99), aipw_blip = FALSE) |>
        set_treatment(d) |>
        treat_model(engine = "logit", formula = d ~ m0) |>
        outreg_model(engine = "lm", formula =  ~ d + m0, separate = FALSE) |>
        set_treatment(m) |>
        treat_model(engine = "ranger_class", formula = ps_xz_form, separate = FALSE) |>
        outreg_model(engine = "ranger_reg", formula =  or_xz_form, separate = FALSE) |>
        estimate(y_diff ~ d + m, data = dat, n_folds = 5, n_splits = n_splits)
      results[s, c('rf_cond0_est', 'rf_cond1_est', 'rf_marg_est')] <- my_did_rf$estimates$estimate
      results[s, c('rf_cond0_se', 'rf_cond1_se', 'rf_marg_se')] <- my_did_rf$estimates$std.err


    did_no_z <- lm(update(or_x_form, y_diff ~ . + m), data = dat)
    results$did_no_z_est[s] <- coef(did_no_z)["d"]
    results$did_no_z_se[s] <- sqrt(vcov(did_no_z)["d", "d"])
      
    did_with_z <- lm(update(or_xz_form, y_diff ~ .), data = dat)
    results$did_with_z_est[s] <- coef(did_with_z)["d"]
    results$did_with_z_se[s] <- sqrt(vcov(did_with_z)["d", "d"])

    dat_0 <- dat
    dat_0$m <- 0
    dat_1 <- dat
    dat_1$m <- 1
    dat$long_reg_0 <- predict(did_with_z, dat_0)
    dat$long_reg_1 <- predict(did_with_z, dat_1)
    short_reg_0 <- lm(update(or_x_form, long_reg_0 ~ .), data = dat)
    short_reg_1 <- lm(update(or_x_form, long_reg_1 ~ .), data = dat)
    dat_d0 <- dat
    dat_d0$d <- 0
    dat_d1 <- dat
    dat_d1$d <- 1
    tau_0_out <- mean(predict(short_reg_0, dat_d1)[dat$m0 == 0] - predict(short_reg_0, dat_d0)[dat$m0 == 0])
    tau_1_out <- mean(predict(short_reg_1, dat_d1)[dat$m0 == 1] - predict(short_reg_1, dat_d0)[dat$m0 == 1])
    tau_marg_out <- mean(dat$m0)*tau_1_out + (1 - mean(dat$m0))*tau_0_out
    boot_0_out <- rep(NA, n_boots)
    boot_1_out <- rep(NA, n_boots)
    boot_marg_out <- rep(NA, n_boots)
      for (i in seq_len(n_boots)){
        rows <- sample(1:n, n, replace = TRUE)
        dat_b <- dat[rows, ]
        long_fit_b <- lm(update(or_xz_form, y_diff ~ .), data = dat_b)
        dat_b_0 <- dat_b
        dat_b_0$m <- 0
        dat_b_1 <- dat_b
        dat_b_1$m <- 1
        dat_b$long_reg_0 <- predict(long_fit_b, dat_b_0)
        dat_b$long_reg_1 <- predict(long_fit_b, dat_b_1)        
        short_reg_b_0 <- lm(update(or_x_form, long_reg_0 ~ .), data = dat_b)
        short_reg_b_1 <- lm(update(or_x_form, long_reg_1 ~ .), data = dat_b)
        dat_b_d0 <- dat_b
        dat_b_d0$d <- 0
        dat_b_d1 <- dat_b
        dat_b_d1$d <- 1
        b_0 <- mean(predict(short_reg_b_0, dat_b_d1)[dat_b$m0 == 0] - predict(short_reg_b_0, dat_b_d0)[dat_b$m0 == 0])
        b_1 <- mean(predict(short_reg_b_1, dat_b_d1)[dat_b$m0 == 1] - predict(short_reg_b_1, dat_b_d0)[dat_b$m0 == 1])
        b_marg <- mean(dat_b$m0)*b_1 + (1 - mean(dat_b$m0))*b_0
        boot_0_out[i] <- b_0
        boot_1_out[i] <- b_1
        boot_marg_out[i] <- b_marg
      }
      results$out_reg_cond0_est[s] <- tau_0_out
      results$out_reg_cond1_est[s] <- tau_1_out
      results$out_reg_marg_est[s] <- tau_marg_out
      results$out_reg_cond0_se[s] <- sd(boot_0_out)
      results$out_reg_cond1_se[s] <- sd(boot_1_out)
      results$out_reg_marg_se[s] <- sd(boot_marg_out)

      prop_score <- glm(ps_xz_form, family = binomial(link = "logit"), data = dat)
      preds_0 <- predict(prop_score, dat_d0)
      dat_d0$pi <- plogis(preds_0)
      pi_0_1 <- quantile(dat_d0$pi, 0.01)
      if (pi_0_1 == 0 & min(dat_d0$pi[dat_d0$pi > 0]) != 1) {pi_0_1 <- min(dat_d0$pi[dat_d0$pi > 0])}
      pi_0_99 <- quantile(dat_d0$pi, 0.99)
      if (pi_0_99 == 1 & max(dat_d0$pi[dat_d0$pi < 1]) != 0){pi_0_99 <- max(dat_d0$pi[dat_d0$pi < 1])}
      dat_d0$pi <- ifelse(dat_d0$pi > pi_0_99, pi_0_99, dat_d0$pi)
      dat_d0$pi <- ifelse(dat_d0$pi < pi_0_1, pi_0_1, dat_d0$pi)
      preds_1 <- predict(prop_score, dat_d1)
      dat_d1$pi <- plogis(preds_1)
      pi_1_1 <- quantile(dat_d1$pi, 0.01)
      if (pi_1_1 == 0 & min(dat_d1$pi[dat_d1$pi > 0]) != 1) {pi_1_1 <- min(dat_d1$pi[dat_d1$pi > 0])}
      pi_1_99 <- quantile(dat_d1$pi, 0.99)
      if (pi_1_99 == 1 & max(dat_d1$pi[dat_d1$pi < 1]) != 0){pi_1_99 <- max(dat_d1$pi[dat_d1$pi < 1])}
      dat_d1$pi <- ifelse(dat_d1$pi > pi_1_99, pi_1_99, dat_d1$pi)
      dat_d1$pi <- ifelse(dat_d1$pi < pi_1_1, pi_1_1, dat_d1$pi)
      delta <- mean(dat$d)
      weights_0 <- dat$d*(1 - dat$m)/(delta*(1 - dat_d1$pi)) - (1 - dat$d)*(1 - dat$m)/((1 - delta)*(1 - dat_d0$pi))
      #weights_0 <- ifelse(weights_0 > 10, 10, weights_0)
      #weights_0 <- ifelse(weights_0 < -10, -10, weights_0)
      tau_0_ipw <- mean((weights_0*dat$y_diff)[dat$m0 == 0])
      weights_1 <- dat$d*dat$m/(delta*dat_d1$pi) - (1 - dat$d)*dat$m/((1 - delta)*dat_d0$pi)
      #weights_1 <- ifelse(weights_1 > 10, 10, weights_1)
      #weights_1 <- ifelse(weights_1 < -10, -10, weights_1)
      tau_1_ipw <- mean((weights_1*dat$y_diff)[dat$m0 == 1])
      tau_marg_ipw <- mean(dat$m0)*tau_1_ipw + (1 - mean(dat$m0))*tau_0_ipw
      boot_0_ipw <- rep(NA, n_boots)
      boot_1_ipw <- rep(NA, n_boots)
      boot_marg_ipw <- rep(NA, n_boots)
      for (i in seq_len(n_boots)){
        rows <- sample(1:n, n, replace = TRUE)
        dat_b <- dat[rows, ]
        dat_b_d0 <- dat_b
        dat_b_d0$d <- 0
        dat_b_d1 <- dat_b
        dat_b_d1$d <- 1
        prop_score_b <- glm(ps_xz_form, family = binomial(link = "logit"), data = dat_b)
        preds_b_0 <- predict(prop_score_b, dat_b_d0)
        dat_b_d0$pi <- plogis(preds_b_0)
        pi_b_0_1 <- quantile(dat_b_d0$pi, 0.01)
        if (pi_b_0_1 == 0 & min(dat_b_d0$pi[dat_b_d0$pi > 0]) != 1) {pi_b_0_1 <- min(dat_b_d0$pi[dat_b_d0$pi > 0])}
        pi_b_0_99 <- quantile(dat_b_d0$pi, 0.99)
        if (pi_b_0_99 == 1 & max(dat_b_d0$pi[dat_b_d0$pi < 1]) != 0){pi_b_0_99 <- max(dat_b_d0$pi[dat_b_d0$pi < 1])}
        dat_b_d0$pi <- ifelse(dat_b_d0$pi > pi_b_0_99, pi_b_0_99, dat_b_d0$pi)
        dat_b_d0$pi <- ifelse(dat_b_d0$pi < pi_b_0_1, pi_b_0_1, dat_b_d0$pi)
        preds_b_1 <- predict(prop_score_b, dat_b_d1)
        dat_b_d1$pi <- plogis(preds_b_1)
        pi_b_1_1 <- quantile(dat_b_d1$pi, 0.01)
        if (pi_b_1_1 == 0 & min(dat_b_d1$pi[dat_b_d1$pi > 0]) != 1) {pi_b_1_1 <- min(dat_b_d1$pi[dat_b_d1$pi > 0])}
        pi_b_1_99 <- quantile(dat_b_d1$pi, 0.99)
        if (pi_b_1_99 == 1 & max(dat_b_d1$pi[dat_b_d1$pi < 1]) != 0){pi_b_1_99 <- max(dat_b_d1$pi[dat_b_d1$pi < 1])}
        dat_b_d1$pi <- ifelse(dat_b_d1$pi > pi_b_1_99, pi_b_1_99, dat_b_d1$pi)
        dat_b_d1$pi <- ifelse(dat_b_d1$pi < pi_b_1_1, pi_b_1_1, dat_b_d1$pi)
        delta_b <- mean(dat_b$d)
        weights_b_0 <- dat_b$d*(1 - dat_b$m)/(delta_b*(1 - dat_b_d1$pi)) - (1 - dat_b$d)*(1 - dat_b$m)/((1 - delta_b)*(1 - dat_b_d0$pi))
        #weights_b_0 <- ifelse(weights_b_0 > 10, 10, weights_b_0)
        #weights_b_0 <- ifelse(weights_b_0 < -10, -10, weights_b_0)
        weights_b_1 <- dat_b$d*dat_b$m/(delta_b*dat_b_d1$pi) - (1 - dat_b$d)*dat_b$m/((1 - delta_b)*dat_b_d0$pi)
        #weights_b_1 <- ifelse(weights_b_1 > 10, 10, weights_b_1)
        #weights_b_1 <- ifelse(weights_b_1 < -10, -10, weights_b_1)
        b_0 <- mean(((weights_b_0)*dat_b$y_diff)[dat_b$m0 == 0])
        b_1 <- mean(((weights_b_1)*dat_b$y_diff)[dat_b$m0 == 1])
        b_marg <- mean(dat_b$m0)*b_1 + (1 - mean(dat_b$m0))*b_0
        boot_0_ipw[i] <- b_0
        boot_1_ipw[i] <- b_1
        boot_marg_ipw[i] <- b_marg
      }
      results$ipw_cond0_est[s] <- tau_0_ipw
      results$ipw_cond1_est[s] <- tau_1_ipw
      results$ipw_marg_est[s] <- tau_marg_ipw
      results$ipw_cond0_se[s] <- sd(boot_0_ipw)
      results$ipw_cond1_se[s] <- sd(boot_1_ipw)
      results$ipw_marg_se[s] <- sd(boot_marg_ipw)
  }

  
  return(results)
}


set.seed(12345)
Ns <- c(250, 500, 1000)
sims <- 1000
Specification <- c('Correct', 'Both Incorrect', 'Incorrect Propensity Score', 'Incorrect Outcome Regressions')
specs <- expand.grid(
  n = Ns,
  specification = Specification,
  sims = rep(sims / 10, 10)
)

future::plan("multisession")
cat("Number of cores: ", parallelly::availableCores())
cat("Number of workers: ", future::nbrOfWorkers())
aa <- proc.time()
with_progress({
  p <- progressor(nrow(specs) * sims / 10)
  results <- furrr::future_pmap(
    specs, simulation_de_ml,
    t = 0.2, m0 = 0.3, m1 = 0.3, beta0 = 1, rho_1 = 0, rho_2 = 1.5,
    rho_3 = 0, rho_4 = 1.5, n_splits = 10, n_boots = 500,
    .options = furrr::furrr_options(seed = TRUE)
  )
})
bb <- proc.time()
runtime <- (bb - aa)[3]/60  # Convert to minutes
cat("Runtime: ", runtime, "\n")

results <- Reduce(rbind, results)

saveRDS(results, file = "results/sim-out.rds")
results <- readRDS(file = "results/sim-out.rds")


tmp <- results %>% group_by(specification, N) %>%
  dplyr::select(ends_with("_est") | ends_with("_se"), true_marg) %>%
  mutate(truth = mean(true_marg)) %>%
  pivot_longer(ends_with("_est") | ends_with("_se"), names_to = c("Method", ".value"), names_pattern = "(.*)_(est|se)") %>%
  group_by(specification, N, Method) %>%
  summarize(
    bias = abs(mean(est - truth, na.rm = T)),
    rmse = sqrt(mean((est - truth) ^ 2, na.rm = T)),
    coverage = mean(est - qnorm(0.975) * se <= truth &
                      est + qnorm(0.975) * se >= truth, na.rm = T),
  ) %>%
  filter(Method %in% c("logit_marg", "lasso_marg", "rf_marg", "did_no_z", "did_with_z", "out_reg_marg", "ipw_marg")) %>%
  mutate(
    Method = recode(Method,
      logit_marg = "DR ACDE Estimator (Logit)",
      lasso_marg = "DR ACDE Estimator (Lasso)",
      rf_marg = "DR ACDE Estimator (RF)",
      did_no_z = "DID + Mediator",
      did_with_z = "DID + Mediator + Covariates",
      out_reg_marg = "Outcome Regressions",
      ipw_marg = "Inverse Propensity Weighting"
    ),
    Method = as.factor(Method)
  ) %>%
  rename(Specification = specification)



# Now create one big facet grid
plot_covars_exp_long <- tmp %>%
  pivot_longer(cols = c(bias, rmse, coverage), names_to = 'Measure') %>% 
  mutate(line_int = recode(Measure, bias = 0, rmse = 0, coverage = 0.95)) %>%
  mutate(Measure = factor(Measure, levels = c('bias', 'rmse', 'coverage'))) %>%
  mutate(Specification = factor(Specification, levels = c('Correct', 'Incorrect Propensity Score', 'Incorrect Outcome Regressions', 'Both Incorrect')))

dummy <- data.frame(value = 1.05, N = 600, Measure = "coverage", Method = "DID + Mediator") |>
  mutate(Measure = factor(Measure, levels = c('bias', 'rmse', 'coverage')))

gl <- guide_legend(nrow = 3, keywidth = unit(2, "cm"))
full_plot <- ggplot(plot_covars_exp_long, mapping = aes(x = N, y = value, colour = Method, linetype = Method, shape = Method)) + 
  xlab('N') +
  facet_wrap(Specification ~ Measure, nrow = 4, scales = "free_y", 
             labeller = as_labeller(c(bias = 'Bias', rmse = 'RMSE', coverage = 'Coverage', 
                                      Correct = 'Correct', 
                                      `Incorrect Propensity Score` = 'Incorrect Propensity Score',
                                      `Incorrect Outcome Regressions` = 'Incorrect Outcome Regressions', 
                                      `Both Incorrect` = 'Both Models Incorrect'))) + 
  ## facet_grid(cols = vars(Measure), rows = vars(Specification), scales = 'free',
  ##            labeller = as_labeller(c(bias = 'Bias', rmse = 'RMSE', coverage = 'Coverage', 
  ##                                     Correct = 'Correct', Incorrect = 'Incorrect'))) + 
  geom_line(size = 1) +
  geom_point(size = 2.5) + 
  geom_hline(data = plot_covars_exp_long, aes(yintercept = line_int)) +
  geom_blank(data = dummy) + 
  theme_light() + 
  theme(axis.title.y = element_blank(), legend.position = 'bottom') +
  guides(colour = gl, linetype = gl, shape = gl)
full_plot

cairo_pdf('output/ml_full_plot.pdf', width = 9, height = 12, family = "Fira Sans")
full_plot
dev.off()
