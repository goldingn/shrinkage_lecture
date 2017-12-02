powerify <- function (x, times = length(x)) {
  sapply(seq_len(times),
         function(y, x) x ^ y,
         x)
}

f <- function(x)
  sin(x * 4) * pmax(0, cos(x * 3))

simulate_data <- function () {
  
  set.seed(1)
  n <- 50
  powers <- 10
  
  x <- powerify(runif(n, -1.5, 1.5),
                powers)
  
  y <- f(x[, 1]) + rnorm(n, 0, 0.1)
  y <- scale(y)
  m <- lm(y ~ . - 1, data = data.frame(x))
  
  x_seq <- seq(-1, 1, length.out = 1000)
  x_seq <- powerify(x_seq, powers)

  fit <- predict(m, data.frame(x_seq))
  
  # refit to scaled data to get scaled coefficients
  # don't want to bother messing with rescaled predictions
  m <- lm(y ~ . - 1, data = data.frame(scale(x)))
  beta <- coef(m)
  
  list(y = y,
       x = x,
       x_seq = x_seq,
       fit = fit,
       beta = beta)
}

predict_shrunk <- function (lambdas, data) {
  require (glmnet)
  # ridge
  m_ridge <- glmnet::glmnet(data$x, data$y, alpha = 0, lambda = lambdas,
                            standardize = TRUE, intercept = FALSE)
  preds_ridge <- predict(m_ridge, data$x_seq)
  beta_ridge <- as.matrix(coef(m_ridge))[-1, ]
  
  m_lasso <- glmnet::glmnet(data$x, data$y, alpha = 1, lambda = lambdas,
                            standardize = TRUE, intercept = FALSE)
  preds_lasso <- predict(m_lasso, data$x_seq)
  beta_lasso <- as.matrix(coef(m_lasso))[-1, ]
  
  list(ridge = list(preds = preds_ridge,
                    betas = beta_ridge),
       lasso = list(preds = preds_lasso,
                    betas = beta_lasso))
  
}

add_axis <- function (which) {
  axis(which,
       las = 1,
       lty = 0,
       col.axis = grey(0.7),
       line = -0.5)
}

plot_backdrop <- function (data) {
  
  mar <- par("mar")
  on.exit(par(mar = mar))
  par(mar = c(5, 4, 0, 2))
  plot.new()
  plot.window(x = range(data$x_seq[, 1]),
              y = range(data$y) * 1.2)
  
  rect(-10, -10, 10, 10, col = grey(0.98))
  
  abline(h = 0,
         col = grey(0.85),
         lwd = 1)
  
  points(data$y ~ data$x[, 1],
         pch = 16,
         col = grey(0.8))
  
  add_axis(1)
  add_axis(2)
  
  mtext("x", 1,
        las = 1,
        line = 3)
  mtext("y", 2,
        las = 1,
        line = 3)
  
  lines(data$fit ~ data$x_seq[, 1],
        lwd = 3,
        col = grey(0.8))
  
}

add_fit <- function (input, lambdas, data, preds) {
  
  i <- which.min(abs(lambdas - input$lambda))
  preds <- preds[[input$type]]$preds
  
  lines(preds[, i] ~ data$x_seq[, 1],
        lwd = 4,
        col = 'hotpink')
  
}

plot_shrunk <- function (input, lambdas, data, preds) {
  plot_backdrop(data)
  add_fit(input, lambdas, data, preds)
}

plot_betas <- function (input, lambdas, data, preds) {
  
  # get beta & shrinkage beta
  i <- which.min(abs(lambdas - input$lambda))
  beta <- data$beta
  beta_shrunk <- preds[[input$type]]$beta[, i]
  
  # get ratio (shrinkage factor)
  ratio <- abs(beta_shrunk / beta)
  ratio_mat <- rbind(ratio, 1 - ratio)
  
  nm <- rep("", length(beta))
  
  par(mfrow = c(2, 1),
      mar = c(3, 6, 3, 2))
  
  # plot beta
  barplot(beta,
          axes = FALSE,
          names.arg = nm,
          border = NA,
          col = grey(0.85))
  axis(2,
       las = 1,
       col.axis = grey(0.5),
       cex.axis = 0.8,
       line = -1,
       lty = 0)
  axis(1,
       at = bp,
       labels = seq_along(beta),
       col.axis = grey(0.5),
       cex.axis = 0.8,
       lty = 0)
  mtext(expression(hat(beta)),
        side = 2,
        line = 3,
        las = 1,
        cex = 1.5)
  
  
  bp <- barplot(ratio_mat,
          ylim = c(0, 1),
          axes = FALSE,
          names.arg = nm,
          border = NA,
          col = c("hot pink", grey(0.9)))
  axis(2,
       at = c(0, 0.5, 1),
       las = 1,
       col.axis = grey(0.5),
       cex.axis = 0.8,
       line = -1,
       lty = 0)
  axis(1,
       at = bp,
       labels = seq_along(beta),
       col.axis = grey(0.5),
       cex.axis = 0.8,
       lty = 0)
  
  mtext(bquote(abs( frac(hat(beta)^.(input$type), hat(beta)) )),
        side = 2,
        line = 2,
        las = 1,
        cex = 1.5)
  
  removed <- beta_shrunk == 0
  if (any(removed)) {
    points(bp[removed], rep(0.5, sum(removed)),
           pch = 4, cex = 1.5, col = "red", lwd = 2)
  }
  
}
