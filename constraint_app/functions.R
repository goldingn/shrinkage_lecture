library (shiny)
library (mvtnorm)
library (glmnet)

# computation functions

# simulate a dataset
simulate_data <- function () {
  set.seed(1)
  mu <- c(0.5, 1.2)
  corr <- -0.8
  x <- mvtnorm::rmvnorm(100, c(0, 0), matrix(c(1, corr, corr, 1), ncol = 2))
  x <- scale(x)
  y <- x %*% mu + rnorm(100, 0 , 0.1)
  list(y = y, x = x)
}

# least squares fit
fit_ls <- function (data) {
  # get the least squares solution
  ls_soln <- lm(y ~ x, data = data)
  mu <- coef(ls_soln)[-1]
  sigma <- vcov(ls_soln)[-1, -1]
  list(mu = mu, sigma = sigma)
}

density_surface <- function (fit) {
  
  # set up the plotting data
  n <- 151
  beta_seq <- seq(-5, 5, length.out = n)
  betas <- expand.grid(beta_1 = beta_seq,
                       beta_3 = beta_seq)
  
  nlls <- apply(betas, 1, nll, fit)
  nll_mat <- matrix(nlls, nrow = n)
  
  list(beta_seq = beta_seq,
       nll_mat = nll_mat)
  
}

nll <- function (beta, fit) {
  require (mvtnorm)
  -mvtnorm::dmvnorm(beta,
                    mean = fit$mu,
                    sigma = fit$sigma,
                    log = TRUE)
}

find_c <- function (coords, type = c("ridge", "lasso")) {
  type <- match.arg(type)
  switch (type,
          ridge = ridge_c(coords),
          lasso = lasso_c(coords))
}

ridge_c <- function (coords) {
  sqrt(rowSums(coords ^ 2))
}

lasso_c <- function (coords) {
  rowSums(coords)
}

# find points on the constraint line
constraint_points <- function (c, type = c("ridge", "lasso"), n = 30) {
  type <- match.arg(type)
  switch (type,
          ridge = ridge_pts(c, n),
          lasso = lasso_pts(c, n))
}

lasso_pts <- function (c, n) {
  u <- seq(0, c, length.out = n)
  x <- c(u, rev(u), -u, rev(-u))
  y <- c(rev(u), -u, rev(-u), u)
  cbind(x, y)
}

ridge_pts <- function (c, n) {
  angles <- seq(0, 1, length.out = n * 4)
  x <- c * cos(2 * pi * angles)
  y <- c * sin(2 * pi * angles)
  cbind(x, y)
}

# get constraint plotting info for each model type
plotting_data <- function (lambdas, type = c("ridge", "lasso")) {
  
  require (glmnet)
  
  type <- match.arg(type)
  
  # fit the model
  alpha <- switch(type, ridge = 0, lasso = 1)
  soln <- glmnet::glmnet(data$x, data$y, alpha = alpha, lambda = lambdas)
  coords <- t(as.matrix(soln$beta))
  
  # get the corresponding RSS
  ellipse_densities <- apply(coords, 1, nll, fit)
  
  # get the corresponding constraint constant
  constraints <- find_c(coords, type = type)
  
  # get points to plot the contraint boundaries
  constraint_lines <- lapply(constraints, constraint_points, type)
  
  list(beta = coords,
       RSS = ellipse_densities,
       constraint = constraint_lines)
}

# plotting functions

add_axis <- function (which) {
  axis(which,
       las = 1,
       lty = 0,
       col.axis = grey(0.7),
       line = -0.5)
}

plot_setup <- function() {
  
  plot.new()  
  plot.window(xlim = c(-3, 3),
              ylim = c(-3, 3),
              asp = 1,
              pty = "s")
  
  rect(-10, -10, 10, 10, col = grey(0.98))
  
  abline(v = 0, h = 0,
         col = grey(0.5),
         lwd = 1)
  
  add_axis(1)
  add_axis(2)
  
  mtext(expression(beta[1]), side = 1, las = 1, line = 2.5)
  mtext(expression(beta[2]), side = 2, las = 1, line = 2.5)
  
}

add_density <- function (density) {
  
  levels <- seq(-10, 5000, by = 1000)
  beta_seq <- density$beta_seq
  mat <- density$nll_mat

  idx <- which(mat == min(mat), arr.ind = TRUE)
  peak <- beta_seq[idx[1, ]]
  
  pal <- colorRampPalette(c(grey(0.8), grey(0.95)))
  cols <- pal(length(levels))
  
  .filled.contour(beta_seq, beta_seq, mat,
                  levels = levels,
                  col = cols)
  
  abline(v = 0, h = 0,
         col = grey(0.5),
         lwd = 1)
  
  contour(beta_seq, beta_seq, mat,
          levels = levels,
          col = grey(0.6),
          drawlabels = FALSE,
          add = TRUE)
  
  points(peak[1], peak[2], pch = 16)
  
  text(peak[1] + 0.2, peak[2] + 0.2, expression(hat(beta)))
}

add_contour <- function (level) {
  with(density,
       contour(beta_seq, beta_seq, nll_mat,
               levels = level,
               drawlabels = FALSE,
               add = TRUE,
               col = "blue",
               lwd = 2)
       )
}

add_constraint <- function (constraint_line) {
  lines(constraint_line,
        col = "red",
        lwd = 2)
}

# plot the solution for a given lambda
add_solution <- function (lambda, type, data) {
  
  # find the matching result
  i <- which.min(abs(lambda - lambdas))
  beta_i <- data$beta[i, ]
  
  # plot the two polygons
  add_contour(data$RSS[i])
  add_constraint(data$constraint[[i]])
  
  # add trajectory and solution
  idx <- rev(which(lambdas < lambda))
  traj <- rbind(fit$mu, data$beta[idx, ])
  lines(traj, col = grey(0.5), lty = 2)
  points(beta_i[1], beta_i[2], pch = 16, col = "black")
  
  # add the title
  beta_text <- paste(round(beta_i, 3), collapse = ", ")
  title(bquote(hat(beta)^{.(type)} == .(beta_text)))
}

plot_all <- function (input) {
  plot_setup()
  add_density(density)
  add_solution(input$lambda, input$type, get(input$type))
}

plot_legend <- function () {
  plot.new()
  plot.window(c(0, 1), ylim = c(0, 4))
  mar <- par("mar")
  on.exit(par(mar = mar))
  par(mar = rep(0, 4))
  text(0, 3, "density of likelihood", col = "grey", adj = 0)
  text(0, 2, "contour of likelihood (equivalent to solution)", col = "blue", adj = 0)
  text(0, 1, "contour of constraint", col = "red", adj = 0)
}
