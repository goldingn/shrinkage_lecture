# numerical functions

nll <- function (beta, mu, sigma) {
  require (mvtnorm)
  -mvtnorm::dmvnorm(beta,
                    mean = mu,
                    sigma = sigma,
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
  soln <- glmnet::glmnet(x, y, alpha = alpha, lambda = lambdas)
  coords <- t(as.matrix(soln$beta))
  
  # get the corresponding RSS
  ellipse_densities <- apply(coords, 1, nll, mu, sigma)
  
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

plot_density <- function (mat, levels, pal = gray.colors) {
  
  idx <- which(mat == min(mat), arr.ind = TRUE)
  peak <- beta_seq[idx[1, ]]
  
  plot.new()  
  plot.window(xlim = c(-3, 3),
              ylim = c(-3, 3),
              asp = 1,
              pty = "s")
  
  rect(-10, -10, 10, 10, col = grey(0.98))
  pal <- colorRampPalette(c(grey(0.8), grey(0.95)))
  cols <- pal(length(levels))
  
  .filled.contour(beta_seq, beta_seq, mat,
                  levels = levels,
                  col = cols)
  
  abline(v = 0, h = 0,
         col = grey(0.5),
         lwd = 1)
  
  add_axis(1)
  add_axis(2)
  
  mtext(expression(beta[1]), side = 1, las = 1, line = 2.5)
  mtext(expression(beta[2]), side = 2, las = 1, line = 2.5)
  
  contour(beta_seq, beta_seq, mat,
          levels = levels,
          col = grey(0.6),
          drawlabels = FALSE,
          add = TRUE)
  
  points(peak[1], peak[2], pch = 16)
  
  text(peak[1] + 0.2, peak[2] + 0.2, expression(hat(beta)))
}

add_contour <- function (density) {
  contour(beta_seq, beta_seq, nll_mat,
          levels = density,
          drawlabels = FALSE,
          add = TRUE,
          col = "blue",
          lwd = 2)
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
  traj <- rbind(mu, data$beta[idx, ])
  lines(traj, col = grey(0.5), lty = 2)
  points(beta_i[1], beta_i[2], pch = 16, col = "black")
  
  # add the title
  beta_text <- paste(round(beta_i, 3), collapse = ", ")
  title(bquote(hat(beta)^{.(type)} == .(beta_text)))
}

plot_all <- function (input) {
  plot_density(nll_mat, nll_levels)
  add_solution(input$lambda, input$type, get(input$type))
}

# simulate a dataset
set.seed(1)
mu <- c(0.5, 1.2)
corr <- -0.8
x <- mvtnorm::rmvnorm(100, c(0, 0), matrix(c(1, corr, corr, 1), ncol = 2))
x <- scale(x)
y <- x %*% mu + rnorm(100, 0 , 0.1)

# get the least squares solution
ls_soln <- lm(y ~ x)
mu <- coef(ls_soln)[-1]
sigma <- vcov(ls_soln)[-1, -1]

# set up the plotting data
n <- 201
beta_seq <- seq(-5, 5, length.out = n)
betas <- expand.grid(beta_1 = beta_seq,
                     beta_3 = beta_seq)

nlls <- apply(betas, 1, nll, mu, sigma)
nll_mat <- matrix(nlls, nrow = n)

nll_levels <- seq(-10, 5000, by = 1000)

# shrinkage parameters
lambda_max <- 4
lambda_min <- 0.01
lambda_by <- 0.01
lambdas <- seq(lambda_max, lambda_min, by = -lambda_by)

ridge <- plotting_data(lambdas, "ridge")
lasso <- plotting_data(lambdas, "lasso")

library (shiny)
ui <- fluidPage(
  
  titlePanel(" "),
  
  sidebarLayout(
    
    sidebarPanel(
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
      radioButtons("type",
                   "model type:",
                   choices = c("ridge", "lasso")),
      uiOutput("lambda_slider")
    ),
    
    mainPanel(
      plotOutput("plot", width = "500px", height = "500px")
    )
  )
)

server <- function (input, output) {
  
  output$lambda_slider <- renderUI({
    max <- switch(input$type,
                  ridge = lambda_max,
                  lasso = lambda_max / 4)
    sliderInput("lambda",
                "lambda:",
                min = lambda_min,
                max = max,
                value = 0.5,
                step = lambda_by)
  })
  
  output$plot <- renderPlot(plot_all(input),
                            height = reactive(ifelse(!is.null(input$innerWidth),
                                                     input$innerWidth * 0.5,
                                                     0)),
                            width = reactive(ifelse(!is.null(input$innerWidth),
                                                    input$innerWidth * 0.5,
                                                    0)))
}

app <- shinyApp(ui, server)



