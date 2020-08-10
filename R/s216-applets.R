#' One proportion simulation-based hypothesis test
#'
#' This function will run a simulation-based hypothesis test for the value of a single proportion.
#' @param probability_success value between 0 and 1, represents null hypothesis value for proportion
#' @param sample_size number of trials used to compute proportion
#' @param number_repetitions number of draws for simulation test
#' @param as_extreme_as observed statistic (between 0 and 1)
#' @param direction one of "greater", "less", or "two-sided" to give direction of hypothsis test
#' @param report_value value to return from simulations - "number" for number of successes or "proportion" for proportion of successes
#' @return Plot of distribution of simulated values, with values as or more extreme than specified value highlighted and reports proportion of successes in simulations as subtitle on plot
#' @examples
#' one_proportion_test(probability_success = 0.5, sample_size = 150, number_repetitions = 100, as_extreme_as = 0.65, direction = "greater", return_value = "proportion")
#' @export


one_proportion_test <- function(probability_success = 0.5,
                                sample_size = 5,
                                number_repetitions = 1,
                                as_extreme_as,
                                direction = c("greater", "less", "two-sided"),
                                report_value = c("number", "proportion")){
  if(number_repetitions < 1 | !(number_repetitions %%1 == 0))
    stop("number of repetitions must be positive and integer valued")
  if(sample_size < 1 | !(sample_size %%1 == 0))
    stop("Sample size must be positive and integer valued")
  if(probability_success < 0 | probability_success > 1)
    stop("Probability of success must be between zero and one")
  if(!(direction %in% c("greater", "less", "two-sided")))
    stop("Direction must be one of 'greater',  'less', or 'two-sided'")
  if(!(report_value %in% c("number", "proportion")))
    stop("Value to report must be either 'number' or 'proportion' of successes")
  if(is.null(as_extreme_as))
    stop("Must enter cutoff value for 'as_extreme_as")

  number_heads <- rbinom(number_repetitions, sample_size, probability_success)
  range_heads <- max(number_heads) - min(number_heads)
  success_tab = table(number_heads)

  if(!(direction == "two-sided")){
    if(report_value == "number"){
      proportion_extreme <- ifelse(direction == "greater",
                                   mean(number_heads >= as_extreme_as),
                                   mean(number_heads <= as_extreme_as))
    }else {
      proportion_extreme <- ifelse(direction == "greater",
                                   mean(number_heads/sample_size >= as_extreme_as),
                                   mean(number_heads/sample_size <= as_extreme_as))
    }

    if(report_value == "number"){
      plot(0,0, "n", xlim = c(min(min(number_heads), as_extreme_as-range_heads/5),
                              max(max(number_heads), as_extreme_as + range_heads/5)),
           ylim = c(0, max(success_tab)+1),
           xlab = "Number of successes", ylab = "", yaxt ="n",
           sub = paste("Proportion of Samples = ",
                       proportion_extreme*number_repetitions,
                       "/", number_repetitions, " = ",
                       round(proportion_extreme,4), sep = ""))
      for(i in 1:length(success_tab)){
        lines(rep(names(success_tab)[i],2), c(0, success_tab[i]), lwd = 5,
              col = ifelse(direction == "greater",
                           ifelse(as.numeric(names(success_tab[i])) >= as_extreme_as, "blue", "black"),
                           ifelse(as.numeric(names(success_tab[i])) <= as_extreme_as, "blue", "black")))
      }
      abline(v = ifelse(direction == "greater", as_extreme_as-0.5, as_extreme_as + 0.5),
             col = "blue", lwd = 2)
      cutoff_label <- ifelse(direction == "greater",
                             paste(">=", as_extreme_as),
                             paste("<=", as_extreme_as))
      text(ifelse(direction == "greater", as_extreme_as-0.25, as_extreme_as+0.25),
           max(success_tab), labels = cutoff_label,
           pos = ifelse(direction == "greater", 4, 2))
    }else{
      plot(0,0, "n", xlim =c(min(min(number_heads)/sample_size, as_extreme_as-range_heads/(5*sample_size)),
                             max(max(number_heads)/sample_size, as_extreme_as + range_heads/(5*sample_size))),
           ylim = c(0, max(success_tab)+1),
           xlab = "Proportion of successes", ylab = "", yaxt ="n",
           sub = paste("Proportion of Samples = ",
                       proportion_extreme*number_repetitions,
                       "/", number_repetitions, " = ",
                       round(proportion_extreme,4), sep = ""))
      for(i in 1:length(success_tab)){
        lines(rep(as.numeric(names(success_tab)[i])/sample_size,2), c(0, success_tab[i]), lwd = 5,
              col = ifelse(direction == "greater",
                           ifelse(as.numeric(names(success_tab[i]))/sample_size >= as_extreme_as,
                                  "blue", "black"),
                           ifelse(as.numeric(names(success_tab[i]))/sample_size <= as_extreme_as,
                                  "blue", "black")))
      }
      abline(v = ifelse(direction == "greater", as_extreme_as-0.5/sample_size,
                        as_extreme_as + 0.5/sample_size), col = "blue", lwd = 2)
      cutoff_label <- ifelse(direction == "greater",
                             paste(">=", as_extreme_as),
                             paste("<=", as_extreme_as))
      text(ifelse(direction == "greater", as_extreme_as-0.25/sample_size, as_extreme_as+0.25/sample_size),
           max(success_tab), labels = cutoff_label,
           pos = ifelse(direction == "greater", 4, 2))
    }
  }else if(report_value == "number"){
    if(as_extreme_as > probability_success*sample_size){
      upper = as_extreme_as
      lower = max(0, 2*probability_success*sample_size-as_extreme_as)
    }else{
      lower = as_extreme_as
      upper = min(sample_size, 2*probability_success*sample_size-as_extreme_as)
    }
    proportion_extreme <- mean(number_heads <= lower | number_heads >= upper)

    plot(0,0, "n", xlim = c(min(min(number_heads), lower-range_heads/5),
                            max(max(number_heads), upper + range_heads/5)),
         ylim = c(0, max(success_tab)+1),
         xlab = "Number of successes", ylab = "", yaxt ="n",
         sub = paste("Proportion of Samples = ",
                     proportion_extreme*number_repetitions,
                     "/", number_repetitions, " = ",
                     round(proportion_extreme,4), sep = ""))
    for(i in 1:length(success_tab)){
      lines(rep(names(success_tab)[i],2), c(0, success_tab[i]), lwd = 5,
            col = ifelse(as.numeric(names(success_tab[i])) <= lower |
                           as.numeric(names(success_tab[i])) >= upper, "blue", "black"))
    }
    abline(v = c(lower + 0.5, upper - 0.5),
           col = "blue", lwd = 2)
    cutoff_label <- c(paste("<=", lower), paste(">=", upper))
    text(c(lower-0.25, upper+0.25),
         rep(max(success_tab),2), labels = cutoff_label,
         pos = c(4, 2))
  }else{
    if(as_extreme_as > probability_success){
      upper = as_extreme_as
      lower = max(0, 2*probability_success-as_extreme_as)
    }else{
      lower = as_extreme_as
      upper = min(1, 2*probability_success-as_extreme_as)
    }
    proportion_extreme <- mean(number_heads <= lower*sample_size | number_heads >= upper*sample_size)

    plot(0,0, "n", xlim =c(min(min(number_heads)/sample_size, lower-range_heads/(5*sample_size)),
                           max(max(number_heads)/sample_size, upper + range_heads/(5*sample_size))),
         ylim = c(0, max(success_tab)+1),
         xlab = "Proportion of successes", ylab = "", yaxt ="n",
         sub = paste("Proportion of Samples = ",
                     proportion_extreme*number_repetitions,
                     "/", number_repetitions, " = ",
                     round(proportion_extreme,4), sep = ""))
    for(i in 1:length(success_tab)){
      lines(rep(as.numeric(names(success_tab)[i])/sample_size,2), c(0, success_tab[i]), lwd = 5,
            col = ifelse(as.numeric(names(success_tab[i]))/sample_size <= lower |
                           as.numeric(names(success_tab[i]))/sample_size >= upper, "blue", "black"))
    }
    abline(v = c(lower + 0.5/sample_size, upper - 0.5/sample_size),
           col = "blue", lwd = 2)
    cutoff_label <- c(paste("<=", lower), paste(">=", upper))
    text(c(lower+0.25/sample_size, upper-0.25/sample_size),
         rep(max(success_tab),2), labels = cutoff_label,
         pos = c(2, 4))
  }

}


#' Function to produce bootstrap confidence interval for one proportion
#'
#' @param sample_size number of trials used to compute proportion
#' @param number_success how many successes were observed in those trials?
#' @param number_repetitions number of draws for simulation test
#' @param confidence_level confidence level for interval in decimal form.  Defaults to 95%
#'
#' @return Produces plot of distribution of bootstrapped values, with values as or more extreme than confidence interval range highlighted and reports CI as subtitle on plot
#' @examples
#' one_proportion_bootstrap_CI(sample_size = 150, number_successes = 98, number_repetitions = 100, confidence_level = 0.99)
#' @export

one_proportion_bootstrap_CI <- function(sample_size, number_successes,
                                        number_repetitions = 100, confidence_level = 0.95){
  if(number_repetitions < 1 | !(number_repetitions %%1 == 0))
    stop("number of repetitions must be positive and integer valued")
  if(number_successes < 1 | !(number_successes %%1 == 0))
    stop("number of successes must be positive and integer valued")
  if(sample_size < 1 | !(sample_size %%1 == 0))
    stop("sample size must be positive and integer valued")
  if(confidence_level < 0 | confidence_level > 1)
    stop("Confidence level must be given in decimal form")

  sim_pop <- rep(0, sample_size)
  sim_pop[1:number_successes] <- 1

  sim_props <- rep(NA, number_repetitions)
  for(i in 1:number_repetitions){
    sim_props[i] <- mean(sample(sim_pop, sample_size, replace = TRUE))
  }

  success_tab = table(sim_props)

  low_ci <- quantile(sim_props, (1-confidence_level)/2)
  high_ci <- quantile(sim_props, 1-(1-confidence_level)/2)

  plot(0,0, "n", xlim = c(min(min(sim_props), low_ci-(max(sim_props)-min(sim_props))/5), max(sim_props)),
       ylim = c(0, max(success_tab)+1),
       xlab = "Bootstrapped values of the proportion", ylab = "", yaxt = "n",
       sub = paste0(100*confidence_level, "% CI: (",
                    low_ci, ", ", high_ci, ")"))
  for(i in 1:length(success_tab)){
    lines(rep(as.numeric(names(success_tab)[i]),2), c(0, success_tab[i]), lwd = 5,
          col = ifelse(as.numeric(names(success_tab[i])) <= low_ci |
                         as.numeric(names(success_tab[i])) >= high_ci, "blue", "black"))
  }
  abline(v = c(low_ci, high_ci),
         col = "blue", lwd = 2)
  cutoff_label <- c(paste(round(100*(1-confidence_level)/2, 1), "percentile"),
                    paste(round(100*(1-(1-confidence_level)/2), 1), "percentile"))
  text(c(low_ci, high_ci),
       rep(max(success_tab),2), labels = cutoff_label,
       pos = c(2, 4), cex = .75)
}



#' Function to produce plot of observed matched pairs data
#'
#' @param data two- or three-column data frame, with values for each group in last two columns
#'
#' @return Produces plot of distribution of observed values, with means & SDs in groups and paired observations linked by a line segment
#'
#' @examples
#' set.seed(117)
#' x <- rnorm(25)
#' y <- x + 1 + rnorm(25, 0, .3)
#' data <- data.frame(x,y)
#' paired_observed_plot(data)
#' @export




paired_observed_plot <- function(data){
  if(ncol(data) < 2 | ncol(data) > 3)
    stop("Data should have two or three variables")
  if(ncol(data) == 3)
    data <- data[,2:3]

  differences = data[,1]-data[,2]
  par(mfrow = c(2,1), mgp = c(2, .5, 0), mar = c(4,4,0,0)+.1)
  plot(0,0, "n", xlim = c(min(data), max(data)),
       ylim = c(0,5), yaxt = "n", xlab = "Outcomes",
       ylab = "")
  points(data[,1], rep(0.5, nrow(data)), pch = 15, col = "blue")
  points(data[,2],rep(3, nrow(data)), pch = 15, col = "red")
  for(i in 1:nrow(data)){
    lines(c(data[i,]), c(.5, 3))
  }

  leg.loc = min(data) + 0.85*(max(data)-min(data))
  legend(leg.loc,5, col = "white", bty = "n",
         legend = c(dimnames(data)[[2]][1], paste("Mean =", round(mean(data[,1],na.rm = T), 3)),
                    paste("SD =", round(sd(data[,1], na.rm = T), 3))),
         text.col = "red", cex = 0.75)
  legend(leg.loc,3 , col = "white", bty = "n",
         legend = c(dimnames(data)[[2]][2], paste("Mean =", round(mean(data[,2],na.rm = T), 3)),
                    paste("SD =", round(sd(data[,2], na.rm = T), 3))),
         text.col = "blue", cex = 0.75)



  hist(differences, xlab = "Differences",
       ylab = "", col = "grey80",
       main = "", breaks = round(nrow(data)/3))
  legend("topright", legend = c(paste("Mean =", round(mean(differences, na.rm = T),3)),
                                paste("SD =", round(sd(differences, na.rm = T),3))),
         col = "white", bty = "n")
}



#' Function to run simulation test for matched pairs
#'
#' @param data vector of differences or a two- or three-column data frame, with values for each group in last two columns
#' @param shift amount to shift differences for bootstrapping of null distribution
#' @param number_repetitions number of draws for simulation test
#' @param as_extreme_as observed statistic
#' @param direction one of "greater", "less", or "two-sided" to give direction of hypothsis test
#' @param which_first For order of subtraction - which column should be first?
#'
#' @return Produces plot of distribution of simulated values, with values as or more extreme than specified value highlighted and count/proportion of those values reported as subtitle on plot
#' @examples
#' set.seed(117)
#' x <- rnorm(25)
#' y <- x + 1 + rnorm(25, 0, .3)
#' data <- data.frame(x,y)
#' obs_diff <- mean(x - y)
#' paired_test(data, shift = obs_diff, direction = "two-sided", as_extreme_as = obs_diff, number_repetitions = 100, which_first = 1)
#' @export


paired_test <- function(data, shift = 0, direction = c("greater", "less", "two-sided"),
                        as_extreme_as, number_repetitions = 3, which_first = 1){
    if(!(direction %in% c("greater", "less", "two-sided")))
        stop("direction must be 'greater', 'less', or 'two-sided'")
    if(is.null(as_extreme_as))
        stop("Must enter cutoff value for 'as_extreme_as")
    if(number_repetitions < 1 | !(number_repetitions %%1 == 0))
        stop("number of repetitions must be positive and integer valued")
    if(!(which_first %in% 1:2))
        stop("which_first must equal 1 or 2 to indicate order of subtraction")
    if(is.vector(data)){
        warning("Assuming data entered is vector of differences")
        differences = data
        n = length(data)
    }else{
        if(ncol(data) < 2 | ncol(data) > 3)
            stop("Data should have two or three variables")
        if(ncol(data) == 3)
            data <- data[,2:3]
        differences = data[,which_first]-data[,ifelse(which_first == 1, 2, 1)]
        n = nrow(data)
    }

    obs_diff <- mean(differences)
    sim_diffs <- rep(NA, number_repetitions)
    for(i in 1:number_repetitions){
        sim_diffs[i] <- mean(sample(x = differences+shift, size = n, replace = TRUE))
    }

    count_extreme <- ifelse(direction == "greater", sum(sim_diffs >= as_extreme_as),
                            ifelse(direction == "less", sum(sim_diffs <= as_extreme_as),
                                   sum(sim_diffs <= -abs(as_extreme_as)) +
                                       sum(sim_diffs >= abs(as_extreme_as))))

    h <- hist(sim_diffs, plot = FALSE, breaks = "FD")
    if(direction == "two-sided"){
        cuts <- cut(h$breaks, c(-Inf, -abs(as_extreme_as), abs(as_extreme_as), Inf))
        col.vec <- rep("grey80", length(cuts))
        col.vec[cuts == levels(cuts)[1]] ="red"
        col.vec[cuts == levels(cuts)[3]] ="red"
    }else if (direction == "greater"){
        cuts <- cut(h$breaks, c(-Inf, as_extreme_as, Inf))
        col.vec <- rep("grey80", length(cuts))
        col.vec[cuts == levels(cuts)[2]] ="red"
    }else{
        cuts <- cut(h$breaks, c(-Inf, as_extreme_as, Inf))
        col.vec <- rep("grey80", length(cuts))
        col.vec[cuts == levels(cuts)[1]] ="red"
    }
    plot(h, col = col.vec, main = "", ylab = "", xlab = "Average Difference",
         yaxt = "n", sub = paste("Count = ",
                                 count_extreme,
                                 "/", number_repetitions, " = ",
                                 round(count_extreme/number_repetitions,4), sep = ""))

    legend("topright", legend = c(paste("Mean =", round(mean(sim_diffs, na.rm = T),3)),
                                  paste("SD =", round(sd(sim_diffs, na.rm = T),3))),
           col = "white", bty = "n")
    if(direction == "two-sided"){
        abline(v = abs(as_extreme_as), col= "red", lwd = 2)
        abline(v = -abs(as_extreme_as), col= "red", lwd = 2)
    }else{
        abline(v = as_extreme_as, col= "red", lwd = 2)
    }
}


#' Function to run bootstrap confidence intervalt for matched pairs data
#'
#' @param data vector of differences or a two- or three-column data frame, with values for each group in last two columns
#' @param number_repetitions number of draws for bootstrap simulation
#' @param which_first For order of subtraction - which column should be first?
#' @param confidence_level confidence level for interval in decimal form.  Defaults to 95%
#'
#' @return Produces plot of distribution of bootstrapped values, with values as or more extreme than confidence interval range highlighted and reports CI as subtitle on plot
#'
#' @examples
#' set.seed(117)
#' x <- rnorm(25)
#' y <- x + 1 + rnorm(25, 0, .3)
#' data <- data.frame(x,y)
#' paired_bootstrap_CI(data, number_repetitions = 100, confidence_level = 0.9, which_first = 1)
#' @export


paired_bootstrap_CI <- function(data, number_repetitions = 100, confidence_level = 0.95,
                                which_first = 1){
    if(number_repetitions < 1 | !(number_repetitions %%1 == 0))
        stop("number of repetitions must be positive and integer valued")
    if(confidence_level < 0 | confidence_level > 1)
        stop("Confidence level must be given in decimal form")
    if(!(which_first %in% 1:2))
        stop("which_first must equal 1 or 2 to indicate order of subtraction")
    if(is.vector(data)){
        warning("Assuming data entered is vector of differences")
        differences = data
        n = length(data)
    }else{
        if(ncol(data) < 2 | ncol(data) > 3)
            stop("Data should have two or three variables")
        if(ncol(data) == 3)
            data <- data[,2:3]
        differences = data[,which_first]-data[,ifelse(which_first == 1, 2, 1)]
        n = nrow(data)
    }


    obs_diff <- mean(differences)
    sim_diffs <- rep(NA, number_repetitions)

    for(i in 1:number_repetitions){
        boot_samp <- sample(differences, n, replace = TRUE)
        sim_diffs[i] <- mean(boot_samp)
    }

    low_ci <- quantile(sim_diffs, (1-confidence_level)/2)
    high_ci <- quantile(sim_diffs, 1-(1-confidence_level)/2)

    h <- hist(sim_diffs, plot = FALSE, breaks = "FD")

    cuts <- cut(h$breaks, c(-Inf, low_ci, high_ci, Inf))
    col.vec <- rep("grey80", length(cuts))
    col.vec[cuts == levels(cuts)[1]] ="red"
    col.vec[cuts == levels(cuts)[3]] ="red"

    break_range <- max(h$breaks) - min(h$breaks)
    plot(h, col = col.vec, main = "", ylab = "", xlim = c(min(min(h$breaks), low_ci-break_range/6),
                                                          max(max(h$breaks), high_ci+break_range/6)),
         xlab = "Bootstrap Mean Difference",
         yaxt = "n", sub = paste0(100*confidence_level, "% CI: (",
                                  round(low_ci,3), ", ", round(high_ci,3), ")"))
    abline(v = c(low_ci, high_ci), col= "red", lwd = 2)

    cutoff_label <- c(paste(round(100*(1-confidence_level)/2, 1), "percentile"),
                      paste(round(100*(1-(1-confidence_level)/2), 1), "percentile"))
    text(c(low_ci, high_ci),
         rep(max(h$counts),2), labels = cutoff_label,
         pos = c(2, 4), cex = .75)
}


#' Function to perform hypothesis test for equality of two proportions using simulation
#' @param formula Formula of the form response~predictor, where predictor defines two groups and response is binary or two-level categorical
#' @param data Dataset with columns for response and predictor variable
#' @param first_in_subtraction Value of predictor that should be first in order of subtraction for computing statistics
#' @param response_value_numerator Value of response that corresponds to "success" computing proportions
#' @param number_repetitions number of draws for simulation test
#' @param as_extreme_as observed statistic
#' @param direction one of "greater", "less", or "two-sided" to give direction of hypothsis test
#'
#' @return Produces mosaic plot of observed proportions and plot of distribution of simulated values, with values as or more extreme than specified value highlighted and count/proportion of those values reported as subtitle on plot
#'
#' @examples
#' data(pt)
#' pt$twoSeconds <- ifelse(pt$responses >=2, "Yes", "No")
#' two_proportion_test(twoSeconds~brand, data = pt, first_in_subtraction = "B1", response_value_numerator = "Yes", number_repetitions = 100, as_extreme_as = -.4, direction = "two-sided")
#'
#' @export


two_proportion_test <- function(formula, data, first_in_subtraction,
                                response_value_numerator,
                                number_repetitions = 1, as_extreme_as,
                                direction = c("greater", "less", "two-sided")){
  if(!(direction %in% c("greater", "less", "two-sided")))
    stop("Direction must be one of 'greater',  'less', or 'two-sided'")
  if(number_repetitions < 1 | number_repetitions %% 1 != 0)
    stop("number of repetitions must be positive and integer valued")
  resp.name <- all.vars(formula)[1]
  pred.name <- all.vars(formula)[2]
  eval(parse(text = paste0("data$", resp.name, " = factor(data$", resp.name, ")")))
  eval(parse(text = paste0("data$", pred.name, " = factor(data$", pred.name, ")")))
  response <- eval(parse(text = paste0("data$", resp.name)))
  predictor <- eval(parse(text = paste0("data$", pred.name)))

  if(!(first_in_subtraction %in% unique(predictor)))
    stop("First term in order of subtraction must match values in data")
  if(!(response_value_numerator %in% unique(response)))
    stop("Value of response in numerator must match values in data")

  row.pcts <- prop.table(table(predictor, response), 1)

  obs.diff <- row.pcts[eval(parse(text = paste0("'", first_in_subtraction, "'"))),
                       eval(parse(text = paste0("'", response_value_numerator, "'")))] -
    row.pcts[setdiff(unique(predictor), eval(parse(text = paste0("'", first_in_subtraction, "'")))),
             eval(parse(text = paste0("'", response_value_numerator, "'")))]


  sim_diffs <- rep(NA, number_repetitions)
  for(i in 1:number_repetitions){
    newResponse <- sample(response)
    row.pcts <- prop.table(table(predictor, newResponse), 1)
    sim_diffs[i] <- row.pcts[eval(parse(text = paste0("'", first_in_subtraction, "'"))),
                             eval(parse(text = paste0("'", response_value_numerator, "'")))] -
      row.pcts[setdiff(unique(predictor), eval(parse(text = paste0("'", first_in_subtraction, "'")))),
               eval(parse(text = paste0("'", response_value_numerator, "'")))]
  }
  par(mfrow = c(1,2), mar = c(4,4,3,0)+0.1, mgp = c(2,.5,0))

  plot(formula, data= data, main = "Observed Data")

  subtraction_order <- paste0("(",eval(parse(text = paste0("'", first_in_subtraction, "'"))), " - ",
                              setdiff(unique(predictor), eval(parse(text = paste0("'", first_in_subtraction, "'")))), ")")
  legend("topright", legend = c(paste("Obs diff =", round(obs.diff, 3)),
                                subtraction_order),
         col = "white", bty = "n")

  count_extreme <- ifelse(direction == "greater", sum(sim_diffs >= as_extreme_as),
                          ifelse(direction == "less", sum(sim_diffs <= as_extreme_as),
                                 sum(sim_diffs <= -abs(as_extreme_as)) +
                                   sum(sim_diffs >= abs(as_extreme_as))))

  h <- hist(sim_diffs, plot = FALSE, breaks = "FD")
  if(direction == "two-sided"){
    cuts <- cut(h$breaks, c(-Inf, -abs(as_extreme_as), abs(as_extreme_as), Inf))
    col.vec <- rep("grey80", length(cuts))
    col.vec[cuts == levels(cuts)[1]] ="red"
    col.vec[cuts == levels(cuts)[3]] ="red"
  }else if (direction == "greater"){
    cuts <- cut(h$breaks, c(-Inf, as_extreme_as, Inf))
    col.vec <- rep("grey80", length(cuts))
    col.vec[cuts == levels(cuts)[2]] ="red"
  }else{
    cuts <- cut(h$breaks, c(-Inf, as_extreme_as, Inf))
    col.vec <- rep("grey80", length(cuts))
    col.vec[cuts == levels(cuts)[1]] ="red"
  }
  plot(h, col = col.vec, main = "", ylab = "", xlab = "Average Difference",
       sub = paste("Count = ",
                   count_extreme,
                   "/", number_repetitions, " = ",
                   round(count_extreme/number_repetitions,4), sep = ""))

  legend("topright", legend = c(paste("Mean =", round(mean(sim_diffs, na.rm = T),3)),
                                paste("SD =", round(sd(sim_diffs, na.rm = T),3))),
         col = "white", bty = "n")
  if(direction == "two-sided"){
    abline(v = abs(as_extreme_as), col= "red", lwd = 2)
    abline(v = -abs(as_extreme_as), col= "red", lwd = 2)
  }else{
    abline(v = as_extreme_as, col= "red", lwd = 2)
  }
}





