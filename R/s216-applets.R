###############
#Function to reproduce one proportion test applet
#Arguments: 
#probability_success: value between 0 and 1, represents null hypothesis value for proportion
#sample_size: number of trials used to compute proportion
#number_repetitions: number of draws for simulation test
#as_extreme_as: observed statistic (between 0 and 1)
#direction: one of "greater", "less", or "two-sided" to give direction of hypothsis test
#report_value: value to return from simulations - "number" for number of successes or "proportion" for proportion of successes
#
#Returns: produced plot of distribution of simulated values, with values as or more extreme
#than specified value highlighted and reports proportion of successes in simulations as subtitle on plot
###############

one_proportion_test <- function(probability_success = 0.5,
                                sample_size = 5,
                                number_repetitions = 1,
                                as_extreme_as,
                                direction = c("greater", "less", "two-sided"),
                                report_value = c("number", "proportion")){
  if(is.null(as_extreme_as))
    stop("Must enter cutoff")
  if(probability_success < 0 | probability_success > 1)
    stop("Probability of success must be between zero and one")
  if(!(direction %in% c("greater", "less", "two-sided")))
    stop("Direction must be one of 'greater',  'less', or 'two-sided'")
  if(!(report_value %in% c("number", "proportion")))
    stop("Value to report must be either 'number' or 'proportion' of successes")
  if(is.null(as_extreme_as))
    stop("Must enter cutoff value for 'as_extreme_as")
  
  number_heads <- rbinom(number_repetitions, sample_size, probability_success)
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
      plot(0,0, "n", xlim = c(min(number_heads), max(number_heads)),
           ylim = c(0, max(success_tab)+1),
           xlab = "Number of successes", ylab = "",
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
      plot(0,0, "n", xlim = c(min(number_heads)/sample_size, max(number_heads)/sample_size),
           ylim = c(0, max(success_tab)+1),
           xlab = "Proportion of successes", ylab = "",
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
    
    plot(0,0, "n", xlim = c(min(number_heads), max(number_heads)),
         ylim = c(0, max(success_tab)+1),
         xlab = "Number of successes", ylab = "",
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
      upper = min(sample_size, 2*probability_success-as_extreme_as)
    }
    proportion_extreme <- mean(number_heads <= lower*sample_size | number_heads >= upper*sample_size)
    
    plot(0,0, "n", xlim = c(min(number_heads)/sample_size, max(number_heads)/sample_size),
         ylim = c(0, max(success_tab)+1),
         xlab = "Proportion of successes", ylab = "",
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
