

####### first we'll calculate the future and present values and get an idea for the progress of the principle's reduction #######
################## let's get an idea of how we can calculate this in R and what knowledge we can gain from it ###################

library(dplyr)
(loans <- data.frame(month = rep(0, times = 10),
                     loan = c(1:10),
                     daily_nom_interest_rate = c(0.0429,0.0429,0.0429,0.0429,0.0376,0.0376,0.0445,0.0445, 0.0505, 0.0505),
                     principle = c(1750,1000,3500,2000,4500,2000,5500,2000,5500,2000),
                     int_due = c(0, 17.52, 0, 35.06, 0, 30.69, 0, 36.33, 0, 41.23),
                     payment = rep(0, times = 10),
                     weight = rep(NA)))
(loans <- loans %>% mutate(weight = replace(weight, values = principle/sum(loans$principle))))
(loans <- loans %>% mutate( monthly_eff_interest_rate = (1 + daily_nom_interest_rate/365)^(365/12)-1 ))

pmt <- NULL
data <- NULL
for(i in 1:120){
  pmt <- 309
  if(i == 1){
    pmt <- 309 - sum(loans$int_due)
    data <- loans
  }
  
  temp <- data %>% filter(month == max(month))
  temp$int_due <- 0
  pmt <- pmt - sum(temp %>% mutate(principle * monthly_eff_interest_rate) %>% pull)
  if(pmt < 0) stop("need to code for interest being left over")
  temp <- temp %>% mutate(weight = replace(weight, values = principle/sum(temp$principle)))
  temp <- temp %>% mutate(payment = replace(payment, values = pmt*weight))
  temp <- temp %>% mutate(principle = replace(principle, values = principle - payment))
  temp$month <- rep(i, times = 10)
  
  data <- rbind.data.frame(data, temp)
}
View(round(data, 3))


# some interesting EDA
data %>% group_by(loan) %>% summarize(min(weight), max(weight))

# PV = $29,943.85
sum(309 * loans$principle / sum(loans$principle) * (1 - (1+loans$monthly_eff_interest_rate)^(-120))/loans$monthly_eff_interest_rate)
# FV = $46,524.36
sum(309 * loans$principle / sum(loans$principle) * (1 - (1+loans$monthly_eff_interest_rate)^(-120))/loans$monthly_eff_interest_rate * (1+loans$monthly_eff_interest_rate)^120)









####### lets see what happens when we play with the payments #######



my_min <- function(x){
  x <- ifelse(x == 0, max(x)[1], x)
  return(min(x))
}

# allocat excess payment to:
excess_pmt_at <- function(data, direction, split, old = NULL){
  if(!split %in% c("single", "multiple")) stop('`split` must be character in ("single", "multiple")')
  if(!direction %in% c("smallest", "biggest")) stop('`split` must be character in ("smallest", "biggest")')
  if(!is.null(old)){
    temp <- data[-c(old), ]
  }else{
    temp <- data
  }
  if(direction=="biggest"){
    loan <- temp[which(temp$principle == max(temp$principle)), "loan"]
    if(split == "single") loan <- loan[1]
    
  }else if(direction=="smallest"){
    loan <- temp[which(temp$principle == my_min(temp$principle)), "loan"]
    if(split == "single") loan <- loan[1]
    
  }else{
    stop('`direction` must be character in ("biggest", "smallest")')
  }
  here <- loan
  return(here)
}

reduce <- function(data, direction, split, excess_pmt = NULL){
  temp <- data
  if(sum(temp$principle - temp$payment > 0) == 0){
    data <<- temp
    return(function(){break} )
  }
  # temp2 <- temp %>% mutate(payment = excess_pmt*weight) %>% filter(weight != 0)
  temp2 <- temp %>% filter(weight != 0)
  for(i in 1:nrow(temp2)){
    if(temp2[i,]$payment > temp2[i,]$principle){
      difference <- temp2[i, "payment"] - temp2[i, "principle"]
      temp2[i, "weight"] <- 0
      # temp[which(temp$weight != 0)[i], "payment"] <- temp[which(temp$weight != 0)[i], "principle"]
      if(i == nrow(temp2)){
        here <- excess_pmt_at(temp, direction, split)
        here_new <- excess_pmt_at(temp, direction, split, old = here)
        temp <- temp %>% mutate(weight = ifelse(loan %in% here_new, 1/length(here_new), 0),
                                payment = payment + difference*weight)
        temp[temp2[i, "loan"], "payment"] <- temp[temp2[i, "loan"], "payment"] - difference
        # return(reduce(temp, direction, split, excess_pmt = difference))
      }else{
        temp[temp2[i, "loan"], "payment"] <- temp[temp2[i, "loan"], "payment"] - difference
        temp2[(i+1), "payment"] <- temp2[(i+1), "payment"] + difference
      }
    }
  }
  return(temp)
}



print("this function isnt perfect and will not return proper principle values at end in the event of 'excess'")
future_values <- function(loans, monthly_pmt, excess_pmt, direction, split, ret = "data"){
  # 309 is the fixed payment
  # if(!is.null(excess_pmt)){
  #   loans <- cbind.data.frame(loans, excess_pmt_indicator = rep(NA, 10))
  # }
  
  data <- loans
  temp <- data
  i = 1
  while(sum(temp$principle) > pmt){
    pmt <- monthly_pmt
    # if (i == 5) stop()
    if(i == 1){
      # basically hard coded (cuz we dont do anything about excess interest ? but this is okay because pmt is also hard coded at 309)
      pmt <- pmt - sum(data$int_due)
      temp$int_due <- 0
    }
    
    pmt <- pmt - sum(temp %>% mutate(principle * monthly_eff_interest_rate) %>% pull)
    if(pmt < 0) stop("need to code for interest being left over")
    temp <- temp %>% mutate(weight = principle/sum(temp$principle))
    temp <- temp %>% mutate(payment = pmt*weight)
    temp2 <- temp %>% select(loan, payment)
    temp <- temp %>% mutate(principle = principle - payment,
                            weight = 0,
                            payment = 0)
    
    if(!is.null(excess_pmt)){
      if(!is.numeric(excess_pmt)){
        stop("need numeric argument")
      }else if(excess_pmt <= 0){
        "skip"
      }else{
        here <- excess_pmt_at(temp, direction, split)
        temp <- temp %>% mutate(weight = ifelse(loan %in% here, 1/length(here), 0))
        temp <- temp %>% mutate(payment = excess_pmt*weight)

        if( sum(temp %>%
                filter(weight != 0) %>%
                mutate(payment > principle) %>%
                pull()
                ) != 0 ){
          payment <- here[which(temp %>%
                                  mutate(payment = excess_pmt*weight) %>%
                                  filter(weight != 0) %>%
                                  mutate(payment > principle) %>%
                                  pull())
                          ]
          temp <- reduce(temp, direction, split)
          # temp <- temp %>% mutate(principle = principle - payment)
        # }else{
          # temp <- temp %>% mutate(principle = principle - payment)
        }
        temp <- temp %>% mutate(principle = principle - payment)
        temp$month <- rep(i, times = 10)
        
      }
    }
    i = i + 1
    temp <- inner_join(temp, temp2, by = c("loan")) %>%
      mutate(payment = payment.x + payment.y) %>%
      select(-payment.x, -payment.y)
    data <- rbind.data.frame(data, temp)
    temp <- data %>% filter(month == max(month))
    # print(temp)
    # warning(i)
  }
  excess_ <- data %>% filter(month == max(data$month)) %>% filter(principle < 0) %>% pull(principle) %>% sum() %>% abs()
  options(scipen = 999)
  data <- rbind.data.frame(data %>% filter(month != max(data$month)),
                           data %>% filter(month == max(data$month)) %>% mutate(principle = ifelse(principle < 0, 0, principle))
                           )
  
  if(ret == "data") print(data %>% filter(month == max(data$month)))
  warning(paste("excess:", excess_))
  print(paste("excess:", excess_))
  
  end <- max(data$month)
  future_value <- data %>%
    mutate(accum_pmt = payment*(1+monthly_eff_interest_rate)^(end - month)) %>% 
    pull(accum_pmt) %>%
    sum()
  future_value <- future_value + loans %>% mutate(int_due*(1+monthly_eff_interest_rate)^(end - month)) %>% pull() %>% sum()
  
  warning(paste("future_value:", future_value))
  print(paste("future_value:", future_value))
  if(ret == "data"){
    return(data)
  }else{
    return(c("month" = end, "excess_" = excess_, "future_value" = future_value))
  }
}

data1 <- future_values(loans, monthly_pmt = 309, excess_pmt = 1000,
                       direction = "biggest", split = "multiple", ret = "(not data)")
data2 <- future_values(loans, monthly_pmt = 309, excess_pmt = 1000,
                      direction = "smallest", split = "multiple", ret = "(not data)")
View(round(data, 3))



# PV = $29,943.85
sum(309 * loans$principle / sum(loans$principle) * (1 - (1+loans$monthly_eff_interest_rate)^(-120))/loans$monthly_eff_interest_rate)
# FV = $46,524.36
sum(309 * loans$principle / sum(loans$principle) * # monthly payment
      (1 - (1+loans$monthly_eff_interest_rate)^(-120)) / loans$monthly_eff_interest_rate * # a angle n
      (1+loans$monthly_eff_interest_rate)^120 # v^-n
    )




