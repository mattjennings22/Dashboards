# clear workspace
rm(list=ls(all=TRUE))

# library(tables)
library(tidyverse)
library(knitr)
library(lubridate)
library(forecast)

airlineForecast <- function(trainingDataFile, validationDataFile) {
  
  # Function 1 - Load and format data
  formatting <- function(csv) {
    data <- read_csv(csv)
    # interpret date strings as dates
    data$departure_date <- as.Date(data$departure_date, "%m/%d/%Y")
    data$booking_date   <- as.Date(data$booking_date  , "%m/%d/%Y")
    # add days until departure
    data$days_to_departure <- (data$departure_date - data$booking_date)
    # we want to model/predict by day of week, so add weekday of departure and booking date
    data$booking_day   <- wday(data$booking_date  , label=TRUE, abbr=FALSE)
    data$departure_day <- wday(data$departure_date, label=TRUE, abbr=FALSE)
    # create forward time scale
    data$t_minus <- (data$booking_date - data$departure_date)
    # create time periods prior to departure as a factor
    data$time_cut <- 
      cut(as.numeric(floor(difftime(data$departure_date, 
                                    data$booking_date, 
                                    unit="days"))),
          breaks = c(0,7,Inf),
          right = FALSE)
    # create daily bookings variable
    n <- length(data$cum_bookings)
    data$bookings <- NA
    data$bookings[2:n] <- 
      data$cum_bookings[2:n] - data$cum_bookings[1:(n-1)]
    
    dayZero = which(data$days_to_departure==60)
    data$bookings[dayZero] <- data$cum_bookings[dayZero]
    # create final bookings variable
    finalBookings <- filter(data, departure_date==booking_date) %>% 
      select(departure_date, final_bookings=cum_bookings)
    
    data <- data %>% 
      dplyr::left_join(finalBookings, by = "departure_date")
    # create bookings remaining
    data$bookings_remaining <- data$final_bookings - data$cum_bookings
    # split data by departure day for easier exploratory analysis
    tdat.mon <- filter(data, departure_day=="Monday")
    tdat.tue <- filter(data, departure_day=="Tuesday")
    tdat.wed <- filter(data, departure_day=="Wednesday")
    tdat.thu <- filter(data, departure_day=="Thursday")
    tdat.fri <- filter(data, departure_day=="Friday")
    tdat.sat <- filter(data, departure_day=="Saturday")
    tdat.sun <- filter(data, departure_day=="Sunday")
    
    return(data)
    
  }
  # Create formatted data frames
  training <- formatting(trainingDataFile)
  validation <- formatting(validationDataFile)
  
  
  # Function 2 - compute MASE scoring and create output list
  scoring <- function(valid.data) {
    # select just stuff needed for scoring
    sDat <- valid.data %>% 
      select(departure_date, booking_date, forecast, naive_forecast, final_demand)
    
    # score our model vs. naive model
    sDat$forecast_error       <- sDat$final_demand - sDat$forecast
    sDat$naive_forecast_error <- sDat$final_demand - sDat$naive_forecast 
    
    idx <- !is.na(sDat$naive_forecast_error)
    
    MASE <- 
      sum(abs(sDat$forecast_error[idx])) / sum(abs(sDat$naive_forecast_error[idx]))
    
    output <- list(MASE, sDat[1:3])
    
    return(output)
    
  }
  
  #' ---------------------------------------------------------------------------
  #' Model #1: Multiple Regression
  #' 
  #' Here we assume the log(cum_bookings) is a polynomial function of days to 
  #' departure. The terms for booking day and departure day allow the model to 
  #' vary to capture seasonal variations present in the data
  #' 
  #' exploratory data analysis shows this linear relationship may not hold for 
  #' very early bookings, so we estimate the model using data starting at 28 days
  #' ahead of departure.
  
  
  # Function 3 - create Model 1
  model1 <- function(train.data, valid.data) {
    
    m1.data.train <- filter(train.data, days_to_departure < 29)
    
    m1.data.train %>% 
      dplyr::group_by(booking_day, departure_day, time_cut) %>% 
      summarize(N=n(), avg_cum=mean(cum_bookings)) %>% 
      kable()
    
    # f1 <- ("log1p(cum_bookings) 
    #        ~ (as.numeric(t_minus))*departure_day")
    # f1 <- ("log1p(cum_bookings)
    #        ~ (as.numeric(t_minus))*booking_day*departure_day")
    # f1 <- ("log1p(cum_bookings)
    #        ~ (poly(as.numeric(t_minus), degree=4, raw=T))*booking_day*departure_day")
    # f1 <- ("log1p(cum_bookings)
    #        ~ (poly(as.numeric(t_minus), degree=2) + time_cut + booking_day)
    #        * departure_day")
    # f1 <- ("log1p(cum_bookings)
    #        ~ (as.numeric(t_minus))*booking_day*time_cut*departure_day")
    f1 <- ("log1p(cum_bookings)
           ~ (as.numeric(t_minus) + booking_day)*time_cut*departure_day")
    f1 <- as.formula(gsub("\n", "", f1))
    m1 <- lm(f1, data = m1.data.train)
    
    m1.pred.train <- predict(m1)
    
    #quick plots
    plot(m1.data.train$t_minus, m1.pred.train)
    plot(m1.data.train$t_minus, log1p(m1.data.train$cum_bookings))
    
    plot(x=(log1p(m1.data.train$cum_bookings) - m1.pred.train),
         y=(log1p(m1.data.train$final_bookings)))
    
    # now our model represents historical averages for booking rate and seasonal
    # effects.
    
    # predicted final bookings based on historical data
    daysOfWeek <- sort(unique(m1.data.train$departure_day))
    timeCuts <- sort(unique(m1.data.train$time_cut))
    m1.pred <- data.frame(t_minus = 0,
                          booking_day = daysOfWeek,
                          departure_day = daysOfWeek,
                          time_cut = timeCuts[1],
                          stringsAsFactors = FALSE)
    
    m1.pred$log_final_bookings.pred.m1 <- predict(m1, newdata = m1.pred)
    m1.pred$final_bookings.pred.m1 <- expm1(m1.pred$log_final_bookings.pred.m1)
    
    #
    # add final bookings as predicted from historical data to training data
    #
    m1.data.train <- left_join(m1.data.train, 
                               m1.pred[c("departure_day", "log_final_bookings.pred.m1")], 
                               by="departure_day")
    
    # plot deviation from historical final bookings vs deviation from historical
    # cum_bookings
    eCum <- log1p(m1.data.train$cum_bookings) - m1.pred.train
    eFinal <- log1p(m1.data.train$final_bookings) - m1.data.train$log_final_bookings.pred.m1
    plot(x=eCum, y=eFinal)
    
    cor(eCum, eFinal)
    # [1] 0.769487
    

    # sort in time order per departure date
    m1.data.train <- dplyr::arrange(m1.data.train, departure_date, booking_date)
    
    # calculate sliding average of offest
    m1.data.train$log_cum_bookings <- log1p(m1.data.train$cum_bookings)
    m1.data.train$log_cum_bookings.pred.m1 <- predict(m1, newdata = m1.data.train)
    
    m1.data.train$log_offset.m1 <- 
      m1.data.train$log_cum_bookings - m1.data.train$log_cum_bookings.pred.m1
    
    m1.data.train$log_offset.m1.mean <- NA
    n <- 0
    for(i in 1:length(m1.data.train$log_offset.m1)) {
      # accumulate and calculate mean log offset for each departure
      n <- min((n+1), 28)
      acc_offset <- sum(m1.data.train$log_offset.m1[(i-n+1):i])
      m1.data.train$log_offset.m1.mean[i] <- acc_offset / n
      
      # # reset accumulator at transition to next deprture date
      # if (m1.data.train$departure_date[i] == m1.data.train$booking_date[i]) {
      #   n <- 0
      # }
    }
    
    # plot deviation from historical final bookings vs deviation from historical
    # cum_bookings
    eCumAvg <- m1.data.train$log_offset.m1.mean
    eFinal <- log1p(m1.data.train$final_bookings) - m1.data.train$log_final_bookings.pred.m1
    plot(x=eCumAvg, y=eFinal)
    
    cor(eCumAvg, eFinal)
    # [1] 0.769487
    
    
    # forecast final bookings in validation data 
    #
    valid.data <- left_join(valid.data, 
                            m1.pred[c("departure_day", "log_final_bookings.pred.m1")], 
                            by="departure_day")
    
    valid.data$log_cum_bookings <- log1p(valid.data$cum_bookings)
    valid.data$log_cum_bookings.pred.m1 <- predict(m1, newdata = valid.data)
    
    valid.data$log_offset.m1 <- 
      valid.data$log_cum_bookings - valid.data$log_cum_bookings.pred.m1
    
    # sort in time order per departure date
    valid.data <- dplyr::arrange(valid.data, departure_date, booking_date)
    
    # calculate sliding average of offest
    valid.data$log_offset.m1.mean <- NA
    n <- 0
    windowLen = 1
    for(i in 1:length(valid.data$log_offset.m1)) {
      # accumulate and calculate mean log offset for each departure
      n <- min((n+1), windowLen)
      
      if(n<windowLen){
        end <- length(m1.data.train$log_offset.m1)
        beg <- end - (windowLen - n) + 1
        blend = sum(m1.data.train$log_offset.m1[beg:end])
      } 
      else(
        blend = 0
      )
      
      acc_offset <- sum(valid.data$log_offset.m1[(i-n+1):i]) + blend
      valid.data$log_offset.m1.mean[i] <- acc_offset / windowLen
      
      # # reset accumulator at transition to next deprture date
      # if (valid.data$departure_date[i] == valid.data$booking_date[i]) {
      #   n <- 0
      # }
    }
    
    logFinal.hist <- valid.data$log_final_bookings.pred.m1
    valid.data$forecast <- expm1(logFinal.hist + valid.data$log_offset.m1.mean)
    
    return(valid.data)
  }
  m1 <- model1(training, validation)
  output1 <- scoring(m1)
  
  #' ---------------------------------------------------------------------------
  #' Model #2: Multiple Regression
  #'
  #' Here we estimate the percent of bookings remaining as function of days to
  #' departure and bookings on hand. The terms for booking day and departure day
  #' allow the model to vary to capture seasonal variations present in the data.
  #'
  #' exploratory data analysis shows this linear relationship may not hold for
  #' very early bookings, so we estimate the model using data starting at 28 days
  #' ahead of departure.
  
  # Function 4 - create Model 2
  model2 <- function(train.data, valid.data) {
    m2.data.train <- filter(train.data, days_to_departure < 29)

    # f2 <- ("log1p(bookings_remaining)
    #        ~ (poly(as.numeric(t_minus), degree=4) + booking_day )*time_cut*departure_day")
    f2 <- ("log1p(bookings_remaining)
           ~ (poly(as.numeric(t_minus), degree=4) + booking_day + time_cut)*departure_day")
    f2 <- as.formula(gsub("\n", "", f2))
    m2 <- lm(f2, data = m2.data.train)

    m2.pred.train <- predict(m2)

    #quick plot
    plot(m2.data.train$t_minus, m2.pred.train)
    plot(m2.data.train$t_minus, log1p(m2.data.train$bookings_remaining))

    #
    # forecast final bookings in validation data
    #
    valid.data$log_bookings_remaining.pred.m2 <- predict(m2, newdata = valid.data)

    valid.data$forecast <-
      valid.data$cum_bookings + expm1(valid.data$log_bookings_remaining.pred.m2)

    return(valid.data)
  }

  m2 <- model2(training, validation)
  output2 <- scoring(m2)

  #' ---------------------------------------------------------------------------
  #' Model #3: Multiple Regression
  #'
  #' Here we estimate the proportion of bookings remaining as function of days to
  #' departure and bookings on hand. The terms for booking day and departure day
  #' allow the model to vary to capture seasonal variations present in the data.
  #'
  #' exploratory data analysis shows this linear relationship may not hold for
  #' very early bookings, so we estimate the model using data starting at 28 days
  #' ahead of departure.

  # Function 5 - create Model 3
  model3 <- function(train.data, valid.data) {

    # add percent bookings remaining to training data
    train.data$prop_bookings_remaining <-
      train.data$bookings_remaining / train.data$cum_bookings

    #quick plot
    plot(train.data$t_minus, (train.data$prop_bookings_remaining)^(1/3))

    m3.data.train <- filter(train.data, days_to_departure < 29)

    f3 <- ("(prop_bookings_remaining)
           ~ (poly(as.numeric(t_minus), degree=4) + booking_day)*time_cut*departure_day")
    # f3 <- ("(prop_bookings_remaining)
    #        ~ (poly(as.numeric(t_minus), degree=4) + booking_day + time_cut)*departure_day")
    # f3 <- ("I(prop_bookings_remaining^(1/3))
    #        ~ (poly(as.numeric(t_minus), degree=4) + booking_day + time_cut)*departure_day")
    f3 <- as.formula(gsub("\n", "", f3))
    m3 <- lm(f3, data = m3.data.train)

    m3.pred.train <- predict(m3)

    #quick plot
    plot(m3.data.train$t_minus, m3.pred.train)
    plot(m3.data.train$t_minus, (m3.data.train$prop_bookings_remaining))
    # plot(m3.data.train$t_minus, (m3.data.train$prop_bookings_remaining^(1/3)))

    #
    # forecast final bookings in validation data
    #
    valid.data$prop_bookings_remaining.pred.m3 <- (predict(m3, newdata = valid.data))
    # valid.data$prop_bookings_remaining.pred.m3 <- (predict(m3, newdata = valid.data))^3

    valid.data$forecast <-
      valid.data$cum_bookings * (1 + valid.data$prop_bookings_remaining.pred.m3)

    return(valid.data)

  }

  m3 <- model3(training, validation)
  output3 <- scoring(m3)
  
  return(output1)
}

airlineForecast("airline_booking_trainingData.csv", "airline_booking_validationData.csv")

f1 <- airlineForecast("airline_booking_trainingData.csv", "airline_booking_validationData.csv")
check <- as.data.frame(f1[2])
