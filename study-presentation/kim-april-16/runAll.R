source('R/util.R')
source('R/grouped.forecast.R')
source('R/postprocess.R')

train <- raw.train()
test <- raw.test()

# Make the 3 simple models, shift their values and average them.
# The shifted average gets 2503 on the final board.
# This first part runs in about 5 minutes.
simple.names <- c('tslm.basic', 'seasonal.naive', 'product') 
shifts <- c(2.5, 2, 2)
simple.nums <- 0 * shifts
system.time(                                                                    # 171초
  for(k in 1:3){
    print(paste('Predicting on model:', simple.names[k]))
    pred <- grouped.forecast(train, test, simple.names[k])
    print(paste('Shifting predictions for model:', simple.names[k]))
    pred <- postprocess(train, pred, shift=shifts[k])
    simple.nums[k] <- write.submission(pred)  # 1.tslm(2606.86, 20등), 2.snaive(2674.51, 25등), 3.product(2767.60, 44등)
  }
)
system.time(pred <- make.average(simple.nums))                                  # 1.27초
print('This is the shifted average of simple models.')
# keep the number, because this goes into the final model
system.time(sub.nums <- write.submission(pred))                                 # 0.83초  4.mean(2426.77, 5등)

# This is model 5 from the post, regression on Fourier series terms with 
# non-seasonal arima errors. This model scores poorly on its own, but 
# improves the average anyway. This model is shifted by 1 because its
# period is 365/7, not 52. It is also very smooth, so the shift actually
# makes no difference here anyway.
#
# NB: This model may take a couple of hours to run
system.time(pred.fa <- grouped.forecast(train, test, 'fourier.arima', k=12))    # 5613초(1시간 34분) 5.fourier.arima(2385.77, 3등)
system.time(result$uni.fa <- postprocess(train, pred.fa, shift=1))              # 22.85초
s.num <- write.submission(result$uni.fa)                                        # shift=1를 안하면 (2845.78, 64등)
sub.nums <- c(sub.nums, s.num)

# This is model 1 from the post. It gets 2348 on the final board.
system.time(pred.sse <- grouped.forecast(train, test, 'stlf.svd', model.type='ets', n.comp=12))   # 156.87초
system.time(result$uni.sse <- postprocess(train, pred.sse, shift=2.5))                            # 23초
system.time(s.num <- write.submission(result$uni.sse))                                            # 6.stlf.svd.ets(2328.86, 2등)
system.time(sub.nums <- c(sub.nums, s.num))                                                       # 

# This is model 2 from the post.
system.time(pred.ssa <- grouped.forecast(train, test, 'stlf.svd', model.type='arima', n.comp=12)) # 297.56초
system.time(result$uni.ssa <- postprocess(train, pred.ssa, shift=2.5))                            # 23.50초
system.time(s.num <- write.submission(result$uni.ssa))                                            # 7.stlf.svd.arima(2350.25,2등)
system.time(sub.nums <- c(sub.nums, s.num))                                                       # 

# This is model 3 from the post, the one that averages predictions.
system.time(pred.sn <- grouped.forecast(train, test, 'stlf.nn', k=5, level1=0.95, level2=0.8))    # 122초
system.time(result$uni.sn <- postprocess(train, pred.sn, shift=2.5))                              # 23초
s.num <- write.submission(result$uni.sn)                                                          # 8.stlf.nn(2376.88, 3등)
sub.nums <- c(sub.nums, s.num)

# This is model 4, the seasonal arima model.
# It gets shifted 2 days, because all of the models are (p, d, q)(0, 1, 0)[52],
# they are like arima errors on a seasonal naive model.
# n.comp=15 is what I actually used.




# This is the final result.
system.time(pred.mean <- make.average(sub.nums))                                                   # 2.33초
system.time(final.num <- write.submission(pred.mean))                                              # 평균값(2250.60, 1등)
print(paste0('The final average is submission', final.num, '.csv'))
