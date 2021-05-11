# Загрузили котировки
GBPUSD <- Data() %>%
  modify(period = 5,
         from = as.POSIXct("2016-05-01")) %>%
  stock(c('GBPUSD'),
        src = 'Finam') %>%
  getSymbols
GBPUSD$dates <- unique(GBPUSD$dates)

# Получает на вход вектор цен, вектор сигналов длины = длине вектора цен (1 = лонг, -1 = шорт, 0 = сигнала нет)
# Также получает величину окна и стоп лосс
# Выдаёт величину тейк профита  
e<- new.env()
optimum_take_profit <- function(prices, signals, window, Stop_Loss, forall = FALSE, pr_of_success = 0.25){
  
  trade_times <- which(signals != 0)        # Моменты когда были сигналы на вход
  trade_times <- trade_times[trade_times <= length(prices) - window]  
  
  if(forall == FALSE){ # Если учитываем непересекающиеся окна
    # Отсеиваем чтоб трейды не перекрывались 
    # Но некоторые сигналы тут не учитываем
    t<-trade_times[1]
    acc_window <- 0
    for(i in 1:(length(trade_times)-1)){
      if(trade_times[i+1]-trade_times[i] >=window){
        t <- c(t, trade_times[i+1])
        acc_window <- 0
      }
      else{
        acc_window <- acc_window + (trade_times[i+1]-trade_times[i])
      }
      if(acc_window >= window){
        t <- c(t, trade_times[i+1])
        acc_window <- 0
      }
    }
    trade_times <- t
  }
  
  MP <- numeric(length = length(trade_times)) # Максимальные профиты на окне 
  ML <- numeric(length = length(trade_times)) # Убытки по каждому трейду  
  k<-1
  loses <- 1 # Зачем-то они мне были нужны. Пусть будут
  
  t_L <- window
  for(i in trade_times){     #t_L -- число свечек до лося внутри окна(= длине окна, если лось не достигается)
    if(signals[i] == 1){
      
      if(any(prices[i:(i+window-1)] - prices[i] <= Stop_Loss)){
        t_L <- which(prices[i:(i+window-1)] - prices[i] <= Stop_Loss)[1]
        loses <- loses + 1
      }
      else t_L <- (window)
      MP[k] <- max(prices[i:(i+t_L-1)] - prices[i])
      ML[k] <- min(prices[i:(i+t_L-1)] - prices[i])
      ML[k] <- max(Stop_Loss,prices[(i+t_L-1)] - prices[i])  # В последний день закрываем либо по стопу (если c стоп дошел)
    }                                                        # Либо по цене в последний момент окна
    else if(signals[i] == -1){
      if(any(prices[i:(i+window-1)] - prices[i] >= -Stop_Loss)){
        t_L <- which(prices[i:(i+window-1)] - prices[i] >= -Stop_Loss)[1]
        loses <- loses + 1
      }
      else t_L <- (window)
      ML[k] <- -max(prices[i:(i+t_L-1)] - prices[i])
      MP[k] <- -min(prices[i:(i+t_L-1)] - prices[i])
      ML[k] <- max(Stop_Loss,-prices[(i+t_L-1)] + prices[i])
    }
    k<-k+1
  }
  k<-1
  
  MP <- round(MP, 4)      # Для валюты, для удобства
  N<-length(unique(MP))
  nums <- numeric(length = N)      # Количество равных MP(i) среди MP
  losts <- numeric(length = N)    # Убытки по трейдам в окнах, где достигаетcя MP(i)
  for(i in 1:N) {
    nums[i] <- sum(MP == sort(unique(MP))[i])      # Количество равных MP(i) среди MP
    losts[i] <- sum(ML[(MP == sort(unique(MP))[i])])
  }
  
  # В скольки процентах трейдов достигалась MP(i)? -- pr_of_attain
  pr_of_attain <- (length(trade_times) - c(0,cumsum(nums))[-N-1]) / length(trade_times)
  
  k<- sum(pr_of_attain >= pr_of_success) # Сколько сделок достигло 25%  (это же есть номер максимального MP(k), 
  # который встречался чаще 25% (в силу монотонности) )
  
  returns <- sort(unique(MP)) * (length(trade_times) - cumsum(c(0,nums))[-N-1]) +   # Функция доходности в случае
    cumsum(c(0,nums))[-N-1] *  c(0, cumsum(losts) / cumsum(nums))[-N-1]             # когда ТП = MP(i)
  # 0 нужен, ибо есть MP(1)=0
  
  #print(max(returns[1:k]))                                 # Выбираю лишь те тейки, которые встречаются не очень редко
  Take_Profit <- sort(unique(MP))[which.max(returns[1:k])] 
  return(c(Take_Profit,max(returns[1:k])))                  # Нунжно для оптимизации
}

bb_signals_GBP <- BBands(GBPUSD$mat$close[1:(350000)], n=20, nsd = 2, matype = EMA)
bb_times <- (-(bb_signals_GBP[,4] < 1) * (Lag(bb_signals_GBP[,4]) >= 1)) +
  (bb_signals_GBP[,4] >= 0) * (Lag(bb_signals_GBP[,4]) < 0)
bb_times[is.na(bb_times)]<-0
# bb_times - это сигналы, построенные по pctB

take_pr <- optimum_take_profit(GBPUSD$mat$close[1:(350000)], bb_times, 10, -0.0002,forall = T)[1]
st_l <- 0.0002 


this <- Strategy() %>% 
  setParams(
    predicted_value = bb_times,    
    max_bars = 30) %>%  
  # addIndicator(
  # name = 'predicted_value',
  #  expr = {
  #    pr_value  
  #     }
  # ) %>%
  addRule(
    name = 'long',
    expr = ({
      sign(predicted_value[i])==1
    }
    ),
    on_success = {
      timer(this,bars_in_pos)            # Считаем число баров в сделке
      Entry <- c(data$mat$adjusted[i],1) # цена входа + направление 
    },
    block = 'blocklong',
    pathwise = TRUE,
    position  =   {
      trunc(getMoney(this) / data$mat$adjusted[i])    
    }
  ) %>% 
  addRule(
    name = 'short',
    expr = ({
      sign(predicted_value[i])==-1
    }
    ),
    on_success = {
      timer(this,bars_in_pos)             # Считаем число баров в сделке
      Entry <- c(data$mat$adjusted[i],-1) # цена входа + направление 
    },
    pathwise = TRUE,
    block = 'blockshort',
    position  =   {
      -trunc(getMoney(this)  / data$mat$adjusted[i])
    }
  ) %>%
  addRule(
    name = 'take',
    expr = ({ 
      ((Entry[2]==1) * (-Entry[1] + data$mat$adjusted[i] >= take_pr) )|
        ((Entry[2]==-1) * (-Entry[1] + data$mat$adjusted[i] <= -take_pr)) 
    }), 
    #take_pr
    type = 'exit',
    price = {
      if(Entry[2]==1)  {
        t <- Entry[1] + take_pr     # Выходим по тейку
        Entry <- 0
        t
      }
      else {
        t <-   (Entry[1] - take_pr)  # Выходим по тейку
        Entry <- 0
        t
      }
    },
    pathwise = TRUE,
    block = 'all'
  ) %>%
  addRule(                          # Чтоб не пересиживать в сделке
    expr = bars_in_pos >= max_bars,
    type = 'exit',
    pathwise = TRUE,
    block = 'all'
  ) %>%
  addRule(
    name = 'stop',
    expr = ({
      (Entry[2]==1) * (-Entry[1] + data$mat$adjusted[i] <= -st_l) |     #st_l у нас положительный просто
        (Entry[2]==-1) * (-Entry[1] + data$mat$adjusted[i] >= st_l) 
    }),
    price = {
      if(Entry[2]==1)  {
        return(Entry[1] - st_l)       # Вышли по стопу
      }
      else if(Entry[2]==-1) {
        return(Entry[1] + st_l) 
      }
    },
    type = 'exit',
    pathwise = TRUE,
    block = 'all'
  )%>% addToReport(Stats$sharpe, Stats$trades.year, Stats$ntrades,
                   Stats$return.ann) #%>% setCommission(data$mat$adjusted[i,] * abs(pos_change) * 0.00005)


x <- EURUSD_DATA[[4]][,2]                        
dataa <- data_from_xts(xts(x, EURUSD_DATA[[4]][,1]))  # У меня массив прогнозов завязан на i

setData(this, dataa)
perform(this)
getReport(this)

plotPnL(this)

mean(getTrades(this)[[2]] - getTrades(this)[[1]]) # средняя жизнь сделки

sum(getTrades(this)[[5]] < 0) / length(getTrades(this)[[5]]) # Процент неудачных

x <- xts(x, GBPUSD$dates[1:(350000)])
write.zoo(
  x,
  file = ("GBPUSD5.csv"))

#----------------------------------------------------------------------------------------------------------
# Забектестим это всё на разных стратежках и валютах
#----------------------------------------------------------------------------------------------------------

bb_stratagy_signals <- function(data, lengt){
  bb_signals <- BBands(data[1:(lengt)], n=20, nsd = 2, matype = EMA)
  bb_times <- (-(bb_signals[,4] < 1) * (Lag(bb_signals[,4]) >= 1)) +
    (bb_signals[,4] >= 0) * (Lag(bb_signals[,4]) < 0)
  bb_times[is.na(bb_times)]<-0
  return(bb_times)
}

rsi_stratagy_signals <- function(data, lengt, period=9){
  rsi_ <- RSI(data[1:(lengt)], n=period)
  tr_times <- (-(rsi_ < 70) * (Lag(rsi_) >= 70)) + (rsi_ >= 30) * (Lag(rsi_) < 30)
  tr_times[is.na(tr_times)]<-0
  return(tr_times)
}



# Сбор данных 
EURUSD_DATA <- list()
EURUSD <- list()

{
  EURUSD[[1]] <- Data() %>%
    modify(period = 1,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
  
  EURUSD[[2]] <- Data() %>%
    modify(period = 5,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
  
  EURUSD[[3]] <- Data() %>%
    modify(period = 15,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
  
  EURUSD[[4]] <- Data() %>%
    modify(period = 30,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
  
  EURUSD[[5]] <- Data() %>%
    modify(period = 60,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
  
  EURUSD[[6]] <- Data() %>%
    modify(period = 240,
           from =  as.POSIXct("2012-01-01"),
           to = as.POSIXct("2021-04-01")) %>%
    stock('EURUSD',
          src = 'Finam') %>%
    getSymbols
}

for(k in 1:5){
  EURUSD[[k]]$dates <- EURUSD[[k]]$dates[!is.na(EURUSD[[k]]$mat$close)]
  EURUSD[[k]]$dates <- unique(EURUSD[[k]]$dates)
  EURUSD[[k]]$mat$close <- (EURUSD[[k]]$mat$close[!is.na(EURUSD[[k]]$mat$close)])
  EURUSD[[k]]$mat$close <- EURUSD[[k]]$mat$close[1:length(EURUSD[[k]]$dates)]
  
  EURUSD_DATA[[k]] <- data.frame(
    dates = EURUSD[[k]]$dates[1:(length(EURUSD[[k]]$mat$close))],
    close = EURUSD[[k]]$mat$close,
    BB_signals = bb_stratagy_signals(EURUSD[[k]]$mat$close, length(EURUSD[[k]]$mat$close)),
    Rsi_signals = rsi_stratagy_signals(EURUSD[[k]]$mat$close, length(EURUSD[[k]]$mat$close))
  )
}
rm(EURUSD)

# EURUSD_DATA[[k]] -- данные по EURUSD в c(1,5,15,30,60,240)[k]-m таймфрейме + прогнозы по стратегиям

EURUSD_DATA[[2]] %>% View()

# Далее - Подбор оптимального стоп лосса и тейкпрофита (по 2 шт: оптимальный и оптимальный с частым достижением)

stop_loss <- 0
take_profit <- 0
window <- 0
nrowws <- 6 * 2 # Число строк в ДФ 
STRATAGY_DATA <- data.frame(Currency = rep("EURUSD",nrowws),   # Это будет ключевое хранилище данных по стратегиям
                            Time_Frame = rep(0,nrowws),
                            Stratagy_type = rep("AA",nrowws),
                            Take_Profit = rep(0,nrowws),
                            Stop_Loss = rep(0,nrowws), 
                            Window = rep(0,nrowws),
                            Res = rep(0,nrowws),
                            Sharpe = rep(0,nrowws),
                            Trades.year = rep(0,nrowws),
                            Ntrades = rep(0,nrowws),
                            Return.ann = rep(0,nrowws)
)


repor <- data.frame() # временное хранилище getReport(this)
pnl.max <- 0
tmp_res <- c(0,0)
res <- 0

e <- new.env()
system.time(
  for(TF_number in 1:6){ # Перебор по тайм фреймам
    for(str_number in 1:2){           # Перебор по стратегиям
      pnl.max <- 0
      take_profit <- 0
      stop_loss <- 0
      window <- 0   
      for(windows in seq(4,20,by=4)){  # Перебор по окнам
        for(st_loss in seq(0.0001, 0.0051, by = 0.0002)){
          tmp_res  <- optimum_take_profit(
            EURUSD_DATA[[TF_number]][,2],
            EURUSD_DATA[[TF_number]][,str_number + 2],
            windows,
            -st_loss, T)
          if(tmp_res[2] > pnl.max){
            pnl.max <- tmp_res[2]
            take_profit <- tmp_res[1]
            stop_loss <- st_loss
            window <- windows
          }
        }
      }
      STRATAGY_DATA$Take_Profit[(TF_number-1) * 2 + str_number] <- take_profit
      STRATAGY_DATA$Stop_Loss[(TF_number-1) * 2 + str_number] <- stop_loss
      STRATAGY_DATA$Window[(TF_number-1) * 2 + str_number] <- window
      STRATAGY_DATA$Res[(TF_number-1) * 2 + str_number] <- pnl.max
      
      x <- EURUSD_DATA[[TF_number]][,2]                        
      dataa <- data_from_xts(xts(x, EURUSD_DATA[[TF_number]][,1])) 
      
      st_l <- stop_loss          # st_l и take_pr - это параметры внутри стратегии
      take_pr <- take_profit
      
      setParams(this,
                predicted_value = EURUSD_DATA[[TF_number]][,2 + str_number],    
                max_bars = window)
      
      setData(this, dataa)
      perform(this)
      repor <- getReport(this)
      STRATAGY_DATA$Sharpe[(TF_number-1) * 2 + str_number] <- repor$sharpe
      STRATAGY_DATA$Trades.year[(TF_number-1) * 2 + str_number] <- repor$trades.year
      STRATAGY_DATA$Ntrades[(TF_number-1) * 2 + str_number] <- repor$ntrades
      STRATAGY_DATA$Return.ann[(TF_number-1) * 2 + str_number] <- repor$return.ann
    }
  }
)

STRATAGY_DATA$Stratagy_type <- rep(c('BB', 'RSI'), 6)
STRATAGY_DATA$Time_Frame <- rep(c(1,5,15,30,60,240), each = 2)


STRATAGY_DATA %>% View() 

#------------------------------------------------------------------------------------------------------------------------
# Проблема - маленькие стоп лоссы "оптимальны " на больших ТФ -- из-за отсутствия промежуточных цен между свечками 
# (Проскальзывание не учитывается)
# Идея решения - перенести сигналы с больших ТФ на 1-минутный ТФ, а на них уже оптимизировать стопы и тейки


EURUSD_DATA[[1]] <- cbind.data.frame(EURUSD_DATA[[1]],
                                     BB_signals_5 = rep(0, nrow(EURUSD_DATA[[1]])),
                                     Rsi_signals_5 = rep(0, nrow(EURUSD_DATA[[1]]))
)

# Внимание-внимание, сейчас будет  костыль ! (но он решает проблему)

dates_of_5min_signals <- EURUSD_DATA[[2]][which(EURUSD_DATA[[2]]$BB_signals !=0 | EURUSD_DATA[[2]]$Rsi_signals !=0),]$dates

# Даты 5-мин сигналов, которые есть в массиве с 1-минутными данными 
dates_of_5min_signals_in_1m_array <-  EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_5min_signals,]$dates

# Сигналы 5-минуток записываем в массив с 1-минутными данными 
# Вся тема с in нужна, чтоб даты совпадали

EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_5min_signals,
][,5:6] <- EURUSD_DATA[[2]][EURUSD_DATA[[2]]$dates %in% dates_of_5min_signals_in_1m_array ,3:4]
# Теперь в EURUSD_DATA[[1]][,5:6] содержатся сигналы 5-минуток (в подходящие даты)
# Аналагочино можно добавить сигналы 15/30/60/240 - минуток. Нужно бы только побольше данных по евробаксу на 1-минутках

EURUSD_DATA[[1]] <- cbind.data.frame(EURUSD_DATA[[1]],
                                     BB_signals_15 = rep(0, nrow(EURUSD_DATA[[1]])),
                                     Rsi_signals_15 = rep(0, nrow(EURUSD_DATA[[1]]))
)

dates_of_15min_signals <- EURUSD_DATA[[3]][which(EURUSD_DATA[[3]]$BB_signals !=0 | EURUSD_DATA[[3]]$Rsi_signals !=0),]$dates
dates_of_15min_signals_in_1m_array <-  EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_15min_signals,]$dates

EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_15min_signals,
][,7:8] <- EURUSD_DATA[[3]][EURUSD_DATA[[3]]$dates %in% dates_of_15min_signals_in_1m_array ,3:4]

EURUSD_DATA[[1]] <- cbind.data.frame(EURUSD_DATA[[1]],
                                     BB_signals_30 = rep(0, nrow(EURUSD_DATA[[1]])),
                                     Rsi_signals_30 = rep(0, nrow(EURUSD_DATA[[1]]))
)

dates_of_30min_signals <- EURUSD_DATA[[4]][which(EURUSD_DATA[[4]]$BB_signals !=0 | EURUSD_DATA[[4]]$Rsi_signals !=0),]$dates
dates_of_30min_signals_in_1m_array <-  EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_30min_signals,]$dates

EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_30min_signals,
][,9:10] <- EURUSD_DATA[[4]][EURUSD_DATA[[4]]$dates %in% dates_of_30min_signals_in_1m_array ,3:4]

EURUSD_DATA[[1]] <- cbind.data.frame(EURUSD_DATA[[1]],
                                     BB_signals_60 = rep(0, nrow(EURUSD_DATA[[1]])),
                                     Rsi_signals_60 = rep(0, nrow(EURUSD_DATA[[1]]))
)

dates_of_60min_signals <- EURUSD_DATA[[5]][which(EURUSD_DATA[[5]]$BB_signals !=0 | EURUSD_DATA[[5]]$Rsi_signals !=0),]$dates
dates_of_60min_signals_in_1m_array <-  EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_60min_signals,]$dates

EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_60min_signals,
][,11:12] <- EURUSD_DATA[[5]][EURUSD_DATA[[5]]$dates %in% dates_of_60min_signals_in_1m_array ,3:4]

EURUSD_DATA[[1]] <- cbind.data.frame(EURUSD_DATA[[1]],
                                     BB_signals_240 = rep(0, nrow(EURUSD_DATA[[1]])),
                                     Rsi_signals_240 = rep(0, nrow(EURUSD_DATA[[1]]))
)

dates_of_240min_signals <- EURUSD_DATA[[6]][which(EURUSD_DATA[[6]]$BB_signals !=0 | EURUSD_DATA[[6]]$Rsi_signals !=0),]$dates
dates_of_240min_signals_in_1m_array <-  EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_240min_signals,]$dates

EURUSD_DATA[[1]][EURUSD_DATA[[1]]$dates %in% dates_of_240min_signals,
][,13:14] <- EURUSD_DATA[[6]][EURUSD_DATA[[6]]$dates %in% dates_of_240min_signals_in_1m_array ,3:4]



#------------------------------------------------------------------------------------------------------------------------

for(i in seq(0.0001, 0.0020, by=0.0001)) print(
  optimum_take_profit(EURUSD_DATA[[1]]$close, EURUSD_DATA[[1]]$BB_signals_30, 600, -i, T,0.25))

take_pr <- 0.0023
st_l <- 0.0009

setParams(this,
          predicted_value = EURUSD_DATA[[1]]$BB_signals_30,    
          max_bars = 600)
x <- EURUSD_DATA[[1]][,2]                        
dataa <- data_from_xts(xts(x, EURUSD_DATA[[1]][,1])) 

setData(this, dataa)
system.time(perform(this)) # 140 сек на 850k 1-min свечках. Это с 2019 года
getReport(this)
plotPnL(this)
getTrades(this)

mean(getTrades(this)[[2]] - getTrades(this)[[1]]) # средняя жизнь сделки
sum(getTrades(this)[[5]] < 0) / length(getTrades(this)[[5]]) # Процент неудачных

