library(stratbuilder3)
library(forecast)
library(arfima)
library(tseries)

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
optimum_take_profit <- function(prices, signals, window, Stop_Loss, forall = FALSE){
  
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
  for(i in trade_times){     #t_L -- время до лося внутри окна(= длине окна, если лось не достигается)
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
  
  k<- sum(pr_of_attain >= 0.2) # Сколько достигли 20%  (это же есть номер максимального MP(k), 
  # который встречался чаще 20% в силу монотонности
  
  returns <- sort(unique(MP)) * (length(trade_times) - cumsum(c(0,nums))[-N-1]) +   # Функция доходности в случае
    cumsum(c(0,nums))[-N-1] *  c(0, cumsum(losts) / cumsum(nums))[-N-1]             # когда ТП = MP(i)
  # 0 нужен, ибо есть MP(1)=0
  
  print(max(returns[2:k]))                                 # Выбираю лишь те тейки, которые встречаются не очень редко
  Take_Profit <- sort(unique(MP))[which.max(returns[2:k])] # не 1:k , чтоб нолик не дропался
  return(Take_Profit)
}

bb_signals_GBP <- BBands(GBPUSD$mat$close[1:(350000)], n=20, nsd = 2, matype = EMA)
bb_times <- (-(bb_signals_GBP[,4] < 1) * (Lag(bb_signals_GBP[,4]) >= 1)) +
  (bb_signals_GBP[,4] >= 0) * (Lag(bb_signals_GBP[,4]) < 0)
bb_times[is.na(bb_times)]<-0
# bb_times - это сигналы, построенные по pctB

take_pr <- optimum_take_profit(GBPUSD$mat$close[1:(350000)], bb_times, 10, -0.0002,forall = T)
st_l <- 0.0002 


this <- Strategy() %>% 
  setParams(
    max_bars = 10) %>%  
  addIndicator(
    name = 'predicted_value',
    expr = {
      bb_times  
    }
  ) %>%
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
      trunc(money_last / data$mat$adjusted[i])    
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
      -trunc(money_last  / data$mat$adjusted[i])
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
  )%>% addToReport(Stats$return.pos.drawdown, Stats$sharpe, Stats$trades.year, Stats$ntrades,
                   Stats$return.ann) #%>% setCommission(data$mat$adjusted[i,] * abs(pos_change) * 0.00005)


x <- GBPUSD$mat$close[1:(350000)]                         
dataa <- data_from_xts(xts(x, GBPUSD$dates[1:(350000)]))  # У меня массив прогнозов завязан на i

setData(this, dataa)
perform(this)
getReport(this)

plotPnL(this)

mean(getTrades(this)[[2]] - getTrades(this)[[1]]) # средняя жизнь сделки





