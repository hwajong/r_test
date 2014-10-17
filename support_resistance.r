# 데이터 로딩
load_data = function(fname, ndata)
{
  data_org = read.csv(fname)  
  
  date = data_org[[1]] # 날짜
  price = data_org[[2]] # 종가 
  
  # 최근 ndata 개수만 처리함
  date = tail(date, ndata) 
  price = tail(price, ndata) 
  date_d = as.Date(date, format='%Y.%m.%d') # 날짜타입변환
  
  data = list("date"=date_d, "price"=price)
  
  return(data)
}

# 로컬 고점 찾기
find_local_peaks = function(data, delta)
{
  date = data[["date"]]
  price = data[["price"]]
  
  peak_date = c()
  peak_price = c()
  
  n = length(price)
  for(i in 1:n) 
  {
    index_left  = i - delta
    index_right = i + delta
    if(index_left < 1) index_left = 1
    if(index_right > n) index_right = n
    
    if(max(price[index_left:index_right]) == price[i])
    {
      #print("local peak : ", str(price[i]))
      peak_date = c(peak_date, date[i])
      peak_price = c(peak_price, price[i])
    }
  }
  
  class(peak_date) = "Date" # 날짜타입변환
  
  peaks = data.frame("date"=peak_date, "price"=peak_price)
  
  # 종가 기준으로 정렬
  peaks = peaks[order(-peaks[,2]), ]
  
  return(peaks)
}

# 비슷한 고점을 카운팅
count_similar_peak = function(peaks, threshold)
{
  date = peaks$date
  price = peaks$price
  count = rep(0, length(date))
  
  # 비슷한 고점 카운팅
  n = length(price)
  for(i in 1:n) 
  {
    cur_price = price[i]
    
    a = price > cur_price * (1-threshold)
    b = price < cur_price * (1+threshold)
    
    c = a*b
    count[i] = length(price[c])
  }
  
  peaks = data.frame("date"=date, "price"=price, count)
  
  # 카운트 순으로 정렬
  peaks = peaks[order(-peaks$count), ]
  
  return(peaks)
}

# 비슷한 고점 제거
cut_similar_peak = function(peaks, threshold)
{
  n = length(peaks$price)
  i = 1
  prev_color = ""
  while(i < n)
  {
    prices = peaks$price
    cur_price = prices[i]
    
    a = prices > cur_price * (1-threshold)
    b = prices < cur_price * (1+threshold)
    c = a*b 
    c[i] = 0 
    #print(c);
    c = c * c(1:n)
    #print(c)
    similar = peaks[c,]

    rand_color = prev_color
    while(rand_color ==  prev_color)
    {
      rand_color = sample(c("#FF0000", "#FFFF00", "#FF00FF", "#00FFFF", "#00FF00", "#0000FF"), 1, replace=TRUE)
      rand_color = paste(rand_color, collapse = '')      
    }
    
    prev_color = rand_color
    points(similar$date, similar$price, lwd=3, col=rand_color)
    #points(peaks$date[i], peaks$price[i], lwd=10, col=rand_color)

    print(sum(c))
    if(sum(c) > 0)
    {
      peaks = peaks[-c,]  
      #print(peaks)    
    }
    
    n = length(peaks$price)
    i = i + 1
  }
  
  # count > NTREND_THRESHOLD 인 가격이 저항선
  peaks = peaks[peaks$count >= NTREND_THRESHOLD, ]  
  
  return(peaks)
}

# 차트 그리기
draw_chart = function(data)
{
  plot(data[["date"]], data[["price"]], type="l", lwd = 1, col="blue", xlab="Date", ylab="Price")
  grid()  
}

# 저항선 그리기
draw_resistance_line = function(data, peaks)
{
  n = length(peaks$price)
  xx = c(data[["date"]][1], data[["date"]][length(data[["date"]])])
  for(i in 1:n)
  {
    yy = c(peaks$price[i], peaks$price[i])
    lines(xx, yy, lwd=2, lty='dashed', col='red');
    
    points(peaks$date[i], peaks$price[i], lwd=5, col='black')
  }  
}

#########################################################################
# 설정할 파라미터
NDATA = 300 # 처리할 데이터 개수
LDELTA = 2  # 로컬 고점 찾을때 좌우 검색 데이터 개수
SIMILAR_PERCENT = 0.05 # 비슷한 고점을 찾을때의 기준 +- 차이값 비율
NTREND_THRESHOLD = 5  # 저항선 판단 기준(비슷한 고점이 몇개이상 일때)

# 데이터 로딩
data = load_data('하이소닉(106080).txt', NDATA)

# 차트 그리기
draw_chart(data)

# 로컬 고점 찾기
peaks_local = find_local_peaks(data, LDELTA)

# 로컬 고점 표시
points(peaks_local$date, peaks_local$price, lwd=2, col="#BBBBBB")

# 비슷한 고점 카운팅
peaks_local = count_similar_peak(peaks_local, SIMILAR_PERCENT)

# 비슷한 고점 제거 & 비슷한 고점 표시
peaks_uniq = cut_similar_peak(peaks_local, SIMILAR_PERCENT)

# 저항선 그리기
draw_resistance_line(data, peaks_uniq)

print(peaks_uniq)

####### TODO 
# 저항선 가격 표시





