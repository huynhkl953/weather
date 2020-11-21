#' A Weather Simulation Function
#' Write a function to simulate the weather forecast in Richmond. Assume there are two states of weather in Richmond: sunny and rainy. If a day is sunny, the probability that the next day is sunny is 0.85. If a day is rainy, the probability that the next day is rainy is 0.35. If a day is rainy, the amount of rainfall accumulation in the city is governed by an Exponential(λ=2) distribution, where the value from that distribution is the rainfall in inches. If a day is sunny, there can be no rain.
#' Specifically, given an initial day’s weather conditions, simulate the following 10 days of weather and calculate the projected rainfall accumulation in inches.
#' @param n Number of Simulation
#' @param initial_sun Do the first day sun? (y/n)
#' @export
#' @examples
#' weather_sim()

weather_sim=function(n,initial_sun=T){
  weather=matrix(0,nrow=10,ncol=n)
  rainfail=matrix(0,nrow=10,ncol=n)
  for(i in 1:n){
    if(initial_sun==T){
      weather[1,i]=T
      rainfail[1,i]=0
      for(j in 2:10){
        if(weather[j-1,i]==T){
          weather[j,i]=rbernoulli(1,0.85)
          if(weather[j,i]==T){
            rainfail[j,i]=0}else{rainfail[j,i]=rexp(1,2)}
        }else{
          weather[j,i]=rbernoulli(1,1-0.35)
          if(weather[j,i]==T){
            rainfail[j,i]=0}else{rainfail[j,i]=rexp(1,2)}
        }
      }
    }else{
      weather[1,i]=F
      rainfail[1,i]=rexp(1,2)
      for(j in 2:10){
        if(weather[j-1,i]==T){
          weather[j,i]=rbernoulli(1,0.85)
          if(weather[j,i]==T){
            rainfail[j,i]=0}else{rainfail[j,i]=rexp(1,2)}
        }else{
          weather[j,i]=rbernoulli(1,1-0.35)
          if(weather[j,i]==T){
            rainfail[j,i]=0}else{rainfail[j,i]=rexp(1,2)}
        }
      }
    }
  }
  mylist=list("weather"=weather,"rainfail"=rainfail)
  return(mylist)
}
