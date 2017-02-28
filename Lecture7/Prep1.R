maxval = function(input){
  if(length(input)<1) return(-1)
  maxval = input[1]
  for(i in 2:length(input)){
    if(input[i]>maxval){
      maxval = input[i]
    }
  }
  return(maxval)
}
a = c(15,25,32,44,21,199)
maxval(a)

b = c()
maxval(b)
