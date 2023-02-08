countingSort <- function(arr, exp1){

  n = length(arr)
  output = rep(0, n)
  count = rep(0, 10)

  for (i in 0:n-1){
    index = floor(arr[i+1] / exp1)
    count[(index %% 10) + 1] = count[(index %% 10) + 1] + 1
  }

  for (i in 2:10){
    count[i] = count[i] + count[i-1]
  }

  i = n
  while(i > 0){
    index = floor(arr[i] / exp1)
    output[count[(index %% 10) + 1]] = arr[i]
    count[(index %% 10) + 1] = count[(index %% 10) + 1] - 1
    i = i -1
  }
  return(output)
}

radixSort <- function(arr){
  max1 = max(arr)
  exp = 1
  while (floor(max1 / exp) > 0){
    cs <- countingSort(arr, exp)
    arr<-cs
    exp = exp * 10
  }
  return(cs)
}


#code for the decimal numbers

#radix sort function for decimal numbers

radix_sort_decimal <- function(arr){
  decimal_number <- max(arr) #choose the max number
  whole_number_part <- floor(decimal_number) #get the floor of that number
  decimal_part <- decimal_number - whole_number_part #calculate the decimal part
  arr = arr * 10^decimal_part #make the number as integer number
  arr = as.integer(arr)
  arr = radixSort(arr) #sort using the radix sort we have
  arr = arr / 10^decimal_part #return to the original number
  return(arr)
}
