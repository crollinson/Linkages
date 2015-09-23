tempe <- function(temp.vec){
  ddbase = 5.56 #temp above which degree days are counted
  degd = 0
  days = c(31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.)
  #sum degree days for consecutive months
  for(i in 1:12){
    if(temp.vec[i] < ddbase) next
    degd <- degd + (temp.vec[i] - ddbase) * days[i]
  }
  return(list(degd=degd))
}