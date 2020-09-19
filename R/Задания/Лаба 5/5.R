{
check.integer <- function(N){
if (!grepl("[[:digit:]]", format(N, digits = 20)) || grepl("[[:alpha:]]", format(N, digits = 20))) {
print('Содержатся буквы')
}
else if (grepl("[,]", format(N, digits = 20))) {
print('Вводить число нужно с точкой, а не запятой!')
}
else {
N <- as.double((N))
n1 <- as.double(round(N,digits= 20))
n2 <-as.double(round(N,digits= 0))
if (n1 == n2 && n2 == N) {
print('TRUE')
} else {
print('False')
}
}
}
check.integer(readline('Введите число: '))
}