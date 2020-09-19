a = c(7:4, 0)
b = c(8, 10.5, 0, -2, 9)
c = a + b
print(c(c, 'суммм'))
c = a * b
print(c(c, 'произведение'))
c = a / b
print(c(c, 'частное'))


print(paste( mean(a) , 'среднее арифметическое a'))
print(paste0( mean(b), ' среднее арифметическое b' ))

print(paste( sum(a) , 'сумма a'))
print(paste0( sum(b), ' сумма b' ))

