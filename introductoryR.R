# Jung Mee Park
# jmpark@arizona.edu

# functions
add = function (a,b){
  result = a + b
  return(result)
}

# branching
if (logical expression) {
  statements
} else {
  alternative statements
}

# for loops
for (i in 1:10) {
  print (i*i)
}

i = 1
while(i<=10) {
  print(i*i)
  i=i+sqrt(i)
}

# lapply, sapply, apply
# it is more efficient than for loops

# grep useful for subsetting datasets
grep ("L", a, value=T)