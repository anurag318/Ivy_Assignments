#Question -1  
new.function <- function(a,b,c)
{
  if(c=="add")
  {print(a+b)}
  else if(c=="subtract")
  {print(a-b)}
  else if(c=="divide")
  {print(a/b)}
  else if (c=="multiply")
  {print(a*b)}
  else if (c=="log")
  {print(log(a,base=b))}
  else if (c=="power") 
  {print(b^a)}
  else
    return(-1)
  
}
# Question 2
new.function1 <- function(a)
 { i <- 1
while(i<=a)
  { 
  print(i)
  i = i+2
}  
}

# Question 3

a <- LETTERS[1:26]
 for(i in a)
 {if(i=="A" || i=='E'||i=="I"||i=="O"||i=="U")
   print(i)
   }
# Question 4

function4 <- function(a)
{ print(class(a))}

# Question 5

a <- 2

n = 0
if(a > 1) {
  n = 1
  for(i in 2:(a-1)) {
    if ((a %% i) == 0) {
      n = 0
      break
    }
  }
} 

if(a==2)
  n=1

if(n == 1) {
  print(paste(a,"is a prime number"))
} else {
  print(paste(a,"is not a prime number"))
}