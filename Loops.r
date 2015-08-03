for(i in 1:10)
    print(i)

print(1:10)

fruitlength <- c("Apple","Pomegranate","Watermelon","Kiwi")
fruit<- rep(NA,length(fruitlength))             #repeats NA as many times as the no. of elements in fruitlength
names(fruit)<-fruitlength                       #assigns column names

for (n in fruitlength)                          #iterates for no. of times the no of elements present in fruitlength and takes the string as value
{
    fruit[n] <- nchar(n)
}

fruitlength2<-nchar(fruitlength)
names(fruitlength2) <- fruitlength

identical(fruit,fruitlength2)

#####################   Next and Break ###############################

for (i in 1:10) 
    {
    if(i == 3)
    {
        next
    }
    print(i)
    
}

for (i in 1:10) 
{
    if(i == 4)
    {
        break
    }
    print(i)
    
}
############### While Loop ###############################
x<-5
while(x<=5 && x>=1)
{
    print(x)
    x<-x-1
}