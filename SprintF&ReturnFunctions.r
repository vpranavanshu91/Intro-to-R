#SPRINTF

sprintf("hello %s","Jared")
sprintf("hello %s, today is %s","Jared","Sunday")

hello.person <- function(first,last="Doe",...)      #... ignores any additional parameters passed onto the function 
{
    print(sprintf("Hello %s %s",first,last))
}

hello.person("Bob")
hello.person("Bob","Charlton")
hello.person("Bob","Charlton","Liverpool")

#Returning Functions

val.dub<-function(x)
{
return(x*2)    #Return is the last statement which is executed in a function
}

val.dub(6)
