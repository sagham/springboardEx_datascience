## Read the data set into R

path<-file.path("R-projects/springboardExercises","refine.csv") ##define the path
refine.ex1 <- read.csv(path, header = TRUE) # load and assign the path
class(refine.ex1)

## Get the "shape" of the data
dim(refine.ex1)
head(refine.ex1)
tail(refine.ex1)
str(refine.ex1)

summary(refine.ex1) # Gives summary statistics

#analyze company data as Character to study the pattern
as.character(refine.ex1$company)

#Optional
#identify patterns and run sub function to substitute companies into company1
refine.ex1$company1<-sub('.*[ps]$','philips',refine.ex1$company1,perl=TRUE)
refine.ex1$company1=sub('.*[Hh]outen$','van houten',refine.ex1$company1,perl=TRUE)
refine.ex1$company1=sub('[aA]k.[zZ]*','akzo',refine.ex1$company1,perl=TRUE)
refine.ex1$company1=sub('uni.*','unilever',refine.ex1$company1)

#load dplyr library 
library(dplyr)


#since patern and replacement is going to be used over multiple columns - it is better to right a function
#the function will take patern vector  and replace it with replace vector

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- sub(pattern[i], replacement[i], result, ...)
  }
  result
}

#use  function call and apply to the Company and ProductCode..number

refine.ex1$company<-mgsub(c('.*[ps|S]$','.*[Hh]outen$','.*[aAkK].[zZ].*','[uU]ni.*'),
                          c('philips','van houten','akzo','unilever'),refine.ex1$company)
refine.ex1$Product.code...number<-mgsub(c('^p','^x','^v','^q'),c('Smartphone','Laptop','TV',
                                                                 'Tablet'),refine.ex1$Product.code...number)


library(tidyr)
#use separate from tidyr package to split col by a delim

 refine.ex1<-separate(refine.ex1,Product.code...number,c('product.code','number'),sep='-')


#library(reshape2)
#use colsplit to split the productcode and number and that same time delim 
#refine.ex1<-cbind(refine.ex1,colsplit(refine.ex1$Product.code...number,"-",names=c('Product.Code','number')))


#using standard paste function to combine colums. 
#refine.ex1$full_address <-paste(refine.ex1$address,refine.ex1$city,refine.ex1$country,sep=',')

#or 
refine.ex1<-unite(refine.ex1,"full_address",address,city,country,sep=",")
summary(refine.ex1)


#use spread function in tidyr package to create dummy variable for Company and product

refine.ex1<-refine.ex1 %>% mutate(DummyValue=TRUE) %>% spread("Product.Code","DummyValue",fill=FALSE)
refine.ex1<-refine.ex1 %>% mutate(DummyValue=TRUE) %>% spread("company","DummyValue",fill=FALSE)
summary(refine.ex1)



