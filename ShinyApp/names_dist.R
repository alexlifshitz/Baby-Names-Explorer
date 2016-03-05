my.dist <- function(list1, list2, n) {
     # Calculate a distance between two names lists
     
     a<-abs(match(list1[1:n], list2)-1:n)
     a[is.na(a)]<-length(list2)
     a[a>=2*n]=2*n
     d1<-sum(a)/n
     
     a<-abs(match(list2[1:n], list1)-1:n)
     a[is.na(a)]<-length(list1)
     a[a>=2*n]=2*n
     
     d2<-sum(a)/n
     d<-(d1+d2)/2

     return(d)
}