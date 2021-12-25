



  x=c(60000,50000,40000,25000,1100,1100,1100,1100)
  y=c(440,440,440,440,10000,20000,50000,60000)




  x=c(40,30,20,15,0.23,0.23,0.23,0.23)
  y=c(0.88,0.88,0.88,0.88,3,4,5,30)




                 d=cbind(x,y)
                 z=data.frame(d)

                                 x=z$x
                                  y=z$y



         b=2000
         c=0.2


       fit= nls(y ~ b*I(x^c) ,z , start=list(b=b,c=c))
         
           summary(fit)

  plot(x,y)

 xx= seq(min(y),max(y),len=8)


plot(xx,predict(fit,xx))




