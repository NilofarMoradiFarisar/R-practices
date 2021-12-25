

  #########-------- Damage ---------#########


D=seq(0.1,0.3,length=27)
W=seq(0.384,0.678,length=27)
#yp=predict(s)


yp=function(x1,x2)
      {o=0.46047 +((x1)^(5.78283))*((x2)^(-6.51550))
              return(o)
         }


 points=as.matrix(expand.grid(D,W))
n=length(D)
fval=matrix(0,n,n)
 for(i in 1:n){
 for(j in 1:n){
 fval[i,j]=yp(D[i],W[j])
 }
 }

nrz <- nrow(fval)
ncz <- ncol(fval)

jet.colors <- colorRampPalette( c("blue", "green","violet","red") )

nrow=nrz
nbcol=nrz
color <- jet.colors(nbcol)

zfacet <- fval[-1, -1] + fval[-1, -ncz] + fval[-nrz, -1] + fval[-nrz, -ncz]

facetcol <- cut(zfacet, nbcol)




persp(D,W,fval, col=color[facetcol],scale = TRUE,
#zlab="  "
zlab=""
, xlab="",
ylab="",nticks = 5, theta=300,phi=30 
,box=TRUE,ticktype = "detailed")


image(D,W,fval,col=terrain.colors(100))
contour(D,W,fval,col = "black",add = TRUE)

