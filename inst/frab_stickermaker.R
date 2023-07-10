library("hexSticker")

bmp(file="frab_icon.bmp",bg="#7733FF",width=2000,height=1500)
par(mar=c(5,2,4,2)+0)
par(pty="s")
d <- sqrt(3)
plot(NA,xlim=c(-1.5,1.5),ylim=c(0,5.5),asp=1,axes=FALSE,xlab='',ylab='')
points(rbind(
    cbind(-d ,c(  2,3,4 )),
    cbind( 0 ,c(1,2,3, 5)),
    cbind(+d ,c(  2,3,4 ))
) ,pch=16,cex=28)
segments(x0=-d,y0=2,x1=0,y1=5,lwd=36)
segments(x0=-d,y0=3,x1=0,y1=2,lwd=36)
segments(x0=-d,y0=4,x1=0,y1=3,lwd=36)
segments(x0= 0,y0=2,x1=d,y1=2,lwd=36)
segments(x0= 0,y0=1,x1=d,y1=3,lwd=36)
segments(x0= 0,y0=5,x1=d,y1=4,lwd=36)


dev.off()

sticker("frab_icon.bmp", package="frab", p_size=36,
        s_x=1, s_y=0.9,
s_width=1.7,asp=sqrt(3)/2, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="frab.png")
