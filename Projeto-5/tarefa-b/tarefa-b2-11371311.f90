program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-b2-11371311.dat",status="replace")
pi=acos(-1.0d0)
conv=0.1
do while(1<8)
    teta=pi*conv/180.0d0
    T=2*pi*(1+(teta)**2/16)
    conv=conv+0.1
    if(conv>5)then
        exit
    endif
    write(1,*)teta,T
enddo
end program