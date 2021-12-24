program Qc
    implicit real*8(a-h,o-z)
    open(1,file="saida1-c-11371311.dat",status="replace")
    open(2,file="saida2-c-11371311.dat",status="replace")
    open(3,file="saida3-c-11371311.dat",status="replace")
    open(4,file="saida4-c-11371311.dat",status="replace")
    open(9,file="saida9-c-11371311.dat",status="replace")
    open(7,file="saida7-c-11371311.dat",status="replace")
    open(8,file="saida8-c-11371311.dat",status="replace")
    write(*,*)"digite o valor de teta"
    read(*,*)teta
    write(*,*)"digite o valor de gamma"
    read(*,*)gama
    write(*,*)"digite o valor de f0"
    read(*,*)f0
    write(*,*)"digite o valor de omega"
    read(*,*)omega
    write(*,*)"digite o valor de det"
    read(*,*)det
    !condições iniciais
    !omega=2d0/3
    i=1
    w=0
    !primeiro pendulo
    alpha1=-sin(teta)-gama*w+f0*sin(omega*t)
    w1=w+(alpha1)*det 
    teta1=teta+w1*det
    !segundo pendulo
    alpha2=-sin(teta+0.001)-gama*w+f0*sin(omega*t)
    w2=w+(alpha2)*det 
    teta2=(teta+0.001)+w2*det
    !loop
    do while(1<8)
        t=i*det
        !primeiro pendulo
        alpha1=-sin(teta1)-gama*w1+f0*sin(omega*t)
        w1=w1+(alpha1)*det
        teta1=teta1+w1*det
        !segundo pendulo
        alpha2=-sin(teta2)-gama*w2+f0*sin(omega*t)
        w2=w2+(alpha2)*det 
        teta2=teta2+w2*det
        i=i+1
        if(t>=100)then
            exit
        endif
        write(1,*)t,teta1
        write(2,*)t,teta2
        write(3,*)teta1,w1
        write(4,*)teta2,w2
        write(7,*)t,(teta2-teta1)
        write(8,*)t,dlog(abs(teta2-teta1))
        write(9,*)t,dlog(abs(teta2-teta1))/t 
    enddo
end program