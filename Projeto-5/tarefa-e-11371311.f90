program Qe
    implicit real*8(a-h,o-z)
    open(1,file="saida1-e-11371311.dat",status="replace")
    open(2,file="saida2-e-11371311.dat",status="replace")
    open(3,file="saida3-e-11371311.dat",status="replace")
    write(*,*)"digite o valor de teta"
    read(*,*)conv
    write(*,*)"digite o valor de gamma"
    read(*,*)gama
    write(*,*)"digite o valor de f0"
    read(*,*)f0
    write(*,*)"digite o valor de omega"
    read(*,*)omega
    write(*,*)"digite o valor de det"
    read(*,*)det!variação do tempo
    !conversão para radiano
    pi=acos(-1.0d0)
    teta=pi*conv/180.0d0
    !colocar o angulo entre -2pi e 2pi
    do while(-2*pi>=teta .or. teta>=2*pi)
        if(teta>2*pi)then
            teta=teta-2*pi
        else
            teta=teta+2*pi
        endif
    enddo
    !condições iniciais
    !omega=2d0/3
    i=1
    n=1
    t=det
    w=0
    !primeiro det
    alpha=-sin(teta)-gama*w+f0*sin(omega*t)
    w=w+(alpha)*det 
    teta=teta+w*det
    !loop
    do while(1<8)
        t=i*det
        alpha=-sin(teta)-gama*w+f0*sin(omega*t)
        w=w+(alpha)*det
        teta=teta+w*det
        i=i+1
        if(teta>pi)then
            teta=teta-2*pi
        elseif(teta<(-pi))then
            teta=teta+2*pi
        endif
        if(t>=1000000)then
            exit
        endif
        if(abs(t-n*pi/omega)<det/2)then
            write(1,*)t,teta
            write(2,*)t,w
            write(3,*)teta,w
            n=n+1
        endif
    enddo
end program