program Qb23
implicit real*8(a-h,o-z)
open(1,file="saida1-a5-11371311.dat",status="replace")
open(2,file="saida2-a5-11371311.dat",status="replace")
write(*,*)"digite o tempo que o programa vai ficar rodando em anos"
read(*,*)ano
!repeticao para mudar a massa de jupiter
    !condições iniciais
    a=0
    det=0.000001!det é delta t
    pi=acos(-1.0d0)
    t=det

    gms=4*pi**2
    gmt=gms*(3*10**(-6))
    gml=gmt*1.23*10**(-2)
    !primeiro det terra
    xt0=1
    yt0=0

    xt1=1
    yt1=6.2831*det
    !primeiro det lua
    xl0=1+0.00257
    yl0=0

    xl1=1+0.00257
    yl1=6.2831*det
    !metodo de verlet
    do while(t<=ano)
        t=t+det
        !calculandos o raio

        !terra
        rtl=sqrt((xt1-xl1)**2+(yt1-yl1)**2)
        rts=sqrt(xt1**2+yt1**2)
        yt2=2*yt1-yt0-gms*yt1/((rts)**(3))*(det)**2
        xt2=2*xt1-xt0-gms*xt1/((rts)**(3))*(det)**2
        !mudando as coordenadas terra
        xt0=xt1
        yt0=yt1
        xt1=xt2
        yt1=yt2
        !lua
        rtl=sqrt((xt1-xl1)**2+(yt1-yl1)**2)
        rls=sqrt(xl1**2+yl1**2)
        yl2=2*yl1-yl0-gms*yl1/((rls)**(3))*(det)**2-gmt*(-yt1+yl1)*(det)**2/rtl**3
        xl2=2*xl1-xl0-gms*xl1/((rls)**(3))*(det)**2-gmt*(-xt1+xl1)*(det)**2/rtl**3
        !mudando as coordanadas da lua
        xl0=xl1
        yl0=yl1
        xl1=xl2
        yl1=yl2
        write(1,*)xt2,yt2
        write(2,*)xl2,yl2
    enddo
end program