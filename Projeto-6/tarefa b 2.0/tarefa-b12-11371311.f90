program Qb23
implicit real*8(a-h,o-z)
real*8,dimension(3)::m
open(11,file="saida1-a2-11371311.dat",status="replace")
open(12,file="saida2-a2-11371311.dat",status="replace")
open(13,file="saida3-a2-11371311.dat",status="replace")
open(14,file="saida4-a2-11371311.dat",status="replace")
open(15,file="saida5-a2-11371311.dat",status="replace")
open(16,file="saida6-a2-11371311.dat",status="replace")
m=(/1,100,1000/)
write(*,*)"digite o tempo que o programa vai ficar rodando em anos"
read(*,*)ano
!repeticao para mudar a massa de jupiter
do i=1,3
    !condições iniciais
    write(*,*)"Jupiter massa",m(i),"vezes"
    rtsa=0
    a=0
    det=0.00001!det é delta t
    pi=acos(-1.0d0)
    t=det

    gms=4*pi**2
    gmt=gms/(3*10**5)
    gmj=gms/(10**3)*m(i)
    !primeiro det terra
    xt0=1
    yt0=0

    xt1=1
    yt1=6.2831*det
    !primeiro det jupiter
    xj0=5.20
    yj0=0

    xj1=5.20
    yj1=2.7571*det
    !metodo de verlet
    do while(t<=ano)
        t=t+det
        !calculandos o raio

        !terra
        rtj=sqrt((xt1-xj1)**2+(yt1-yj1)**2)
        rts=sqrt(xt1**2+yt1**2)
        yt2=2*yt1-yt0-gms*yt1/((rts)**(3))*(det)**2-gmj*(yt1-yj1)*(det)**2/rtj**3
        xt2=2*xt1-xt0-gms*xt1/((rts)**(3))*(det)**2-gmj*(xt1-xj1)*(det)**2/rtj**3
        !mudando as coordenadas terra
        xt0=xt1
        yt0=yt1
        xt1=xt2
        yt1=yt2
        !jupiter
        rtj=sqrt((xt1-xj1)**2+(yt1-yj1)**2)
        rjs=sqrt(xj1**2+yj1**2)
        yj2=2*yj1-yj0-gms*yj1/((rjs)**(3))*(det)**2-gmt*(-yt1+yj1)*(det)**2/rtj**3
        xj2=2*xj1-xj0-gms*xj1/((rjs)**(3))*(det)**2-gmt*(-xt1+xj1)*(det)**2/rtj**3
        !mudando as coordanadas de jupiter
        xj0=xj1
        yj0=yj1
        xj1=xj2
        yj1=yj2
        !Ver o raio para cada ano
        if ((1-det)<=(t-a) .and. (t-a)<=(1+det) )then
        a=a+1
            write(*,*)"raio da terra",(rts-rtsa)
            write(*,*)"Ano",a
            rtsa=rts!rtsa é raio da terra anterior para comparamos a variação da terra
        endif
        write(9+i*2,*)xt2,yt2
        write(10+i*2,*)xj2,yj2
    enddo
enddo
end program