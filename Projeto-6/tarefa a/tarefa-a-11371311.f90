program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-a1-11371311.dat",status="replace")
open(2,file="saida2-a1-11371311.dat",status="replace")
write(*,*)"digite x_inicial"
read(*,*)x1
write(*,*)"digite y_inicial"
read(*,*)y1
write(*,*)"digite velocidadex"
read(*,*)vx
write(*,*)"digite velocidadey"
read(*,*)vy
!condicoes iniciais
det=0.00001
pi=acos(-1.0d0)
t=det
a=0
b=0
area=0
gms=4*pi**2!(x1**2+y1**2)**(1/2)**3/ano**2
!primeiro det
x2=x1+vx*det
y2=y1+vy*det
!metodo de verlet
do while(1<8)
    !write(*,*)t
    t=t+det
    x0=x1
    y0=y1
    x1=x2
    y1=y2
    r=sqrt(x1**2+y1**2)
    y2=2*y1-y0-gms*y1/((r)**(3))*(det)**2
    x2=2*x1-x0-gms*x1/((r)**(3))*(det)**2
    !para calcular a area da elipse na parte a2
    vx=vx-(4*pi**2)*x1*det/(r)**3
    vy=vy-(4*pi**2)*y1*det/(r)**3
    v=sqrt(vy**2+vx**2)
    !usando produto vetorial conseguimos um boa aproximcao da area percorrida.
    area=area+r*v*det/2.0d0
    write(2,*)r*v*det/2.0d0
    if (abs(x2)<=0.001.and. y2>b)then
        a=y2
    endif
    if (abs(y2)<=0.001 .and. x2>a)then
        b=x2
    endif
    if(abs(x2-r)<=0.00001 .and. t>=0.1 .and. x2-x1<=0)then!fizemos a consideracao que sempre usaremos o eixo x como inicial
        exit
    endif
    write(1,*)x2,y2
enddo
write(*,*)"Terceira Lei de Kleper A1"
write(*,*)t**2/r**3
write(*,*)'Terceira Lei de Kleper A2'
write(*,*)t**2/a**3
write(*,*)"excentricidade"
write(*,*)((a**2-b**2))**(0.5)/a!para evitamos o valores negativos dentro 
write(*,*)"area da elipse"
write(*,*)area
end program