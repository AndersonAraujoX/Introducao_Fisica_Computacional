program Qa
implicit real*8(a-h,o-z)
real*8,dimension(10)::x1,y1,m,gm,vy,x0,y0
open(21,file="saida1-a3-11371311.dat",status="replace")
open(22,file="saida2-a3-11371311.dat",status="replace")
open(23,file="saida3-a3-11371311.dat",status="replace")
open(24,file="saida4-a3-11371311.dat",status="replace")
open(25,file="saida5-a3-11371311.dat",status="replace")
open(26,file="saida6-a3-11371311.dat",status="replace")
open(27,file="saida7-a3-11371311.dat",status="replace")
open(28,file="saida8-a3-11371311.dat",status="replace")
open(29,file="saida9-a3-11371311.dat",status="replace")
!condições iniciais para vetores
!os vetores vao seguir a seguinte ordem
!1-sol,2-mercurio,3-venus,4-terra,5-marte,6-jupiter,7-saturno,8-urano,9-neturno,10-plutão
vy=(/0.0d0,10.21d0,7.41d0,6.2831d0,5.080d0,2.7571d0,1.9713d0,1.4299d0,1.1461d0,1.0d0/)
x0=(/0.0d0,0.39d0,0.72d0,1d0,1.52d0,5.2d0,9.24d0,19.19d0,30.06d0,39.53d0/)
y0=(/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)
x1=(/0.0d0,0.39d0,0.72d0,1d0,1.52d0,5.2d0,9.24d0,19.19d0,30.06d0,39.53d0/)
y1=(/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)
m=(/2.0d0*10**7,2.40d0,49.0d0,60.0d0,6.60d0,19000.0d0,5700.0d0,880.0d0,103.0d0,0.13d0/)!esse ultimo valor é razão entre o sol com ele mesmo 
!condições iniciais para variaveis
ano=12
det=0.00001d0
pi=acos(-1.0d0)
t=det
!primeiro det terra
do i=1,10
gm(i)=4*pi**2*(m(i)/m(1))
y1(i)=y0(i)+vy(i)*det
!x1(i)=x0(i)+vx(i)*det
enddo
!metodo de verlet
do while(t<=ano)
    do j=2,10
        y2=2*y1(j)-y0(j)
        x2=2*x1(j)-x0(j)
        do i=1,10!somando cada iteracao da aceleracao gravitacional
            !calculandos os raios
            if(i/=j)then
                r=dsqrt((-x1(i)+x1(j))**2+(-y1(i)-x1(j))**2)
                x2=x2-gm(i)*(x1(j)-x1(i))/(r*r*r)*(det)**2
                y2=y2-gm(i)*(y1(j)-y1(i))/(r*r*r)*(det)**2
            endif
        enddo
        !mudando as coordenadas
        x0(j)=x1(j)
        y0(j)=y1(j)
        x1(j)=x2
        y1(j)=y2
        write(j+19,*)x1(j),y1(j)
    enddo
    t=t+det
enddo
end program 