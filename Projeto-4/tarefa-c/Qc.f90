program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-c-11371311.dat",status="replace")
open(2,file="saida2-c-11371311.dat",status="replace")
open(3,file="saida3-c-11371311.dat",status="replace")
write(*,*)"coloque algo"
read(*,*)graus
rad=acos(-1.0)*graus/180.0d0
!condições iniciais
e0=0.0001
v=10
y=100
x=0
t=0
vy=v*sin(rad)
vx=v*cos(rad)
!primeiro e0
vy=vy-10*e0/2.0d0
vx=vx-10*e0/2.0d0
do while(y>=0)
    x=x+vx*e0
    y=y+vy*e0
    vy=vy-10*e0
    t=t+e0
    write(1,*)y,t
    write(2,*)x,t
    write(3,*)y,x
enddo
end program