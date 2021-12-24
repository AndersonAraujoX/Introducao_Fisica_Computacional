program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-d-11371311.dat",status="replace")
open(2,file="saida2-d-11371311.dat",status="replace")
open(3,file="saida3-d-11371311.dat",status="replace")
!v0=(/10,0/)
!e0=(/0.01,0.001,0.0001/)
write(*,*)"coloque algo"
read(*,*)graus
rad=acos(-1.0)*graus/180.0d0
!condições iniciais
e0=0.0001
v=10
gama=0.1
y=100
x=0
t=0
vy=v*sin(rad)
vx=v*cos(rad)
!primeiro e0
ay=-10-gama*vy
ax=-gama*vx
vy=vy+ay*e0/2.0d0
vx=vx+ax*e0/2.0d0
do while(y>=0)
    x=x+vx*e0
    y=y+vy*e0
    ay=-10-gama*vy
    ax=-gama*vx
    vy=vy+ay*e0
    vx=vx+ax*e0
    t=t+e0
    write(1,*)t,y
    write(2,*)t,x
    write(3,*)x,y
enddo
end program