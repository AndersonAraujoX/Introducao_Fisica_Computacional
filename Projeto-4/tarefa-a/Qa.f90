program Qa
real*8::v,y,t
open(1,file="saida1-a-11371311.dat",status="replace")
!v0=(/10,0/)
!e0=(/0.01,0.001,0.0001/)
read(*,*)v
read(*,*)e0
y=100
t=0
!primeiro e0
v=v-10*e0/2
y=y+v*e0
do while(y>=0)
    v=v-10*e0
    y=y+v*e0
    t=t+e0
    write(1,*)y,t
    write(2,*)v,t
enddo
end program