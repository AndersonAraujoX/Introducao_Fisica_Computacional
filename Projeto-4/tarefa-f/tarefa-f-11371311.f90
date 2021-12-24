program Qf
real*8::vx,vy,x,y,t
open(1,file="saida1-f-11371311.dat",status="replace")
open(2,file="saida2-f-11371311.dat",status="replace")
open(3,file="saida3-f-11371311.dat",status="replace")
!v0=(/10,0/)
!e0=(/0.01,0.001,0.0001/)
write(*,*)"Coloque a velocidade"
read(*,*)v
write(*,*)"Coloque o valor de dx desejado"
read(*,*)dx
write(*,*)"Coloque o valor de alpha(Graus)"
read(*,*)graus


rad=acos(-1.0)*graus/180.0d0
y=100
e0=0.0001
vy=v*sin(rad)
vx=v*cos(rad)
x=0


!primeiro e0
t=e0
ay=-10-gama*vy
ax=-gama*vx
vy=vy+ay*e0/2.0d0
vx=vx+ax*e0/2.0d0
x=x+vx*e0
y=y+vy*e0
write(1,*)t,y
write(2,*)t,x
write(3,*)x,y

!loop
do while(0==0)
    x=x+vx*e0
    y=y+vy*e0
    xr=x
    ay=-10-gama*vy
    ax=-gama*vx
    vy=vy+ay*e0
    vx=vx+ax*e0
    t=t+e0!caso a diferença de entre os pontos onde a bolinha quicou for menor 10**-3 o loop para
    if(abs(xr-xl)<=0.001 .and. y<=0)then
        exit
    elseif(y<=0)then
        xl=x
        vy=abs(vy)*0.7
        vx=abs(vx)*(1-dx)
        y=0
    endif
    write(1,*)t,y
    write(2,*)t,x
    write(3,*)x,y
enddo
end program