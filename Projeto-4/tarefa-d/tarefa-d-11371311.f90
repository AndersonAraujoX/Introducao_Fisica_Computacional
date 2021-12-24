program Qd
real*8::vx,vy,x,y,t
real*8,dimension(3)::graus
character::nome(9)*21
data nome/"saida1-d-11371311.dat","saida2-d-11371311.dat","saida3-d-11371311.dat","saida4-d-11371311.dat", &
&"saida5-d-11371311.dat" ,"saida6-d-11371311.dat","saida7-d-11371311.dat","saida8-d-11371311.dat",&
&"saida9-d-11371311.dat"/
graus=(/45.0d0,0.0d0,-45.0d0/)
rad=acos(-1.0)*graus(1)/180.0d0
!condições iniciais
e0=0.0001
i=1
v=10
gama=0.1
y=100
x=0
t=0
k=11
vy=v*sin(rad)
vx=v*cos(rad)

!primeiro e0
ay=-10-gama*vy
ax=-gama*vx
vy=vy+ay*e0/2.0d0
vx=vx+ax*e0/2.0d0
x=x+vx*e0
y=y+vy*e0
open(k,file=nome(k-10))!colocams k-10 por causa que quando colocamos certos numeros para k dá erro
open(k+1,file=nome(k-9))
open(k+2,file=nome(k-8))
write(k,*)t,y
write(k+1,*)t,x
write(k+2,*)x,y

do while(y>=0)
    ay=-10-gama*vy
    ax=-gama*vx
    x=x+vx*e0
    y=y+vy*e0
    vy=vy+ay*e0
    vx=vx+ax*e0
    t=t+e0
    if(y<=0 .and. i<3)then!caso y chegue a 0 e0 permuta
        y=100
        x=0
        i=i+1
        k=k+3
        t=0
        !convercao de graus para radiano
        rad=acos(-1.0)*graus(i)/180.0d0
        vy=v*sin(rad)
        vx=v*cos(rad)
        !primeiro e0
        ay=-10-gama*vy
        ax=-gama*vx
        vy=vy+ay*e0/2.0d0
        vx=vx+ax*e0/2.0d0
        x=x+vx*e0
        y=y+vy*e0
        open(k,file=nome(k-10))!colocams k-10 por causa que quando colocamos certos numeros para k dá erro
        open(k+1,file=nome(k-9))
        open(k+2,file=nome(k-8))
    endif
    write(k,*)t,y
    write(k+1,*)t,x
    write(k+2,*)x,y
enddo
end program