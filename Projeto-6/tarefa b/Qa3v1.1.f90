program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-a3-11371311.dat",status="replace")
open(2,file="saida2-a3-11371311.dat",status="replace")
open(3,file="saida3-a3-11371311.dat",status="replace")
open(4,file="saida4-a3-11371311.dat",status="replace")
!condições iniciais
ano=100
det=0.0001
pi=acos(-1.0d0)
t=det
gms=4*pi**2
gmj=gms/(1000)

!jupiter
xj0 = 5.200
yj0 = 0

xj1 = 5.200
yj1 = 2.755*det

!asteoride I
xaI0=3.00d0
yaI0=0

xaI1=3.0d0
yaI1=3.628*det

!asteoride II
xaII0=3.276
yaII0=0

xaII1=3.276
yaII1=3.4761*det

!asteoride III
xaIII0=3.7
yaIII0=0

xaIII1=3.7
yaIII1=3.267*det

!metodo de verlet
do while (1<8)
    t=t+det
    !mudando as coordenadas de jupiter
    rjs = sqrt(xj1**2 + yj1**2)!raio jupiter
    yj2 = 2*yj1 - yj0 - gms*yj1*det**2/(rjs**3)
    xj2 = 2*xj1 - xj0 - gms*xj1*det**2/(rjs**3)
    
    xj0 = xj1
    yj0 = yj1
    xj1 = xj2
    yj1 = yj2
    
    !mudando as coordenadas do asteroide I
    raIs = sqrt(xaI1**2+yaI1**2)!raio aI
    raIj = sqrt((xaI1-xj1)**2+(yaI1-yj1)**2)
    yaI2 = 2*yaI1 - yaI0 - gms*yaI1/((raIs)**(3))*(det)**2 - gmj*(yaI1-yj1)*(det)**2/(raIj**3)
    xaI2 = 2*xaI1 - xaI0 - gms*xaI1/((raIs)**(3))*(det)**2 - gmj*(xaI1-xj1)*(det)**2/(raIj**3)

    xaI0=xaI1
    yaI0=yaI1
    xaI1=xaI2
    yaI1=yaI2
    !mudando as coordenadas do asteroide II
    raIIs=sqrt(xaII1**2+yaII1**2)!raio aII
    raIIj=sqrt((xaII1-xj1)**2+(yaII1-yj1)**2)
    yaII2=2*yaII1 - yaII0 - gms*yaII1/((raIIs)**(3))*(det)**2 - gmj*(yaII1-yj1)*(det)**2/(raIIj**3)
    xaII2=2*xaII1 - xaII0 - gms*xaII1/((raIIs)**(3))*(det)**2 - gmj*(xaII1-xj1)*(det)**2/(raIIj**3)

    xaII0=xaII1
    yaII0=yaII1
    xaII1=xaII2
    yaII1=yaII2
    !mudando as coordenadas do asteroide III
    raIIIs=sqrt(xaIII1**2+yaIII1**2)!raio aIII
    raIIIj=sqrt((xaIII1-xj1)**2+(yaIII1-yj1)**2)
    yaIII2=2*yaIII1 - yaIII0-gms*yaIII1/((raIIIs)**(3))*(det)**2 - gmj*(yaIII1-yj1)*(det)**2/(raIIIj**3)
    xaIII2=2*xaIII1 - xaIII0-gms*xaIII1/((raIIIs)**(3))*(det)**2 - gmj*(xaIII1-xj1)*(det)**2/(raIIIj**3)

    xaIII0=xaIII1
    yaIII0=yaIII1
    xaIII1=xaIII2
    yaIII1=yaIII2
    
    
    if(t>=ano)then
        exit
    endif
    write(1,*)xaI2,yaI2
    write(2,*)xaII2,yaII2
    write(3,*)xaIII2,yaIII2
    write(4,*) xj2, yj2
enddo
end program