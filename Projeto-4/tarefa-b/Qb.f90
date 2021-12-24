program Qa
    implicit real*8(a-h,o-z)
    
    open(1,file="saida1-b-11371311.dat",status="replace")
    open(2,file="saida2-b-11371311.dat",status="replace")
    open(3,file="saida3-b-11371311.dat",status="replace")
    !v0=(/10,0/)
    !e0=(/0.01,0.001,0.0001/)
    read(*,*)v
    read(*,*)e0
    read(*,*)gama
    !condições iniciais
    y=100
    t=0
    !primeiro e0
    a=-gama*v-10
    v=v+e0*a/2.0d0
    !loop
    do while(y<=0)
        y=y+v*e0
        a=-gama*v-10
        v=v+a*e0
        t=t+e0
        write(1,*)t,y
        write(2,*)t,v
    enddo
    end program