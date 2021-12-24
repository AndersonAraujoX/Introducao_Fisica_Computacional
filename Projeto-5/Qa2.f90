program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1-a2-11371311.dat",status="replace")
open(2,file="saida2-a2-11371311.dat",status="replace")
open(3,file="saida3-a2-11371311.dat",status="replace")
write(*,*)"digite o angulo em graus"
read(*,*)conv
write(*,*)"digite a variação do tempo"
read(*,*)det!det é delta t
!conversão para radiano
pi=acos(-1.0d0)
teta=pi*conv/180.0d0
!colocar o angulo entre -2pi e 2pi
do while(-2*pi>=teta .or. teta>=2*pi)
    if(teta>2*pi)then
        teta=teta-2*pi
    else
        teta=teta+2*pi
    endif
enddo
!condições iniciais
t=det
i=1
w=0
!primeiro det
w=w-teta*det
teta=teta+w*det
!metodo de euler-cromer
do while(1<8)
    t=i*det
    w=w-teta*det
    teta=teta+w*det
    i=i+1
    e=9.8**2*(w**2)/2+9.8*teta**2/2!para teta pequeno utlizamos essa energia
    if(t>=20)then
        exit
    endif
    write(1,*)t,teta
    write(2,*)t,w
    write(3,*)t,e
enddo
end program