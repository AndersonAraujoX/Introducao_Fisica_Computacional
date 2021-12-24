program Qa
implicit real*8(a-h,o-z)
real*8,dimension(2)::v0
real*8,dimension(3)::e0
character::nome(12)*22
!open(1,file="saida1-a-11371311.dat",status="replace")
!open(2,file="saida2-a-11371311.dat",status="replace")
v0=(/10d0,0d0/)
e0=(/0.01d0,0.001d0,0.0001d0/)
!nome dos arquivos os valores impares são y(t) e os arquivos pares vão v(t)
data nome/"saida1-a-11371311.dat","saida2-a-11371311.dat","saida3-a-11371311.dat","saida4-a-11371311.dat", &
&"saida5-a-11371311.dat" ,"saida6-a-11371311.dat","saida7-a-11371311.dat","saida8-a-11371311.dat",&
&"saida9-a-11371311.dat","saida10-a-11371311.dat","saida11-a-11371311.dat","saida12-a-11371311.dat"/
!condições iniciais
y=100
t=e0(i)
i=1
j=1
k=11
!primeiro e0
v=v0(j)-10*e0(i)/2
y=y+v*e0(i)
open(k,file=nome(k-10))
open(k+1,file=nome(k-9))
write(k,*)t,y
write(k+1,*)t,v
do while(y>=0 .and. j<=2)
    v=v-10*e0(i)
    y=y+v*e0(i)
    t=t+e0(i)
    if(y<=0 .and. i<3)then!caso y chegue a 0 e0 permuta
        y=100
        i=i+1
        k=k+2
        t=0
        !primeiro e0
        v=v0(j)-10*e0(i)/2
        open(k,file=nome(k-10))!foi colocado  k-10 por causa que quando colocamos certos numeros para k dá erro
        open(k+1,file=nome(k-9))
    elseif(y<=0 .and. i==3 .and. j<2)then!caso y chegue a 0 e0 permuta e j=6 a velocidade troca
        y=100
        t=0
        j=j+1
        i=1
        k=k+2
        !primeiro e0
        v=v0(j)-10*e0(i)/2
        open(k,file=nome(k-10))!foi colocado  k-10 por causa que quando colocamos certos numeros para k dá erro
        open(k+1,file=nome(k-9))
    endif
    write(k,*)t,y
    write(k+1,*)t,v
enddo
end program