function poli(x)
  real*8::x,poli
  poli=x**3-14*x-20
end function poli
program QC
implicit real*8(a-h,o-z)
real*8,dimension(6)::intervalo
open(1,file="saida-c-11371311.dat",status="replace")
j=1!metodo de busca direta
xl=-10
xr=xl+0.5d0
inte=1
raiz=0
h=0.5d0
a2=(poli(xr)*xl-poli(xl)*xr)/(poli(xr)-poli(xl))
write(1,*)"busca direta"
do while(raiz<3)!busca direta
  if(poli(xl)*poli(xr)<0.0 .and. poli(xl)/=0 )then!caso o produto de negativo ira fazer uma reta entre os dois pontos
    a1=a2
    a2=(poli(xr)*xl-poli(xl)*xr)/(poli(xr)-poli(xl))
    h=h/2.0d0
    xr=xl+h
    inte=inte+1
    write(1,1)"interações",inte,"Raiz",a2
    if(abs(a2-a1)<=0.0001)then!se diferença for menor que o erro entao 'e raiz
    	h=0.5d0
    	xl=a2
    	xr=a2+h
    	inte=0
    	raizes=raizes+1
    endif
  else if(poli(xl)*poli(xr)>0.0 .or. poli(xl)==0)then!caso o produto de positivo continue procurando
    xl=xr
    xr=xl+h
    inte=0
  else if(poli(xl)*poli(xr)==0)then!caso de zero a raiz esta em xr
    inte=inte+1
    write(1,1)"interações",inte,"Raiz",xr
    xl=xr
    xr=xl+h
    raiz=raiz+1
  end if
enddo
x=-10
do while(j<=6)!xs e x são os intervalos que vamos utilizar para encontrar as raizes no metodos seguintes 
  xs=x!xs é menor valor do intervalo
  x=x+0.5!x é o maior valor do intervalo
  if(poli(xs)*poli(x)<0.0)then
    intervalo(j)=xs!Caso o polinomio o produto do intervalo dar negativo significa no intervalo do xs e x está a raiz 
    intervalo(j+1)=x
    j=j+2
  else if(poli(xs)*poli(x)<=(10**(-6)) .and. poli(x)==0)then
   intervalo(j)=xs!Caso o produto de zero a raiz está em uma das variavéis
   j=j+1!porém queremos o valor não nulo para o intervalo
  else if(poli(xs)*poli(x)<=(10**(-6)) .and. poli(xs)==0)then
    intervalo(j)=x
    j=j+1
  end if 
enddo!Intervalos
write(1,*)"intervalo da primeira raiz:",intervalo(1),intervalo(2)
write(1,*)"intervalo da segunda  raiz:",intervalo(3),intervalo(4)
write(1,*)"intervalo da terceira raiz:",intervalo(5),intervalo(6)
j=1!metodo de newton-raphson
write(1,*)"metodo de newton-raphson"
do while(j<=5)!nesse metodo usarei o menor valor intervalo para encontrar as raizes
  x=intervalo(j)!está j=1,3,5
  j=j+2!por isso pulamos dois
  inte=1
  do while(0/=1)
    polider=3*(x)**2-14!derivada do polinomio
    x=x-poli(x)/polider
    if(abs(poli(x))<=abs(10**(-6)))then!caso o polinômio for menor que o erro saia do loop
      exit
    endif
    write(1,1)"interações",inte,"Raiz",x
    inte=inte+1
  enddo
enddo
j=1!metodo da secante
write(1,*)"metodo da secante"
do while(J<=6)
  x1=intervalo(j+1)
  x0=intervalo(j)
  inte=1
  do while(0/=1)
  x2=x1-poli(x1)*(x1-x0)/(poli(x1)-poli(x0))
  if(abs(poli(x2))<=abs(10**(-6)))then!caso o poli(x2) for menor que o erro saia do loop
    j=j+2
    exit
  endif
  write(1,1)"interações",inte,"Raiz",x2
  inte=inte+1
  x0=x1
  x1=x2
  enddo
enddo
1 format('|| ',a14,' | ',i14,"|",a14,'|',f18.15,'||')
end program
