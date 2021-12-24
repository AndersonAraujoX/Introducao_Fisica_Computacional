program QB
  real*8 :: soma,media,soma2,media2,c,p
  integer::a,b,be,n,beb,int,k
  integer,dimension(10000)::intervalo
  !arquivos para salvar os passos para o histograma
  open(11,file="bebado.dat",status="replace")
  !entrada de dados
  write(*,*)"digite a quantidade de bebados"
  read(*,*)be
  write(*,*)"digite a quantidade de passos"
  read(*,*)n
  write(*,*)"digite o valor do denominador da razão 1/c da chance de ir para direita"
  read(*,*)c
  soma2=0
  soma=0
  p=1/c
  !passos do bebedos
  do b=1,be
  	mov=0
  	do a=1,n
		passo=rand()
 	 if(p<=passo)then
  		mov=mov-1
  	 else
  		mov=mov+1
  	 endif
    enddo
    !escrevendo em um arquivo as coordenadas
    intervalo(b)=mov
  !Processamentos dos passos dos bebados
  !somatória do <x>  
  soma=soma+mov
  !somatória do <x^2>
  soma2=soma2+(mov*mov)
  enddo
  !<x>
  media=soma/be
  !<x^2>
  media2=soma2/be
  !saida de dados
  write(*,*)"valor <x^2>",media2
  write(*,*)"valor <x>",media
  do int=-n,n,5
    qnt=0
      do beb=1,be
      if((int<=intervalo(beb)).AND.(intervalo(beb)<=(int+5)))then
        qnt=qnt+1
       endif 
      enddo
      !escrevendo do arquivo que vai fazer o histograma
    write(11,*)int,qnt
    enddo
  close(11)
end program
