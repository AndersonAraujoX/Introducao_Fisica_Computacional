program QD
  real*8 :: soma,pi
  integer::a,cx,cy,n,k,x,y,e,f,part,pas,pa
  integer,dimension(10000,2)::p
  integer,dimension(4)::ipx,ipy
  integer,dimension(2)::max,min
  open(10,file="particula.dat",status="replace")
  !entrada de dados
  write(*,*)"Digite a quantidade de particulas"
  read(*,*)pa
  write(*,*)"Digite a quantidade de passos"
  read(*,*)n
  do part=1,pa
  	x=0
    y=0
    ipx(1)=1
   ipx(2)=-1
   ipx(3)=0
   ipx(4)=0
   ipy(1)=0
   ipy(2)=0
   ipy(3)=1
   ipy(4)=-1
    !do para o passos da particula
  	do pas=1,n
    passo=rand()
    !condições para ver qual caminho as particulas vão fazer
    irr=4*rand()+1
    !condições para ver qual caminho bebados vão fazer
    x=x+ipx(irr)
    y=y+ipy(irr)
  	enddo
    !escrevendo em um arquivo as posições que os particulas ficaram
  p(part,1)=x
  p(part,2)=y
  min=minval(p,dim=1)
  max=maxval(p,dim=1)
  enddo
  !etapa de leitura das moleculas
    !reticulo 
    !Verificar todo reticulos em uma area
    !Os primeiros dois do vão ficar permutando as coordenadas para 
    !ficar examinando se molecula está dentro do reticulado
   soma=0
    do cx=min(2),max(2),1
        do cy=min(1),max(1),1
          mol=0
            do a=1,pa
              !coondições para verificar se particula está na região
              if((((cx<=p(a,1)).AND.(p(a,1)<=(cx+1))).and.((cy<=p(a,2)).AND.(p(a,2)<=(cy+1)))))then
                    mol=mol+1
                 endif
            enddo
            !so realizar se o valor nao for zero
            if(mol>0)then
              pi=real(mol)/pa
              soma=soma-pi*log(pi)
            endif
         enddo
      enddo
    !saida dos dados
    write(*,*)soma
end program
