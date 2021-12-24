program QC
  real*8 :: somax,somax2,somay,somay2,mediax,mediax2,mediay,mediay2,mediav,delta
  integer*8::a,b,be,n,x,y,irr
  integer,dimension(5)::ipx,ipy
  open(10,file="bebadoh.dat",status="replace")
  write(*,*)"digite a quantidade de bebados"
  read(*,*)be
  write(*,*)"digite a quantidade de passos"
  read(*,*)n
  !zerar as veriavéis para caso o código já foi rodado outro vez
  somax=0
  somay=0
  somax2=0
  somay2=0
  ipx(1)=1
  ipx(2)=-1
  ipx(3)=0
  ipx(4)=0
  ipy(1)=0
  ipy(2)=0
  ipy(3)=1
  ipy(4)=-1
  !do  que ira realizar a movimentação do numero de bebados escolhidos pelo usuario
  do b=1,be
  	x=0
    y=0
    !do para fazer contagem de passos do bebados nos 4 sentidos
  	do a=1,n
      irr=4*rand()+1
      !condições para ver qual caminho bebados vão fazer
      x=x+ipx(irr)
      y=y+ipy(irr)
    enddo
    !salvando em arquivo para mostrar a difusão d0s bebados no histograma
    write(10,*)x,y
    !soma das posições do bebados
    somax=somax+x
    somax2=somax2+x*x
    somay=somay+y
    somay2=somay2+y*y	
  enddo
  !calculos das medias das posições
  mediax=somax/be
  mediax2=somax2/be
  mediay=somay/be
  mediay2=somay2/be
  !calculo da média vetorial r=((x)**2+(y)**2)**0.5
  !onde x e y são a médias das suas respetivas coordenas 
  mediav=((mediax)**2+(mediay)**2)**(0.5)
  mediav2=((mediax2)**2+(mediay2)**2)**(0.5)
  delta=mediav2-mediav*mediav
  write(*,*)mediav,delta
  close(10)
end program
