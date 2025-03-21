program Qb
  implicit real*8(a-h,o-z)
  open(1,file="saida1-b-11371311.dat",status="replace")
  open(2,file="saida2-b-11371311.dat",status="replace")
  !valor de pi
  pi=dacos(-1d0)
  !========================================================================================================================================================================================
  !chegando no valor da integral exp(x/2)*cos(pi*x) x=0 até 1 o valor da integral chegamos
  !que o valor é:-(2+2*(e)**(0.5))/(1+4*(pi)**2)
  !=========================================================================================================================================================================================
  resultado=-(2+2*dexp(real(0.5,8)))/(1+4*(pi)**2)
  write(*,13)"N","h","trapezio","simpson","bode"
  write(1,13)"N","h","trapezio","simpson","bode"
  write(2,13)"N","h","trapezio","simpson","bode"
  do j=2,12
  h=1.0d0/(2**j)
  n=2**j
  trape=0.0d0
  simp=0.0d0
  bode=0.0d0
  do m=1,n,2!começo com m=1 para quando x*m-1=0, assim inicio em f(0)
    !processamento do trapezio e de simpson
    trape=trape+(f(h*m-h)+2*f(h*m)+f(h*m+h))*h/2.0
    simp=simp+(f(h*m-h)+4*f(h*m)+f(h*m+h))*h/3.0
  enddo
  x=0.0d0!começando pelo menor valor do intervalo
  do m=1,n,4
    !processamento de bode
    bode=bode+2.0*h/45.0*(7*f(x)+32*f(x+h)+12*f(x+2*h)+32*f(x+3*h)+7*f(x+4*h))
    x=x+4*h
  enddo
  !saida de dados
  write(*,14)2**j,h,abs(resultado-trape),abs(resultado-simp),abs(resultado-bode)
  write(2,14)2**j,h,abs(resultado-trape),abs(resultado-simp),abs(resultado-bode)
  write(1,14)2**j,h,trape,simp,bode
  enddo
  write(1,*)"------------------------------------------------------------------------------------------------"
  write(2,*)"------------------------------------------------------------------------------------------------"
  write(2,11)"N","h","simpsons3/8"
  write(1,11)"N","h","simpsons3/8"
  do j=2,8
    l=3**j
    hl=1.0d0/(3**j)
    y=0.0d0
    simp3=0.0d0
    do m=1,l,3
      !processamento de simp3/8
      simp3=simp3+3.0*hl/8.0*(f(y)+3*f(y+hl)+3*f(y+2*hl)+f(y+3*hl))
      y=y+3*hl
    enddo
    write(2,12)3**j,hl,abs(resultado-simp3)
    write(1,12)3**j,hl,simp3
  enddo
  write(1,*)"valor analitico da integral",resultado
  write(2,*)"valor analitico da integral",resultado
  14 format('|| ',i14,' | ',f18.15,' | ',f18.15,' | ',f20.18,' | ',f20.18,'||')
  13 format('|| ',a14,' | ',a18,' | ',a18,' | ',a20,' | ',a20,'||')
  12 format('|| ',i14,' | ',f18.15,' | ',f18.15,"||")
  11 format('|| ',a14,' | ',a18,' | ',a18,"||")
  end program
  function f(x)
    real*8::x,f
    f=dexp(x/2.0)*dcos(dacos(-1d0)*x)
  end function