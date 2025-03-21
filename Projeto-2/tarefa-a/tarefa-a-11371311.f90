program QA
  real*8 :: soma,media
  integer::a
  !entrada de dados
  write(*,*)"Digite o expoente n:"
  read(*,*)n
  !zerar o valor soma para caso a variavel esteja com algum valor de outros programas
  soma=0
  !uma repetição que irá realizar a soma do número aleatório 
  !soma será elevado por um expoente para fazer a média <x**n>
  do a=1,50000000
  soma=soma+(2*rand())**n
  enddo
  !soma será dividida pela quantidade de pontos obtidos
  media=soma/a
  !saida de dados
  write(*,*)"valor da média",media
end program
