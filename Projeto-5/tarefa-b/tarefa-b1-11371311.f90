program Qa
implicit real*8(a-h,o-z)
    write(*,*)"digite o angulo em graus"
    read(*,*)conv
    !conversao para radiano
    pi=acos(-1.0d0)
    graus=pi*conv/180.0d0
    do while(-2*pi>=graus .or. graus>=2*pi)
      if(graus>2*pi)then
          graus=graus-2*pi
      else
          graus=graus+2*pi
      endif
    enddo
    !valor de pi
    pi=dacos(-1d0)
    teta=graus
    loop=1
    i=1
    w=0
    !primeiro det
    det=0.00001
    w=w-dsin(teta)*det
    teta=teta+w*det
    wi=w
    !metodo de euler-cromer
    do while(loop<=2)
        t=i*det
        w=w-dsin(teta)*det
        teta=teta+w*det
        i=i+1
        if(wi*w<=0)then
            loop=loop+1
        endif
        wi=w
    enddo
    write(*,*)"Periodo para o pendulo simples, atráves do metodo de Euler"
    write(*,*)t
    j=22
    h=graus/(2**j)
    n=2**j
    trape=0.0d0
    simp=0.0d0
    bode=0.0d0
    do m=-(n-2),n-2,2!começo com m=1 para quando x*m-1=0, assim inicio em f(0)
      !processamento do trapezio e de simpson
      trape=trape+(f(h*m-h,graus)+2*f(h*m,graus)+f(h*m+h,graus))*h/2.0
      simp=simp+(f(h*m-h,graus)+4*f(h*m,graus)+f(h*m+h,graus))*h/3.0
    enddo
    !Vamos utilizar o epsilon=2*h que é o intervalo que tiramos no calculo numerico
    A=2**(0.5)*2*(2*h)**(0.5)/(sin(graus)**(0.5))
    !saida de dados
    write(*,*)"Epsilon utilizado"
    write(*,*)2*h
    write(*,*)"Metodo do trapézio"
    write(*,*)2**(0.5)*trape+2*A
    write(*,*)"Metodo de simpson"
    write(*,*)2**(0.5)*simp+2*A
end program
    function f(x,graus)
      real*8::x,f,graus
      f=1/(dcos(x)-dcos(graus))**(0.5)
    end function