program Qe
    implicit real*8(a-h,o-z)
    open(1,file="saida1-e-11371311.dat",status="replace")
    open(2,file="saida2-e-11371311.dat",status="replace")
    open(3,file="saida3-e-11371311.dat",status="replace")

    !entrada de dados
    write(*,*)"Coloque a velocidade"
    read(*,*)v
    write(*,*)"Coloque o valor de dx desejado"
    read(*,*)dx
    write(*,*)"Coloque o valor de alpha(Graus)"
    read(*,*)graus

    !convercao de graus para radiano
    rad=acos(-1.0)*graus/180.0d0
    !condicoes iniciais
    e0=0.0001
    y=100
    x=0
    t=0
    vy=v*sin(rad)
    vx=v*cos(rad)


    !primeiro e0
    vy=vy-10*e0/2.0d0
    y=y+vy*e0
    write(1,*)t,y
    write(2,*)t,x
    write(3,*)x,y



    do while(0==0)
        x=x+vx*e0
        xr=x
        y=y+vy*e0
        vy=vy-10*e0
        t=t+e0

        if(abs(xr-xl)<=0.001 .and. y<=0)then
            exit!caso a distância entre os ponto em que bolinha quicou for menor 0.001 o loop para.

        
        elseif(y<=0)then
            vy=abs(vy)*0.7
            vx=abs(vx)*(1-dx)
            xl=x
            !y=0 para caso y de negativo entrando aqui. 
            y=0
        endif


        write(1,*)t,y
        write(2,*)t,x
        write(3,*)x,y
    enddo
end program