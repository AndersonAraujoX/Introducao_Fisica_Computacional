program QC
    implicit real*8(a-h,o-z)
    real*8,dimension(3)::graus
    character::nome(9)*21
    data nome/"saida1-c-11371311.dat","saida2-c-11371311.dat","saida3-c-11371311.dat","saida4-c-11371311.dat", &
    &"saida5-c-11371311.dat" ,"saida6-c-11371311.dat","saida7-c-11371311.dat","saida8-c-11371311.dat",&
    &"saida9-c-11371311.dat"/
    graus=(/45.0d0,0.0d0,-45.0d0/)
    
    !condições iniciais
    e0=0.0001
    v=10
    y=100
    x=0
    i=1
    t=e0
    k=11
    rad=acos(-1.0)*graus(1)/180.0d0
    vy=v*sin(rad)
    vx=v*cos(rad)

    !primeiro e0
    vy=vy-10*e0/2.0d0
    y=y+vy*e0
    x=x+vx*e0
    open(k,file=nome(k-10))!foi colocado k-10 por causa que quando colocamos certos numeros para k dá erro
    open(k+1,file=nome(k-9))
    open(k+2,file=nome(k-8))
    write(k,*)t,y
    write(k+1,*)t,x
    write(k+2,*)x,y

    do while(y>=0)
        x=x+vx*e0
        y=y+vy*e0
        vy=vy-10*e0
        t=t+e0

        if(y<=0 .and. i<3)then!caso y chegue a 0 e0 permuta

            y=100
            x=0
            i=i+1
            k=k+3
            t=0

            rad=acos(-1.0)*graus(i)/180.0d0
            vy=v*sin(rad)
            vx=v*cos(rad)!primeiro e0
            vy=vy-10*e0/2.0d0
            open(k,file=nome(k-10))!colocams k-10 por causa que quando colocamos certos numeros para k dá erro
            open(k+1,file=nome(k-9))
            open(k+2,file=nome(k-8))

        endif

        write(k,*)t,y
        write(k+1,*)t,x
        write(k+2,*)x,y
    enddo
end program