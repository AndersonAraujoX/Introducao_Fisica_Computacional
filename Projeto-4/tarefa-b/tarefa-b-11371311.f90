program Qb
    implicit real*8(a-h,o-z)
    real*8,dimension(2)::v0
    real*8,dimension(2)::gama
    character::nome(8)*22
    !nome dos arquivos os valores impares são y(t) e os arquivos pares vão v(t)
    data nome/"saida1-b-11371311.dat","saida2-b-11371311.dat","saida3-b-11371311.dat","saida4-b-11371311.dat", &
    &"saida5-b-11371311.dat" ,"saida6-b-11371311.dat","saida7-b-11371311.dat","saida8-b-11371311.dat"/

    !condicoes iniciais no vetor
    v0=(/10d0,0d0/)
    gama=(/0.1d0,0.01d0/)

    !condições iniciais
    e0=0.0001
    y=100
    j=1
    l=1
    t=e0
    k=11

    !primeiro e0
    a=-gama(l)*v0(j)-10
    v=v0(j)+e0*a/2.0d0
    y=y+e0*v
    open(k,file=nome(k-10))
    open(k+1,file=nome(k-9))
    write(k,*)t,y
    write(k+1,*)t,v

    !loop para verifica
    do while(y>=0 .and. j<=2)
        a=-gama(l)*v-10
        v=v+e0*a
        y=y+v*e0
        t=t+e0
        if(y<=0 .and. j<2)then!caso y chegue a 0 e0 permuta
            y=100!nesse caso ele verifica todos e0 antes de mudar v0
            j=j+1
            k=k+2
            t=e0
            !primeiro e0
            a=-gama(l)*v0(j)-10
            v=v0(j)+a*e0/2
            open(k,file=nome(k-10))!foi colocado  k-10 por causa que quando colocamos certos numeros para k dá erro
            open(k+1,file=nome(k-9))
        elseif(y<=0 .and. j==2 .and. l<2)then!caso y chegue a 0 e0 permuta e j=6 a velocidade troca
            y=100!nesse caso ele verifica todos v0 antes de mudar gama
            t=e0
            l=l+1
            j=1
            k=k+2
            !primeiro e0
            a=-gama(l)*v0(j)-10
            v=v0(j)+a*e0/2
            open(k,file=nome(k-10))!foi colocado  k-10 por causa que quando colocamos certos numeros para k dá erro
            open(k+1,file=nome(k-9))
        endif
        write(k,*)t,y
        write(k+1,*)t,v
    enddo
end program