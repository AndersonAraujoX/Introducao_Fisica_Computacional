program tarefab3
   implicit real*8(a-h,o-z)
   real*8,dimension(10)::m,x0,y0,x1,y1,vx,vy
   !vetores 
   x0 = (/0.0d0,0.39d0,0.72d0,1.0d0,1.52d0,5.20d0,9.24d0,19.19d0,30.06d0,39.53d0/)
   y0 = (/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)
   vy = (/0.0d0,10.21d0,7.416d0,6.283d0,5.080d0,2.757d0,1.971d0,1.430d0,1.146d0,1.001d0/)
   vx = (/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)
   m = (/2.0d0*10**7,2.40d0,49.0d0,60.0d0,6.60d0,19000.0d0,5700.0d0,880.0d0,103.0d0,0.13d0/)
   !
   pi=acos(-1.0d0)
   det=0.0001d0
   t=0
   write(*,*)"digite o tempo que o programa vai ficar rodando em anos"
   read(*,*)ano

   do i=1,10
      x1(i)=x0(i)+vx(i)*det
      y1(i)=y0(i)+vy(i)*det
   enddo
   
	open(21,file='Mercurio',status="replace")
	open(22,file='Venus',status="replace")
	open(23,file='Terra',status="replace")
	open(24,file='Marte',status="replace")
	open(25,file='Jupiter',status="replace")
	open(26,file='Saturno',status="replace")
	open(27,file='Urano',status="replace")
	open(28,file='Netuno',status="replace")
	open(29,file='Plutao',status="replace")

   do while(t<=ano)
      do i = 2,10

         write(i+19,*)x1(i),y1(i)

         x2=2*x1(i)-x0(i)
         y2=2*y1(i)-y0(i)

         do j=1,10
            if(i/=j)then
            r=sqrt((x1(i)-x1(j))**2+(y1(i)-y1(j))**2)
            x2=x2-4*pi**2*(m(j)/m(1))*(x1(i)-x1(j))/(r**3)*det**2
            y2=y2-4*pi**2*(m(j)/m(1))*(y1(i)-y1(j))/(r**3)*det**2
            endif

         enddo
         
         x0(i)=x1(i)
         x1(i)=x2
         y0(i)=y1(i)
         y1(i)=y2
      enddo
      t=t+det
   enddo

end program