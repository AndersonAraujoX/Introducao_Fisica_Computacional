program asteroides
	implicit real*8(a-h,o-z)
	dimension raio(3),velocidade(3)
	raio = (/3.00,3.2760,3.700/)
	velocidade = (/3.6280,3.4710,3.2670/)	

	open(1,file='asteroide1')
	open(2,file='asteroide2')
	open(3,file='asteroide3')	
	
	print*, 'Digite o tempo em anos até o qual se calculara a posicao dos asteroides'
	read*, tempo

	pi = dacos(-1.0d0)
	m_s = 2000
	m_j = 1.9
	dt = 0.0001

	do i = 1,3 

		t = 0

		x_a_1 = raio(i)
		y_a_1 = 0
		
		x_j_1 = 5.200
		y_j_1 = 0

		v_x_a = 0
		v_y_a = velocidade(i)
	
		v_x_j = 0
		v_y_j = 2.755

		x_a_0 = x_a_1 - v_x_a*dt
		y_a_0 = y_a_1 - v_y_a*dt

		x_j_0 = x_j_1 - v_x_j*dt
		y_j_0 = y_j_1 - v_y_j*dt		


		do while (t.lt.tempo)

			write(i,*) x_a_1,y_a_1 
			
			r_a = dsqrt(x_a_1**2 + y_a_1**2)
			r_j = dsqrt(x_j_1**2 + y_j_1**2)
			r_aj = dsqrt((x_a_1 - x_j_1)**2 + (y_a_1 - y_j_1)**2)

			x_a = 2*x_a_1 - x_a_0 - (4*(pi**2)*x_a_1/(r_a**3)+4*(pi**2)*(m_j/m_s)*(x_a_1 - x_j_1)/(r_aj**3))*(dt**2)
			y_a = 2*y_a_1 - y_a_0 - (4*(pi**2)*y_a_1/(r_a**3)+4*(pi**2)*(m_j/m_s)*(y_a_1 - y_j_1)/(r_aj**3))*(dt**2)

			x_j = 2*x_j_1 - x_j_0 - (4*(pi**2)*x_j_1/(r_j**3))*(dt**2)
			y_j = 2*y_j_1 - y_j_0 - (4*(pi**2)*y_j_1/(r_j**3))*(dt**2)
			
			x_a_0 = x_a_1
			y_a_0 = y_a_1

			x_a_1 = x_a
			y_a_1 = y_a

			x_j_0 = x_j_1
			y_j_0 = y_j_1

			x_j_1 = x_j
			y_j_1 = y_j
			
			t = t+dt
			

		end do		
	

	end do 

end program
