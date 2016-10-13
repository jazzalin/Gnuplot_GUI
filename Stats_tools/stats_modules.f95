!Modules for GD
module data_points
    implicit none

    real, dimension(:,:), allocatable :: points
	real :: h, limit,eps,step0
	integer :: batch

end module data_points

module functions
    use data_points
    implicit none
contains
	!Subroutine to reinit random generator seed (not my own)
	subroutine init_random_seed()
		implicit none
		integer :: i, n, clock
		integer, dimension(:), allocatable :: seed

		call random_seed(size = n)
		allocate(seed(n))

		call system_clock(count=clock)

		seed = clock + 37 * (/ (i - 1, i = 1, n) /)
		call random_seed(put = seed)

		deallocate(seed)
	end subroutine init_random_seed

    !Evaluation of objective function
	real function error_computation(m,b)

        real, intent(in) :: m, b
        integer :: i,N
        real    :: x, y, total_error

        N = size(points,1)
        total_error = 0.0
        do i = 1, N
            x = points(i,1)
            y = points(i,2)
            total_error = total_error + (y - (m*x+b))**2
        end do
        error_computation = total_error/(1.0*N)

    end function error_computation

	!Correlation calculation
	real function r_calculation()
		use data_points
		implicit none
		integer :: i, N
		real :: x,y,x2,y2,xy

		x=0.
		y=0.
		x2=0.
		y2=0.
		xy=0.
		N = size(points,1)
		do i = 1, N
            x = x + points(i,1)
            y = y + points(i,2)
			x2 = x2 + points(i,1)*points(i,1)
			y2 = y2 + points(i,2)*points(i,2)
			xy = xy + points(i,1)*points(i,2)
        end do
		r_calculation = (N*xy-x*y)/(sqrt(N*x2-x*x)*sqrt(N*y2-y*y))

	end function r_calculation

	!Coefficient of determination
	real function r2_calculation(m,b)
		use data_points
		implicit none
		real, intent(in) :: m,b
		integer :: i, N
		!Regression sum-of-squares, Total sum-of-squares, Predicted y
		real :: mean,reg_sos,total_sos,pre_y

		N = size(points,1)
		mean = 0.
		do i = 1, N
			mean = mean + points(i,2)
		end do
		mean = mean/(1.0*N)

		reg_sos = 0.
		total_sos = 0.
		do i = 1, N
            pre_y = m*points(i,1)+b
			reg_sos = reg_sos + (pre_y-mean)**2
			total_sos = total_sos + (points(i,2)-mean)**2
        end do
		r2_calculation = reg_sos/total_sos

	end function r2_calculation


	!Residual data for graphical analysis
	subroutine residual_data(m,b,channel)
		use data_points
		implicit none
		real, intent(in) :: m,b
		integer, intent(in) :: channel
		integer :: i, N
		real :: pre_y, r

		N = size(points,1)
		do i = 1, N
			pre_y = m*points(i,1)+b
			r = points(i,2) - pre_y
			write(channel,*) points(i,1), r
		end do

	end subroutine residual_data

	!Root-mean-squared-error (RMSE)
	real function rmse(m,b)
		use data_points
		implicit none
		real, intent(in) :: m,b
		integer :: i, N
		real :: pre_y, r, res_sos

		N = size(points,1)
		res_sos = 0.
		do i = 1, N
			pre_y = m*points(i,1)+b
			res_sos = res_sos + (pre_y - points(i,2))**2
		end do
		rmse = sqrt(res_sos/(1.0*N))

	end function rmse

	!Random number selector (within bounds)
	integer function rand_select(a,b)
		implicit none
		integer, intent(in) :: a,b !a is lower bound of range; b is size of range
		real :: tmp

		call random_number(tmp)
		tmp = a + floor((b+1-a)*tmp)
		rand_select = int(tmp)
	end function rand_select

	subroutine fyshuffle()
		use data_points
		implicit none
		integer :: i, N, rand_index
		real :: tmp_x,tmp_y

		!reinit seed for random number generator
		call init_random_seed()

		N = size(points,1)
		do i = 1, N
			!select random index within range of data
			rand_index = rand_select(i,N)

			!swap x and y coordinate (a do loop could be used instead)
			tmp_x = points(rand_index,1)
			tmp_y = points(rand_index,2)
			points(rand_index,1) = points(i,1)
			points(rand_index,2) = points(i,2)
			points(i,1) = tmp_x
			points(i,2) = tmp_y
		end do
	end subroutine fyshuffle

! GRADIENT DESCENT functions -----------------------------------------------------------------------------------------------
    !GD launcher
	subroutine gradient_runner(starting_m, starting_b, incr, num, minibatch)
        use data_points
        implicit none
        real, intent(inout) :: starting_m, starting_b
        integer, intent(in) :: num
        real, intent(inout) :: incr
		logical, intent(in) :: minibatch
        integer :: i
		real :: m, b

		b = starting_b
		m = starting_m
        do i = 1, num
!            incr = incr/(1+(i-1)*eps)
			write(10,*)i, error_computation(m,b)
			if(minibatch) then
				call mini_batch(m,b,incr,batch)
			else
				call gradient_opt(m,b,incr)
			end if
        end do
		starting_b = b
		starting_m = m

    end subroutine gradient_runner

	!Batch gradient descent (GD)
    subroutine gradient_opt(m_current,b_current,step)

        real, intent(inout) :: m_current,b_current
        real, intent(in)    :: step
        real :: m_gradient, b_gradient, x, y
        integer :: N, i, j

        N = size(points,1)
        m_gradient = 0.
        b_gradient = 0.

        !take partial derivatives of error function --> del*E(a)
        do i = 1, N
            x = points(i,1)
            y = points(i,2)
            m_gradient = m_gradient - ((2.0/N)*x*(y-((m_current*x)+b_current)))
            b_gradient = b_gradient - ((2.0/N)*(y-((m_current*x)+b_current)))
        end do
        m_current = m_current - (step*m_gradient)
        b_current = b_current - (step*b_gradient)

    end subroutine gradient_opt

	!Mini-batch gradient descent
	subroutine mini_batch(m_current,b_current,step,N)
		use data_points
		implicit none
        real, intent(inout) :: m_current,b_current
        real, intent(in)    :: step
		integer, intent(in) :: N
        real :: m_gradient, b_gradient, x, y
        integer :: i, j

        m_gradient = 0.
        b_gradient = 0.

		!Shuffle the data points
		call fyshuffle()
        do i = 1, N
            x = points(i,1)
            y = points(i,2)
            m_gradient = m_gradient - ((2.0/N)*x*(y-((m_current*x)+b_current)))
            b_gradient = b_gradient - ((2.0/N)*(y-((m_current*x)+b_current)))
            m_current = m_current - (step*m_gradient)
            b_current = b_current - (step*b_gradient)
        end do


    end subroutine mini_batch

	!Stochastic gradient descent (SGD)
	subroutine stochastic_gd(m,b,step,num)
		use data_points
		implicit none
		real, intent(inout) :: m, b
        integer, intent(in) :: num
        real, intent(inout) :: step
        integer :: i,j,N
		real :: m_gradient, b_gradient, x, y

		N = size(points,1)
		do i = 1, num
			step = step0/(1+i/eps)
			write(10,*)i, error_computation(m,b)
			!Shuffle the data points
			call fyshuffle()
			do j = 1, N
				x = points(i,1)
				y = points(i,2)
				m_gradient = -((2.0/N)*x*(y-((m*x)+b)))
				b_gradient = -((2.0/N)*(y-((m*x)+b)))
				m = m - (step*m_gradient)
				b = b - (step*b_gradient)
			end do
		end do

	end subroutine stochastic_gd


! END GRADIENT DESCENT  ----------------------------------------------------------------------------------------------------


! PATTERN SEARCH functions -------------------------------------------------------------------------------------------------

	!Pattern Search Optimization
subroutine ps_optimization(bpoint)
	use data_points
	implicit none

	real, dimension(2), intent(inout) :: bpoint
	real, dimension(2) :: xpoint, old_bpoint, new_bpoint
	real :: U0, U1, Ui, penalty
	xpoint = bpoint
	old_bpoint = bpoint
	new_bpoint = bpoint
	!Initial error
	U0 = error_computation(bpoint(1),bpoint(2))

	optimization : do while (h > limit)
!						print *, "Value of U0 and h: ", U0, h
!						print *, bpoint
						U1 = explore(bpoint,xpoint)
!						print*, "Value of U1: ", U1
						do while (U1 < U0)
							U0 = U1
							old_bpoint = xpoint
!							print*, "New value of U0: ", U0
							call pattern(bpoint,xpoint,new_bpoint)
!							print*, "Pattern bpoint: ", new_bpoint
							xpoint = new_bpoint
							!Informal: to determine if patter move was successful
							!Note: Ui is irrelevant
							Ui = error_computation(new_bpoint(1),new_bpoint(2))
!							print*, "Informal U: ", Ui
							U1 = explore(new_bpoint,xpoint)
							bpoint = new_bpoint
!							print*, "---------PATTERN SUCCESS-------------"
						end do
						bpoint = old_bpoint
						xpoint = bpoint
						call mod_step_size(h, limit)
!						print*, "-------------PATTERN FAIL----------------"
					end do optimization
!					print*, "FINAL: ", bpoint
!					print*, "FINAL: ", xpoint


end subroutine ps_optimization

	!  Exploratory move
real function explore(bpoint,xpoint)
	implicit none
    integer :: i
    real, dimension(2), intent(inout) :: xpoint
    real, dimension(2), intent(in) :: bpoint
    real :: y0,y1,penalty
    i = 1
    y0 = error_computation(bpoint(1),bpoint(2)) !value of objective function at last known base point
!       Start exploration
    increment: do while (i <= 2)
        xpoint(i) = xpoint(i) + h
        y1 = error_computation(xpoint(1),xpoint(2))
!        print *, "y1", y1, i, y0
!        print*, xpoint
        if (y1 < y0) then
            y0 = y1
        else
            xpoint(i) = xpoint(i) - 2*h
            y1 = error_computation(xpoint(1),xpoint(2))
!            print *, "y1", y1, i, y0
!            print*, xpoint
            if (y1 < y0) then
                y0 = y1
            else
                xpoint(i) = bpoint(i)
!                print *, "xpoint ", xpoint
!                print *, "bpoint ", bpoint
                y1 = error_computation(xpoint(1),xpoint(2))
            end if
        end if
        i = i + 1
    end do increment
    explore = y1
!    print *, "xpoint: ", xpoint
!    print *, "U1: ", y1
end function explore

!   Reduction of increment
subroutine mod_step_size(h0,TOL)
	use data_points
    real, intent(inout) :: h0
    real, intent(in) :: TOL
!       If no progress has been made, the increment is halved (Fermi, Metropolis)
    if (h >= TOL) then
        h = h/5.
    else
!           Reached the smallest increment possible -->  closest approximation possible
        print *, "----------END----------"
    end if
end subroutine mod_step_size

!   Pattern move subroutine
subroutine pattern(bpoint,xpoint,new_bpoint)
    implicit none
    real, dimension(2), intent(in) :: xpoint
    real, dimension(2), intent(in) :: bpoint
    real, dimension(2), intent(out) :: new_bpoint

    new_bpoint = 2*xpoint - bpoint
end subroutine pattern

! END PATTERN SEARCH  ------------------------------------------------------------------------------------------------------

end module functions
