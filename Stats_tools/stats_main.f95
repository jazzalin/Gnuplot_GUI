! Gradient Descent Adaptation
! By James Azzalini

program gradient_descent
  use data_points
  use functions
  implicit none

  integer                   :: N, ierror, numit, i
  logical                   :: bool
  real                      :: b0, m0, LR, U, r, num
  real, dimension(2)        :: input

  !Data file
  open(unit=1, file='data.csv', status='old', action='read', iostat=ierror)
  if (ierror /= 0) stop 'Error while opening data.csv file'

  !Residual data -- GD
  open(unit=2, file='GD_residual.dat', status='replace', action='write', iostat=ierror)
  if (ierror /= 0) stop 'Error while opening GD_residual.dat file'

  !Error progression -- GD
  open(unit=10, file='GD_error.dat', status='replace', action='write', iostat=ierror)
  if (ierror /= 0) stop 'Error while opening GD_error.dat file'

  !Residual data -- PS
  open(unit=3, file='PS_residual.dat', status='replace', action='write', iostat=ierror)
  if (ierror /= 0) stop 'Error while opening PS_residual.dat file'

  !Error progression -- PS
  open(unit=11, file='PS_error.dat', status='replace', action='write', iostat=ierror)
  if (ierror /= 0) stop 'Error while opening PS_error.dat file'


  !Determining number of lines in data file
  ierror = 0
  N = 0
  do while (ierror == 0)
    read(1,*,iostat=ierror)
    N = N + 1
  end do
  rewind 1
  N = N - 1

  !Allocate memory
  allocate(points(N,2))

  !Reading data into array
  do i = 1, N
    read(1,*) points(i,1), points(i,2)
  end do
  !GRADIENT DESCENT ----------------------------------------------------------------------------------

  !Initialize variables for GD
  LR = 0.00004
  step0 = LR
  b0 = 0.0
  m0 = 0.0
  numit = 1000
  eps = 50
  batch = 10
  bool = .false.

  print '(a,f4.1,a,f4.1)', "Starting GD at b = ", b0, " and m = ", m0
  print '(a,f10.3)', "Initial error: ", error_computation(m0,b0)

  !Run gradient optimization
!  call gradient_runner(m0,b0,LR,numit,bool)

  !Run stochastic gradient optimization
  call stochastic_gd(m0,b0,LR,numit)

  !Run mini batch gradient optimization
!  call gradient_runner(m0,b0,LR,numit,bool)

  !Results
  print '(a,f10.3,a,f10.3)', "After optimization, we have b = ", b0, " and m = ", m0
  print '(a,f10.3)', "Final error: ", error_computation(m0,b0)

  !Correlation (r)
!  r = r_calculation()
!  print*, "Correlation: ", r

  !Coefficient of determination (r-squared)
!  print*, "R squared: ", r2_calculation(m0,b0)

  !Root-mean-squared-error
!  print*, "RMSE: ", rmse(m0,b0)

  !Residual analysis
!  call residual_data(m0,b0,2)

  print*, "---------------------------------------------------"
  !PATTERN SEARCH ------------------------------------------------------------------------------------
  h = 0.25
  limit = 0.000000000001
  input(1) = 0.0	!m
  input(2) = 0.0	!b

!  print*, "INITIAL ERROR: ", error_computation(input(1),input(2))

  !Run Pattern Search Optimization
!  call ps_optimization(input)

  !Results
!  print*, "FINAL RESULT: ", input
!  print*, "FINAL ERROR: ", error_computation(input(1),input(2))

  !Correlation
!  print*, "Correlation: ", r

  !Coefficient of determination (r-squared)
!  print*, "R squared: ", r2_calculation(input(1),input(2))

  !Root-mean-squared-error
!  print*, "RMSE: ", rmse(input(1),input(2))

  !Residual analysis
!  call residual_data(input(1),input(2),3)



  deallocate(points)
  close(1)
  close(2)
  close(3)
  close(10)
  close(11)
end program gradient_descent
