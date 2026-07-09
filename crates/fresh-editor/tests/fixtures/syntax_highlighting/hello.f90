module linear_algebra
  implicit none
  private
  public :: saxpy

contains

  subroutine saxpy(a, x, y)
    real, intent(in) :: a
    real, intent(in) :: x(:)
    real, intent(inout) :: y(:)
    integer :: i

    do concurrent (i = 1:size(x))
      y(i) = a * x(i) + y(i)
    end do
  end subroutine saxpy

end module linear_algebra

program hello_fortran
  use linear_algebra, only: saxpy
  implicit none
  real :: x(3) = [1.0, 2.0, 3.0]
  real :: y(3) = [4.0, 5.0, 6.0]

  call saxpy(2.0, x, y)
  print '(A,3F6.2)', 'result:', y
end program hello_fortran
