module integracion
implicit none

contains

subroutine trapecio(n, y, h, s)
    implicit none

    integer, intent(in) :: n
    real(8), intent(in) :: y(n), h
    real(8), intent(out) :: s
    integer :: i

    s = (y(1) + y(n))/2.0_8

    do i = 2, n-1
        s = s + y(i)
    end do

    s = h*s

end subroutine trapecio

subroutine simpson(n, y, h, s)
    implicit none

    integer, intent(in) :: n
    real(8), intent(in) :: y(n), h
    real(8), intent(out) :: s
    integer :: i

    s = y(1) + y(n)

    do i = 2, n-1, 2
        s = s + 4.0_8*y(i)
    end do

    do i = 3, n-2, 2
        s = s + 2.0_8*y(i)
    end do

    s = (h/3.0_8)*s

end subroutine simpson

end module integracion
