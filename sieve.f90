program sieve
    use iso_fortran_env
    implicit none

    integer(kind=int64), parameter:: i_max = 7812500
    integer(kind=int64), parameter:: b_max = 64*i_max
    integer(kind=int64), parameter:: n_max = 2*b_max+1
    integer(kind=int64), parameter:: n_up = sqrt(real(n_max))+1
    integer(kind=int64), parameter:: b_up = n_up/2

    integer(kind=int64):: i, j, b
    integer(kind=int64), dimension(i_max):: is_prime

    ! the b-th bit (0-63) of is_prime(i) contains whether
    ! 2*(64*(i-1)+b)+3 is a prime number or not.
    ! The sieve is done on odd numbers from 3 to
    ! 2*b_max+1.

    do b = 0, 63
        is_prime(1) = ibset(is_prime(1), b)
    enddo
    is_prime(2:i_max) = is_prime(1)

    do b = 1, b_up
        if (.not. btest(is_prime(1+(b-1)/64), mod(b-1, 64))) cycle
        do j = ((2*b+1)**2-1)/2, b_max, 2*b+1
            i = 1 + (j-1)/64
            is_prime(i) = ibclr(is_prime(i), mod(j-1, 64))
        end do
    end do

    open(unit=11, file='sieve.out')
    do b = 0, b_max-1
        if (btest(is_prime(1+b/64), mod(b, 64))) write(11, '(I10)') 2*b+3
    end do
    close(11)
end program sieve
