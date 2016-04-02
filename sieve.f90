program sieve
    use iso_fortran_env
    implicit none

    integer(kind=int64), parameter:: i_max = 1000
    integer(kind=int64), parameter:: n_max = 2*i_max+1
    integer(kind=int64), parameter:: n_up = sqrt(real(n_max))+1
    integer(kind=int64), parameter:: i_up = n_up/2

    integer(kind=int64):: i, j
    logical, dimension(i_max):: is_prime

    ! is_prime(i) contains whether 2*i+1 is a prime
    ! number or not.
    ! The sieve is done on odd numbers from 3 to
    ! 2*i_max+1.

    is_prime(:) = .true.
    do i = 1, i_up
        if (.not.is_prime(i)) cycle
        do j = ((2*i+1)**2-1)/2, i_max, 2*i+1
            is_prime(j) = .false.
        end do
    end do

    open(unit=11, file='sieve.out')
    do i = 1, i_max
        if (is_prime(i)) write(11, '(I10)') 2*i+1
    end do
    close(11)
end program sieve
