!> A permutation together with its cost.
module qap_solution
  use iso_fortran_env, only: int64
  use qap_instance, only: qap_instance_t, cost
  use qap_rng, only: random_stream_t
  implicit none
  private
  public :: solution_t, evaluated, random_solution, is_permutation, inverse

  type :: solution_t
    integer, allocatable :: p(:)              ! p(i) = location of facility i
    integer(int64) :: cost = huge(1_int64)
  end type solution_t

contains

  function evaluated(qap, p) result(sol)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:)
    type(solution_t) :: sol

    sol%p = p
    sol%cost = cost(qap, p)
  end function evaluated

  function random_solution(qap, rng) result(sol)
    type(qap_instance_t), intent(in) :: qap
    type(random_stream_t), intent(inout) :: rng
    type(solution_t) :: sol
    integer :: p(qap%n)

    call rng%permutation(p)
    sol = evaluated(qap, p)
  end function random_solution

  pure logical function is_permutation(p, n)
    integer, intent(in) :: p(:), n
    logical :: seen(n)
    integer :: i

    is_permutation = .false.
    if (size(p) /= n) return
    seen = .false.
    do i = 1, n
      if (p(i) < 1 .or. p(i) > n) return
      if (seen(p(i))) return
      seen(p(i)) = .true.
    end do
    is_permutation = .true.
  end function is_permutation

  pure function inverse(p) result(q)
    integer, intent(in) :: p(:)
    integer :: q(size(p))
    integer :: i

    do i = 1, size(p)
      q(p(i)) = i
    end do
  end function inverse

end module qap_solution
