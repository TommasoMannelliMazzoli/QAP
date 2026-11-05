!> VND and GVNS of the thesis (chapter 4).
!>
!> VND alternates 2-opt and 3-opt (first or best improvement) until the
!> solution is a local optimum for both. GVNS shakes the incumbent with a
!> random swap, then a random rotation, then the exchange of its two halves
!> (deterministic, as in the thesis), applies VND after each shake and
!> restarts from the first shake whenever the incumbent improves. It stops
!> at the time limit or after max_loops_without_improvement loops.
module qap_vns
  use iso_fortran_env, only: int64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t, evaluated
  use qap_moves, only: swap_delta, rotation_delta, apply_swap, apply_rotation, ROTATE_LEFT
  use qap_local_search, only: two_opt_first, two_opt_best, three_opt_first, three_opt_best
  use qap_params, only: gvns_params_t
  use qap_rng, only: random_stream_t
  use qap_run, only: run_t
  implicit none
  private
  public :: vnd, gvns

  integer, parameter :: SWAP_SHAKE = 1, ROTATION_SHAKE = 2, HALVES_SHAKE = 3
  integer, parameter :: SWAPS = 1, ROTATIONS = 2

contains

  subroutine vnd(qap, sol, best_improvement, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    logical, intent(in) :: best_improvement
    type(run_t), intent(in) :: run
    integer :: neighbourhood
    integer(int64) :: cost_before

    neighbourhood = SWAPS
    do while (neighbourhood <= ROTATIONS)
      if (run%is_over()) exit
      cost_before = sol%cost
      call descend(qap, sol, neighbourhood, best_improvement, run)
      if (sol%cost < cost_before) then
        neighbourhood = SWAPS
      else
        neighbourhood = neighbourhood + 1
      end if
    end do
  end subroutine vnd

  subroutine gvns(qap, start, best_improvement, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: start
    logical, intent(in) :: best_improvement
    type(gvns_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: incumbent, candidate
    integer :: shake, loops_without_improvement

    incumbent = start
    call run%record(incumbent)
    loops_without_improvement = 0
    do while (loops_without_improvement < params%max_loops_without_improvement)
      if (run%is_over()) exit
      shake = SWAP_SHAKE
      do while (shake <= HALVES_SHAKE)
        if (run%is_over()) exit
        candidate = shaken(qap, incumbent, shake, rng)
        call vnd(qap, candidate, best_improvement, run)
        call run%count_iteration()
        if (candidate%cost < incumbent%cost) then
          incumbent = candidate
          call run%record(incumbent)
          loops_without_improvement = 0
          shake = SWAP_SHAKE
        else
          shake = shake + 1
        end if
      end do
      loops_without_improvement = loops_without_improvement + 1
    end do
  end subroutine gvns

  subroutine descend(qap, sol, neighbourhood, best_improvement, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    integer, intent(in) :: neighbourhood
    logical, intent(in) :: best_improvement
    type(run_t), intent(in) :: run

    if (neighbourhood == SWAPS .and. best_improvement) call two_opt_best(qap, sol, run)
    if (neighbourhood == SWAPS .and. .not. best_improvement) call two_opt_first(qap, sol, run)
    if (neighbourhood == ROTATIONS .and. best_improvement) call three_opt_best(qap, sol, run)
    if (neighbourhood == ROTATIONS .and. .not. best_improvement) call three_opt_first(qap, sol, run)
  end subroutine descend

  function shaken(qap, sol, shake, rng) result(moved)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: sol
    integer, intent(in) :: shake
    type(random_stream_t), intent(inout) :: rng
    type(solution_t) :: moved
    integer :: picked(3)

    moved = sol
    select case (shake)
    case (SWAP_SHAKE)
      picked(1:2) = distinct_indices(rng, qap%n, 2)
      call apply_swap(moved, picked(1), picked(2), swap_delta(qap, sol%p, picked(1), picked(2)))
    case (ROTATION_SHAKE)
      if (qap%n < 3) return
      picked = distinct_indices(rng, qap%n, 3)
      call apply_rotation(moved, picked(1), picked(2), picked(3), ROTATE_LEFT, &
                          rotation_delta(qap, sol%p, picked(1), picked(2), picked(3), ROTATE_LEFT))
    case (HALVES_SHAKE)
      moved = evaluated(qap, halves_exchanged(sol%p))
    end select
  end function shaken

  !> [1 4 | 3 2] -> [3 2 | 1 4]; with n odd the last ceiling(n/2) entries go first.
  pure function halves_exchanged(p) result(q)
    integer, intent(in) :: p(:)
    integer :: q(size(p))
    integer :: half

    half = (size(p) + 1) / 2
    q(1 : half) = p(size(p) - half + 1 :)
    q(half + 1 :) = p(1 : size(p) - half)
  end function halves_exchanged

  function distinct_indices(rng, n, count) result(picked)
    type(random_stream_t), intent(inout) :: rng
    integer, intent(in) :: n, count
    integer :: picked(count)
    integer :: i

    do i = 1, count
      picked(i) = rng%integer_between(1, n)
      do while (any(picked(1 : i - 1) == picked(i)))
        picked(i) = rng%integer_between(1, n)
      end do
    end do
  end function distinct_indices

end module qap_vns
