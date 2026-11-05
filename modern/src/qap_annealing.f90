!> Simulated annealing on swaps. It replaces the original TEMPLE (never
!> used for the thesis' results), whose fixed initial temperature T = 5000
!> ignored the scale of the instance: the initial temperature now makes
!> `initial_acceptance` the probability of accepting an average worsening
!> swap. Geometric cooling as in TEMPLE; when the temperature becomes
!> negligible the search is reheated, until the time limit.
module qap_annealing
  use iso_fortran_env, only: int64, real64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t, random_solution
  use qap_moves, only: swap_delta, apply_swap
  use qap_params, only: annealing_params_t
  use qap_rng, only: random_stream_t
  use qap_run, only: run_t
  implicit none
  private
  public :: simulated_annealing

  integer, parameter :: CALIBRATION_SWAPS = 1000
  integer, parameter :: LARGE_FROM = 50
  real(real64), parameter :: REHEAT_BELOW = 1.0e-4_real64   ! fraction of the initial temperature

contains

  subroutine simulated_annealing(qap, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(annealing_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: current
    real(real64) :: initial, temperature, cooling
    integer :: moves_per_temperature

    current = random_solution(qap, rng)
    call run%record(current)
    initial = initial_temperature(qap, current, params%initial_acceptance, rng)
    temperature = initial
    cooling = merge(params%cooling_large, params%cooling_small, qap%n >= LARGE_FROM)
    moves_per_temperature = max(1, nint(params%moves_per_temperature_factor * real(qap%n, real64)**2))
    do while (.not. run%is_over())
      call metropolis_moves(qap, current, temperature, moves_per_temperature, rng, run)
      temperature = cooling * temperature
      if (temperature < REHEAT_BELOW * initial) temperature = initial
    end do
  end subroutine simulated_annealing

  subroutine metropolis_moves(qap, current, temperature, moves, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: current
    real(real64), intent(in) :: temperature
    integer, intent(in) :: moves
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    integer :: move, r, s
    integer(int64) :: d

    do move = 1, moves
      call random_pair(rng, qap%n, r, s)
      d = swap_delta(qap, current%p, r, s)
      if (accepts(d, temperature, rng)) then
        call apply_swap(current, r, s, d)
        if (d < 0) call run%record(current)
      end if
      call run%count_iteration()
      if (run%is_over()) return
    end do
  end subroutine metropolis_moves

  logical function accepts(d, temperature, rng)
    integer(int64), intent(in) :: d
    real(real64), intent(in) :: temperature
    type(random_stream_t), intent(inout) :: rng

    accepts = d <= 0
    if (.not. accepts) accepts = rng%uniform() < exp(-real(d, real64) / temperature)
  end function accepts

  !> T0 with exp(-mean worsening / T0) = acceptance, from random swaps.
  real(real64) function initial_temperature(qap, start, acceptance, rng) result(t0)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: start
    real(real64), intent(in) :: acceptance
    type(random_stream_t), intent(inout) :: rng
    real(real64) :: worsening_sum
    integer :: i, r, s, worsening_count
    integer(int64) :: d

    worsening_sum = 0
    worsening_count = 0
    do i = 1, CALIBRATION_SWAPS
      call random_pair(rng, qap%n, r, s)
      d = swap_delta(qap, start%p, r, s)
      if (d > 0) then
        worsening_sum = worsening_sum + real(d, real64)
        worsening_count = worsening_count + 1
      end if
    end do
    t0 = 1
    if (worsening_count > 0) t0 = -(worsening_sum / worsening_count) / log(acceptance)
  end function initial_temperature

  subroutine random_pair(rng, n, r, s)
    type(random_stream_t), intent(inout) :: rng
    integer, intent(in) :: n
    integer, intent(out) :: r, s

    r = rng%integer_between(1, n)
    s = rng%integer_between(1, n - 1)
    if (s >= r) s = s + 1
  end subroutine random_pair

end module qap_annealing
