!> Ant colonies: the ACO of the thesis (chapter 4) and HAS-QAP
!> (Gambardella, Taillard, Dorigo 1999), which share one colony loop:
!>
!>   every ant applies R pheromone-guided swaps to its permutation, then
!>   2-opt first improvement; with intensification active an ant keeps the
!>   better of its old and new permutation; intensification stays active
!>   while the iteration improves the best solution; the trail evaporates
!>   and is reinforced; after S iterations without improvement the trail
!>   is reset and the ants restart from random permutations.
!>
!> What differs is in the colony rules:
!>   thesis: trail initialised from 2-opt descents of the n cyclic shifts,
!>           reinforced with the "M1 + M2" matrix of the ant that last found
!>           the best solution; on restart all the ants are random.
!>   HAS:    uniform trail tau0 = 1/(Q z_best), reinforced on the best
!>           permutation by alpha2/z_best; on restart one ant keeps the best.
!>
!> Fixed defects of the original code: the ants now keep their permutation
!> between iterations (they were regenerated at random every iteration),
!> the R swaps are cumulative (only the last one survived), and the
!> stagnation counter is reset (after 5n iterations the trail was
!> re-initialised at every iteration). As the thesis' "T <- 0" would leave
!> no information to sample from, its restart re-initialises the trail.
module qap_colony
  use iso_fortran_env, only: int64, real64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t, evaluated, random_solution
  use qap_moves, only: swap_delta, apply_swap
  use qap_local_search, only: swap_observer_t, two_opt_first, two_opt_best
  use qap_params, only: colony_params_t
  use qap_rng, only: random_stream_t
  use qap_run, only: run_t
  implicit none
  private
  public :: thesis_ant_colony, hybrid_ant_system

  type, abstract :: colony_rules_t
    real(real64), allocatable :: trail(:,:)   ! trail(i, l): facility i at location l
    real(real64) :: evaporation = 0
    logical :: restart_keeps_best = .false.
  contains
    procedure(reset_trail), deferred :: reset
    procedure(improve_ant), deferred :: improve
    procedure(note_best), deferred :: note_new_best
    procedure(reinforce_trail), deferred :: reinforce
  end type colony_rules_t

  abstract interface
    subroutine reset_trail(self, qap, best, run)
      import :: colony_rules_t, qap_instance_t, solution_t, run_t
      class(colony_rules_t), intent(inout) :: self
      type(qap_instance_t), intent(in) :: qap
      type(solution_t), intent(in) :: best
      type(run_t), intent(in) :: run
    end subroutine reset_trail

    subroutine improve_ant(self, qap, ant, run)
      import :: colony_rules_t, qap_instance_t, solution_t, run_t
      class(colony_rules_t), intent(inout) :: self
      type(qap_instance_t), intent(in) :: qap
      type(solution_t), intent(inout) :: ant
      type(run_t), intent(in) :: run
    end subroutine improve_ant

    !> `ant`, just returned by improve, is the new best solution.
    subroutine note_best(self, ant)
      import :: colony_rules_t, solution_t
      class(colony_rules_t), intent(inout) :: self
      type(solution_t), intent(in) :: ant
    end subroutine note_best

    subroutine reinforce_trail(self, best)
      import :: colony_rules_t, solution_t
      class(colony_rules_t), intent(inout) :: self
      type(solution_t), intent(in) :: best
    end subroutine reinforce_trail
  end interface

  !> Records the "M2" matrix of the thesis: for every swap of a descent,
  !> -1 on the assignments it breaks and +1 on those it creates.
  type, extends(swap_observer_t) :: assignment_changes_t
    real(real64), allocatable :: m(:,:)
  contains
    procedure :: notify => record_swap
  end type assignment_changes_t

  type, extends(colony_rules_t) :: thesis_rules_t
    real(real64), allocatable :: initial_trail(:,:), reinforcement(:,:)
    type(assignment_changes_t) :: last_descent
  contains
    procedure :: reset => thesis_reset
    procedure :: improve => thesis_improve
    procedure :: note_new_best => thesis_note_new_best
    procedure :: reinforce => thesis_reinforce
  end type thesis_rules_t

  type, extends(colony_rules_t) :: has_rules_t
    real(real64) :: reinforcement = 0
    real(real64) :: q_constant = 0
  contains
    procedure :: reset => has_reset
    procedure :: improve => has_improve
    procedure :: note_new_best => has_note_new_best
    procedure :: reinforce => has_reinforce
  end type has_rules_t

contains

  subroutine thesis_ant_colony(qap, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(colony_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(thesis_rules_t) :: rules

    rules%evaporation = params%evaporation
    rules%restart_keeps_best = .false.
    call ant_colony(qap, params, rules, rng, run)
  end subroutine thesis_ant_colony

  subroutine hybrid_ant_system(qap, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(colony_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(has_rules_t) :: rules

    rules%evaporation = params%evaporation
    rules%reinforcement = params%reinforcement
    rules%q_constant = params%q_constant
    rules%restart_keeps_best = .true.
    call ant_colony(qap, params, rules, rng, run)
  end subroutine hybrid_ant_system

  subroutine ant_colony(qap, params, rules, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(colony_params_t), intent(in) :: params
    class(colony_rules_t), intent(inout) :: rules
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t), allocatable :: ants(:)
    type(solution_t) :: candidate
    integer :: k, stagnation, stagnation_limit, swaps
    logical :: intensify, improved

    swaps = swaps_per_ant(params, qap%n)
    stagnation_limit = max(1, nint(params%stagnation_factor * qap%n))
    allocate (ants(params%ants))
    ants = new_ants(qap, params%ants, rules, rng, run)
    call rules%reset(qap, run%best, run)
    intensify = .true.
    stagnation = 0
    do while (.not. run%is_over())
      improved = .false.
      do k = 1, size(ants)
        candidate = ants(k)
        call guided_swaps(qap, candidate, rules%trail, swaps, params%exploitation, rng)
        call rules%improve(qap, candidate, run)
        if (candidate%cost < run%best%cost) then
          call run%record(candidate)
          call rules%note_new_best(candidate)
          improved = .true.
        end if
        if (.not. intensify .or. candidate%cost < ants(k)%cost) ants(k) = candidate
        if (run%is_over()) return
      end do
      intensify = improved
      call rules%reinforce(run%best)
      call run%count_iteration()
      stagnation = merge(0, stagnation + 1, improved)
      if (stagnation >= stagnation_limit) then
        call rules%reset(qap, run%best, run)
        ants = new_ants(qap, params%ants, rules, rng, run)
        if (rules%restart_keeps_best) ants(1) = run%best
        intensify = .true.
        stagnation = 0
      end if
    end do
  end subroutine ant_colony

  function new_ants(qap, count, rules, rng, run) result(ants)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: count
    class(colony_rules_t), intent(inout) :: rules
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: ants(count)
    integer :: k

    do k = 1, count
      ants(k) = random_solution(qap, rng)
      call rules%improve(qap, ants(k), run)
      call run%record(ants(k))
    end do
  end function new_ants

  pure integer function swaps_per_ant(params, n)
    type(colony_params_t), intent(in) :: params
    integer, intent(in) :: n

    if (params%swaps_factor > 0) then
      swaps_per_ant = max(1, nint(params%swaps_factor * n))
    else
      swaps_per_ant = params%swaps
    end if
  end function swaps_per_ant

  !> R swaps applied one after the other: r at random, its partner s the
  !> most desirable with probability `exploitation`, otherwise drawn with
  !> probability proportional to trail(r, p(s)) + trail(s, p(r)).
  subroutine guided_swaps(qap, ant, trail, count, exploitation, rng)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: ant
    real(real64), intent(in) :: trail(:,:)
    integer, intent(in) :: count
    real(real64), intent(in) :: exploitation
    type(random_stream_t), intent(inout) :: rng
    real(real64) :: desirability(qap%n)
    integer :: i, r, s, f

    do i = 1, count
      r = rng%integer_between(1, qap%n)
      do f = 1, qap%n
        desirability(f) = trail(r, ant%p(f)) + trail(f, ant%p(r))
      end do
      desirability(r) = -1
      if (rng%uniform() < exploitation) then
        s = maxloc(desirability, dim=1)
      else
        s = roulette(desirability, r, rng)
      end if
      call apply_swap(ant, r, s, swap_delta(qap, ant%p, r, s))
    end do
  end subroutine guided_swaps

  ! An index other than `excluded`, with probability proportional to its
  ! weight; uniform if all the weights are zero.
  integer function roulette(weight, excluded, rng) result(chosen)
    real(real64), intent(in) :: weight(:)
    integer, intent(in) :: excluded
    type(random_stream_t), intent(inout) :: rng
    real(real64) :: total, target, cumulative
    integer :: s

    chosen = merge(2, 1, excluded == 1)
    total = sum(weight, mask=weight > 0)
    if (total <= 0) then
      chosen = rng%integer_between(1, size(weight) - 1)
      if (chosen >= excluded) chosen = chosen + 1
      return
    end if
    target = rng%uniform() * total
    cumulative = 0
    do s = 1, size(weight)
      if (s == excluded .or. weight(s) <= 0) cycle
      chosen = s                      ! the last eligible index absorbs rounding
      cumulative = cumulative + weight(s)
      if (target < cumulative) return
    end do
  end function roulette

  ! --- the thesis' rules ------------------------------------------------

  subroutine thesis_reset(self, qap, best, run)
    class(thesis_rules_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: best
    type(run_t), intent(in) :: run

    if (.not. allocated(self%initial_trail)) then
      self%initial_trail = trail_from_cyclic_shifts(qap, run)
    end if
    self%trail = self%initial_trail
    if (.not. allocated(self%reinforcement)) allocate (self%reinforcement(qap%n, qap%n))
    self%reinforcement = 0
  end subroutine thesis_reset

  !> T = M1 + M2 over the 2-opt best descents from the n permutations
  !> [i, i+1, ..., n, 1, ..., i-1]: M1 counts the final assignments, M2
  !> the assignments made and broken along the way (shifted to be >= 0).
  function trail_from_cyclic_shifts(qap, run) result(trail)
    type(qap_instance_t), intent(in) :: qap
    type(run_t), intent(in) :: run
    real(real64) :: trail(qap%n, qap%n)
    type(assignment_changes_t) :: changes
    type(solution_t) :: sol
    real(real64) :: final_assignments(qap%n, qap%n)
    integer :: shift, k

    allocate (changes%m(qap%n, qap%n))
    changes%m = 0
    final_assignments = 0
    do shift = 1, qap%n
      sol = evaluated(qap, [(modulo(shift + k - 2, qap%n) + 1, k = 1, qap%n)])
      call two_opt_best(qap, sol, run, changes)
      do k = 1, qap%n
        final_assignments(k, sol%p(k)) = final_assignments(k, sol%p(k)) + 1
      end do
    end do
    trail = final_assignments + nonnegative(changes%m)
  end function trail_from_cyclic_shifts

  subroutine thesis_improve(self, qap, ant, run)
    class(thesis_rules_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: ant
    type(run_t), intent(in) :: run

    if (.not. allocated(self%last_descent%m)) allocate (self%last_descent%m(qap%n, qap%n))
    self%last_descent%m = 0
    call two_opt_first(qap, ant, run, self%last_descent)
  end subroutine thesis_improve

  subroutine thesis_note_new_best(self, ant)
    class(thesis_rules_t), intent(inout) :: self
    type(solution_t), intent(in) :: ant
    integer :: i

    self%reinforcement = nonnegative(self%last_descent%m)
    do i = 1, size(ant%p)
      self%reinforcement(i, ant%p(i)) = self%reinforcement(i, ant%p(i)) + 1
    end do
  end subroutine thesis_note_new_best

  subroutine thesis_reinforce(self, best)
    class(thesis_rules_t), intent(inout) :: self
    type(solution_t), intent(in) :: best

    self%trail = (1 - self%evaporation) * self%trail + self%reinforcement
  end subroutine thesis_reinforce

  subroutine record_swap(self, p, r, s)
    class(assignment_changes_t), intent(inout) :: self
    integer, intent(in) :: p(:), r, s

    self%m(r, p(r)) = self%m(r, p(r)) - 1
    self%m(s, p(s)) = self%m(s, p(s)) - 1
    self%m(r, p(s)) = self%m(r, p(s)) + 1
    self%m(s, p(r)) = self%m(s, p(r)) + 1
  end subroutine record_swap

  pure function nonnegative(m) result(shifted)
    real(real64), intent(in) :: m(:,:)
    real(real64) :: shifted(size(m, 1), size(m, 2))

    shifted = m - min(0.0_real64, minval(m))
  end function nonnegative

  ! --- HAS-QAP rules ----------------------------------------------------

  subroutine has_reset(self, qap, best, run)
    class(has_rules_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: best
    type(run_t), intent(in) :: run

    if (.not. allocated(self%trail)) allocate (self%trail(qap%n, qap%n))
    self%trail = 1 / (self%q_constant * cost_scale(best))
  end subroutine has_reset

  subroutine has_improve(self, qap, ant, run)
    class(has_rules_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: ant
    type(run_t), intent(in) :: run

    call two_opt_first(qap, ant, run)
  end subroutine has_improve

  subroutine has_note_new_best(self, ant)
    class(has_rules_t), intent(inout) :: self
    type(solution_t), intent(in) :: ant

  end subroutine has_note_new_best

  subroutine has_reinforce(self, best)
    class(has_rules_t), intent(inout) :: self
    type(solution_t), intent(in) :: best
    integer :: i

    self%trail = (1 - self%evaporation) * self%trail
    do i = 1, size(best%p)
      self%trail(i, best%p(i)) = self%trail(i, best%p(i)) + self%reinforcement / cost_scale(best)
    end do
  end subroutine has_reinforce

  !> z_best as a divisor; 1 when the optimum is 0 (esc16f).
  pure real(real64) function cost_scale(best)
    type(solution_t), intent(in) :: best
    cost_scale = real(max(1_int64, best%cost), real64)
  end function cost_scale

end module qap_colony
