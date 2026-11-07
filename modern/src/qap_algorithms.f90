!> Runs an algorithm by name.
module qap_algorithms
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t, random_solution
  use qap_greedy, only: greedy1, greedy2, greedy3
  use qap_local_search, only: two_opt_first, two_opt_best, three_opt_first, three_opt_best
  use qap_vns, only: gvns
  use qap_tabu, only: thesis_tabu_search, robust_tabu_search
  use qap_colony, only: thesis_ant_colony, hybrid_ant_system
  use qap_annealing, only: simulated_annealing
  use qap_params, only: params_t
  use qap_rng, only: random_stream_t
  use qap_run, only: run_t
  implicit none
  private
  public :: solve, is_known_algorithm, ALGORITHM_NAMES

  character(len=*), parameter :: ALGORITHM_NAMES = &
    'greedy1 greedy2 greedy3 2opt-first 2opt-best 3opt-first 3opt-best ' // &
    'gvns-first gvns-best ts rots aco has sa'

contains

  logical function is_known_algorithm(name)
    character(*), intent(in) :: name

    is_known_algorithm = index(' ' // ALGORITHM_NAMES // ' ', ' ' // trim(name) // ' ') > 0
  end function is_known_algorithm

  !> The constructive heuristics and the single descents ignore the time
  !> limit; GVNS and the thesis' tabu search start from Greedy3, as in the
  !> thesis; the other metaheuristics from random permutations.
  subroutine solve(algorithm, qap, params, rng, run)
    character(*), intent(in) :: algorithm
    type(qap_instance_t), intent(in) :: qap
    type(params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: sol

    select case (algorithm)
    case ('greedy1')
      call run%record(greedy1(qap))
    case ('greedy2')
      call run%record(greedy2(qap))
    case ('greedy3')
      call run%record(greedy3(qap))
    case ('2opt-first')
      sol = random_solution(qap, rng)
      call two_opt_first(qap, sol, run)
      call run%record(sol)
    case ('2opt-best')
      sol = random_solution(qap, rng)
      call two_opt_best(qap, sol, run)
      call run%record(sol)
    case ('3opt-first')
      sol = random_solution(qap, rng)
      call three_opt_first(qap, sol, run)
      call run%record(sol)
    case ('3opt-best')
      sol = random_solution(qap, rng)
      call three_opt_best(qap, sol, run)
      call run%record(sol)
    case ('gvns-first')
      call gvns(qap, greedy3(qap), .false., params%gvns, rng, run)
    case ('gvns-best')
      call gvns(qap, greedy3(qap), .true., params%gvns, rng, run)
    case ('ts')
      call thesis_tabu_search(qap, params%tabu, rng, run)
    case ('rots')
      call robust_tabu_search(qap, params%rots, rng, run)
    case ('aco')
      call thesis_ant_colony(qap, params%aco, rng, run)
    case ('has')
      call hybrid_ant_system(qap, params%has, rng, run)
    case ('sa')
      call simulated_annealing(qap, params%annealing, rng, run)
    case default
      error stop 'unknown algorithm: ' // algorithm
    end select
  end subroutine solve

end module qap_algorithms
