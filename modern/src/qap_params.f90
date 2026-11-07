!> Parameters of the algorithms, grouped by algorithm. Defaults are the
!> values of the thesis, or of the original papers for rots and has.
!> A "factor" is multiplied by n (or by n**2 where stated).
module qap_params
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: params_t, thesis_tabu_params_t, robust_tabu_params_t, &
            colony_params_t, gvns_params_t, annealing_params_t

  !> Tabu search of the thesis (Skorin-Kapov style).
  type :: thesis_tabu_params_t
    integer :: list_entries = 20               ! s of the thesis: two entries per move
    real(real64) :: k_small = -1.8_real64      ! mu = k * mean(D)
    real(real64) :: k_large = -1.4_real64
    integer :: large_from = 50                 ! n from which k_large is used
  end type thesis_tabu_params_t

  !> Robust tabu search (Taillard 1991).
  type :: robust_tabu_params_t
    real(real64) :: tenure_min_factor = 0.9_real64
    real(real64) :: tenure_max_factor = 1.1_real64
    real(real64) :: aspiration_factor = 5.0_real64   ! x n**2 iterations
  end type robust_tabu_params_t

  !> Ant colonies: the thesis' ACO and HAS-QAP share these settings.
  type :: colony_params_t
    integer :: ants = 10
    real(real64) :: swaps_factor = 0             ! 0 = use `swaps`
    integer :: swaps = 2                         ! R
    real(real64) :: exploitation = 0.85_real64   ! q
    real(real64) :: evaporation = 0.25_real64    ! alpha
    real(real64) :: reinforcement = 0            ! HAS only: alpha2
    real(real64) :: q_constant = 0               ! HAS only: tau0 = 1 / (Q * z_best)
    real(real64) :: stagnation_factor = 5.0_real64   ! S
  end type colony_params_t

  type :: gvns_params_t
    integer :: max_loops_without_improvement = 10000
  end type gvns_params_t

  type :: annealing_params_t
    real(real64) :: cooling_small = 0.9_real64   ! n <= 49, as TEMPLE
    real(real64) :: cooling_large = 0.95_real64
    real(real64) :: initial_acceptance = 0.5_real64
    real(real64) :: moves_per_temperature_factor = 1.0_real64   ! x n**2
  end type annealing_params_t

  type :: params_t
    type(thesis_tabu_params_t) :: tabu = thesis_tabu_params_t()
    type(robust_tabu_params_t) :: rots = robust_tabu_params_t()
    type(colony_params_t) :: aco = colony_params_t()
    type(colony_params_t) :: has = colony_params_t(ants=10, swaps_factor=1.0_real64/3, &
                                     swaps=0, exploitation=0.9_real64, evaporation=0.1_real64, &
                                     reinforcement=0.1_real64, q_constant=100.0_real64, &
                                     stagnation_factor=0.5_real64)
    type(gvns_params_t) :: gvns = gvns_params_t()
    type(annealing_params_t) :: annealing = annealing_params_t()
  end type params_t

end module qap_params
