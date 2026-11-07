!> Tabu searches.
!>
!> thesis_tabu_search: the algorithm of the thesis (chapter 4): a descent
!>   that only takes improving admissible swaps, then a restart by Greedy3
!>   on the distances modified by the long-term memory. The defects of the
!>   original code are fixed:
!>   * the current cost is tracked exactly (the original added the previous
!>     best delta, so the reported values were not the costs found);
!>   * the tabu list stores facility pairs and is checked against facility
!>     pairs (the original stored locations and compared positions);
!>   * the long-term memory counts swapped LOCATION pairs, the quantity the
!>     thesis adds to the distances, and the restart really uses the
!>     modified distances (the original ignored them: every restart gave
!>     the same permutation).
!>
!> robust_tabu_search: Taillard (1991). The best admissible swap at every
!>   iteration, also when it worsens; a swap is tabu if both facilities
!>   would go back to locations they left recently; random tenure in
!>   [0.9n, 1.1n] redrawn every 2*tmax iterations; aspiration by the best
!>   cost and by default after aspiration_factor * n**2 iterations.
module qap_tabu
  use iso_fortran_env, only: int64, real64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t, evaluated, random_solution
  use qap_moves, only: apply_swap, swap_delta_table_t
  use qap_greedy, only: greedy3
  use qap_params, only: thesis_tabu_params_t, robust_tabu_params_t
  use qap_rng, only: random_stream_t
  use qap_run, only: run_t
  implicit none
  private
  public :: thesis_tabu_search, robust_tabu_search

  !> Thesis' short-term memory: the last swapped facility pairs (FIFO).
  type :: tabu_list_t
    integer, allocatable :: first(:), second(:)
    integer :: newest = 0
  contains
    procedure :: forbids
    procedure :: push
  end type tabu_list_t

  !> Thesis' long-term memory: how often each pair of locations was swapped,
  !> and the distances it modifies for the restarts.
  type :: long_term_memory_t
    real(real64), allocatable :: frequency(:,:), modified_distances(:,:)
    real(real64) :: penalty = 0
    integer, allocatable :: last_start(:)
  contains
    procedure :: remember_swap
    procedure :: restart
  end type long_term_memory_t

  !> roTS memory: the iteration until which each facility may not return to
  !> each location.
  type :: recency_t
    integer(int64), allocatable :: forbidden_until(:,:)
    integer(int64) :: now = 0
    integer(int64) :: aspiration_delay = 0
  contains
    procedure :: allows
    procedure :: has_long_forgotten
    procedure :: forbid_return
  end type recency_t

  type :: tenure_t
    integer :: shortest, longest, current
  contains
    procedure :: refresh
  end type tenure_t

contains

  subroutine thesis_tabu_search(qap, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(thesis_tabu_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: current
    type(tabu_list_t) :: tabu
    type(long_term_memory_t) :: memory

    current = greedy3(qap)
    call run%record(current)
    tabu = new_tabu_list(max(1, params%list_entries / 2))
    memory = new_long_term_memory(qap, penalty(qap, params))
    memory%last_start = current%p
    do while (.not. run%is_over())
      call tabu_restricted_descent(qap, current, tabu, memory, run)
      if (run%is_over()) exit
      current = memory%restart(qap, rng)
      call run%record(current)
    end do
  end subroutine thesis_tabu_search

  subroutine tabu_restricted_descent(qap, sol, tabu, memory, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    type(tabu_list_t), intent(inout) :: tabu
    type(long_term_memory_t), intent(inout) :: memory
    type(run_t), intent(inout) :: run
    type(swap_delta_table_t) :: table
    integer :: r, s

    call table%build(qap, sol%p)
    do
      call best_admissible_improving_swap(table, tabu, sol%cost, run%best%cost, r, s)
      if (r == 0) return
      call memory%remember_swap(sol%p, r, s)
      call apply_swap(sol, r, s, table%delta(r,s))
      call table%update_after_swap(qap, sol%p, r, s)
      call tabu%push(r, s)
      call run%record(sol)
      call run%count_iteration()
      if (run%is_over()) return
    end do
  end subroutine tabu_restricted_descent

  !> Admissible: not tabu, or leading below the best cost (aspiration).
  pure subroutine best_admissible_improving_swap(table, tabu, current_cost, best_cost, r, s)
    type(swap_delta_table_t), intent(in) :: table
    type(tabu_list_t), intent(in) :: tabu
    integer(int64), intent(in) :: current_cost, best_cost
    integer, intent(out) :: r, s
    integer(int64) :: least, d
    integer :: i, j

    least = 0
    r = 0
    s = 0
    do j = 2, size(table%delta, 2)
      do i = 1, j - 1
        d = table%delta(i,j)
        if (d >= least) cycle
        if (tabu%forbids(i, j) .and. current_cost + d >= best_cost) cycle
        least = d
        r = i
        s = j
      end do
    end do
  end subroutine best_admissible_improving_swap

  real(real64) function penalty(qap, params)
    type(qap_instance_t), intent(in) :: qap
    type(thesis_tabu_params_t), intent(in) :: params
    real(real64) :: k

    k = merge(params%k_large, params%k_small, qap%n >= params%large_from)
    penalty = k * sum(real(qap%b, real64)) / real(qap%n, real64)**2
  end function penalty

  function new_tabu_list(capacity) result(tabu)
    integer, intent(in) :: capacity
    type(tabu_list_t) :: tabu

    allocate (tabu%first(capacity), tabu%second(capacity))
    tabu%first = 0
    tabu%second = 0
  end function new_tabu_list

  pure logical function forbids(self, r, s)
    class(tabu_list_t), intent(in) :: self
    integer, intent(in) :: r, s

    forbids = any((self%first == r .and. self%second == s) .or. &
                  (self%first == s .and. self%second == r))
  end function forbids

  !> Replaces the oldest pair when the list is full.
  pure subroutine push(self, r, s)
    class(tabu_list_t), intent(inout) :: self
    integer, intent(in) :: r, s

    self%newest = modulo(self%newest, size(self%first)) + 1
    self%first(self%newest) = r
    self%second(self%newest) = s
  end subroutine push

  function new_long_term_memory(qap, penalty) result(memory)
    type(qap_instance_t), intent(in) :: qap
    real(real64), intent(in) :: penalty
    type(long_term_memory_t) :: memory

    allocate (memory%frequency(qap%n, qap%n))
    memory%frequency = 0
    memory%modified_distances = real(qap%b, real64)
    memory%penalty = penalty
  end function new_long_term_memory

  pure subroutine remember_swap(self, p, r, s)
    class(long_term_memory_t), intent(inout) :: self
    integer, intent(in) :: p(:), r, s

    self%frequency(p(r), p(s)) = self%frequency(p(r), p(s)) + 1
    self%frequency(p(s), p(r)) = self%frequency(p(s), p(r)) + 1
  end subroutine remember_swap

  !> D~ <- D~ + mu*M as in the thesis, then Greedy3 on D~. If the memory
  !> did not change the restart, the same descent would repeat until the
  !> end of the run: the restart is then perturbed (not in the thesis).
  function restart(self, qap, rng) result(start)
    class(long_term_memory_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    type(random_stream_t), intent(inout) :: rng
    type(solution_t) :: start

    self%modified_distances = self%modified_distances + self%penalty * self%frequency
    start = greedy3(qap, self%modified_distances)
    if (all(start%p == self%last_start)) start = perturbed(qap, start, rng)
    self%last_start = start%p
  end function restart

  function perturbed(qap, sol, rng) result(moved)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(in) :: sol
    type(random_stream_t), intent(inout) :: rng
    type(solution_t) :: moved
    integer :: p(qap%n), i, r, s

    p = sol%p
    do i = 1, max(1, qap%n / 4)
      r = rng%integer_between(1, qap%n)
      s = rng%integer_between(1, qap%n)
      if (r /= s) p([r, s]) = p([s, r])
    end do
    moved = evaluated(qap, p)
  end function perturbed

  subroutine robust_tabu_search(qap, params, rng, run)
    type(qap_instance_t), intent(in) :: qap
    type(robust_tabu_params_t), intent(in) :: params
    type(random_stream_t), intent(inout) :: rng
    type(run_t), intent(inout) :: run
    type(solution_t) :: current
    type(swap_delta_table_t) :: table
    type(recency_t) :: recency
    type(tenure_t) :: tenure
    integer :: r, s

    current = random_solution(qap, rng)
    call run%record(current)
    call table%build(qap, current%p)
    recency = new_recency(qap%n, params)
    tenure = new_tenure(qap%n, params)
    do while (.not. run%is_over())
      recency%now = recency%now + 1
      call tenure%refresh(recency%now, rng)
      call choose_robust_move(table, recency, current, run%best%cost, r, s)
      call recency%forbid_return(current%p, r, s, tenure%current)
      call apply_swap(current, r, s, table%delta(r,s))
      call table%update_after_swap(qap, current%p, r, s)
      call run%record(current)
      call run%count_iteration()
    end do
  end subroutine robust_tabu_search

  !> Taillard's rule: an aspired swap beats any other; among swaps of the
  !> same kind the smaller delta wins; a non-aspired swap must be allowed.
  !> If every swap is tabu, the one with the smallest delta is taken.
  !> One flat loop over the n(n-1)/2 swaps: the hot spot of roTS.
  pure subroutine choose_robust_move(table, recency, current, best_cost, r, s)
    type(swap_delta_table_t), intent(in) :: table
    type(recency_t), intent(in) :: recency
    type(solution_t), intent(in) :: current
    integer(int64), intent(in) :: best_cost
    integer, intent(out) :: r, s
    integer(int64) :: d, chosen_delta, least_delta
    integer :: i, j, least_i, least_j
    logical :: aspired, chosen_aspired

    r = 0
    s = 0
    least_i = 1
    least_j = 2
    chosen_delta = huge(1_int64)
    least_delta = huge(1_int64)
    chosen_aspired = .false.
    do j = 2, size(table%delta, 2)
      do i = 1, j - 1
        d = table%delta(i,j)
        if (d < least_delta) then
          least_delta = d
          least_i = i
          least_j = j
        end if
        aspired = current%cost + d < best_cost .or. recency%has_long_forgotten(current%p, i, j)
        if (aspired .and. .not. chosen_aspired) then
          chosen_aspired = .true.
        else if (aspired .neqv. chosen_aspired) then
          cycle
        else if (d >= chosen_delta) then
          cycle
        else if (.not. aspired .and. .not. recency%allows(current%p, i, j)) then
          cycle
        end if
        chosen_delta = d
        r = i
        s = j
      end do
    end do
    if (r == 0) then
      r = least_i
      s = least_j
    end if
  end subroutine choose_robust_move

  function new_recency(n, params) result(recency)
    integer, intent(in) :: n
    type(robust_tabu_params_t), intent(in) :: params
    type(recency_t) :: recency
    integer :: i, l

    ! as in Taillard's code: distinct, slightly negative "departure times"
    allocate (recency%forbidden_until(n, n))
    do l = 1, n
      do i = 1, n
        recency%forbidden_until(i, l) = -(int(n, int64) * (i - 1) + l)
      end do
    end do
    recency%aspiration_delay = nint(params%aspiration_factor * real(n, real64)**2, int64)
  end function new_recency

  !> The swap is allowed if at least one of the two facilities may go to
  !> its new location.
  pure logical function allows(self, p, r, s)
    class(recency_t), intent(in) :: self
    integer, intent(in) :: p(:), r, s

    allows = self%forbidden_until(r, p(s)) < self%now .or. &
             self%forbidden_until(s, p(r)) < self%now
  end function allows

  pure logical function has_long_forgotten(self, p, r, s)
    class(recency_t), intent(in) :: self
    integer, intent(in) :: p(:), r, s

    has_long_forgotten = self%forbidden_until(r, p(s)) < self%now - self%aspiration_delay .or. &
                         self%forbidden_until(s, p(r)) < self%now - self%aspiration_delay
  end function has_long_forgotten

  !> Before the swap: r and s may not go back to their current locations.
  pure subroutine forbid_return(self, p, r, s, tenure)
    class(recency_t), intent(inout) :: self
    integer, intent(in) :: p(:), r, s, tenure

    self%forbidden_until(r, p(r)) = self%now + tenure
    self%forbidden_until(s, p(s)) = self%now + tenure
  end subroutine forbid_return

  function new_tenure(n, params) result(tenure)
    integer, intent(in) :: n
    type(robust_tabu_params_t), intent(in) :: params
    type(tenure_t) :: tenure

    tenure%shortest = max(1, int(params%tenure_min_factor * n))
    tenure%longest = max(tenure%shortest, ceiling(params%tenure_max_factor * n))
    tenure%current = tenure%shortest
  end function new_tenure

  subroutine refresh(self, iteration, rng)
    class(tenure_t), intent(inout) :: self
    integer(int64), intent(in) :: iteration
    type(random_stream_t), intent(inout) :: rng

    if (mod(iteration - 1, int(2 * self%longest, int64)) == 0) then
      self%current = rng%integer_between(self%shortest, self%longest)
    end if
  end subroutine refresh

end module qap_tabu
