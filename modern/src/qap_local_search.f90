!> Local search on the swap (2-opt) and rotation (3-opt) neighbourhoods.
!>
!> First improvement applies an improving move as soon as it finds one and
!> keeps scanning, until a full pass finds none. Best improvement applies
!> the best move of the whole neighbourhood each time. Every search stops
!> early, with a consistent solution, when the run is over; an optional
!> observer is told of every swap (the thesis' ACO learns from them).
module qap_local_search
  use iso_fortran_env, only: int64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t
  use qap_moves, only: swap_delta, rotation_delta, apply_swap, apply_rotation, &
                       swap_delta_table_t, ROTATE_LEFT, ROTATE_RIGHT
  use qap_run, only: run_t
  implicit none
  private
  public :: swap_observer_t, two_opt_first, two_opt_best, three_opt_first, three_opt_best

  type, abstract :: swap_observer_t
  contains
    procedure(notify_swap), deferred :: notify
  end type swap_observer_t

  abstract interface
    !> Called before facilities r and s of p exchange their locations.
    subroutine notify_swap(self, p, r, s)
      import :: swap_observer_t
      class(swap_observer_t), intent(inout) :: self
      integer, intent(in) :: p(:), r, s
    end subroutine notify_swap
  end interface

contains

  subroutine two_opt_first(qap, sol, run, observer)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    type(run_t), intent(in) :: run
    class(swap_observer_t), intent(inout), optional :: observer
    integer :: r, s
    integer(int64) :: d
    logical :: improved

    improved = .true.
    do while (improved)
      improved = .false.
      do r = 1, qap%n - 1
        do s = r + 1, qap%n
          d = swap_delta(qap, sol%p, r, s)
          if (d < 0) then
            if (present(observer)) call observer%notify(sol%p, r, s)
            call apply_swap(sol, r, s, d)
            improved = .true.
          end if
        end do
        if (run%is_over()) return
      end do
    end do
  end subroutine two_opt_first

  !> With the delta table: O(n**2) per move instead of the original O(n**3).
  subroutine two_opt_best(qap, sol, run, observer)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    type(run_t), intent(in) :: run
    class(swap_observer_t), intent(inout), optional :: observer
    type(swap_delta_table_t) :: table
    integer :: r, s

    call table%build(qap, sol%p)
    do
      call best_swap(table, r, s)
      if (r == 0) exit
      if (present(observer)) call observer%notify(sol%p, r, s)
      call apply_swap(sol, r, s, table%delta(r,s))
      call table%update_after_swap(qap, sol%p, r, s)
      if (run%is_over()) return
    end do
  end subroutine two_opt_best

  subroutine three_opt_first(qap, sol, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    type(run_t), intent(in) :: run
    integer :: i, j, k, direction
    integer(int64) :: d
    logical :: improved

    improved = .true.
    do while (improved)
      improved = .false.
      do i = 1, qap%n - 2
        do j = i + 1, qap%n - 1
          do k = j + 1, qap%n
            do direction = ROTATE_LEFT, ROTATE_RIGHT
              d = rotation_delta(qap, sol%p, i, j, k, direction)
              if (d < 0) then
                call apply_rotation(sol, i, j, k, direction, d)
                improved = .true.
              end if
            end do
          end do
        end do
        if (run%is_over()) return
      end do
    end do
  end subroutine three_opt_first

  subroutine three_opt_best(qap, sol, run)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t), intent(inout) :: sol
    type(run_t), intent(in) :: run
    integer :: i, j, k, direction, best(4)
    integer(int64) :: d, best_delta

    do
      best_delta = 0
      best = 0
      do i = 1, qap%n - 2
        do j = i + 1, qap%n - 1
          do k = j + 1, qap%n
            do direction = ROTATE_LEFT, ROTATE_RIGHT
              d = rotation_delta(qap, sol%p, i, j, k, direction)
              if (d < best_delta) then
                best_delta = d
                best = [i, j, k, direction]
              end if
            end do
          end do
        end do
        if (run%is_over()) return
      end do
      if (best_delta >= 0) exit
      call apply_rotation(sol, best(1), best(2), best(3), best(4), best_delta)
    end do
  end subroutine three_opt_best

  !> The improving swap with the most negative delta, or r = 0.
  pure subroutine best_swap(table, r, s)
    type(swap_delta_table_t), intent(in) :: table
    integer, intent(out) :: r, s
    integer(int64) :: least
    integer :: i, j

    least = 0
    r = 0
    s = 0
    do j = 2, size(table%delta, 2)
      do i = 1, j - 1
        if (table%delta(i,j) < least) then
          least = table%delta(i,j)
          r = i
          s = j
        end if
      end do
    end do
  end subroutine best_swap

end module qap_local_search
