!> Constructive heuristics of the thesis (chapter 3). Each one tries every
!> starting pair (facility, location) and returns the best permutation;
!> ties go to the lowest index, as MAXLOC/MINLOC did in the original code.
module qap_greedy
  use iso_fortran_env, only: int64, real64
  use qap_instance, only: qap_instance_t, assignment_cost
  use qap_solution, only: solution_t, inverse
  implicit none
  private
  public :: greedy1, greedy2, greedy3

  integer, parameter :: LARGEST = 1, SMALLEST = -1

  !> The order in which a greedy visits facilities (by flow) or locations
  !> (by distance): next = the free unit with the most extreme entry in
  !> the row or column of the current one.
  type :: unit_order_t
    real(real64), allocatable :: m(:,:), mt(:,:)
    integer :: direction = LARGEST
  contains
    procedure :: next
  end type unit_order_t

contains

  !> Greedy1: facilities in order of maximum flow with the previous one;
  !> each goes to the free location that adds the least cost. O(n**5).
  function greedy1(qap) result(best)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t) :: best

    best = cheapest_insertion_from_every_start(qap%a, qap%b, LARGEST)
  end function greedy1

  !> Greedy2: Greedy1 with the roles exchanged. Locations in order of
  !> minimum distance, each receiving the free facility that adds the least
  !> cost (argmin, as in the thesis; the original code took the argmax).
  function greedy2(qap) result(best)
    type(qap_instance_t), intent(in) :: qap
    type(solution_t) :: best
    type(solution_t) :: by_location

    by_location = cheapest_insertion_from_every_start(qap%b, qap%a, SMALLEST)
    best%p = inverse(by_location%p)
    best%cost = by_location%cost
  end function greedy2

  !> Greedy3: facilities in order of maximum flow and locations in order of
  !> minimum distance, paired in that order. O(n**4). `distances` replaces
  !> b when choosing the locations (the thesis' tabu search restarts with a
  !> modified matrix); the returned cost is always the true one.
  function greedy3(qap, distances) result(best)
    type(qap_instance_t), intent(in) :: qap
    real(real64), intent(in), optional :: distances(:,:)
    type(solution_t) :: best
    type(unit_order_t) :: facilities, locations
    integer :: first_facility, first_location

    facilities = unit_order(real(qap%a, real64), LARGEST)
    if (present(distances)) then
      locations = unit_order(distances, SMALLEST)
    else
      locations = unit_order(real(qap%b, real64), SMALLEST)
    end if
    do first_facility = 1, qap%n
      do first_location = 1, qap%n
        call keep_better(best, qap%a, qap%b, &
                         paired_orders(facilities, locations, first_facility, first_location))
      end do
    end do
  end function greedy3

  ! Greedy1 when x = flows, y = distances (q maps facilities to locations);
  ! Greedy2 when x = distances, y = flows (q maps locations to facilities).
  function cheapest_insertion_from_every_start(x, y, direction) result(best)
    integer(int64), intent(in) :: x(:,:), y(:,:)
    integer, intent(in) :: direction
    type(solution_t) :: best
    type(unit_order_t) :: units
    integer :: first_unit, first_partner

    units = unit_order(real(x, real64), direction)
    do first_unit = 1, size(x, 1)
      do first_partner = 1, size(x, 1)
        call keep_better(best, x, y, &
                         cheapest_insertion(x, y, units, first_unit, first_partner))
      end do
    end do
  end function cheapest_insertion_from_every_start

  function cheapest_insertion(x, y, units, first_unit, first_partner) result(q)
    integer(int64), intent(in) :: x(:,:), y(:,:)
    type(unit_order_t), intent(in) :: units
    integer, intent(in) :: first_unit, first_partner
    integer :: q(size(x, 1))
    integer :: placed_units(size(x, 1)), placed_partners(size(x, 1)), step, unit, partner
    logical :: free_unit(size(x, 1)), free_partner(size(x, 1))

    free_unit = .true.
    free_partner = .true.
    call place(1, first_unit, first_partner)
    do step = 2, size(x, 1)
      unit = units%next(placed_units(step - 1), free_unit)
      partner = cheapest_partner(x, y, unit, placed_units(1 : step - 1), &
                                 placed_partners(1 : step - 1), free_partner)
      call place(step, unit, partner)
    end do

  contains

    subroutine place(step, unit, partner)
      integer, intent(in) :: step, unit, partner

      q(unit) = partner
      placed_units(step) = unit
      placed_partners(step) = partner
      free_unit(unit) = .false.
      free_partner(partner) = .false.
    end subroutine place

  end function cheapest_insertion

  ! The free partner c minimising the cost added by q(unit) = c:
  !   sum_t x(unit, u_t) * y(c, c_t) + x(u_t, unit) * y(c_t, c).
  pure integer function cheapest_partner(x, y, unit, placed_units, placed_partners, free) result(best)
    integer(int64), intent(in) :: x(:,:), y(:,:)
    integer, intent(in) :: unit, placed_units(:), placed_partners(:)
    logical, intent(in) :: free(:)
    integer(int64) :: added, least
    integer :: c

    least = huge(1_int64)
    best = 0
    do c = 1, size(free)
      if (.not. free(c)) cycle
      added = sum(x(unit, placed_units) * y(c, placed_partners) &
                + x(placed_units, unit) * y(placed_partners, c))
      if (added < least) then
        least = added
        best = c
      end if
    end do
  end function cheapest_partner

  function paired_orders(facilities, locations, first_facility, first_location) result(p)
    type(unit_order_t), intent(in) :: facilities, locations
    integer, intent(in) :: first_facility, first_location
    integer :: p(size(facilities%m, 1))
    logical :: free_facility(size(p)), free_location(size(p))
    integer :: step, f, l

    free_facility = .true.
    free_location = .true.
    f = first_facility
    l = first_location
    call pair(f, l)
    do step = 2, size(p)
      f = facilities%next(f, free_facility)
      l = locations%next(l, free_location)
      call pair(f, l)
    end do

  contains

    subroutine pair(f, l)
      integer, intent(in) :: f, l

      p(f) = l
      free_facility(f) = .false.
      free_location(l) = .false.
    end subroutine pair

  end function paired_orders

  function unit_order(m, direction) result(order)
    real(real64), intent(in) :: m(:,:)
    integer, intent(in) :: direction
    type(unit_order_t) :: order

    order%m = m
    order%mt = transpose(m)
    order%direction = direction
  end function unit_order

  ! The row of `current` wins only if strictly more extreme than its
  ! column, as the MAXVAL/MINVAL comparison of the original code.
  pure integer function next(self, current, free)
    class(unit_order_t), intent(in) :: self
    integer, intent(in) :: current
    logical, intent(in) :: free(:)
    real(real64) :: sign, row_best, column_best
    integer :: k, row_k, column_k

    sign = real(self%direction, real64)
    row_best = -huge(1.0_real64)
    column_best = -huge(1.0_real64)
    row_k = 0
    column_k = 0
    do k = 1, size(free)
      if (.not. free(k)) cycle
      if (sign * self%mt(k, current) > row_best) then
        row_best = sign * self%mt(k, current)
        row_k = k
      end if
      if (sign * self%m(k, current) > column_best) then
        column_best = sign * self%m(k, current)
        column_k = k
      end if
    end do
    next = merge(row_k, column_k, row_best > column_best)
  end function next

  subroutine keep_better(best, x, y, q)
    type(solution_t), intent(inout) :: best
    integer(int64), intent(in) :: x(:,:), y(:,:)
    integer, intent(in) :: q(:)
    integer(int64) :: z

    z = assignment_cost(x, y, q)
    if (z < best%cost) then
      best%p = q
      best%cost = z
    end if
  end subroutine keep_better

end module qap_greedy
