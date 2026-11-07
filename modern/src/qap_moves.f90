!> The two moves of the algorithms and their cost variations.
!>
!> swap of facilities r and s:      they exchange their locations
!> rotation of facilities i, j, k:  ROTATE_LEFT  {i,j,k} -> {j,k,i}  (original delta1)
!>                                  ROTATE_RIGHT {i,j,k} -> {k,i,j}  (original delta2)
!>
!> The O(n) loops run over every k, so that they vectorise, and remove the
!> terms of the moved facilities afterwards.
module qap_moves
  use iso_fortran_env, only: int64
  use qap_instance, only: qap_instance_t
  use qap_solution, only: solution_t
  implicit none
  private
  public :: swap_delta, rotation_delta, apply_swap, apply_rotation, rotate
  public :: swap_delta_table_t, ROTATE_LEFT, ROTATE_RIGHT

  integer, parameter :: ROTATE_LEFT = 1, ROTATE_RIGHT = 2

  !> All the swap deltas of a permutation, kept up to date in O(n**2) per
  !> swap (Taillard 1991) instead of O(n**3).
  type :: swap_delta_table_t
    integer(int64), allocatable :: delta(:,:)     ! delta(r,s), r < s
  contains
    procedure :: build
    procedure :: update_after_swap
  end type swap_delta_table_t

contains

  pure function swap_delta(qap, p, r, s) result(d)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:), r, s
    integer(int64) :: d
    integer(int64) :: sum_over_k
    integer :: k, pk, pr, ps

    pr = p(r)
    ps = p(s)
    associate (a => qap%a, b => qap%b, at => qap%at, bt => qap%bt)
      d = (a(r,r) - a(s,s)) * (b(ps,ps) - b(pr,pr))
      sum_over_k = 0
      if (qap%symmetric) then
        do k = 1, qap%n
          pk = p(k)
          sum_over_k = sum_over_k + (a(k,r) - a(k,s)) * (b(pk,ps) - b(pk,pr))
        end do
        sum_over_k = sum_over_k - (a(r,r) - a(r,s)) * (b(pr,ps) - b(pr,pr)) &
                                - (a(s,r) - a(s,s)) * (b(ps,ps) - b(ps,pr))
        d = d + 2 * sum_over_k
      else
        d = d + (a(r,s) - a(s,r)) * (b(ps,pr) - b(pr,ps))
        do k = 1, qap%n
          pk = p(k)
          sum_over_k = sum_over_k + (a(k,r) - a(k,s)) * (b(pk,ps) - b(pk,pr)) &
                                  + (at(k,r) - at(k,s)) * (bt(pk,ps) - bt(pk,pr))
        end do
        sum_over_k = sum_over_k - (a(r,r) - a(r,s)) * (b(pr,ps) - b(pr,pr)) &
                                - (at(r,r) - at(r,s)) * (bt(pr,ps) - bt(pr,pr)) &
                                - (a(s,r) - a(s,s)) * (b(ps,ps) - b(ps,pr)) &
                                - (at(s,r) - at(s,s)) * (bt(ps,ps) - bt(ps,pr))
        d = d + sum_over_k
      end if
    end associate
  end function swap_delta

  pure function rotation_delta(qap, p, i, j, k, direction) result(d)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:), i, j, k, direction
    integer(int64) :: d
    integer :: moved(3), old(3), new(3), x

    moved = [i, j, k]
    old = p(moved)
    new = old(rotation_order(direction))
    d = 0
    do x = 1, 3
      d = d + sum(qap%a(moved, moved(x)) * (qap%b(new, new(x)) - qap%b(old, old(x))))
    end do
    do x = 1, 3
      d = d + one_end_moved(qap, p, moved, old, moved(x), new(x))
    end do
  end function rotation_delta

  ! Variation of the pairs (facility, f) and (f, facility) with facility
  ! moved from its old location to new_location and f not moved.
  pure function one_end_moved(qap, p, moved, old, facility, new_location) result(d)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:), moved(3), old(3), facility, new_location
    integer(int64) :: d
    integer :: f, pf, y, old_location

    old_location = p(facility)
    d = 0
    associate (a => qap%a, b => qap%b, at => qap%at, bt => qap%bt)
      if (qap%symmetric) then
        do f = 1, qap%n
          pf = p(f)
          d = d + a(f,facility) * (b(pf,new_location) - b(pf,old_location))
        end do
        do y = 1, 3
          d = d - a(moved(y),facility) * (b(old(y),new_location) - b(old(y),old_location))
        end do
        d = 2 * d
      else
        do f = 1, qap%n
          pf = p(f)
          d = d + a(f,facility) * (b(pf,new_location) - b(pf,old_location)) &
                + at(f,facility) * (bt(pf,new_location) - bt(pf,old_location))
        end do
        do y = 1, 3
          d = d - a(moved(y),facility) * (b(old(y),new_location) - b(old(y),old_location)) &
                - at(moved(y),facility) * (bt(old(y),new_location) - bt(old(y),old_location))
        end do
      end if
    end associate
  end function one_end_moved

  !> Which old location each of i, j, k receives.
  pure function rotation_order(direction) result(order)
    integer, intent(in) :: direction
    integer :: order(3)

    if (direction == ROTATE_LEFT) then
      order = [2, 3, 1]
    else
      order = [3, 1, 2]
    end if
  end function rotation_order

  pure subroutine rotate(p, i, j, k, direction)
    integer, intent(inout) :: p(:)
    integer, intent(in) :: i, j, k, direction
    integer :: moved(3), old(3)

    moved = [i, j, k]
    old = p(moved)
    p(moved) = old(rotation_order(direction))
  end subroutine rotate

  pure subroutine apply_swap(sol, r, s, delta)
    type(solution_t), intent(inout) :: sol
    integer, intent(in) :: r, s
    integer(int64), intent(in) :: delta

    sol%p([r, s]) = sol%p([s, r])
    sol%cost = sol%cost + delta
  end subroutine apply_swap

  pure subroutine apply_rotation(sol, i, j, k, direction, delta)
    type(solution_t), intent(inout) :: sol
    integer, intent(in) :: i, j, k, direction
    integer(int64), intent(in) :: delta

    call rotate(sol%p, i, j, k, direction)
    sol%cost = sol%cost + delta
  end subroutine apply_rotation

  pure subroutine build(self, qap, p)
    class(swap_delta_table_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:)
    integer :: r, s

    if (.not. allocated(self%delta)) allocate (self%delta(qap%n, qap%n))
    do s = 2, qap%n
      do r = 1, s - 1
        self%delta(r,s) = swap_delta(qap, p, r, s)
      end do
    end do
  end subroutine build

  !> p is the permutation after the swap of u and v. The pairs disjoint
  !> from {u, v} take Taillard's O(1) correction; the 2n-3 pairs touching
  !> u or v are recomputed. Kept as one flat loop: it is the hot spot of
  !> the tabu searches.
  pure subroutine update_after_swap(self, qap, p, u, v)
    class(swap_delta_table_t), intent(inout) :: self
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:), u, v
    integer :: r, s, pr, ps, pu, pv, f

    pu = p(u)
    pv = p(v)
    associate (a => qap%a, b => qap%b, at => qap%at, bt => qap%bt, delta => self%delta)
      if (qap%symmetric) then
        do s = 2, qap%n
          ps = p(s)
          do r = 1, s - 1
            pr = p(r)
            delta(r,s) = delta(r,s) + 2 * (a(r,u) - a(r,v) + a(s,v) - a(s,u)) &
                                        * (b(ps,pu) - b(ps,pv) + b(pr,pv) - b(pr,pu))
          end do
        end do
      else
        do s = 2, qap%n
          ps = p(s)
          do r = 1, s - 1
            pr = p(r)
            delta(r,s) = delta(r,s) &
              + (a(r,u) - a(r,v) + a(s,v) - a(s,u)) * (b(ps,pu) - b(ps,pv) + b(pr,pv) - b(pr,pu)) &
              + (at(r,u) - at(r,v) + at(s,v) - at(s,u)) * (bt(ps,pu) - bt(ps,pv) + bt(pr,pv) - bt(pr,pu))
          end do
        end do
      end if
      do f = 1, qap%n
        if (f /= u) delta(min(f,u), max(f,u)) = swap_delta(qap, p, min(f,u), max(f,u))
        if (f /= u .and. f /= v) delta(min(f,v), max(f,v)) = swap_delta(qap, p, min(f,v), max(f,v))
      end do
    end associate
  end subroutine update_after_swap

end module qap_moves
