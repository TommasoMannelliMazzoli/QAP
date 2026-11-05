!> Reproducible random numbers: MRG32k3a (L'Ecuyer 1999) split into
!> independent streams 2**127 draws apart (L'Ecuyer et al. 2002).
!> The arithmetic is exact in int64, so a stream yields the same numbers
!> with any compiler on any machine.
module qap_rng
  use iso_fortran_env, only: int64, real64
  implicit none
  private
  public :: random_stream_t, random_stream

  integer(int64), parameter :: M1 = 4294967087_int64, M2 = 4294944443_int64
  integer(int64), parameter :: A12 = 1403580_int64, A13N = 810728_int64
  integer(int64), parameter :: A21 = 527612_int64, A23N = 1370589_int64
  integer(int64), parameter :: DEFAULT_SEED = 12345_int64
  integer, parameter :: LOG2_STREAM_SPACING = 127
  real(real64), parameter :: NORM = 1.0_real64 / real(M1 + 1, real64)

  type :: random_stream_t
    private
    integer(int64) :: state(6) = DEFAULT_SEED
  contains
    procedure :: uniform
    procedure :: integer_between
    procedure :: permutation
    procedure :: skip
  end type random_stream_t

  ! Transition matrices of the two linear recurrences of the generator.
  type :: transition_t
    integer(int64) :: first(3,3), second(3,3)
  end type transition_t

contains

  function random_stream(stream_number) result(rng)
    integer(int64), intent(in) :: stream_number
    type(random_stream_t) :: rng
    type(transition_t) :: jump
    integer :: i

    jump = one_step()
    do i = 1, LOG2_STREAM_SPACING
      jump = squared(jump)
    end do
    call apply_power(rng%state, jump, stream_number)
  end function random_stream

  !> Uniform deviate in the open interval (0,1).
  function uniform(self) result(u)
    class(random_stream_t), intent(inout) :: self
    real(real64) :: u
    integer(int64) :: p1, p2

    associate (s => self%state)
      p1 = modulo(A12 * s(2) - A13N * s(1), M1)
      s(1:3) = [s(2), s(3), p1]
      p2 = modulo(A21 * s(6) - A23N * s(4), M2)
      s(4:6) = [s(5), s(6), p2]
    end associate
    if (p1 > p2) then
      u = real(p1 - p2, real64) * NORM
    else
      u = real(p1 - p2 + M1, real64) * NORM
    end if
  end function uniform

  !> Every integer in [lo, hi] with the same probability (the original
  !> NINT((n-1)*u+1) gave half probability to 1 and n).
  function integer_between(self, lo, hi) result(k)
    class(random_stream_t), intent(inout) :: self
    integer, intent(in) :: lo, hi
    integer :: k

    k = min(hi, lo + int(self%uniform() * real(hi - lo + 1, real64)))
  end function integer_between

  !> Uniformly random permutation of 1..size(p) (Fisher-Yates).
  subroutine permutation(self, p)
    class(random_stream_t), intent(inout) :: self
    integer, intent(out) :: p(:)
    integer :: i, j

    p = [(i, i = 1, size(p))]
    do i = size(p), 2, -1
      j = self%integer_between(1, i)
      if (j /= i) p([i, j]) = p([j, i])
    end do
  end subroutine permutation

  !> Advances the stream by `draws` numbers in O(log draws).
  subroutine skip(self, draws)
    class(random_stream_t), intent(inout) :: self
    integer(int64), intent(in) :: draws

    call apply_power(self%state, one_step(), draws)
  end subroutine skip

  pure subroutine apply_power(state, t, exponent)
    integer(int64), intent(inout) :: state(6)
    type(transition_t), intent(in) :: t
    integer(int64), intent(in) :: exponent
    type(transition_t) :: power
    integer(int64) :: remaining

    power = t
    remaining = exponent
    do while (remaining > 0)
      if (btest(remaining, 0)) then
        state(1:3) = matvec_mod(power%first, state(1:3), M1)
        state(4:6) = matvec_mod(power%second, state(4:6), M2)
      end if
      power = squared(power)
      remaining = ishft(remaining, -1)
    end do
  end subroutine apply_power

  pure function one_step() result(t)
    type(transition_t) :: t

    t%first = transpose(reshape([0_int64, 1_int64, 0_int64, &
                                 0_int64, 0_int64, 1_int64, &
                                 M1 - A13N, A12, 0_int64], [3, 3]))
    t%second = transpose(reshape([0_int64, 1_int64, 0_int64, &
                                  0_int64, 0_int64, 1_int64, &
                                  M2 - A23N, 0_int64, A21], [3, 3]))
  end function one_step

  pure function squared(t) result(t2)
    type(transition_t), intent(in) :: t
    type(transition_t) :: t2

    t2%first = matmul_mod(t%first, t%first, M1)
    t2%second = matmul_mod(t%second, t%second, M2)
  end function squared

  !> a*b mod m for 0 <= a, b < m < 2**32, without int64 overflow.
  pure function mulmod(a, b, m) result(r)
    integer(int64), intent(in) :: a, b, m
    integer(int64) :: r

    r = modulo(ishft(a, -16) * b, m)
    r = modulo(ishft(r, 16) + iand(a, 65535_int64) * b, m)
  end function mulmod

  pure function matmul_mod(x, y, m) result(c)
    integer(int64), intent(in) :: x(3,3), y(3,3), m
    integer(int64) :: c(3,3)
    integer :: j

    do j = 1, 3
      c(:,j) = matvec_mod(x, y(:,j), m)
    end do
  end function matmul_mod

  pure function matvec_mod(x, v, m) result(w)
    integer(int64), intent(in) :: x(3,3), v(3), m
    integer(int64) :: w(3)
    integer :: i, k

    w = 0
    do i = 1, 3
      do k = 1, 3
        w(i) = modulo(w(i) + mulmod(x(i,k), v(k), m), m)
      end do
    end do
  end function matvec_mod

end module qap_rng
