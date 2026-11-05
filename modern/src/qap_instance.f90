!> A QAP instance and its objective function
!>     z(p) = sum_{i,j} a(i,j) * b(p(i), p(j)),
!> where p(i) is the location of facility i. Costs are exact int64.
module qap_instance
  use iso_fortran_env, only: int64, error_unit
  implicit none
  private
  public :: qap_instance_t, new_instance, load_instance, cost, assignment_cost

  integer, parameter :: LARGEST_SUPPORTED_N = 100000

  type :: qap_instance_t
    character(len=:), allocatable :: name
    integer :: n = 0
    integer(int64), allocatable :: a(:,:), b(:,:)    ! flows, distances
    integer(int64), allocatable :: at(:,:), bt(:,:)  ! transposes: the delta loops read columns
    logical :: symmetric = .false.
  end type qap_instance_t

contains

  function new_instance(name, a, b) result(qap)
    character(*), intent(in) :: name
    integer(int64), intent(in) :: a(:,:), b(:,:)
    type(qap_instance_t) :: qap

    qap%name = name
    qap%n = size(a, 1)
    allocate (qap%a, source=a)
    allocate (qap%b, source=b)
    allocate (qap%at, source=transpose(a))
    allocate (qap%bt, source=transpose(b))
    qap%symmetric = all(a == qap%at) .and. all(b == qap%bt)
  end function new_instance

  !> Reads a QAPLIB file ("n A B") or a file of the original repository
  !> ("sim n A B"; sim is ignored, symmetry is computed from the data).
  subroutine load_instance(path, qap, stat, errmsg)
    character(*), intent(in) :: path
    type(qap_instance_t), intent(out) :: qap
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    integer(int64), allocatable :: values(:)
    integer :: n, header_length

    errmsg = ''
    call read_integers(path, values, stat, errmsg)
    if (stat /= 0) return
    call locate_matrices(values, path, n, header_length, stat, errmsg)
    if (stat /= 0) return
    associate (first => header_length + 1)
      qap = new_instance(basename(path), &
                         matrix_from_rows(values(first : first + n * n - 1), n), &
                         matrix_from_rows(values(first + n * n : first + 2 * n * n - 1), n))
    end associate
  end subroutine load_instance

  pure function cost(qap, p) result(z)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:)
    integer(int64) :: z

    z = assignment_cost(qap%a, qap%b, p)
  end function cost

  !> sum_{i,j} x(i,j) * y(q(i), q(j)); with x = b, y = a and q = p**(-1)
  !> it gives the same value as with x = a, y = b and p.
  pure function assignment_cost(x, y, q) result(z)
    integer(int64), intent(in) :: x(:,:), y(:,:)
    integer, intent(in) :: q(:)
    integer(int64) :: z
    integer :: i, j

    z = 0
    do j = 1, size(q)
      do i = 1, size(q)
        z = z + x(i,j) * y(q(i), q(j))
      end do
    end do
  end function assignment_cost

  ! The data must be two n x n matrices after a header of one value (n) or
  ! two (sim, n). A header inconsistent with the data (esc32f.dat of the
  ! original repository declares n = 132) is overridden, with a warning, by
  ! the size the data imply.
  subroutine locate_matrices(values, path, n, header_length, stat, errmsg)
    integer(int64), intent(in) :: values(:)
    character(*), intent(in) :: path
    integer, intent(out) :: n, header_length, stat
    character(len=:), allocatable, intent(out) :: errmsg

    stat = 0
    do header_length = 1, 2
      if (declares_its_size(values, header_length)) then
        n = int(values(header_length))
        return
      end if
    end do
    do header_length = 2, 1, -1
      n = size_implied_by(size(values) - header_length)
      if (n > 0) then
        write (error_unit, '(a,i0,a)') 'warning: ' // path // &
          ': the header does not match the data, using n = ', n, ' implied by the data'
        return
      end if
    end do
    stat = 1
    errmsg = path // ': not a QAP instance ("n A B" or "sim n A B")'
  end subroutine locate_matrices

  pure logical function declares_its_size(values, header_length)
    integer(int64), intent(in) :: values(:)
    integer, intent(in) :: header_length

    declares_its_size = .false.
    if (size(values) < header_length) return
    associate (n => values(header_length))
      if (n < 2 .or. n > LARGEST_SUPPORTED_N) return
      declares_its_size = size(values) == header_length + 2 * n * n
    end associate
  end function declares_its_size

  !> n such that 2*n**2 == count, or 0.
  pure integer function size_implied_by(count) result(n)
    integer, intent(in) :: count

    n = nint(sqrt(real(max(count, 0)) / 2))
    if (n < 2 .or. 2 * n * n /= count) n = 0
  end function size_implied_by

  !> The files store the matrices row by row.
  pure function matrix_from_rows(values, n) result(m)
    integer(int64), intent(in) :: values(:)
    integer, intent(in) :: n
    integer(int64) :: m(n,n)

    m = transpose(reshape(values, [n, n]))
  end function matrix_from_rows

  subroutine read_integers(path, values, stat, errmsg)
    character(*), intent(in) :: path
    integer(int64), allocatable, intent(out) :: values(:)
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    character(len=:), allocatable :: text

    errmsg = ''
    allocate (values(0))
    call read_text(path, text, stat)
    if (stat /= 0) then
      errmsg = path // ': cannot read the file'
      return
    end if
    call parse_integers(text, values, stat)
    if (stat /= 0) errmsg = path // ': the data contain a token that is not an integer'
  end subroutine read_integers

  subroutine read_text(path, text, stat)
    character(*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out) :: stat
    integer :: unit, length

    open (newunit=unit, file=path, access='stream', form='unformatted', &
          status='old', action='read', iostat=stat)
    if (stat /= 0) return
    inquire (unit=unit, size=length)
    allocate (character(len=length) :: text)
    read (unit, iostat=stat) text
    close (unit)
  end subroutine read_text

  pure subroutine parse_integers(text, values, stat)
    character(*), intent(in) :: text
    integer(int64), allocatable, intent(out) :: values(:)
    integer, intent(out) :: stat
    integer(int64), allocatable :: found(:)
    integer :: count, pos

    allocate (found(len(text) / 2 + 1))
    count = 0
    pos = 1
    stat = 0
    do
      call skip_blanks(text, pos)
      if (pos > len(text)) exit
      count = count + 1
      call parse_integer_at(text, pos, found(count), stat)
      if (stat /= 0) return
    end do
    values = found(1:count)
  end subroutine parse_integers

  ! Parses an optionally signed integer starting at text(pos:), which must
  ! end at a blank or at the end of the text; pos moves past it.
  pure subroutine parse_integer_at(text, pos, value, stat)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos
    integer(int64), intent(out) :: value
    integer, intent(out) :: stat
    logical :: negative
    integer :: digits

    negative = text(pos:pos) == '-'
    if (negative .or. text(pos:pos) == '+') pos = pos + 1
    value = 0
    digits = 0
    do while (pos <= len(text))
      if (.not. is_digit(text(pos:pos))) exit
      value = 10 * value + (iachar(text(pos:pos)) - iachar('0'))
      digits = digits + 1
      pos = pos + 1
    end do
    if (negative) value = -value
    stat = 0
    if (digits == 0) stat = 1
    if (pos <= len(text)) then
      if (.not. is_blank(text(pos:pos))) stat = 1
    end if
  end subroutine parse_integer_at

  pure subroutine skip_blanks(text, pos)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos

    do while (pos <= len(text))
      if (.not. is_blank(text(pos:pos))) exit
      pos = pos + 1
    end do
  end subroutine skip_blanks

  pure logical function is_blank(c)
    character, intent(in) :: c
    is_blank = c == ' ' .or. (iachar(c) >= 9 .and. iachar(c) <= 13)
  end function is_blank

  pure logical function is_digit(c)
    character, intent(in) :: c
    is_digit = c >= '0' .and. c <= '9'
  end function is_digit

  pure function basename(path) result(name)
    character(*), intent(in) :: path
    character(len=:), allocatable :: name
    integer :: dir_end, ext_start

    dir_end = scan(path, '/\', back=.true.)
    ext_start = index(path, '.', back=.true.)
    if (ext_start <= dir_end) ext_start = len(path) + 1
    name = path(dir_end + 1 : ext_start - 1)
  end function basename

end module qap_instance
