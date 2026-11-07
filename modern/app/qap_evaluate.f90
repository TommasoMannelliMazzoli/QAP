!> Checks a QAPLIB solution file against its instance:
!>   qap_evaluate INSTANCE SOLUTION
!> A solution file holds n and the stated cost, then the permutation.
!> Prints `instance,n,stated,computed` and fails if the permutation does not
!> have the stated cost. A few QAPLIB files list the inverse permutation:
!> that is accepted, with a note.
program qap_evaluate
  use iso_fortran_env, only: int64, output_unit, error_unit
  use qap_instance, only: qap_instance_t, load_instance, cost
  use qap_solution, only: is_permutation, inverse
  implicit none

  type(qap_instance_t) :: qap
  integer, allocatable :: p(:)
  integer(int64) :: stated, computed

  if (command_argument_count() /= 2) error stop 'usage: qap_evaluate INSTANCE SOLUTION'
  qap = loaded(argument(1))
  call read_solution(argument(2), qap%n, p, stated)
  computed = cost(qap, p)
  if (computed /= stated .and. cost(qap, inverse(p)) == stated) then
    write (error_unit, '(a)') qap%name // ': the file lists the inverse permutation'
    computed = stated
  end if
  write (output_unit, '(a,",",i0,",",i0,",",i0)') qap%name, qap%n, stated, computed
  if (computed /= stated) error stop 'the permutation does not have the stated cost'

contains

  function loaded(path) result(qap)
    character(*), intent(in) :: path
    type(qap_instance_t) :: qap
    integer :: stat
    character(len=:), allocatable :: errmsg

    call load_instance(path, qap, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end function loaded

  subroutine read_solution(path, n, p, stated)
    character(*), intent(in) :: path
    integer, intent(in) :: n
    integer, allocatable, intent(out) :: p(:)
    integer(int64), intent(out) :: stated
    integer :: unit, stat, listed_n

    open (newunit=unit, file=path, status='old', action='read', iostat=stat)
    if (stat /= 0) error stop 'cannot open ' // path
    read (unit, *, iostat=stat) listed_n, stated
    if (stat /= 0 .or. listed_n /= n) error stop path // ': size differs from the instance'
    allocate (p(n))
    read (unit, *, iostat=stat) p
    if (stat /= 0 .or. .not. is_permutation(p, n)) error stop path // ': not a permutation'
    close (unit)
  end subroutine read_solution

  function argument(position) result(text)
    integer, intent(in) :: position
    character(len=:), allocatable :: text
    integer :: length

    call get_command_argument(position, length=length)
    allocate (character(len=length) :: text)
    call get_command_argument(position, text)
  end function argument

end program qap_evaluate
