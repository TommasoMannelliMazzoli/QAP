!> Command line of qap_solve.
module qap_cli
  use iso_fortran_env, only: int64, real64, error_unit
  use qap_params, only: params_t
  use qap_algorithms, only: is_known_algorithm, ALGORITHM_NAMES
  implicit none
  private
  public :: options_t, parsed_options, USAGE

  character(len=*), parameter :: USAGE = &
    'usage: qap_solve INSTANCE [--algorithm NAME] [--time SECONDS] [--runs N]' // new_line('a') // &
    '                 [--seed N] [--threads N] [--bks VALUE | --bks-file FILE]' // new_line('a') // &
    '                 [--params FILE] [--header]' // new_line('a') // &
    'algorithms: ' // ALGORITHM_NAMES // new_line('a') // &
    'run k uses random stream seed+k-1; --params reads the namelist' // new_line('a') // &
    '  &qap_params params%aco%ants = 5, params%rots%aspiration_factor = 2.0 /'

  type :: options_t
    character(len=:), allocatable :: instance, algorithm, bks_file
    real(real64) :: time_limit = 1
    integer :: runs = 1, threads = 1
    integer(int64) :: first_stream = 1
    integer(int64) :: bks = -1                   ! -1 = unknown
    logical :: header = .false.
    type(params_t) :: params
  end type options_t

contains

  function parsed_options() result(options)
    type(options_t) :: options
    character(len=:), allocatable :: flag
    integer :: i

    options%algorithm = 'rots'
    options%bks_file = ''
    i = 1
    do while (i <= command_argument_count())
      flag = argument(i)
      select case (flag)
      case ('--algorithm')
        options%algorithm = argument(i + 1)
      case ('--time')
        options%time_limit = real_argument(i + 1)
      case ('--runs')
        options%runs = int(integer_argument(i + 1))
      case ('--seed')
        options%first_stream = integer_argument(i + 1)
      case ('--threads')
        options%threads = int(integer_argument(i + 1))
      case ('--bks')
        options%bks = integer_argument(i + 1)
      case ('--bks-file')
        options%bks_file = argument(i + 1)
      case ('--params')
        options%params = params_from_namelist(argument(i + 1))
      case ('--header')
        options%header = .true.
        i = i - 1
      case ('-h', '--help')
        call fail('')
      case default
        if (flag(1:1) == '-' .or. allocated(options%instance)) call fail('unexpected argument ' // flag)
        options%instance = flag
        i = i - 1
      end select
      i = i + 2
    end do
    call validate(options)
  end function parsed_options

  subroutine validate(options)
    type(options_t), intent(in) :: options

    if (.not. allocated(options%instance)) call fail('missing INSTANCE')
    if (.not. is_known_algorithm(options%algorithm)) call fail('unknown algorithm ' // options%algorithm)
    if (options%time_limit <= 0) call fail('--time must be positive')
    if (options%runs < 1 .or. options%threads < 1) call fail('--runs and --threads must be >= 1')
    if (options%first_stream < 0) call fail('--seed must be >= 0')
  end subroutine validate

  !> The defaults, overridden by the values that the namelist file sets.
  function params_from_namelist(path) result(params)
    character(*), intent(in) :: path
    type(params_t) :: params
    integer :: unit, stat
    namelist /qap_params/ params

    params = params_t()
    open (newunit=unit, file=path, status='old', action='read', iostat=stat)
    if (stat == 0) read (unit, nml=qap_params, iostat=stat)
    if (stat /= 0) call fail('cannot read the namelist &qap_params in ' // path)
    close (unit)
  end function params_from_namelist

  function argument(i) result(value)
    integer, intent(in) :: i
    character(len=:), allocatable :: value
    integer :: length

    if (i > command_argument_count()) call fail('missing value after the last option')
    call get_command_argument(i, length=length)
    allocate (character(len=length) :: value)
    call get_command_argument(i, value)
  end function argument

  integer(int64) function integer_argument(i) result(value)
    integer, intent(in) :: i
    character(len=:), allocatable :: text
    integer :: stat

    text = argument(i)
    read (text, *, iostat=stat) value
    if (stat /= 0) call fail('not an integer: ' // text)
  end function integer_argument

  real(real64) function real_argument(i) result(value)
    integer, intent(in) :: i
    character(len=:), allocatable :: text
    integer :: stat

    text = argument(i)
    read (text, *, iostat=stat) value
    if (stat /= 0) call fail('not a number: ' // text)
  end function real_argument

  subroutine fail(message)
    character(*), intent(in) :: message

    if (len(message) > 0) write (error_unit, '(a)') 'error: ' // message
    write (error_unit, '(a)') USAGE
    error stop 2
  end subroutine fail

end module qap_cli
