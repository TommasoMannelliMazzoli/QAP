!> Runs an algorithm on an instance and prints one CSV row per run:
!>   instance,n,algorithm,run,stream,cost,bks,gap_percent,time_to_best,elapsed,iterations
!> Every reported cost is checked against the cost of the reported
!> permutation; the program fails if they differ.
program qap_solve
  use iso_fortran_env, only: int64, real64, output_unit, error_unit
  use qap_cli, only: options_t, parsed_options
  use qap_instance, only: qap_instance_t, load_instance, cost
  use qap_solution, only: is_permutation
  use qap_algorithms, only: solve
  use qap_rng, only: random_stream_t, random_stream
  use qap_run, only: run_t, new_run
  implicit none

  type(options_t) :: options
  type(qap_instance_t) :: qap
  type(run_t), allocatable :: runs(:)
  integer(int64) :: bks

  options = parsed_options()
  qap = loaded(options%instance)
  bks = best_known(options, qap%name)
  runs = executed_runs(qap, options)
  call print_rows(qap, options, bks, runs)
  call print_summary(runs, bks)
  if (.not. all_verified(qap, runs)) error stop 'a reported cost is not the cost of its permutation'

contains

  function loaded(path) result(qap)
    character(*), intent(in) :: path
    type(qap_instance_t) :: qap
    integer :: stat
    character(len=:), allocatable :: errmsg

    call load_instance(path, qap, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end function loaded

  !> Independent runs, in parallel when compiled with OpenMP: run k uses the
  !> random stream first_stream + k - 1, so results do not depend on the
  !> number of threads (as long as threads <= physical cores, since the
  !> budget is wall-clock time).
  function executed_runs(qap, options) result(runs)
    type(qap_instance_t), intent(in) :: qap
    type(options_t), intent(in) :: options
    type(run_t) :: runs(options%runs)
    integer :: k

    !$omp parallel do schedule(dynamic) num_threads(options%threads)
    do k = 1, options%runs
      runs(k) = executed_run(qap, options, options%first_stream + k - 1)
    end do
    !$omp end parallel do
  end function executed_runs

  function executed_run(qap, options, stream) result(run)
    type(qap_instance_t), intent(in) :: qap
    type(options_t), intent(in) :: options
    integer(int64), intent(in) :: stream
    type(run_t) :: run
    type(random_stream_t) :: rng

    rng = random_stream(stream)
    run = new_run(options%time_limit)
    call solve(options%algorithm, qap, options%params, rng, run)
  end function executed_run

  !> --bks, else the value listed for `name` in --bks-file, else -1.
  integer(int64) function best_known(options, name) result(bks)
    type(options_t), intent(in) :: options
    character(*), intent(in) :: name
    character(len=256) :: listed_name
    integer(int64) :: listed_value
    integer :: unit, stat

    bks = options%bks
    if (bks >= 0 .or. len(options%bks_file) == 0) return
    open (newunit=unit, file=options%bks_file, status='old', action='read', iostat=stat)
    if (stat /= 0) error stop 'cannot open ' // options%bks_file
    do
      read (unit, *, iostat=stat) listed_name, listed_value
      if (stat /= 0) exit
      if (lowercase(trim(listed_name)) == lowercase(name)) bks = listed_value
    end do
    close (unit)
  end function best_known

  subroutine print_rows(qap, options, bks, runs)
    type(qap_instance_t), intent(in) :: qap
    type(options_t), intent(in) :: options
    integer(int64), intent(in) :: bks
    type(run_t), intent(in) :: runs(:)
    integer :: k

    if (options%header) write (output_unit, '(a)') &
      'instance,n,algorithm,run,stream,cost,bks,gap_percent,time_to_best,elapsed,iterations'
    do k = 1, size(runs)
      write (output_unit, '(a,",",i0,",",a,",",i0,",",i0,",",i0,",",a,",",a,",",f0.4,",",f0.4,",",i0)') &
        qap%name, qap%n, options%algorithm, k, options%first_stream + k - 1, runs(k)%best%cost, &
        optional_integer(bks), gap_text(runs(k)%best%cost, bks), runs(k)%time_to_best, &
        runs(k)%elapsed(), runs(k)%iterations
    end do
  end subroutine print_rows

  subroutine print_summary(runs, bks)
    type(run_t), intent(in) :: runs(:)
    integer(int64), intent(in) :: bks
    integer(int64) :: costs(size(runs))
    integer :: k

    costs = [(runs(k)%best%cost, k = 1, size(runs))]
    write (error_unit, '(a,i0,a,i0,a,f0.1,a,i0,a,f0.3,a)') 'best ', minval(costs), '  worst ', maxval(costs), &
      '  mean ', sum(real(costs, real64)) / size(costs), '  bks ', bks, &
      '  mean time to best ', sum([(runs(k)%time_to_best, k = 1, size(runs))]) / size(runs), ' s'
    if (bks >= 0 .and. minval(costs) < bks) write (error_unit, '(a)') &
      'warning: cost below the best known solution: check the BKS value'
  end subroutine print_summary

  logical function all_verified(qap, runs)
    type(qap_instance_t), intent(in) :: qap
    type(run_t), intent(in) :: runs(:)
    integer :: k

    all_verified = .true.
    do k = 1, size(runs)
      all_verified = all_verified .and. is_permutation(runs(k)%best%p, qap%n) .and. &
                     runs(k)%best%cost == cost(qap, runs(k)%best%p)
    end do
  end function all_verified

  function gap_text(z, bks) result(text)
    integer(int64), intent(in) :: z, bks
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    text = ''
    if (bks <= 0) return
    write (buffer, '(f0.4)') 100 * real(z - bks, real64) / real(bks, real64)
    text = trim(buffer)
  end function gap_text

  function optional_integer(value) result(text)
    integer(int64), intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    text = ''
    if (value < 0) return
    write (buffer, '(i0)') value
    text = trim(buffer)
  end function optional_integer

  pure function lowercase(text) result(lower)
    character(*), intent(in) :: text
    character(len=len(text)) :: lower
    integer :: i

    lower = text
    do i = 1, len(text)
      if (text(i:i) >= 'A' .and. text(i:i) <= 'Z') lower(i:i) = achar(iachar(text(i:i)) + 32)
    end do
  end function lowercase

end program qap_solve
