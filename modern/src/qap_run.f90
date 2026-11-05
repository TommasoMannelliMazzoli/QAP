!> Bookkeeping of one run: its budget and the best solution it found.
module qap_run
  use iso_fortran_env, only: int64, real64
  use qap_solution, only: solution_t
  implicit none
  private
  public :: run_t, new_run, wall_time

  type :: run_t
    real(real64) :: time_limit = 0
    integer(int64) :: iteration_limit = huge(1_int64)
    integer(int64) :: iterations = 0
    type(solution_t) :: best
    real(real64) :: time_to_best = 0
    real(real64) :: start_time = 0
  contains
    procedure :: record
    procedure :: count_iteration
    procedure :: is_over
    procedure :: elapsed
  end type run_t

contains

  function new_run(time_limit, iteration_limit) result(run)
    real(real64), intent(in) :: time_limit
    integer(int64), intent(in), optional :: iteration_limit
    type(run_t) :: run

    run%time_limit = time_limit
    if (present(iteration_limit)) run%iteration_limit = iteration_limit
    run%start_time = wall_time()
  end function new_run

  !> Keeps sol if it is the best solution of the run so far.
  subroutine record(self, sol)
    class(run_t), intent(inout) :: self
    type(solution_t), intent(in) :: sol

    if (sol%cost < self%best%cost) then
      self%best = sol
      self%time_to_best = self%elapsed()
    end if
  end subroutine record

  subroutine count_iteration(self)
    class(run_t), intent(inout) :: self
    self%iterations = self%iterations + 1
  end subroutine count_iteration

  logical function is_over(self)
    class(run_t), intent(in) :: self
    is_over = self%iterations >= self%iteration_limit
    if (.not. is_over) is_over = self%elapsed() >= self%time_limit
  end function is_over

  real(real64) function elapsed(self)
    class(run_t), intent(in) :: self
    elapsed = wall_time() - self%start_time
  end function elapsed

  !> Wall-clock seconds: CPU_TIME would add up the threads of parallel runs.
  real(real64) function wall_time()
    integer(int64) :: count, rate
    call system_clock(count, rate)
    wall_time = real(count, real64) / real(rate, real64)
  end function wall_time

end module qap_run
