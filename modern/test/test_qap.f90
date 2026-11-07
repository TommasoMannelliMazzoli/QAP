!> Unit and end-to-end tests. Usage: test_qap [instances directory]
program test_qap
  use iso_fortran_env, only: int64, real64, output_unit
  use qap_rng, only: random_stream_t, random_stream
  use qap_instance, only: qap_instance_t, new_instance, load_instance, cost
  use qap_solution, only: solution_t, random_solution, is_permutation
  use qap_moves, only: swap_delta, rotation_delta, rotate, swap_delta_table_t, &
                       ROTATE_LEFT, ROTATE_RIGHT
  use qap_greedy, only: greedy1, greedy2, greedy3
  use qap_local_search, only: two_opt_first, two_opt_best, three_opt_first, three_opt_best
  use qap_algorithms, only: solve, ALGORITHM_NAMES
  use qap_params, only: params_t
  use qap_run, only: run_t, new_run
  implicit none

  integer :: checks = 0, failures = 0
  character(len=:), allocatable :: data_dir

  data_dir = instances_directory()

  call test_skip_matches_drawing()
  call test_same_stream_same_numbers()
  call test_streams_differ()
  call test_uniform_is_inside_unit_interval_with_mean_one_half()
  call test_integer_between_is_uniform_including_endpoints()
  call test_permutation_is_valid()

  call test_loads_qaplib_layout()
  call test_loads_original_repository_layout()
  call test_infers_size_when_header_is_wrong()
  call test_rejects_non_integer_data()

  call test_swap_delta_matches_cost_difference()
  call test_rotation_delta_matches_cost_difference()
  call test_delta_table_stays_exact_after_many_swaps()

  call test_greedy_costs_are_exact()
  call test_greedy3_with_true_distances_is_greedy3()
  call test_local_searches_end_in_local_optima()

  call test_every_algorithm_reports_its_true_cost()
  call test_metaheuristics_find_the_optimum_of_small_instances()

  write (output_unit, '(i0,a,i0,a)') checks - failures, ' of ', checks, ' checks passed'
  if (failures > 0) error stop 1

contains

  ! --- random numbers ------------------------------------------------------

  subroutine test_skip_matches_drawing()
    type(random_stream_t) :: drawn, skipped
    real(real64) :: u
    integer :: i

    drawn = random_stream(0_int64)
    skipped = random_stream(0_int64)
    do i = 1, 1000
      u = drawn%uniform()
    end do
    call skipped%skip(1000_int64)
    call check(bitwise_equal(drawn%uniform(), skipped%uniform()), 'skip(1000) equals 1000 draws')
  end subroutine test_skip_matches_drawing

  subroutine test_same_stream_same_numbers()
    type(random_stream_t) :: first, second
    real(real64) :: from_first(100), from_second(100)
    integer :: i
    logical :: same

    first = random_stream(7_int64)
    second = random_stream(7_int64)
    do i = 1, 100
      from_first(i) = first%uniform()
      from_second(i) = second%uniform()
    end do
    same = all([(bitwise_equal(from_first(i), from_second(i)), i = 1, 100)])
    call check(same, 'a stream is reproducible')
  end subroutine test_same_stream_same_numbers

  subroutine test_streams_differ()
    type(random_stream_t) :: first, second

    first = random_stream(1_int64)
    second = random_stream(2_int64)
    call check(.not. bitwise_equal(first%uniform(), second%uniform()), 'different streams give different numbers')
  end subroutine test_streams_differ

  subroutine test_uniform_is_inside_unit_interval_with_mean_one_half()
    type(random_stream_t) :: rng
    real(real64) :: u, total
    integer :: i
    logical :: inside

    rng = random_stream(3_int64)
    total = 0
    inside = .true.
    do i = 1, 100000
      u = rng%uniform()
      inside = inside .and. u > 0 .and. u < 1
      total = total + u
    end do
    call check(inside, 'uniform() lies in (0,1)')
    call check(abs(total / 100000 - 0.5_real64) < 0.005_real64, 'uniform() has mean 1/2')
  end subroutine test_uniform_is_inside_unit_interval_with_mean_one_half

  ! The original NINT((n-1)*u+1) gave the endpoints half the probability.
  subroutine test_integer_between_is_uniform_including_endpoints()
    integer, parameter :: values = 7, draws_per_value = 20000
    type(random_stream_t) :: rng
    integer :: frequency(values), i

    rng = random_stream(4_int64)
    frequency = 0
    do i = 1, values * draws_per_value
      associate (k => rng%integer_between(1, values))
        frequency(k) = frequency(k) + 1
      end associate
    end do
    call check(all(abs(frequency - draws_per_value) < draws_per_value / 20), &
               'integer_between(1,7) is uniform, endpoints included')
  end subroutine test_integer_between_is_uniform_including_endpoints

  subroutine test_permutation_is_valid()
    type(random_stream_t) :: rng
    integer :: p(50)

    rng = random_stream(5_int64)
    call rng%permutation(p)
    call check(is_permutation(p, 50), 'permutation() returns a permutation')
  end subroutine test_permutation_is_valid

  ! --- reading instances ----------------------------------------------------

  subroutine test_loads_qaplib_layout()
    type(qap_instance_t) :: qap

    call load_text('3' // new_line('a') // '0 1 2  3 0 4  5 6 0' // new_line('a') // &
                   '0 7 8  7 0 9  8 9 0', qap)
    call check(qap%n == 3, 'QAPLIB layout: n')
    call check(qap%a(2,3) == 4 .and. qap%b(3,1) == 8, 'QAPLIB layout: matrices read by rows')
    call check(.not. qap%symmetric, 'QAPLIB layout: asymmetric flows detected')
  end subroutine test_loads_qaplib_layout

  subroutine test_loads_original_repository_layout()
    type(qap_instance_t) :: chr, bur

    chr = repository_instance('chr12a')
    bur = repository_instance('bur26a')
    call check(chr%n == 12 .and. chr%symmetric, 'chr12a: n = 12, symmetric')
    call check(bur%n == 26 .and. .not. bur%symmetric, 'bur26a: n = 26, asymmetric')
  end subroutine test_loads_original_repository_layout

  subroutine test_infers_size_when_header_is_wrong()
    type(qap_instance_t) :: qap

    call load_text('1 13' // repeat(' 1', 18), qap)
    call check(qap%n == 3, 'a wrong header (as esc32f.dat) is overridden by the data')
  end subroutine test_infers_size_when_header_is_wrong

  subroutine test_rejects_non_integer_data()
    character(len=*), parameter :: path = 'test_non_integer.tmp'
    type(qap_instance_t) :: qap
    integer :: stat
    character(len=:), allocatable :: errmsg

    call write_text(path, '2  0 1.5 1 0  0 1 1 0')
    call load_instance(path, qap, stat, errmsg)
    call delete_file(path)
    call check(stat /= 0, 'non-integer data are rejected')
  end subroutine test_rejects_non_integer_data

  ! --- moves ----------------------------------------------------------------

  subroutine test_swap_delta_matches_cost_difference()
    type(qap_instance_t) :: cases(5)
    type(random_stream_t) :: rng
    integer :: c, trial, r, s
    integer, allocatable :: p(:), q(:)
    logical :: exact

    cases = test_instances()
    rng = random_stream(10_int64)
    exact = .true.
    do c = 1, size(cases)
      allocate (p(cases(c)%n))
      do trial = 1, 200
        call rng%permutation(p)
        call distinct_pair(rng, cases(c)%n, r, s)
        q = p
        q([r, s]) = p([s, r])
        exact = exact .and. swap_delta(cases(c), p, r, s) == cost(cases(c), q) - cost(cases(c), p)
      end do
      deallocate (p)
    end do
    call check(exact, 'swap_delta equals the cost difference (symmetric and asymmetric)')
  end subroutine test_swap_delta_matches_cost_difference

  subroutine test_rotation_delta_matches_cost_difference()
    type(qap_instance_t) :: cases(5)
    type(random_stream_t) :: rng
    integer :: c, trial, direction, picked(3)
    integer, allocatable :: p(:), q(:)
    logical :: exact

    cases = test_instances()
    rng = random_stream(11_int64)
    exact = .true.
    do c = 1, size(cases)
      allocate (p(cases(c)%n))
      do trial = 1, 200
        call rng%permutation(p)
        picked = three_distinct(rng, cases(c)%n)
        do direction = ROTATE_LEFT, ROTATE_RIGHT
          q = p
          call rotate(q, picked(1), picked(2), picked(3), direction)
          exact = exact .and. rotation_delta(cases(c), p, picked(1), picked(2), picked(3), direction) &
                              == cost(cases(c), q) - cost(cases(c), p)
        end do
      end do
      deallocate (p)
    end do
    call check(exact, 'rotation_delta equals the cost difference, both directions')
  end subroutine test_rotation_delta_matches_cost_difference

  subroutine test_delta_table_stays_exact_after_many_swaps()
    type(qap_instance_t) :: cases(5)
    type(random_stream_t) :: rng
    type(swap_delta_table_t) :: updated, rebuilt
    integer :: c, step, r, s
    integer, allocatable :: p(:)
    logical :: exact

    cases = test_instances()
    rng = random_stream(12_int64)
    exact = .true.
    do c = 1, size(cases)
      allocate (p(cases(c)%n))
      call rng%permutation(p)
      call updated%build(cases(c), p)
      do step = 1, 300
        call distinct_pair(rng, cases(c)%n, r, s)
        p([r, s]) = p([s, r])
        call updated%update_after_swap(cases(c), p, min(r, s), max(r, s))
      end do
      call rebuilt%build(cases(c), p)
      exact = exact .and. same_upper_triangle(updated%delta, rebuilt%delta)
      deallocate (p, updated%delta, rebuilt%delta)
    end do
    call check(exact, 'the O(n**2) table update equals a rebuild after 300 swaps')
  end subroutine test_delta_table_stays_exact_after_many_swaps

  ! --- heuristics and local search ------------------------------------------

  subroutine test_greedy_costs_are_exact()
    type(qap_instance_t) :: qap
    type(solution_t) :: g(3)
    integer :: k
    logical :: exact

    qap = repository_instance('bur26a')
    g = [greedy1(qap), greedy2(qap), greedy3(qap)]
    exact = .true.
    do k = 1, 3
      exact = exact .and. is_permutation(g(k)%p, qap%n) .and. g(k)%cost == cost(qap, g(k)%p)
    end do
    call check(exact, 'Greedy1-3 return permutations with their exact cost')
  end subroutine test_greedy_costs_are_exact

  subroutine test_greedy3_with_true_distances_is_greedy3()
    type(qap_instance_t) :: qap
    type(solution_t) :: plain, explicit

    qap = repository_instance('chr12a')
    plain = greedy3(qap)
    explicit = greedy3(qap, real(qap%b, real64))
    call check(all(plain%p == explicit%p), 'Greedy3 with D~ = D is Greedy3')
  end subroutine test_greedy3_with_true_distances_is_greedy3

  subroutine test_local_searches_end_in_local_optima()
    type(qap_instance_t) :: qap
    type(random_stream_t) :: rng
    type(solution_t) :: start, sol
    type(run_t) :: run
    integer :: search

    qap = random_instance(10, .false., 20_int64)
    rng = random_stream(21_int64)
    start = random_solution(qap, rng)
    run = new_run(10.0_real64)
    do search = 1, 4
      sol = start
      select case (search)
      case (1)
        call two_opt_first(qap, sol, run)
      case (2)
        call two_opt_best(qap, sol, run)
      case (3)
        call three_opt_first(qap, sol, run)
      case (4)
        call three_opt_best(qap, sol, run)
      end select
      call check(sol%cost == cost(qap, sol%p), 'local search tracks the exact cost')
      if (search <= 2) call check(no_improving_swap(qap, sol%p), '2-opt ends in a 2-opt local optimum')
      if (search >= 3) call check(no_improving_rotation(qap, sol%p), '3-opt ends in a 3-opt local optimum')
    end do
  end subroutine test_local_searches_end_in_local_optima

  ! --- whole algorithms -------------------------------------------------------

  !> The class of defect of the original tabu search: a reported cost that
  !> is not the cost of the reported permutation.
  subroutine test_every_algorithm_reports_its_true_cost()
    type(qap_instance_t) :: qap
    character(len=16), allocatable :: names(:)
    integer :: k

    qap = random_instance(12, .false., 30_int64)
    names = split(ALGORITHM_NAMES)
    do k = 1, size(names)
      associate (run => run_of(trim(names(k)), qap, 0.2_real64, 31_int64))
        call check(is_permutation(run%best%p, qap%n) .and. run%best%cost == cost(qap, run%best%p), &
                   trim(names(k)) // ' reports the true cost of a permutation')
      end associate
    end do
  end subroutine test_every_algorithm_reports_its_true_cost

  subroutine test_metaheuristics_find_the_optimum_of_small_instances()
    character(len=*), parameter :: metaheuristics(*) = &
      [character(len=10) :: 'rots', 'has', 'aco', 'gvns-first', 'gvns-best', 'sa', 'ts']
    type(qap_instance_t) :: cases(3)
    integer(int64) :: optimum
    integer :: c, k

    cases = [random_instance(8, .false., 40_int64), random_instance(8, .true., 41_int64), &
             repository_instance('Neos5')]
    do c = 1, size(cases)
      optimum = brute_force_optimum(cases(c))
      do k = 1, size(metaheuristics)
        associate (run => run_of(trim(metaheuristics(k)), cases(c), 0.5_real64, 42_int64))
          call check(run%best%cost == optimum, trim(metaheuristics(k)) // ' finds the optimum of ' // &
                     cases(c)%name)
        end associate
      end do
    end do
  end subroutine test_metaheuristics_find_the_optimum_of_small_instances

  ! --- helpers ----------------------------------------------------------------

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(*), intent(in) :: description

    checks = checks + 1
    if (.not. condition) then
      failures = failures + 1
      write (output_unit, '(a)') 'FAIL: ' // description
    end if
  end subroutine check

  function run_of(algorithm, qap, seconds, stream) result(run)
    character(*), intent(in) :: algorithm
    type(qap_instance_t), intent(in) :: qap
    real(real64), intent(in) :: seconds
    integer(int64), intent(in) :: stream
    type(run_t) :: run
    type(random_stream_t) :: rng
    type(params_t) :: params

    rng = random_stream(stream)
    run = new_run(seconds)
    call solve(algorithm, qap, params, rng, run)
  end function run_of

  function test_instances() result(cases)
    type(qap_instance_t) :: cases(5)

    cases = [random_instance(9, .false., 1_int64), random_instance(9, .true., 2_int64), &
             repository_instance('chr12a'), repository_instance('bur26a'), &
             repository_instance('els19')]
  end function test_instances

  !> Entries 0..9, diagonals included, to exercise every term of the deltas.
  function random_instance(n, symmetric, stream) result(qap)
    integer, intent(in) :: n
    logical, intent(in) :: symmetric
    integer(int64), intent(in) :: stream
    type(qap_instance_t) :: qap
    type(random_stream_t) :: rng
    integer(int64) :: a(n,n), b(n,n)
    integer :: i, j
    character(len=16) :: name

    rng = random_stream(stream)
    do j = 1, n
      do i = 1, n
        a(i,j) = rng%integer_between(0, 9)
        b(i,j) = rng%integer_between(0, 9)
      end do
    end do
    if (symmetric) then
      a = a + transpose(a)
      b = b + transpose(b)
    end if
    write (name, '(a,i0,a)') 'random', n, merge('s', 'a', symmetric)
    qap = new_instance(trim(name), a, b)
  end function random_instance

  function repository_instance(name) result(qap)
    character(*), intent(in) :: name
    type(qap_instance_t) :: qap
    integer :: stat
    character(len=:), allocatable :: errmsg

    call load_instance(data_dir // '/' // name // '.dat', qap, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end function repository_instance

  integer(int64) function brute_force_optimum(qap) result(best)
    type(qap_instance_t), intent(in) :: qap
    integer :: p(qap%n), i
    logical :: more

    p = [(i, i = 1, qap%n)]
    best = huge(1_int64)
    more = .true.
    do while (more)
      best = min(best, cost(qap, p))
      call next_permutation(p, more)
    end do
  end function brute_force_optimum

  !> Lexicographic successor; more = .false. after the last permutation.
  subroutine next_permutation(p, more)
    integer, intent(inout) :: p(:)
    logical, intent(out) :: more
    integer :: i, j

    i = size(p) - 1
    do while (i >= 1)
      if (p(i) < p(i + 1)) exit
      i = i - 1
    end do
    more = i >= 1
    if (.not. more) return
    j = size(p)
    do while (p(j) < p(i))
      j = j - 1
    end do
    p([i, j]) = p([j, i])
    p(i + 1 :) = p(size(p) : i + 1 : -1)
  end subroutine next_permutation

  logical function no_improving_swap(qap, p)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:)
    integer :: r, s

    no_improving_swap = .true.
    do r = 1, qap%n - 1
      do s = r + 1, qap%n
        if (swap_delta(qap, p, r, s) < 0) no_improving_swap = .false.
      end do
    end do
  end function no_improving_swap

  logical function no_improving_rotation(qap, p)
    type(qap_instance_t), intent(in) :: qap
    integer, intent(in) :: p(:)
    integer :: i, j, k, direction

    no_improving_rotation = .true.
    do i = 1, qap%n - 2
      do j = i + 1, qap%n - 1
        do k = j + 1, qap%n
          do direction = ROTATE_LEFT, ROTATE_RIGHT
            if (rotation_delta(qap, p, i, j, k, direction) < 0) no_improving_rotation = .false.
          end do
        end do
      end do
    end do
  end function no_improving_rotation

  logical function bitwise_equal(x, y)
    real(real64), intent(in) :: x, y
    bitwise_equal = transfer(x, 1_int64) == transfer(y, 1_int64)
  end function bitwise_equal

  logical function same_upper_triangle(x, y)
    integer(int64), intent(in) :: x(:,:), y(:,:)
    integer :: j

    same_upper_triangle = .true.
    do j = 2, size(x, 2)
      same_upper_triangle = same_upper_triangle .and. all(x(1 : j - 1, j) == y(1 : j - 1, j))
    end do
  end function same_upper_triangle

  subroutine distinct_pair(rng, n, r, s)
    type(random_stream_t), intent(inout) :: rng
    integer, intent(in) :: n
    integer, intent(out) :: r, s

    r = rng%integer_between(1, n)
    s = rng%integer_between(1, n - 1)
    if (s >= r) s = s + 1
  end subroutine distinct_pair

  function three_distinct(rng, n) result(picked)
    type(random_stream_t), intent(inout) :: rng
    integer, intent(in) :: n
    integer :: picked(3)

    call distinct_pair(rng, n, picked(1), picked(2))
    picked(3) = picked(1)
    do while (any(picked(1:2) == picked(3)))
      picked(3) = rng%integer_between(1, n)
    end do
  end function three_distinct

  function split(words) result(list)
    character(*), intent(in) :: words
    character(len=16), allocatable :: list(:)
    integer :: start, finish

    allocate (list(0))
    start = 1
    do while (start <= len_trim(words))
      if (words(start:start) == ' ') then
        start = start + 1
        cycle
      end if
      finish = index(words(start:) // ' ', ' ') + start - 2
      list = [character(len=16) :: list, words(start:finish)]
      start = finish + 1
    end do
  end function split

  subroutine load_text(text, qap)
    character(*), intent(in) :: text
    type(qap_instance_t), intent(out) :: qap
    character(len=*), parameter :: path = 'test_instance.tmp'
    integer :: stat
    character(len=:), allocatable :: errmsg

    call write_text(path, text)
    call load_instance(path, qap, stat, errmsg)
    call delete_file(path)
    if (stat /= 0) error stop errmsg
  end subroutine load_text

  subroutine write_text(path, text)
    character(*), intent(in) :: path, text
    integer :: unit

    open (newunit=unit, file=path, status='replace', action='write')
    write (unit, '(a)') text
    close (unit)
  end subroutine write_text

  subroutine delete_file(path)
    character(*), intent(in) :: path
    integer :: unit

    open (newunit=unit, file=path, status='old')
    close (unit, status='delete')
  end subroutine delete_file

  function instances_directory() result(dir)
    character(len=:), allocatable :: dir
    integer :: length

    if (command_argument_count() < 1) then
      dir = '../Instancies'
      return
    end if
    call get_command_argument(1, length=length)
    allocate (character(len=length) :: dir)
    call get_command_argument(1, dir)
  end function instances_directory

end program test_qap
