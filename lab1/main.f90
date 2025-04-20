program lab1
  implicit none

  real, allocatable :: numbers(:), original_numbers(:)
  integer :: n, i, j, index, alphabet_size, io_status
  character(len=1), allocatable :: alphabet(:)
  integer, allocatable :: transition_matrix(:,:)
  real :: min_val, max_val, step
  character(len=1), allocatable :: sequence(:)
  integer :: row, col

  ! === 1. Задаємо потужність алфавіту ===
  alphabet_size = 10

  ! === 2. Створюємо алфавіт і матрицю ===
  allocate(alphabet(alphabet_size))
  allocate(transition_matrix(alphabet_size, alphabet_size))
  transition_matrix = 0
  do i = 1, alphabet_size
    alphabet(i) = achar(64 + i)  ! 'A', 'B', ...
  end do

  ! === 3. Підрахунок кількості чисел у файлі ===
  open(unit=10, file="3.txt", status="old", action="read")
  n = 0
  do
    read(10, *, iostat=io_status)
    if (io_status /= 0) exit
    n = n + 1
  end do
  close(10)

  ! === 4. Виділення пам’яті ===
  allocate(numbers(n))
  allocate(original_numbers(n))
  allocate(sequence(n))

  ! === 5. Зчитування чисел у original_numbers ===
  open(unit=10, file="3.txt", status="old", action="read")
  do i = 1, n
    read(10, *) original_numbers(i)
  end do
  close(10)

  ! === 6. Копія для сортування — numbers ===
  numbers = original_numbers
  call sort(numbers, n)

  ! === 7. Побудова інтервалів ===
  min_val = numbers(1)
  max_val = numbers(n)
  step = (max_val - min_val) / real(alphabet_size)

  ! === 8. Перетворення чисел у літери (по оригінальному порядку) ===
  do i = 1, n
    index = ceiling((original_numbers(i) - min_val) / step)
    if (index < 1) index = 1
    if (index > alphabet_size) index = alphabet_size
    sequence(i) = alphabet(index)
  end do

  ! === 9. Побудова матриці передування ===
  do i = 1, n - 1
    row = index_in_alphabet(sequence(i), alphabet, alphabet_size)
    col = index_in_alphabet(sequence(i + 1), alphabet, alphabet_size)
    transition_matrix(row, col) = transition_matrix(row, col) + 1
  end do

  ! === 10. Виведення результатів ===
  print *, "Alphabet Size:", alphabet_size
  print *, "Linguistic Sequence:"
  print *, sequence(1:n)
  print *, "Transition Matrix:"
  do i = 1, alphabet_size
    write(*, '(100I4)') (transition_matrix(i, j), j = 1, alphabet_size)
  end do

  ! === 11. Звільнення пам’яті ===
  deallocate(numbers, original_numbers, sequence, alphabet, transition_matrix)

contains

  ! === Функція сортування масиву ===
  subroutine sort(arr, size)
    implicit none
    integer, intent(in) :: size
    real, intent(inout) :: arr(size)
    integer :: i, j
    real :: temp
    do i = 1, size-1
      do j = i+1, size
        if (arr(i) > arr(j)) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
        end if
      end do
    end do
  end subroutine sort

  ! === Функція знаходження індексу символу у алфавіті ===
  function index_in_alphabet(c, alph, size) result(indx)
    implicit none
    character(len=1), intent(in) :: c
    character(len=1), intent(in) :: alph(size)
    integer, intent(in) :: size
    integer :: indx, i
    indx = 0
    do i = 1, size
      if (c == alph(i)) then
        indx = i
        exit
      end if
    end do
  end function index_in_alphabet

end program lab1
