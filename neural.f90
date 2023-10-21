! NICK OVERACKER | 北見工業大学
program neural
	use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
	implicit none

	integer, parameter                           :: INPUT_DIM   = 2
	integer, parameter                           :: NUM_NEURONS = 2
	real(dp), dimension(NUM_NEURONS, 1)          :: b, b_old
	real(dp), dimension(NUM_NEURONS, INPUT_DIM)  :: W, W_old
	integer, dimension(NUM_NEURONS, 1)           :: A
	integer, dimension(NUM_NEURONS, 1)           :: e

	integer, dimension(8, 2)  :: training_inputs
	integer, dimension(8, 2)  :: training_outputs
	integer, dimension(16, 2) :: test_inputs

	integer :: ii

	test_inputs = reshape((/1,1,2,2,-1,-2,-1,-2,4,5,2,-5,3,6,-3,-6,1,2,-1,0,2,1,-1,-2,-1,-2,4,-4,1,2,3,4/), shape(test_inputs))
	training_inputs  = reshape((/1,1,2,2,-1,-2,-1,-2,1,2,-1,0,2,1,-1,-2/), shape(training_inputs))
	training_outputs  = reshape((/0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1/), shape(training_outputs))

	! Initialize all weights and biases to 0
	W = 0
	b = 0

	! These need to be different for the first pass.
	W_old = 1
	b_old = 1

	! Training loop
	do while (.not. (all(W == W_old) .and. all(b == b_old)))
		ii = 1
		W_old = W
		b_old = b
		do while(ii .le. 8)
			call train(training_inputs(ii, :), training_outputs(ii, :))
			ii = ii + 1
		end do
	end do

	! Test loop
	ii = 1
	do while (ii .le. 16)
		call evaluate(test_inputs(ii, :))
		print *, ii, test_inputs(ii, :), A
		ii = ii + 1
	end do

contains

! パーセプトロンの伝達関数
integer elemental function hardlim(x) result(a)
	implicit none
	real(dp), intent(in) :: x
	a = merge(1, 0, x >= 0)
end function

! ニューロン（パーセプトロン）の出力を計算する
subroutine evaluate(p)
	implicit none
	integer, dimension(INPUT_DIM, 1), intent(in) :: p
	A = hardlim(matmul(W, p) + b)
end subroutine

! パーセプトロンのトレーニングを行う
subroutine train(p, t)
	implicit none
	integer, dimension(INPUT_DIM, 1), intent(in) :: p
	integer, dimension(NUM_NEURONS, 1), intent(in) :: t
	call evaluate(p)
	e = t - A
	W = W + dble(matmul(e, transpose(p)))
	b = b + dble(e)
end subroutine

end program neural
