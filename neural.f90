program neural
	use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
	implicit none

	integer, parameter                           :: INPUT_DIM   = 2
	integer, parameter                           :: NUM_NEURONS = 1
	real(dp), dimension(NUM_NEURONS, 1)          :: b, bnew
	real(dp), dimension(NUM_NEURONS, INPUT_DIM)  :: W, Wnew
	integer, dimension(NUM_NEURONS, 1)           :: A
	integer, dimension(NUM_NEURONS, 1)           :: e

	integer, dimension(8, 2)  :: training_inputs
	integer, dimension(8, 1)  :: training_outputs
	integer, dimension(16, 2) :: test_inputs

	integer :: ii

	test_inputs = reshape((/1,1,2,2,3,3,4,4,1,0,3,3,2,2,5,5,4,5,4,5,1,2,1,2,4,5,4,5,1,2,1,2/), shape(test_inputs))

	training_inputs  = reshape((/1,1,2,2,3,3,4,4,4,5,4,5,1,2,1,2/), shape(training_inputs))

	training_outputs  = reshape((/0, 0, 0, 0, 1, 1, 1, 1/), shape(training_outputs))

	! Initialize all weights to 0
	W = 1
	Wnew = 0
	b = 1
	bnew = 0

	! Training loop
	do while (.not. (all(W == Wnew) .and. all(b == bnew)))
		ii = 1
		do while(ii .le. 8)
			W = Wnew
			b = bnew
			call train(training_inputs(ii, :), training_outputs(ii, :))
			ii = ii + 1
		end do
	end do

	W = Wnew
	b = bnew

	ii = 1
	do while (ii .le. 16)
		call evaluate(test_inputs(ii, :))
		print *, ii, test_inputs(ii, :), A
		ii = ii + 1
	end do

contains

integer elemental function hardlim(x) result(a)
	implicit none
	real(dp), intent(in) :: x
	a = merge(1, 0, x >= 0)
end function

subroutine evaluate(p)
	implicit none
	integer, dimension(INPUT_DIM, 1), intent(in) :: p
	A = hardlim(matmul(W, p) + b)
end subroutine

subroutine train(p, t)
	implicit none
	integer, dimension(INPUT_DIM, 1), intent(in) :: p
	integer, dimension(NUM_NEURONS, 1), intent(in) :: t
	call evaluate(p)
	e = t - A
	Wnew = W + dble(matmul(e, transpose(p)))
	bnew = b + dble(e)
end subroutine

end program neural
