!! author: jphaupt
!!
!! Implementation of an AVL tree
!! in princple, I would prefer this to actually inherit/extend a generic BST


module avl_tree
    use iso_fortran_env, only: dp=>real64
    implicit none

    type avl_tree_t
    private
        type(avl_tree_t), pointer :: left=>null()
        type(avl_tree_t), pointer :: right=>null()
        real(dp), public :: value
        integer :: key
    contains
        ! TODO not sure if pass(self) should be here...
        procedure, pass(self) :: insert
        ! procedure :: insert
    end type avl_tree_t

contains

    ! pure 
    subroutine insert(self, key, value)
        class(avl_tree_t), intent(inout) :: self
        integer, intent(in) :: key
        real(dp), intent(in) :: value
        print*, "TODO stub"
    end subroutine insert

end module avl_tree
