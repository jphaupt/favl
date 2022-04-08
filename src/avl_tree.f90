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
        real(dp), public :: val
        integer :: key
    contains
        ! TODO not sure if pass(self) should be here...
        procedure, pass(self) :: insert
        ! procedure :: insert
    end type avl_tree_t

contains

    ! pure 
    subroutine insert(self, key, val)
        ! TODO should this be a pointer??
        class(avl_tree_t), pointer, intent(inout) :: self
        integer, intent(in) :: key
        real(dp), intent(in) :: val
        if (.not. associated(self)) then
            ! this is a null pointer, i.e. empty tree
            ! TODO ? do I need to initialise it?
            self%val=val
            self%key=key
            print*, 'what'
        end if
        print*, "TODO stub"
    end subroutine insert

end module avl_tree
