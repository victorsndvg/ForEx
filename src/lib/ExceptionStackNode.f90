module ExceptionStackNode

USE iso_fortran_env, only : error_unit
USE BaseException

implicit none
private

    type :: ExceptionStackNode_t
    private
        class(Exception),            pointer :: anException
        class(ExceptionStackNode_t), pointer :: Previous => null()
        class(ExceptionStackNode_t), pointer :: Next => null()
    contains
    private
        procedure, non_overridable, public :: Print            => ExceptionStackNode_Print
        procedure, non_overridable, public :: HasNext          => ExceptionStackNode_HasNext
        procedure, non_overridable, public :: SetNext          => ExceptionStackNode_SetNext
        procedure, non_overridable, public :: GetNext          => ExceptionStackNode_GetNext
        procedure, non_overridable, public :: NullifyNext      => ExceptionStackNode_NullifyNext
        procedure, non_overridable, public :: HasPrevious      => ExceptionStackNode_HasPrevious
        procedure, non_overridable, public :: SetPrevious      => ExceptionStackNode_SetPrevious
        procedure, non_overridable, public :: GetPrevious      => ExceptionStackNode_GetPrevious
        procedure, non_overridable, public :: NullifyPrevious  => ExceptionStackNode_NullifyPrevious
        procedure, non_overridable, public :: SetException     => ExceptionStackNode_SetException
        procedure, non_overridable, public :: GetException     => ExceptionStackNode_GetException
        procedure, non_overridable, public :: Free             => ExceptionStackNode_Free
        final                              ::                     Exception_Finalize
    end type ExceptionStackNode_t

public :: ExceptionStackNode_t

contains

    function ExceptionStackNode_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(IN) :: this               !< Stack Node
        logical                                 :: hasNext            !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function ExceptionStackNode_HasNext


    subroutine ExceptionStackNode_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t),          intent(INOUT) :: this   !< Stack Node
        class(ExceptionStackNode_t), pointer, intent(IN)    :: Next   !< Pointer to Next 
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine ExceptionStackNode_SetNext


    function ExceptionStackNode_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(IN) :: this               !< Stack Node
        class(ExceptionStackNode_t), pointer    :: Next               !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function ExceptionStackNode_GetNext


    subroutine ExceptionStackNode_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t),          intent(INOUT) :: this   !< Stack Node
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine ExceptionStackNode_NullifyNext


    function ExceptionStackNode_HasPrevious(this) result(hasPrevious)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(IN) :: this               !< Stack Node
        logical                                 :: hasPrevious        !< Check if Previous is associated
    !-----------------------------------------------------------------
        hasPrevious = associated(this%Previous)
    end function ExceptionStackNode_HasPrevious


    subroutine ExceptionStackNode_SetPrevious(this, Previous)
    !-----------------------------------------------------------------
    !< Set the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t),          intent(INOUT) :: this     !< Stack Node
        class(ExceptionStackNode_t), pointer, intent(IN)    :: Previous !< Pointer to Previous
    !-----------------------------------------------------------------
        this%Previous => Previous
    end subroutine ExceptionStackNode_SetPrevious


    function ExceptionStackNode_GetPrevious(this) result(Previous)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(IN) :: this               !< Stack Node
        class(ExceptionStackNode_t), pointer    :: Previous           !< Pointer to Previous
    !-----------------------------------------------------------------
        nullify(Previous)
        if(this%HasPrevious()) Previous => this%Previous
    end function ExceptionStackNode_GetPrevious


    subroutine ExceptionStackNode_NullifyPrevious(this)
    !-----------------------------------------------------------------
    !< Nullify the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t),          intent(INOUT) :: this   !< Stack Node
    !-----------------------------------------------------------------
        nullify(this%Previous)
    end subroutine ExceptionStackNode_NullifyPrevious


    subroutine ExceptionStackNode_SetException(this, anException, File, Line)
    !-----------------------------------------------------------------
    !< Fill the exception context info
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(inout) :: this            !< Stack Node
        class(Exception), pointer,   intent(in)    :: anException     !< Exception
        Character(Len=*),            intent(in)    :: File            !< File where the exception is launched
        integer,                     intent(in)    :: Line            !< Line where the exception is launched
    !-----------------------------------------------------------------
        if(associated(this%anException)) deallocate(this%anException)
        this%anException => anException
        call this%anException%SetContext(File=file, Line=Line)
    end subroutine ExceptionStackNode_SetException


    function ExceptionStackNode_GetException(this) result(anException)
    !-----------------------------------------------------------------
    !< Return a pointer to the stored exception
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), target, intent(inout) :: this        !< Stack Node
        class(Exception),            pointer               :: anException !< Exception
    !-----------------------------------------------------------------
        anException => this%anException
    end function ExceptionStackNode_GetException


    subroutine ExceptionStackNode_Free(this)
    !-----------------------------------------------------------------
    !< Free the Node
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t), intent(INOUT) :: this            !< Stack Node
    !-----------------------------------------------------------------
        if(associated(this%anException)) then
            call this%anException%free()
            deallocate(this%anException)
        endif
        nullify(this%Next)
    end subroutine ExceptionStackNode_Free


    subroutine ExceptionStackNode_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the Procedure Hash Table List
    !-----------------------------------------------------------------
        class(ExceptionStackNode_t),      intent(IN)  :: this         !< Exception Stack Node
        integer,      optional,           intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer,      optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:),       allocatable           :: prefd        !< Prefixing string.
        integer                                       :: unitd        !< logic unit
        integer                                       :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = ''; unitd = error_unit
        if (present(unit)) unitd = unit
        if (present(prefix)) prefd = prefix
        if(associated(this%anException)) call this%anException%Print(unitd, prefix=prefd, iostat=iostatd, iomsg=iomsgd)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ExceptionStackNode_Print


    subroutine ExceptionStackNode_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(Exception), intent(INOUT):: this                         !< Exception Stack Node
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ExceptionStackNode_Finalize


end module ExceptionStackNode
