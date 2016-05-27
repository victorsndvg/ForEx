module ExceptionContextStackNode

USE iso_fortran_env, only : error_unit
USE BaseExceptionContext

implicit none
private

    type :: ExceptionContextStackNode_t
    private
        type(ExceptionContext)                      :: aContext
        class(ExceptionContextStackNode_t), pointer :: Previous => null()
        class(ExceptionContextStackNode_t), pointer :: Next => null()
    contains
    private
        procedure, non_overridable, public :: Print            => ExceptionContextStackNode_Print
        procedure, non_overridable, public :: HasNext          => ExceptionContextStackNode_HasNext
        procedure, non_overridable, public :: SetNext          => ExceptionContextStackNode_SetNext
        procedure, non_overridable, public :: GetNext          => ExceptionContextStackNode_GetNext
        procedure, non_overridable, public :: NullifyNext      => ExceptionContextStackNode_NullifyNext
        procedure, non_overridable, public :: HasPrevious      => ExceptionContextStackNode_HasPrevious
        procedure, non_overridable, public :: SetPrevious      => ExceptionContextStackNode_SetPrevious
        procedure, non_overridable, public :: GetPrevious      => ExceptionContextStackNode_GetPrevious
        procedure, non_overridable, public :: NullifyPrevious  => ExceptionContextStackNode_NullifyPrevious
        procedure, non_overridable, public :: SetContext       => ExceptionContextStackNode_SetContext
        procedure, non_overridable, public :: GetContext       => ExceptionContextStackNode_GetContext
        procedure, non_overridable, public :: Free             => ExceptionContextStackNode_Free
        final                              ::                     ExceptionContextStackNode_Finalize
    end type ExceptionContextStackNode_t

public :: ExceptionContextStackNode_t

contains

    function ExceptionContextStackNode_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(IN) :: this        ! Context Stack Node
        logical                                        :: hasNext     ! Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function ExceptionContextStackNode_HasNext


    subroutine ExceptionContextStackNode_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t),          intent(INOUT) :: this ! Context Stack Node
        type(ExceptionContextStackNode_t),  pointer, intent(IN)    :: Next ! Pointer to Next 
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine ExceptionContextStackNode_SetNext


    function ExceptionContextStackNode_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(IN) :: this        ! Context Stack Node
        class(ExceptionContextStackNode_t), pointer    :: Next        ! Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function ExceptionContextStackNode_GetNext


    subroutine ExceptionContextStackNode_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(INOUT) :: this     ! Context Stack Node
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine ExceptionContextStackNode_NullifyNext


    function ExceptionContextStackNode_HasPrevious(this) result(hasPrevious)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(IN) :: this        ! Context Stack Node
        logical                                        :: hasPrevious ! Check if Previous is associated
    !-----------------------------------------------------------------
        hasPrevious = associated(this%Previous)
    end function ExceptionContextStackNode_HasPrevious


    subroutine ExceptionContextStackNode_SetPrevious(this, Previous)
    !-----------------------------------------------------------------
    !< Set the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t),          intent(INOUT) :: this     ! Context Stack Node
        class(ExceptionContextStackNode_t), pointer, intent(IN)    :: Previous ! Pointer to Previous
    !-----------------------------------------------------------------
        this%Previous => Previous
    end subroutine ExceptionContextStackNode_SetPrevious


    function ExceptionContextStackNode_GetPrevious(this) result(Previous)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(IN) :: this        ! Context Stack Node
        class(ExceptionContextStackNode_t), pointer    :: Previous    ! Pointer to Previous
    !-----------------------------------------------------------------
        nullify(Previous)
        if(this%HasPrevious()) Previous => this%Previous
    end function ExceptionContextStackNode_GetPrevious


    subroutine ExceptionContextStackNode_NullifyPrevious(this)
    !-----------------------------------------------------------------
    !< Nullify the pointer to the Previous node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(INOUT) :: this     ! Context Stack Node
    !-----------------------------------------------------------------
        nullify(this%Previous)
    end subroutine ExceptionContextStackNode_NullifyPrevious


    subroutine ExceptionContextStackNode_SetContext(this, File, Line)
    !-----------------------------------------------------------------
    !< Fill the exception context info
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(inout) :: this               ! Stack Node
        Character(Len=*),                   intent(in)    :: File               ! File where the exception is launched
        integer,                            intent(in)    :: Line               ! Line where the exception is launched
    !-----------------------------------------------------------------
        call this%aContext%SetContext(File=file, Line=Line)
    end subroutine ExceptionContextStackNode_SetContext


    function ExceptionContextStackNode_GetContext(this) result(aContext)
    !-----------------------------------------------------------------
    !< Return a pointer to the stored exception
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), target, intent(inout) :: this               ! Context Stack Node
        class(ExceptionContext),            pointer               :: aContext ! Exception Context
    !-----------------------------------------------------------------
        aContext => this%aContext
    end function ExceptionContextStackNode_GetContext


    subroutine ExceptionContextStackNode_Free(this)
    !-----------------------------------------------------------------
    !< Free the Node
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(INOUT) :: this     ! Context Stack Node
    !-----------------------------------------------------------------
        call this%aContext%free()
        nullify(this%Previous)
        nullify(this%Next)
    end subroutine ExceptionContextStackNode_Free


    subroutine ExceptionContextStackNode_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the Procedure Hash Table List
    !-----------------------------------------------------------------
        class(ExceptionContextStackNode_t), intent(IN)  :: this       ! Exception Stack Node
        integer,      optional,             intent(IN)  :: unit       ! Logic unit.
        character(*), optional,             intent(IN)  :: prefix     ! Prefixing string.
        integer,      optional,             intent(OUT) :: iostat     ! IO error.
        character(*), optional,             intent(OUT) :: iomsg      ! IO error message.
        character(len=:),       allocatable             :: prefd      ! Prefixing string.
        integer                                         :: unitd      ! logic unit
        integer                                         :: iostatd    ! IO error.
        character(500)                                  :: iomsgd     ! Temporary variable for IO error message.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = ''; unitd = error_unit
        if (present(unit)) unitd = unit
        if (present(prefix)) prefd = prefix
        call this%aContext%Print(unitd, prefix=prefd, iostat=iostatd, iomsg=iomsgd)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ExceptionContextStackNode_Print


    subroutine ExceptionContextStackNode_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ExceptionContextStackNode_t),      intent(INOUT)  :: this ! Exception Context Stack Node
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ExceptionContextStackNode_Finalize


end module ExceptionContextStackNode
