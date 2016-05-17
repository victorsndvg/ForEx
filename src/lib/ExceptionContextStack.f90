
module ExceptionContextStack

USE iso_fortran_env, only : error_unit
USE ExceptionContextStackNode
USE BaseExceptionContext

implicit none
private

    type :: ExceptionContextStackIterator_t
    private
        class(ExceptionContextStack_t),     pointer :: Stack    => null()
        class(ExceptionContextStackNode_t), pointer :: Current  => null()
    contains
        procedure, non_overridable :: Init        => ExceptionContextStackIterator_Init
        procedure, non_overridable :: Next        => ExceptionContextStackIterator_Next
        procedure, non_overridable :: GetNode     => ExceptionContextStackIterator_GetNode
        procedure, non_overridable :: GetContext  => ExceptionContextStackIterator_GetContext
        procedure, non_overridable :: Print       => ExceptionContextStackIterator_Print
        procedure, non_overridable :: Free        => ExceptionContextStackIterator_Free
        procedure, non_overridable :: HasFinished => ExceptionContextStackIterator_HasFinished
    end type

    type :: ExceptionContextStack_t
    private
        type(ExceptionContextStackNode_t), pointer :: Root  => null()
    contains
    private
        procedure, non_overridable         :: HasRoot          => ExceptionContextStack_HasRoot
        procedure, non_overridable         :: SetRoot          => ExceptionContextStack_SetRoot
        procedure, non_overridable         :: NullifyRoot      => ExceptionContextStack_NullifyRoot
        procedure, non_overridable         :: DeallocateRoot   => ExceptionContextStack_DeallocateRoot
        procedure, non_overridable, public :: isEmpty          => ExceptionContextStack_isEmpty
        procedure, non_overridable, public :: Pop              => ExceptionContextStack_Pop
        procedure, non_overridable, public :: Push             => ExceptionContextStack_Push
        procedure, non_overridable, public :: Print            => ExceptionContextStack_Print
        procedure, non_overridable, public :: Clone            => ExceptionContextStack_Clone
        procedure, non_overridable, public :: Free             => ExceptionContextStack_Free
        final                              ::                     ExceptionContextStack_Finalize 
    end type

public :: ExceptionContextStack_t

contains


    subroutine ExceptionContextStack_SetRoot(this, Root)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t),              intent(INOUT) :: this ! Exception Context Stack
        class(ExceptionContextStackNode_t), pointer, intent(IN)    :: Root ! Context Stack Node correspoing to the head of the list
    !-----------------------------------------------------------------
        this%Root => Root
    end subroutine ExceptionContextStack_SetRoot


    function ExceptionContextStack_Pop(this) result(Root)
    !-----------------------------------------------------------------
    !< Deallocate to the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t),             intent(INout) :: this ! Exception Context Stack
        class(ExceptionContextStackNode_t), pointer               :: Root ! Context Stack Node correspoing to the head of the list
    !-----------------------------------------------------------------
        nullify(Root)
        if(this%HasRoot()) then
            Root => this%Root
            this%Root => Root%GetNext()
            call Root%Free()
            deallocate(Root)
            nullify(Root)
        endif
    end function ExceptionContextStack_Pop


    function ExceptionContextStack_isEmpty(this) result(isEmpty)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is not associated
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), intent(IN) :: this            ! Exception Context Stack
        logical                                    :: isEmpty         ! Check if Root is associated
    !-----------------------------------------------------------------
        isEmpty = .not. this%HasRoot()
    end function ExceptionContextStack_isEmpty


    function ExceptionContextStack_HasRoot(this) result(HasRoot)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), intent(IN) :: this            ! Exception Context Stack
        logical                                    :: hasRoot         !< Check if Root is associated
    !-----------------------------------------------------------------
        hasRoot = associated(this%Root)
    end function ExceptionContextStack_HasRoot


    subroutine ExceptionContextStack_NullifyRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), intent(INOUT) :: this         ! Exception Context Stack
    !-----------------------------------------------------------------
        nullify(this%Root)
    end subroutine ExceptionContextStack_NullifyRoot


    subroutine ExceptionContextStack_DeallocateRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), intent(INOUT) :: this                ! Exception Context Stack
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            call this%Root%Free()
            deallocate(this%Root)
        endif
    end subroutine ExceptionContextStack_DeallocateRoot


    subroutine ExceptionContextStack_Push(this, File, Line)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), intent(INOUT) :: this         ! Exception Context Stack
        character(len=*),               intent(IN)    :: File         ! File where the exception occurs
        integer,                        intent(in)    :: Line         ! Line where the exception occurs
        class(ExceptionContextStackNode_t), pointer   :: NewStackNode ! New Stack Node
    !-----------------------------------------------------------------
        allocate(NewStackNode)
        call NewStackNode%SetContext(File=file, Line=Line)
        if(associated(this%Root)) then
            call NewStackNode%SetNext(this%Root)
            call this%Root%SetPrevious(NewStackNode)
        else
            call NewStackNode%NullifyNext()
        endif
        call NewStackNode%NullifyPrevious()
        this%Root => NewStackNode
    end subroutine ExceptionContextStack_Push


    subroutine ExceptionContextStack_Clone(this, Source)
    !-----------------------------------------------------------------
    !< Clone the Source Context Stack
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t), target, intent(INOUT) :: this      ! Exception Context Stack
        class(ExceptionContextStack_t),         intent(IN)    :: Source    ! Exception Context Stack
        type(ExceptionContextStackIterator_t)                 :: Iterator  ! Stack iterator
        type(ExceptionContext),            pointer            :: aContext  ! Exception Context
        type(ExceptionContextStackNode_t), pointer            :: StackNode ! StackNode
        type(ExceptionContextStackNode_t), pointer            :: Next      ! StackNode
        type(ExceptionContextStackNode_t), pointer            :: Current   ! StackNode
    !-----------------------------------------------------------------
        call Iterator%Init(Source)
        call this%Free()
        if(.not. Source%isEmpty()) then
            allocate(this%Root)
            Current => this%Root
            do while(.not. Iterator%HasFinished())
                StackNode => Iterator%GetNode()
                if(associated(StackNode)) then
                    AContext => StackNode%GetContext()
                    if(associated(aContext)) then
                        call Current%SetContext(aContext%GetFile(), aContext%GetLine())
                        if(StackNode%HasNext()) then
                            allocate(Next)
                            call Current%SetNext(Next)
                            Current => Next
                        else
                            call Current%NullifyNext()
                            nullify(Current)
                        endif
                    endif
                endif
                call Iterator%Next()
            enddo
        endif
    end subroutine ExceptionContextStack_Clone


    subroutine ExceptionContextStack_Free(this)
    !-----------------------------------------------------------------
    !< Free the stack
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t),             intent(INOUT) :: this    ! Exception Context Stack
        type(ExceptionContextStackIterator_t)                     :: Iterator! Current Parameter List Node
        class(ExceptionContextStackNode_t), pointer               :: Current ! Current Parameter List Node
        class(ExceptionContextStackNode_t), pointer               :: Next    ! Next Parameter List Node
    !-----------------------------------------------------------------
        call Iterator%Init(this)
        do while(.not. Iterator%HasFinished())
            Current => Iterator%GetNode()
            call Iterator%Next()
            call Current%Free()
            deallocate(Current)
        enddo
        call Iterator%Free()
        nullify(this%Root)
    end subroutine ExceptionContextStack_Free


    subroutine ExceptionContextStack_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the File/Line pair contained in the context
    !-----------------------------------------------------------------
        class(ExceptionContextStack_t),       intent(IN)     :: this     ! Exception Context Stack
        integer,                    optional, intent(IN)     :: unit     ! Logic unit.
        character(*),               optional, intent(IN)     :: prefix   ! Prefixing string.
        integer,                    optional, intent(OUT)    :: iostat   ! IO error.
        character(*),               optional, intent(OUT)    :: iomsg    ! IO error message.
        character(len=:), allocatable                        :: prefd    ! Prefixing string.
        integer                                              :: unitd    ! IO error.
        integer                                              :: iostatd  ! IO error.
        character(500)                                       :: iomsgd   ! Temporary variable for IO error message.
        type(ExceptionContextStackNode_t), pointer           :: StackNode! Stack iterator
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = ''; unitd = error_unit
        if (present(unit)) unitd = unit
        if (present(prefix)) prefd = prefix
        StackNode => this%Root
        do while(associated(StackNode))
            call StackNode%Print(unit=unitd, prefix=prefd, iostat=iostatd, iomsg=iomsgd )
            StackNode => StackNode%GetNext()
        enddo
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ExceptionContextStack_Print


    subroutine ExceptionContextStack_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ExceptionContextStack_t), intent(INOUT):: this           ! Exception Context Stack
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ExceptionContextStack_Finalize


!---------------------------------------------------------------------
!< Context Stack Iterator procedures
!---------------------------------------------------------------------

    subroutine ExceptionContextStackIterator_Init(this, ExceptionContextStack)
    !-----------------------------------------------------------------
    !< Initialize Exception Context Stack Iterator to the root of the 
    !< given Exception Context Stack
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t), intent(INOUT) :: this                  ! Context Stack Iterator
        class(ExceptionContextStack_t), target, intent(IN)    :: ExceptionContextStack ! Exception Stack
    !-----------------------------------------------------------------
        this%Stack => ExceptionContextStack
        this%Current => this%Stack%Root
    end subroutine ExceptionContextStackIterator_Init


    subroutine ExceptionContextStackIterator_Next(this)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t), intent(INOUT) :: this !< Context Stack Iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) this%Current => this%Current%GetNext()
    end subroutine ExceptionContextStackIterator_Next


    function ExceptionContextStackIterator_GetNode(this) result(aStackNode)
    !-----------------------------------------------------------------
    !< Return a pointer to the Context Stack Node on the top of the stack
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t),  intent(IN) :: this       ! Context Stack Iterator
        class(ExceptionContextStackNode_t),      pointer    :: aStackNode ! Current context in the iterator
    !-----------------------------------------------------------------
        nullify(aStackNode)
        if(.not. this%HasFinished()) then
            aStackNode => this%Current
        endif
    end function ExceptionContextStackIterator_GetNode


    function ExceptionContextStackIterator_GetContext(this) result(aContext)
    !-----------------------------------------------------------------
    !< Return a pointer to the Context on the top of the stack
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t),  intent(IN) :: this     ! Context Stack Iterator
        class(ExceptionContext),          pointer           :: aContext ! Current context in the iterator
    !-----------------------------------------------------------------
        nullify(aContext)
        if(.not. this%HasFinished()) then
            aContext => this%Current%GetContext()
        endif
    end function ExceptionContextStackIterator_GetContext


    subroutine ExceptionContextStackIterator_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the current context
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t), intent(IN)  :: this   ! Context Stack Iterator
        integer,                    optional,   intent(IN)  :: unit   ! Logic unit.
        character(*),               optional,   intent(IN)  :: prefix ! Prefixing string.
        integer,                    optional,   intent(OUT) :: iostat ! IO error.
        character(*),               optional,   intent(OUT) :: iomsg  ! IO error message.
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            call this%Current%Print(unit, prefix, iostat, iomsg)
        endif
    end subroutine ExceptionContextStackIterator_Print


    function ExceptionContextStackIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Return true if the Current pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t), intent(IN) :: this    ! Context Stack Iterator
        logical                                     :: hasFinished    ! Check if Current is associated
    !-----------------------------------------------------------------
        hasFinished = .not. associated(this%Current)
    end function ExceptionContextStackIterator_HasFinished


    subroutine ExceptionContextStackIterator_Free(this)
    !-----------------------------------------------------------------
    !< Free Context Stack Iterator 
    !-----------------------------------------------------------------
        class(ExceptionContextStackIterator_t), intent(INOUT) :: this ! Context Stack Iterator
    !-----------------------------------------------------------------
        nullify(this%Current)
    end subroutine ExceptionContextStackIterator_Free


end module ExceptionContextStack
