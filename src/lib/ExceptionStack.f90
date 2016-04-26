
module ExceptionStack

USE iso_fortran_env, only : error_unit
USE ExceptionStackNode
USE BaseException

implicit none
private
save

    type :: ExceptionStackIterator_t
    private
        class(ExceptionStack_t),     pointer :: Stack    => null()
        class(ExceptionStackNode_t), pointer :: Current  => null()
    contains
        procedure, non_overridable :: Init        => ExceptionStackIterator_Init
        procedure, non_overridable :: Next        => ExceptionStackIterator_Next
        procedure, non_overridable :: Get         => ExceptionStackIterator_Get
        procedure, non_overridable :: Catch       => ExceptionStackIterator_Catch
        procedure, non_overridable :: hasCatched  => ExceptionStackIterator_hasCatched
        procedure, non_overridable :: Del         => ExceptionStackIterator_Del
        procedure, non_overridable :: Print       => ExceptionStackIterator_Print
        procedure, non_overridable :: Free        => ExceptionStackIterator_Free
        procedure, non_overridable :: HasFinished => ExceptionStackIterator_HasFinished
    end type

    type :: ExceptionStack_t
    private
        integer                              :: Frame = 0
        logical                              :: hasCatched = .false.
        class(ExceptionStackNode_t), pointer :: Root  => null()
    contains
    private
        procedure, non_overridable         :: HasRoot          => ExceptionStack_HasRoot
        procedure, non_overridable         :: SetRoot          => ExceptionStack_SetRoot
        procedure, non_overridable         :: NullifyRoot      => ExceptionStack_NullifyRoot
        procedure, non_overridable         :: DeallocateRoot   => ExceptionStack_DeallocateRoot
        procedure, non_overridable, public :: OpenFrame        => ExceptionStack_OpenFrame
        procedure, non_overridable, public :: CloseFrame       => ExceptionStack_CloseFrame
        procedure, non_overridable, public :: isEmpty          => ExceptionStack_isEmpty
        procedure, non_overridable, public :: Pop              => ExceptionStack_Pop
        procedure, non_overridable, public :: Top              => ExceptionStack_Top
        procedure, non_overridable, public :: Push             => ExceptionStack_Push
        procedure, non_overridable, public :: BackTrace        => ExceptionStack_BackTrace
        procedure, non_overridable, public :: Clean            => ExceptionStack_Clean
        procedure, non_overridable, public :: Free             => ExceptionStack_Free
        final                              ::                     ExceptionStack_Finalize 
    end type

    type(ExceptionStack_t)         :: TheExceptionStack
    type(ExceptionStackIterator_t) :: TheExceptionStackIterator

public :: TheExceptionStack
public :: TheExceptionStackIterator

contains


    subroutine ExceptionStack_SetRoot(this, Root)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionStack_t),              intent(INOUT) :: this   !< Exception Stack
        class(ExceptionStackNode_t), pointer, intent(IN)    :: Root   !< Stack Node correspoing to the head of the list
    !-----------------------------------------------------------------
        this%Root => Root
    end subroutine ExceptionStack_SetRoot


    subroutine ExceptionStack_OpenFrame(this)
    !-----------------------------------------------------------------
    !< Increse the TRY hierarchy Frame
    !-----------------------------------------------------------------
        class(ExceptionStack_t),              intent(INOUT) :: this   !< Exception Stack
    !-----------------------------------------------------------------
        this%Frame = this%Frame+1
        this%hasCatched = .false.
    end subroutine ExceptionStack_OpenFrame


    subroutine ExceptionStack_CloseFrame(this)
    !-----------------------------------------------------------------
    !< Increse the TRY hierarchy Frame
    !-----------------------------------------------------------------
        class(ExceptionStack_t),              intent(INOUT) :: this   !< Exception Stack
    !-----------------------------------------------------------------
        this%Frame = this%Frame-1
        this%hasCatched = .false.
        if(this%Frame==0) then
            call this%Backtrace()
            call this%Free()
        endif
    end subroutine ExceptionStack_CloseFrame


    subroutine ExceptionStack_Pop(this)
    !-----------------------------------------------------------------
    !< Deallocate to the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionStack_t),             intent(INout) :: this    !< Exception Stack
        class(ExceptionStackNode_t), pointer               :: Root    !< Stack Node correspoing to the head of the list
    !-----------------------------------------------------------------
        nullify(Root)
        if(this%HasRoot()) then
            Root => this%Root
            this%Root => Root%GetNext()
            call Root%Free()
            deallocate(Root)
        endif
    end subroutine ExceptionStack_Pop


    function ExceptionStack_Top(this) result(anException)
    !-----------------------------------------------------------------
    !< Return a pointer to the exception on the top of the stack
    !-----------------------------------------------------------------
        class(ExceptionStack_t),            intent(IN) :: this        !< Exception Stack
        class(Exception),          pointer             :: anException !< Exception in the top of the stack
    !-----------------------------------------------------------------
        nullify(anException)
        if(this%HasRoot()) then
            anException => this%Root%GetException()
        endif
    end function ExceptionStack_Top


    function ExceptionStack_isEmpty(this) result(isEmpty)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionStack_t), intent(IN) :: this                   !< Exception Stack
        logical                             :: isEmpty                !< Check if Root is associated
    !-----------------------------------------------------------------
        isEmpty = .not. this%HasRoot()
    end function ExceptionStack_isEmpty


    function ExceptionStack_HasRoot(this) result(HasRoot)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionStack_t), intent(IN) :: this                   !< Exception Stack
        logical                             :: hasRoot                !< Check if Root is associated
    !-----------------------------------------------------------------
        hasRoot = associated(this%Root)
    end function ExceptionStack_HasRoot


    subroutine ExceptionStack_NullifyRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionStack_t), intent(INOUT) :: this                !< Exception Stack
    !-----------------------------------------------------------------
        nullify(this%Root)
    end subroutine ExceptionStack_NullifyRoot


    subroutine ExceptionStack_DeallocateRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ExceptionStack_t), intent(INOUT) :: this                !< Exception Stack
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            call this%Root%Free()
            deallocate(this%Root)
        endif
    end subroutine ExceptionStack_DeallocateRoot


    subroutine ExceptionStack_Push(this, anException, file, line)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ExceptionStack_t),    intent(INOUT) :: this             !< Exception Stack
        class(Exception),           intent(IN)    :: anException      !< Exception
        character(len=*),           intent(IN)    :: file             !< File where the exception occurs
        integer,                    intent(in)    :: line             !< Line where the exception occurs
        class(ExceptionStackNode_t), pointer      :: NewStackNode     !< New Stack Node
    !-----------------------------------------------------------------
        if(this%Frame>0) then
            allocate(NewStackNode)
            call NewStackNode%SetException(anException, File=file, Line=Line)
            call NewStackNode%SetNext(this%Root)
            call NewStackNode%NullifyPrevious()
            if(associated(this%Root)) call this%Root%SetPrevious(NewStackNode)
            this%Root => NewStackNode
        else
            call anException%Catch()
        endif
    end subroutine ExceptionStack_Push


    subroutine ExceptionStack_Clean(this)
    !-----------------------------------------------------------------
    !< Clean the stack
    !-----------------------------------------------------------------
        class(ExceptionStack_t),             intent(INOUT) :: this    !< Exception Stack
        class(ExceptionStackNode_t), pointer               :: Current !< Current Parameter List Node
        class(ExceptionStackNode_t), pointer               :: Next    !< Next Parameter List Node
    !-----------------------------------------------------------------
        do while(this%HasRoot()) 
            Next => this%Root%GetNext()
            call this%Root%Free()
            call this%DeallocateRoot()
            call this%SetRoot(Root=Next)
        enddo
    end subroutine ExceptionStack_Clean


    subroutine ExceptionStack_Free(this)
    !-----------------------------------------------------------------
    !< Free the derived type
    !-----------------------------------------------------------------
        class(ExceptionStack_t),             intent(INOUT) :: this     !< Exception Stack
        class(ExceptionStackNode_t), pointer               :: Current  !< Current Parameter List Node
        class(ExceptionStackNode_t), pointer               :: Next     !< Next Parameter List Node
    !-----------------------------------------------------------------
        call this%Clean()
        this%Frame  = 0
    end subroutine ExceptionStack_Free


    subroutine ExceptionStack_BackTrace(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the parameter list
    !-----------------------------------------------------------------
        class(ExceptionStack_t),              intent(IN)  :: this     !< Exception Stack
        integer,                    optional, intent(IN)  :: unit     !< Logic unit.
        character(*),               optional, intent(IN)  :: prefix   !< Prefixing string.
        integer,                    optional, intent(OUT) :: iostat   !< IO error.
        character(*),               optional, intent(OUT) :: iomsg    !< IO error message.
        character(len=:), allocatable                     :: prefd    !< Prefixing string.
        integer                                           :: unitd    !< IO error.
        integer                                           :: iostatd  !< IO error.
        character(500)                                    :: iomsgd   !< Temporary variable for IO error message.
        type(ExceptionStackIterator_t)                    :: iterator !< Stack iterator
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = ''; unitd = error_unit
        if (present(unit)) unitd = unit
        if (present(prefix)) prefd = prefix
        call Iterator%Init(this)
        do while(.not. Iterator%HasFinished())
                call Iterator%Catch()
                call Iterator%Print(unit=unitd, prefix=prefd//'[BackTrace] ', iostat=iostatd, iomsg=iomsgd )
                call Iterator%Next()
        enddo
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ExceptionStack_BackTrace


    subroutine ExceptionStack_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ExceptionStack_t), intent(INOUT):: this                  !< Exception Stack
    !-----------------------------------------------------------------
        call this%Backtrace()
        call this%Free()
    end subroutine ExceptionStack_Finalize


!---------------------------------------------------------------------
!< Exception Stack Iterator procedures
!---------------------------------------------------------------------

    subroutine ExceptionStackIterator_Init(this, ExceptionStack)
    !-----------------------------------------------------------------
    !< Initialize Exception Stack Iterator to the root of the given
    !< Exception Stack
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t), intent(INOUT) :: this           !< Exception Stack Iterator
        class(ExceptionStack_t), target, intent(IN)    :: ExceptionStack !< Exception Stack
    !-----------------------------------------------------------------
        this%Stack => ExceptionStack
        this%Current => this%Stack%Root
    end subroutine ExceptionStackIterator_Init


    subroutine ExceptionStackIterator_Next(this)
    !-----------------------------------------------------------------
    !< Return a true if the Root pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t), intent(INOUT) :: this        !< Exception Stack Iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) this%Current => this%Current%GetNext()
    end subroutine ExceptionStackIterator_Next


    function ExceptionStackIterator_Get(this) result(anException)
    !-----------------------------------------------------------------
    !< Return a pointer to the exception on the top of the stack
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t),  intent(IN) :: this          !< Exception Stack Iterator
        class(Exception),          pointer           :: anException   !< Current Exception in the iterator
    !-----------------------------------------------------------------
        nullify(anException)
        if(.not. this%HasFinished()) then
            anException => this%Current%GetException()
        endif
    end function ExceptionStackIterator_Get


    subroutine ExceptionStackIterator_Catch(this)
    !-----------------------------------------------------------------
    !<  Mark this try Frame as catched 
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t),  intent(INOUT) :: this       !< Exception Stack Iterator
        class(Exception), pointer                       :: anException!< Exception
    !-----------------------------------------------------------------
        if(.not. this%hasFinished()) then
            this%Stack%hasCatched = .true.
            anException => this%Current%GetException()
            if(associated(anException)) call anException%Catch()
        endif
    end subroutine ExceptionStackIterator_Catch


    function ExceptionStackIterator_hasCatched(this) result(hasCatched)
    !-----------------------------------------------------------------
    !<  Check if a exceptions was already catched in this try Frame
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t),  intent(INOUT) :: this       !< Exception Stack
        logical                                         :: hasCatched !< Check if an exception was catched in this Frame
    !-----------------------------------------------------------------
        hasCatched = .False.
        if(associated(this%Stack)) hasCatched = this%Stack%hasCatched
    end function ExceptionStackIterator_hasCatched


    subroutine ExceptionStackIterator_Del(this)
    !-----------------------------------------------------------------
    !< Remove the current exception
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t),  intent(INOUT) :: this       !< Exception Stack Iterator
        class(ExceptionStackNode_t), pointer            :: Next       !< Next Exception in the iterator
        class(ExceptionStackNode_t), pointer            :: Previous   !< Current Exception in the iterator
    !-----------------------------------------------------------------
        nullify(Next)
        nullify(Previous)
        if(.not. this%HasFinished()) then
            Previous => this%Current%GetPrevious()
            Next => this%Current%GetNext()
            if(associated(Next)) then 
                if(associated(Previous)) then
                    call Previous%SetNext(Next)
                    call Next%SetPrevious(Previous)
                else
                    call Next%NullifyPrevious()
                    call this%Stack%SetRoot(Next)
                endif
            else
                if(associated(Previous)) then
                    call Previous%NullifyNext()
                else
                    call this%Stack%NullifyRoot()
                endif
            endif
            call this%Current%Free()
            deallocate(this%Current)
            if(associated(Next)) then
                this%Current => Next
            else
                nullify(this%Current)
            endif
        endif
    end subroutine ExceptionStackIterator_Del


    subroutine ExceptionStackIterator_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Return a pointer to the exception on the top of the stack
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t),      intent(IN)  :: this     !< Exception Stack Iterator
        integer,                    optional, intent(IN)  :: unit     !< Logic unit.
        character(*),               optional, intent(IN)  :: prefix   !< Prefixing string.
        integer,                    optional, intent(OUT) :: iostat   !< IO error.
        character(*),               optional, intent(OUT) :: iomsg    !< IO error message.
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            call this%Current%Print(unit, prefix, iostat, iomsg)
        endif
    end subroutine ExceptionStackIterator_Print


    function ExceptionStackIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Return a true if the Current pointer is associated
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t), intent(IN) :: this           !< Exception Stack Iterator
        logical                                     :: hasFinished    !< Check if Current is associated
    !-----------------------------------------------------------------
        hasFinished = .not. associated(this%Current)
    end function ExceptionStackIterator_HasFinished


    subroutine ExceptionStackIterator_Free(this)
    !-----------------------------------------------------------------
    !< Initialize Exception Stack Iterator to the root of th
    !< given Exception Stack
    !-----------------------------------------------------------------
        class(ExceptionStackIterator_t), intent(INOUT) :: this        !< Exception Stack Iterator
    !-----------------------------------------------------------------
        nullify(this%Current)
    end subroutine ExceptionStackIterator_Free


end module ExceptionStack
