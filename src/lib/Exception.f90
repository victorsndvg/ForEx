module BaseException

USE iso_fortran_env, only : error_unit

implicit none

    type :: Exception
    private
        integer                       :: Code = 0
        character(len=:), allocatable :: Message
        character(len=:), allocatable :: File
        integer                       :: Line = 0
    contains
    private
        procedure, non_overridable, public :: Create     => Exception_Create
        procedure, non_overridable, public :: SetContext => Exception_SetContext
        procedure, non_overridable, public :: GetCode    => Exception_GetCode
        procedure, non_overridable, public :: GetMessage => Exception_GetMessage
        procedure,                  public :: Catch      => Exception_Catch
        procedure, non_overridable, public :: Free       => Exception_Free
        procedure, non_overridable, public :: Print      => Exception_Print
        final                              ::               Exception_Finalize
    end type

    interface Exception
        module procedure Exception_Constructor
    end interface

public :: Exception

contains

    function Exception_Constructor(Code, Message) result(anException)
    !-----------------------------------------------------------------
    !< Set code and message and return a exception
    !-----------------------------------------------------------------
        integer,          intent(in)    :: Code
        character(len=*), intent(in)    :: Message
        class(Exception), pointer       :: anException
    !-----------------------------------------------------------------
        allocate(anException)
        call anException%Create(Code = Code, Message = Message)
    end function Exception_Constructor


    subroutine Exception_Create(this, Code, Message)
    !-----------------------------------------------------------------
    !< Set code and message to the exception derived type
    !-----------------------------------------------------------------
        class(Exception), intent(inout) :: this
        integer,          intent(in)    :: Code
        character(len=*), intent(in)    :: Message
    !-----------------------------------------------------------------
        this%Code    = Code
        this%Message = Message
    end subroutine Exception_Create


    subroutine Exception_SetContext(this, File, Line)
    !-----------------------------------------------------------------
    !< Assign a context (file and line) to the exception
    !-----------------------------------------------------------------
        class(Exception), intent(inout) :: this
        character(len=*), intent(in)    :: File
        integer,          intent(in)    :: Line
    !-----------------------------------------------------------------
        this%File = File
        this%Line = Line
    end subroutine Exception_SetContext


    function Exception_GetCode(this) result(Code)
    !-----------------------------------------------------------------
    !< Return the error code of the exception
    !-----------------------------------------------------------------
        class(Exception), intent(in) :: this
        integer                      :: Code
    !-----------------------------------------------------------------
        Code = this%Code
    end function Exception_GetCode


    function Exception_GetMessage(this) result(Message)
    !-----------------------------------------------------------------
    !< Return the error message of the exception
    !-----------------------------------------------------------------
        class(Exception), intent(in)  :: this
        character(len=:), allocatable :: Message
    !-----------------------------------------------------------------
        Message = this%Message
    end function Exception_GetMessage


    subroutine Exception_Throw(this)
    !-----------------------------------------------------------------
    !< Automatic accion if an exception is throwed
    !< Empty in the base class
    !-----------------------------------------------------------------
        class(Exception), intent(in) :: this
    !-----------------------------------------------------------------
    end subroutine Exception_Throw


    subroutine Exception_Catch(this)
    !-----------------------------------------------------------------
    !< Automatic accion if an exception is catched
    !< Empty in the base class
    !-----------------------------------------------------------------
        class(Exception), intent(in) :: this
    !-----------------------------------------------------------------
    end subroutine Exception_Catch


    subroutine Exception_Free(this)
    !-----------------------------------------------------------------
    !< Free exception derived type
    !-----------------------------------------------------------------
        class(Exception), intent(inout) :: this
    !-----------------------------------------------------------------
        this%code = 0
        this%line = 0
        if(allocated(this%message)) deallocate(this%message)
        if(allocated(this%file))    deallocate(this%file)
    end subroutine Exception_Free


    subroutine Exception_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Exception derived type content
    !-----------------------------------------------------------------
        class(Exception),                 intent(IN)  :: this         !< Exception
        integer,      optional,           intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer,      optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:),       allocatable           :: prefd        !< Prefixing string.
        integer                                       :: unitd        !< logic unit
        integer                                       :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
        character(64)                                 :: ch           !< numeric string
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = ''; unitd = error_unit
        if (present(unit)) unitd = unit
        if (present(prefix)) prefd = prefix
        Write(ch,*) this%code
        write(unit=unitd,fmt='(A,$)', iostat=iostatd, iomsg=iomsgd) prefd//' '//trim(this%message)//' ('//trim(adjustl(ch))//') '
        if(allocated(this%File)) then
            Write(ch,*) this%line
            Write(unit=unitd,fmt=*, iostat=iostatd, iomsg=iomsgd) 'throwed in File: '//trim(this%file)//' (L'//trim(adjustl(ch))//')'
        else
            Write(unit=unitd,fmt=*, iostat=iostatd, iomsg=iomsgd) ''
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine Exception_Print


    subroutine Exception_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(Exception), intent(INOUT):: this                         !< Exception
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine Exception_Finalize


end module BaseException
