module BaseExceptionContext

USE iso_fortran_env, only : error_unit

implicit none

    type :: ExceptionContext
    private
        character(len=:), allocatable :: File
        integer                       :: Line = 0
    contains
    private
        procedure, non_overridable, public :: SetContext => ExceptionContext_SetContext
        procedure, non_overridable, public :: GetFile    => ExceptionContext_GetFile
        procedure, non_overridable, public :: GetLine    => ExceptionContext_GetLine
        procedure, non_overridable, public :: Free       => ExceptionContext_Free
        procedure, non_overridable, public :: Print      => ExceptionContext_Print
        final                              ::               ExceptionContext_Finalize
    end type


public :: ExceptionContext

contains

    subroutine ExceptionContext_SetContext(this, File, Line)
    !-----------------------------------------------------------------
    !< Assign a context (file and line) to the ExceptionContext
    !-----------------------------------------------------------------
        class(ExceptionContext), intent(inout) :: this
        character(len=*), intent(in)    :: File
        integer,          intent(in)    :: Line
    !-----------------------------------------------------------------
        this%File = File
        this%Line = Line
    end subroutine ExceptionContext_SetContext


    function ExceptionContext_GetFile(this) result(File)
    !-----------------------------------------------------------------
    !< Return a context file
    !-----------------------------------------------------------------
        class(ExceptionContext), intent(inout) :: this
        character(len=:), allocatable          :: File
    !-----------------------------------------------------------------
        File = this%File
    end function ExceptionContext_GetFile


    function ExceptionContext_GetLine(this) result(Line)
    !-----------------------------------------------------------------
    !< Return a context file
    !-----------------------------------------------------------------
        class(ExceptionContext), intent(inout) :: this
        integer                                :: Line
    !-----------------------------------------------------------------
        Line = this%Line
    end function ExceptionContext_GetLine


    subroutine ExceptionContext_Free(this)
    !-----------------------------------------------------------------
    !< Free ExceptionContext derived type
    !-----------------------------------------------------------------
        class(ExceptionContext), intent(inout) :: this
    !-----------------------------------------------------------------
        this%Line = 0
        if(allocated(this%File))    deallocate(this%File)
    end subroutine ExceptionContext_Free


    subroutine ExceptionContext_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print ExceptionContext derived type content
    !-----------------------------------------------------------------
        class(ExceptionContext),          intent(IN)  :: this         !< ExceptionContext
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
        Write(ch,*) this%line
        if(allocated(this%File)) then
            Write(unit=unitd,fmt=*, iostat=iostatd, iomsg=iomsgd) &
                    prefix//' File: '//trim(this%File)//' (L'//trim(adjustl(ch))//')'
        else
            Write(unit=unitd,fmt=*, iostat=iostatd, iomsg=iomsgd) &
                    prefix//' File: - (L'//trim(adjustl(ch))//')'
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ExceptionContext_Print


    subroutine ExceptionContext_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ExceptionContext), intent(INOUT):: this                         !< ExceptionContext
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ExceptionContext_Finalize


end module BaseExceptionContext
